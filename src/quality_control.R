library(tidyverse)
library(hrbrthemes)

qc_dta <- read_tsv("data/nextclade.tsv") %>%
  rename(strain = seqName)

genome_size = 29926

# length distribution and coverage
total_Ns <- qc_dta %>%
  select(strain,totalMissing) %>%
  mutate(percentage_complete = ((genome_size - totalMissing)/genome_size * 100))
# Import the Ct scores 
ct_scores <- read_csv("data/Ct_scores_2.csv") %>%
  select(-c(run_id,sample_id)) %>%
  distinct()

prop_complete <- qc_dta %>%
  select(strain,totalMissing) %>%
  mutate(percentage_complete = ((genome_size - totalMissing)/genome_size * 100))

Ct_len_dta <- left_join(ct_scores,prop_complete,by="strain") %>%
  distinct()

ct_vs_genome_plot <- ggplot(Ct_len_dta,aes(percentage_complete,Ct_score)) +
  geom_point(fill='gold2',size=3,color='black',shape=21) +
  geom_smooth(method=lm,se=T, colour='black') +
  labs(x="Genome completeness (%)",y="Cycle threshold (Ct)",
       title = "Cycle threshold score vs No. of ambiguous nucleotides in the genome") +
  theme_ipsum()


genome_completeness_plot <- ggplot(total_Ns,aes(reorder (strain,-percentage_complete),percentage_complete)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 80,colour ="red") +
  labs(y = "Proportion complete",x = "",title = "Percentage of complete genomes ") +
  theme_ipsum() +
  theme(axis.text.x = element_blank())

completeness_figure <-  genome_completeness_plot + ct_vs_genome_plot

ggsave(plot = completeness_figure, filename = "figures/genome_completeness_plot.pdf",
       width = 30, height = 10,units = "in",limitsize = FALSE,device = cairo_pdf)

# Mutations 

# tabulate important mutations 
qc_dta_2 <- qc_dta %>%
  filter(totalMissing <= 4000)

mutations_scatter_plot <- ggplot(qc_dta_2,aes(totalMutations,totalMissing)) +
  geom_point(size = 2, colour = "brown") +
  #scale_x_discrete() +
  labs(y = "No. of ambigous bases",
       x="No. of mutations relative to the reference",
       tag = "A.") +
  theme_ipsum() +
  theme(
    legend.title = element_blank(),
    legend.position = "right",
    legend.key.size = unit(0.3, "cm"),
    legend.spacing.x = unit(0.3, 'cm'),
    legend.text = element_text(size = 10)
  )

mutations_dta <- qc_dta_2 %>%
  select(strain,substitutions) %>%
  mutate(D614G = if_else(str_detect(substitutions,"A23403G"),"614G","614D"),
         P314L = if_else(str_detect(substitutions,"C14408T"),"314L","314P"),
         P970L = if_else(str_detect(substitutions,"C16376T"),"970L","970P"),
         R203K = if_else(str_detect(substitutions,"G28881A,G28882A,G28883C"),"R203K","G204R")
  ) %>%
  select(-substitutions)

mutation_spike <- qc_dta %>%
  select(strain, aaSubstitutions,substitutions) %>%
  mutate(
    spike_mutation = if_else(str_detect(aaSubstitutions,"S:"),aaSubstitutions,"none")
  ) %>%
  filter(spike_mutation != "none") %>%
  separate(col = spike_mutation,sep = ",",extra = "merge",into = c("ORF1a","ORF1b","S","ORF3a","E","M","ORF6","ORF7a",
                                                                   "ORF8","ORF9b","N"))

# convert to long format
mutations_dta_long <- mutations_dta %>%
  gather(mutations_group,mutation_type,D614G:R203K) %>%
  group_by(mutations_group,mutation_type) %>%
  summarise(mutation_count = n()) %>%
  mutate(proportion = mutation_count  / sum(mutation_count) * 100)


#######################################
root_tip_divergence <- read_delim("data/root-tip-divergence.txt",delim = "\t") %>%
  mutate(mutations = round(distance * 29926))

root_tip_divergence_plot <- ggplot(root_tip_divergence,aes(date,mutations)) +
  geom_point(size=2,colour = "grey60") +
  geom_smooth(method='lm', formula= y~x) +
  labs(title = "Root-tip divergence") +
  theme_ipsum()


root_tip_divergence_plot <- ggplot(root_tip_divergence,aes(date,mutations)) +
  geom_point(fill='grey',size=3,color='black',shape=21) +
  #scale_fill_manual(values=c('goldenrod1', 'red', 'darkorange'))+
  geom_smooth(method=lm,se=T, colour='black')+
  theme_ipsum() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=15))+
  theme(axis.text.x =element_text(size=15))+
  theme(axis.title=element_text(size=15))+
  #scale_x_date(date_labels = "%d-%b",date_breaks = "2 weeks") +
  labs( y = "Distance (No. of Mutations)", x = "", title = "Root-tip distance",tag = "a") +
  scale_y_continuous()

mutation_colours <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f')

mutations_types_plot <- ggplot(mutations_dta_long,aes(mutations_group,proportion,group=mutation_type)) +
  geom_col(aes(fill=mutation_type),position = "fill",width = 0.5) +
  scale_fill_manual(values = mutation_colours) +
  labs(x="",tag = "b") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.3, "cm"),
        legend.spacing.x = unit(0.3, 'cm'),
        legend.text = element_text(size = 10)
  )


supp_fig_2 <- root_tip_divergence_plot + mutations_types_plot


figure4 <- (lineages_per_week_by_county_plot / lineages_per_week_by_case_history_plot)

ggsave(plot = supp_fig_2 , filename = "figures/supplementary figure 2.pdf",
       width = 10, height = 7,units = "in",limitsize = FALSE,device = cairo_pdf)


ggsave(plot = figure4, filename = "figures/figure4.pdf",
       width = 30, height = 15,units = "in",limitsize = FALSE,device = cairo_pdf)

