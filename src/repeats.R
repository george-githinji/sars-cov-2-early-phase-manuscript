library(tidyverse)
library(reshape2)
library(scales)
library(hrbrthemes)

lineage_colours <- c('#e41a1c','#377eb8','#1a0000','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999')

repeats <- local_metadata %>%
  select(date,strain,pangolin_lineage,repeat_id,division,location,symptoms,age,sex,age_category) %>%
  filter(!is.na(repeat_id),
         !repeat_id %in% c("R_8","R_13")) %>%
  mutate(date  = as.Date(date, format("%d-%b-%y"))) %>%
  arrange(date)

shedding_plot <- ggplot(repeats,aes(date,y=reorder(repeat_id, date))) +
  geom_point(aes(colour = pangolin_lineage),size = 3) +
  geom_line(aes(colour = pangolin_lineage))  +
  scale_colour_manual(values = lineage_colours) +
  scale_x_date(breaks ="2 days", labels = date_format("%d-%m")) +
  labs(x="", y="") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90))

figureS1 <- shedding_plot

ggsave(plot = figureS1, filename = "~/Dropbox/sars_cov2_genome_manu/revision/figures/figure_S1.pdf",
       width = 12, height = 10,units = "in",limitsize = FALSE,device = cairo_pdf)

# write repeat sequences to file
filter_repeats <- function(repeat_label,dataset) {
  dataset %>%
    filter(repeat_id == repeat_label) %>%
    select(seq_id)
}

# write the names of each repeat file
for (i in 1:19) {
  query = paste("R_",i,sep = "")
  dta <- filter_repeats(query,repeats_dta)
  write_csv(dta,paste("~/Dropbox/sars_cov2_genome_manu/repeats/files_names/",query,".txt",sep = ""),col_names = FALSE)
}
