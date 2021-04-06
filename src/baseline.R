library(tidyverse)
library(table1)
library(anytime)
library(lubridate)
library(scales)
library(hrbrthemes)
library(patchwork)

county_colours  <- c("#ffc500","#c710a8","#3ea628","#0f12d6")
lineage_colours <- c("#b34c00","#705ce5","#52c63f","#d265ec","#7ddc62","#d40090","#2a8400","#f61a74","#60de8c",
                     "#df014c","#01c894","#ff5c3b","#01cafc","#a0210d","#9ba4ff","#bcaa00","#85307a","#ffb332",
                     "#01639f","#a78400","#ffa1ed","#008751","#ff8a64","#a6d385","#d0a3d3","#495914",
                     "#ff988e","#684f04","#ffaf71","#843f20","#8d5600","#9a5b50")
source_colours  <- c("#00bfff","#d95f02","#7570b3","grey")

mutation_colours <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f')

# Set a random number seed
set.seed(123)

full_metada <- read_tsv("data/full_metadata.tsv")

travel_history <- read_csv("data/travel_history.csv")

# Import the Ct scores 
ct_scores <- read_csv("data/Ct_scores.csv") %>%
  select(-c(run_id,sample_id)) %>%
  distinct()

# import repeats data
repeats <- read_csv("data/repeats_data.csv")

# import the global dataset
main_dta <- read_tsv("data/metadata.tsv")

local_metadata <- read_tsv("data/local_metadata.tsv")

baseline_coast_table <- table1(~sex + age + age_category + source +
                                 symptoms | coastal_region,
                               data = local_metadata,
                               overall="Total",
                               topclass="Rtable1-zebra")

supplementary_table_2 <- table1(~sex + age + age_category + source +
                                 symptoms | division,
                               data = local_metadata,
                               overall="Total",
                               topclass="Rtable1-zebra")


################################################
## Lineage plots
################################################

first_observation <- local_metadata %>%
  select(strain,division,date,pangolin_lineage,source,symptoms) %>%
  group_by(pangolin_lineage) %>%
  filter(date == min(date))

first_observations_table <- table1(~ source + factor(date) + symptoms | pangolin_lineage,
       data = first_observation,
       overall="Total",
       topclass="Rtable1-zebra",
       transpose = TRUE,droplevels = TRUE)

total_lineages <- local_metadata %>%
  select(strain,pangolin_lineage,division) %>%
  group_by(division,pangolin_lineage,) %>%
  summarize(total = n())
  
per_lineage_proportion <- local_metadata %>%
  select(strain,pangolin_lineage) %>%
  group_by(pangolin_lineage) %>%
  summarise(lineages_count = n()) %>%
  mutate(Total = sum(lineages_count),
    proportion = lineages_count / sum(lineages_count) * 100)

lineages_per_source <- local_metadata %>%
  select(strain,pangolin_lineage,source) %>%
  group_by(pangolin_lineage,source) %>%
  summarise(lineages_count = n()) %>%
  mutate(Total = sum(lineages_count),
         proportion = lineages_count / sum(lineages_count) * 100)

# tabulate lineages per week per county
lineages_per_week <- local_metadata %>%
  select(strain,pangolin_lineage,date,division,source) %>%
  group_by(division,pangolin_lineage,week = cut.Date(as.Date(date),"week")) %>%
  summarise(lineages_count = n()) %>%
  mutate(proportion = lineages_count / sum(n()),
         week = as.Date(week,format = "%Y-%m-%d"))

lineages_per_week_bar_total <- local_metadata %>%
  select(strain,pangolin_lineage,date,division,source) %>%
  group_by(week = cut.Date(as.Date(date),"week"),pangolin_lineage,division) %>%
  summarise(lineages_count = n()) %>%
  mutate(proportion = lineages_count / sum(n()),
         week = as.Date(week,format = "%Y-%m-%d"))

lineages_per_week_total <- local_metadata  %>%
  group_by(division,week = cut.Date(as.Date(date),"week")) %>%
  summarise(week_count = n())

# A plot of lineages per county
lineages_per_week_by_county_plot <- ggplot(lineages_per_week,aes(week,proportion)) +
  geom_bar(aes(fill=pangolin_lineage),position = "fill",stat = "identity") +
  geom_text(data = lineages_per_week_total,aes(week,)) +
  scale_x_date(breaks = "14 days",labels = date_format("%d-%b")) +
  scale_fill_manual(values = lineage_colours) +
  labs(x="week starting on",tag = "A.") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90),
        legend.title = element_blank(),
        legend.position = "right",
        legend.key.size = unit(0.3, "cm"),
        legend.spacing.x = unit(0.3, 'cm'),
        legend.text = element_text(size = 10)) +
  facet_wrap(~division,nrow = 2,scales = "free_y")

# A plot of lineages per case history
lineages_per_week_by_case_history_plot <- ggplot(lineages_per_week,aes(week,proportion)) +
  geom_bar(aes(fill=source),position = "fill",stat = "identity") +
  scale_x_date(breaks = "14 days",labels = date_format("%d-%b")) +
  scale_fill_manual(values = source_colours) +
  labs(x="week starting on",tag = "B.") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90),
        legend.title = element_blank(),
        legend.position = "right",
        legend.key.size = unit(0.3, "cm"),
        legend.spacing.x = unit(0.3, 'cm'),
        legend.text = element_text(size = 10)) +
  facet_wrap(~division,nrow = 2,scales = "free_y")


# Draw the plot
figure4 <- (lineages_per_week_by_county_plot / lineages_per_week_by_case_history_plot)

ggsave(plot = figure4, filename = "figures/figure3.pdf",
       width = 30, height = 15,units = "in",limitsize = FALSE,device = cairo_pdf)


############

mutations_scatter_plot <- ggplot(next_clade_dta,aes(totalMutations,totalMissing)) +
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

mutations_types_plot <- ggplot(mutations_dta_long,aes(mutations_group,proportion,group=mutation_type)) +
  geom_col(aes(fill=mutation_type),position = "fill",width = 0.5) +
  scale_fill_manual(values = mutation_colours) +
  labs(x="",tag = "C.") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.3, "cm"),
        legend.spacing.x = unit(0.3, 'cm'),
        legend.text = element_text(size = 10)
  )

