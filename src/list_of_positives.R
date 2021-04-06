library(tidyverse)
library(tidyverse)
library(scales)
library(patchwork)
library(lubridate)
library(vistime)
library(hrbrthemes)

# define colours for the counties
county_colours  <- c("#ffc500","#c710a8","#3ea628","#0f12d6","grey","black")

ph_dta_country <- read_csv("data/country_health_measures.csv")
ph_dta_county <- read_csv("data/county_health_measures.csv")

testing_data <- read_csv("data/data_Figure1_04Aug2020.csv") %>%
  mutate(date_release= as.Date(date_release, format="%d/%m/%Y"))


tests_and_positives_dta <- read_csv("data/cumulative_totals.csv")

per_county_positives <- tests_and_positives_dta %>%
  mutate(date_tested = as.Date(datetested, format="%d/%m/%Y")) %>%
  select(-datetested) %>%
  pivot_longer(!date_tested, names_to = "entries",values_to = "count") %>%
  separate(entries,into = c("county","test_results"),sep = "_") %>%
  group_by(county,test_results) %>%
  arrange(date_tested)

tests <- per_county_positives %>%
  filter(test_results == "tests")

positives <- per_county_positives %>%
  filter(test_results == "pos")

ggplot() + 
  geom_line(data=tests,aes(date_tested,count,colour = county)) +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90))


total_tests_plot <- ggplot() +
  geom_line(data=tests,aes(date_tested,count,colour = county),size =1) +
  geom_rect(data = ph_dta_county,aes( xmin= start_date,xmax=end_date,
                                       ymin = ymin_position,ymax = ymin_position + 800,
                                       fill = jurisdiction),alpha = 0.3,show.legend = FALSE) +
  geom_text(data = ph_dta_county,aes(x = start_date + 30,
                                      y=ymin_position + 400,
                                      label = public_health_measure
  ),size = 3,alpha = 0.8,colour = "brown") +
  scale_x_date(date_breaks = "14 days",labels=date_format("%d-%b")) +
  scale_colour_manual(values = county_colours) +
 # scale_colour_manual(values = c("darkred","grey"),
 #                     labels = c("cummulative cases","cummulative tests")) +
  labs(x="", y="Number of samples",tag = "",subtitle = "") +
  theme_ipsum() +
  theme(legend.title = element_blank())

per_county_cummulative_plot <- ggplot(per_county_cummulative_dta, aes(x= date_release, y=value)) +
  geom_bar(stat="identity",aes(fill=cummulative),position = position_dodge())  +
  scale_x_date(date_breaks = "14 days",labels=date_format("%d-%b")) +
  scale_fill_manual(values=county_colours,
                    labels=c("Kilifi","Kwale","Mombasa","Taita Taveta"))  +
  facet_wrap(~cummulative,scales = "free_y",ncol = 1) +
  theme_ipsum() +
  labs(x="", y="Cummulative",
       tag = "b",
       subtitle = "") +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        #strip.background.x = element_blank(),
        #strip.background.y = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y.left =element_text(size = 10),
        legend.key.size = unit(0.25, "cm"),
        legend.spacing.x = unit(0.25, 'cm'),
        legend.text = element_text(size = 10),
        legend.title =element_blank(),
        legend.box.background = element_blank()) 


figure2 <- total_tests_plot 

ggsave(plot = figure2, filename = "figures/figure2.pdf",
       width = 10, height = 8,units = "in",limitsize = FALSE,device = cairo_pdf)

