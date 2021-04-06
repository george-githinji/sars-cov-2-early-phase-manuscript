library(tidyverse)
library(ggtree)
library(ape)
library(scales)
library(treeio)
library(patchwork)

county_colours  <- c("#ffc500","#c710a8","#3ea628","#0f12d6","red","black","grey")

lineage_colours <- c("#b34c00","#705ce5","#52c63f","#d265ec","#7ddc62","#d40090","#2a8400","#f61a74","#60de8c",
                      "#df014c","#01c894","#ff5c3b","#01cafc","#a0210d","#9ba4ff","#bcaa00","#85307a","#ffb332",
                      "#01639f","#a78400","#ffa1ed","#008751","#ff8a64","#a6d385","#d0a3d3","#495914",
                      "#ff988e","#684f04","#ffaf71","#843f20","#8d5600","#9a5b50")

source_colours  <- c("#00bfff","#d95f02","#7570b3","grey")

local_nexus_tree_path <- "trees/local_timetree.nexus"
local_tree <- read.nexus(local_nexus_tree_path)

baseline_information <- local_metadata %>%
  select(strain,pangolin_lineage,division,source) %>%
  rename(taxa = strain,
         county = division)

cols <- scale_color(beast_tree,by = "height")

county_plot <- ggtree(local_tree ,mrsd = "2020-07-31")  %<+% baseline_information + 
  geom_tippoint(size = 3,aes(color = county)) +
  scale_color_manual(values = county_colours) +
  scale_x_continuous() +
  labs(subtitle = "",tag = "A") +
  theme_tree2()

lineage_plot <- ggtree(local_tree,mrsd = "2020-07-31",colour="grey") %<+% baseline_information + 
  geom_tippoint(size = 3,aes(color=pangolin_lineage)) +
  scale_color_manual(values = lineage_colours) +
  scale_x_continuous() +
  labs(subtitle = "",tag = "B") +
  theme_tree2()

source_plot <- ggtree(local_tree,mrsd = "2020-07-31") %<+% baseline_information + 
  geom_tippoint(size = 3,aes(color=source)) +
  scale_color_manual(values = source_colours) +
  scale_x_continuous() +
  labs(subtitle = "",tag = "C") +
  theme_tree2()


clade_19B <- "trees/clade_19B_tree.nexus"
clade_19B_tree <- read.nexus(clade_19B)

clade_19B_plot <- ggtree(clade_19B_tree,mrsd = "2020-07-20",colour="grey") %<+% baseline_information + 
  geom_tippoint(size = 3,aes(color=county)) +
  geom_tiplab(size = 3) +
  scale_color_manual(values = lineage_colours) +
  scale_x_continuous() +
  labs(subtitle = "",tag = "") +
  theme_tree2()

sub_clade_20A <- "trees/B.1_subclade_tree.nexus"
sub_clade_20A_tree <- read.nexus(sub_clade_20A)
subclade_20A_meta <- read_tsv("trees/B.1_subclade_metadata.tsv")

clade_19B_plot <- ggtree(clade_19B_tree,mrsd = "2020-07-20",colour="grey") %<+% subclade_20A_meta + 
  geom_tippoint(size = 3) +
  #geom_tiplab(size = 3) +
  #scale_color_manual(values = lineage_colours) +
  scale_x_continuous() +
  labs(subtitle = "",tag = "") +
  theme_tree2()

figure4 <- county_plot + lineage_plot  + source_plot +
  plot_layout(guides = "collect")

figure 
ggsave(plot = figure4, filename = "figures/figure4.pdf",
       width = 20, height = 18,units = "in",limitsize = FALSE,device = cairo_pdf)





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
 
  geom_smooth(method=lm,se=T, colour='black')+
  theme_ipsum() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text=element_text(size=15))+
  theme(axis.text.x =element_text(size=15, angle=90))+
  theme(axis.title=element_text(size=15))+
  ylab("Distance (No. of Mutations)") +
  xlab("Date") +
  scale_y_continuous()+
  ggtitle('Root-tip distance')


ggsave(plot = root_tip_divergence_plot, filename = "figures/root_to_tip.pdf",
       width = 12, height = 10,units = "in",limitsize = FALSE,device = cairo_pdf)
