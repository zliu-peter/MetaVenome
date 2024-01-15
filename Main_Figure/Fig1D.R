rm(list=ls())
library(tidyverse)
library(openxlsx)
library(readxl)
library(scales)
library(magrittr)
library(grid)
library(ComplexHeatmap)
library(igraph)
library(foreach)
library(doParallel)
library(mgsub)
library(data.table)
library(qgraph)
library(stringr)
library(ggsignif)
library(plotly)
library(htmlwidgets)
library(viridisLite)
library(reshape2)
getwd()
load("my_workspace.RData")

# Figure 1D ==================== 6x7 portrait
new_names <- c("Enzyme", "ECM", "Cytokine", "Hormone", "Growth factor", "Heparin binding", "Protease inhibitor", "Neuropeptide", "Receptor", "IgSF")
my_colors <- viridisLite::viridis(n = 4, alpha = 1, option = "E")

plot_1D <- 
  ggplot(data_1D %>% filter(Group!="misc"), aes(x = value, y = Group, fill = Peptides)) + 
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Peptides, scales = "free_x") +
  labs(x = "# of peptides", y = NULL, fill = NULL) +
  scale_fill_manual(values = my_colors) +
  scale_y_discrete(labels = rev(str_wrap(new_names, width = 13))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 10),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(color = "black", size = 12),
        axis.text.x = element_text(color = "black", size = 12),
        legend.position="none")

plot_1D
