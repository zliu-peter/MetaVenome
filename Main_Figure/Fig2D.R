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

### ========================== 2D
# 6.36 x 2.94 cairo

plot_2D <- ggplot(data_2D_AT %>% filter(pep_rank=="parent" | pep_rank=="mature") %>% 
                    mutate(pep_rank="AT_parent_mature"), 
                 aes(number_c, fill=pep_rank)) +
  geom_bar() +
  theme_classic() +
  theme(panel.grid.major=element_blank(), 
        panel.grid.minor=element_blank(),
        legend.title=element_blank(),
        axis.title.x=element_text(color="black", size=18),
        axis.title.y=element_text(color="black", size=18),
        axis.text.x=element_text(color="black", size=14),
        axis.text.y=element_text(color="black", size=14),
        legend.position="none") +
  xlab("# of Cysteines") +
  ylab("# of Peptides") +
  annotate("text", x=14, y=2350, label= "0: 8%\nOdd: 22%\nEven: 70%", hjust = 0, size=3) +
  scale_x_continuous(breaks=seq(0, 20, by=2)) +
  scale_fill_manual(values="gray50", labels="AT_parent_mature")

plot_2D
