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

# Extended Figure 2 ==================== 6x7 landscape

plot_Ex2 <- 
  ggplot(data_Ex2, aes(x = pro_len, fill = category)) + 
  geom_histogram(color = "white", position = "stack", binwidth = 4, boundary = 2) +
  scale_fill_manual(values = c("rosybrown","#5969A4","#B2B6DC","#D28648","thistle",
                               "#938427","#F6BBC6","#3A4F60","cadetblue","sienna","#A7A7A7"),
                    labels = c("Hormone", "Enzyme", "ECM", "Growth factor", "Protease inhibitor", "Heparin binding", "IgSF", "Receptor", "Neuropeptide", "Cytokine", "Miscellaneous")) +
  scale_x_continuous(limits = c(2,90), breaks = c(2,10,20,30,40,50,60,70,80,90)) +
  labs(x = "Peptide length", y = "Count", fill = "Category")+
  theme_classic() +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        axis.text.x = element_text(color = "black", size = 12))

plot_Ex2
