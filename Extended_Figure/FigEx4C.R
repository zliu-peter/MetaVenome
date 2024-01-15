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


# Extended figure 4 ========= 6x4

data_Ex4C %<>%
  dplyr::mutate(across(.cols = FLAG_1:FLAG_3, ~ dplyr::na_if(.x,y=0))) %>%
  dplyr::mutate(FLAGave = rowMeans(.[, c(2:4)], na.rm=TRUE), .after="FLAG_3") %>%
  # those with >=2 NA changed to 0
  dplyr::mutate(FLAG = ifelse(rowSums(is.na(.[,c(2:4)])) >= 2, 0, FLAGave)) %>%
  dplyr::mutate(FtoM = FLAG/miniprep)

data_S5_tab <- data_Ex4C # supplementary table 5 metavenome QC
# table(data_S5_tab$number_c)

data_Ex4C %<>%
  dplyr::mutate(number_c_comb = ifelse(number_c %in% c(14:19), "14+", number_c)) %>%
  dplyr::mutate(number_c_comb = ifelse(number_c %in% c(0,1), "<2", number_c_comb)) %>%
  dplyr::mutate(number_c_comb = factor(number_c_comb, levels=c("<2",2:13,"14+"), ordered=TRUE))

plot_Ex4C <- 
  ggplot(data = data_Ex4C %>% subset(FtoM != "0" & FtoM != Inf), mapping = aes(x = number_c_comb, y = FtoM, fill = "grey")) +
  geom_violin(scale = "width", trim = FALSE, alpha=0.7) +
  geom_boxplot(width = 0.15, fill = "white", alpha = 0.5, outlier.size = 0.5, outlier.shape = NA) +
  scale_y_continuous(trans = "log10", breaks = c(10^-4, 10^-3, 10^-2,10^-1,10^0,10^1,10^2,10^3), limits = c(0.0001, 1000), labels = expression(10^-4, 10^-3, 10^-2,10^-1,10^0,10^1,10^2,10^3)) +
  geom_hline(yintercept = median(data_Ex4C$FtoM %>% subset(data_Ex4C$FtoM != "0" & data_Ex4C$FtoM != Inf)), linetype = "dashed", color = "sienna") +
  xlab('# of Cysteine') + 
  ylab('IP Captured Phage Lib / Phagemid Lib Stock') +
  scale_fill_manual(values = "grey") +
  theme_classic() +
  theme(legend.position="none")

plot_Ex4C

# table(data_S5_tab$number_c)
