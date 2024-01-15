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

## data_2E

data_2E %<>%
  dplyr::mutate(across(.cols = FLAG_1:FLAG_3, ~ dplyr::na_if(.x,y=0))) %>%
  dplyr::mutate(FLAGave = rowMeans(.[, c(2:4)], na.rm=TRUE), .after="FLAG_3") %>%
  # those with >=2 NA changed to 0
  dplyr::mutate(FLAG = ifelse(rowSums(is.na(.[,c(2:4)])) >= 2, 0, FLAGave)) %>%
  dplyr::mutate(FtoM = FLAG/miniprep)
  
data_S4_tab <- data_2E %>%
  #dplyr::mutate(dsbond_pep_comb=ifelse(dsbond_pep %in% c(7:10), "7+", dsbond_pep)) %>%
  dplyr::mutate(dsbond_pep_comb=ifelse(number_c %in% c(0:1), 0, dsbond_pep))
# table: supplementary table 4 AT lib db
# table(data_S4_tab$dsbond_pep_comb, useNA="always")


data_2E %<>%
  dplyr::filter(miniprep != 0) %>%
  dplyr::mutate(dsbond_pep_comb=ifelse(dsbond_pep %in% c(7:10), "7+", dsbond_pep)) %>%
  dplyr::mutate(dsbond_pep_comb=ifelse(number_c %in% c(0:1), 0, dsbond_pep_comb))

  
# 2E; 6x4
plot_2E <- ggplot(data_2E %>% subset(FtoM != 0), aes(x=dsbond_pep_comb, y=FtoM, fill="grey")) +
  geom_violin(scale="width", trim=FALSE, alpha=0.7) +
  geom_boxplot(width=0.15, fill="white", alpha=0.5, outlier.size=0.5, outlier.shape=NA) +
  scale_x_discrete(breaks=c(0, 1, 2, 3, 4, 5, 6, "7+", NA)) +
  geom_hline(yintercept=median(data_2E$FtoM %>% subset(data_2E$FtoM != 0)), linetype = "dashed", color = "sienna") +
  xlab("# of disulfide bond") + 
  ylab("IP Captured Phage Lib / Phagemid Lib Stock") +
  scale_fill_manual(values="grey") +
  theme_classic() +
  theme(legend.position="none")

plot_2E <- plot_2E +
  geom_signif(test="t.test",
              comparisons=list(c(5,6),c(5,7),c(5,8)),
              tip_length=0,
              annotation="*",
              vjust=0.9,
              step_increase=0.04) +
  scale_y_log10(breaks=c(10^-3, 10^-2,10^-1,10^0,10^1,10^2,10^3), limits=c(0.001, 1300), labels=expression(10^-3, 10^-2,10^-1,10^0,10^1,10^2,10^3))

plot_2E