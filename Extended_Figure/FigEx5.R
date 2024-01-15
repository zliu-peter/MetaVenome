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


## data_Ex5: same as data_Ex4C

# Extended figure 5

data_Ex5 <- data_Ex4C %>%
  dplyr::mutate(across(.cols = FLAG_1:FLAG_3, ~ dplyr::na_if(.x,y=0))) %>%
  dplyr::mutate(FLAGave = rowMeans(.[, c(2:4)], na.rm=TRUE), .after="FLAG_3") %>%
  # those with >=2 NA changed to 0
  dplyr::mutate(FLAG = ifelse(rowSums(is.na(.[,c(2:4)])) >= 2, 0, FLAGave)) %>%
  dplyr::mutate(FtoM = FLAG/miniprep)

data_Ex5 %<>%
  dplyr::mutate(number_c_comb = ifelse(number_c %in% c(14:19), "14+", number_c)) %>%
  dplyr::mutate(number_c_comb = ifelse(number_c %in% c(0,1), "<2", number_c_comb)) %>%
  dplyr::mutate(number_c_comb = factor(number_c_comb, levels=c("<2",2:13,"14+"), ordered=TRUE))

# plot: Extended Fig 5A ====== 7x5
plot_Ex5A <- 
  ggplot(data_Ex5 %>% subset(data_Ex5$miniprep != 0), 
         mapping= aes(x= miniprep)) +
  geom_histogram(fill = "darkgrey", color = "grey50", binwidth = 0.09) +
  geom_vline(xintercept = median(data_Ex5$miniprep %>% 
                                   subset(data_Ex5$miniprep != 0)),
             linetype = "dashed", color = "sienna", size = 0.7)+
  geom_vline(xintercept = median(data_Ex5$miniprep %>% 
                                   subset(data_Ex5$miniprep != 0)) *10,
             linetype = "dashed", color = "darkblue", size = 0.7)+
  geom_vline(xintercept = median(data_Ex5$miniprep %>% 
                                   subset(data_Ex5$miniprep != 0)) *0.1,
             linetype = "dashed", color = "darkblue", size = 0.7)+
  scale_x_log10(breaks=c(10^0,10^1,10^2,10^3),labels=expression(10^0,10^1,10^2,10^3)) +
  xlab("Phagemid lib stock counts per million reads") +
  ylab("Frequency") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        axis.text.x = element_text(color = "black", size = 12))

plot_Ex5A


# plot: Extended Fig 5B ====== 7x5
plot_Ex5B <- 
  ggplot(data_Ex5 %>% subset(data_Ex5$FLAG != 0),
         mapping= aes(x= FLAG)) +
  geom_histogram(fill = "darkgrey", color = "grey50", binwidth = 0.09) +
  geom_vline(xintercept = median(data_Ex5$FLAG %>% 
                                   subset(data_Ex5$FLAG != 0)),
             linetype = "dashed", color = "sienna", size = 0.7)+
  geom_vline(xintercept = median(data_Ex5$FLAG %>% 
                                   subset(data_Ex5$FLAG != 0)) *10,
             linetype = "dashed", color = "darkblue", size = 0.7)+
  geom_vline(xintercept = median(data_Ex5$FLAG %>% 
                                   subset(data_Ex5$FLAG != 0)) *0.1,
             linetype = "dashed", color = "darkblue", size = 0.7)+
  scale_x_log10(breaks=c(10^0,10^1,10^2,10^3,10^4),labels=expression(10^0,10^1,10^2,10^3,10^4)) +
  xlab("IP captured phage lib counts per million reads") +
  ylab("Frequency") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        axis.text.x = element_text(color = "black", size = 12))

plot_Ex5B
