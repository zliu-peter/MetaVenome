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

## data_Ex3: same as data_2E

data_Ex3 <- data_2E %>%
  dplyr::mutate(across(.cols = FLAG_1:FLAG_3, ~ dplyr::na_if(.x,y=0))) %>%
  dplyr::mutate(FLAGave = rowMeans(.[, c(2:4)], na.rm=TRUE), .after="FLAG_3") %>%
  # those with >=2 NA changed to 0
  dplyr::mutate(FLAG = ifelse(rowSums(is.na(.[,c(2:4)])) >= 2, 0, FLAGave)) %>%
  dplyr::mutate(FtoM = FLAG/miniprep)

data_Ex3 %<>%
  dplyr::filter(miniprep != 0) %>%
  dplyr::mutate(dsbond_pep_comb=ifelse(dsbond_pep %in% c(7:10), "7+", dsbond_pep)) %>%
  dplyr::mutate(dsbond_pep_comb=ifelse(number_c %in% c(0:1), 0, dsbond_pep_comb))


# plot: Extended Fig 3A ====== 7x5
plot_Ex3A <- 
  ggplot(data_Ex3 %>% subset(data_Ex3$miniprep != 0), 
         mapping= aes(x= miniprep)) +
  geom_histogram(fill = "darkgrey", color = "grey50", binwidth = 0.09) +
  geom_vline(xintercept = median(data_Ex3$miniprep %>% 
                                   subset(data_Ex3$miniprep != 0)),
             linetype = "dashed", color = "sienna", size = 0.7)+
  geom_vline(xintercept = median(data_Ex3$miniprep %>% 
                                   subset(data_Ex3$miniprep != 0)) *10,
             linetype = "dashed", color = "darkblue", size = 0.7)+
  geom_vline(xintercept = median(data_Ex3$miniprep %>% 
                                   subset(data_Ex3$miniprep != 0)) *0.1,
             linetype = "dashed", color = "darkblue", size = 0.7)+
  scale_x_log10(breaks=c(10^1,10^2,10^3),labels=expression(10^1,10^2,10^3)) +
  xlab("Phagemid lib stock counts per million reads") +
  ylab("Frequency") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        axis.text.x = element_text(color = "black", size = 12))

plot_Ex3A

# plot: Extended Fig 3B ============ 7x5
plot_Ex3B <- 
  ggplot(data_Ex3 %>% subset(data_Ex3$FLAG != 0), 
         mapping= aes(x= FLAG)) +
  geom_histogram(fill = "darkgrey", color = "grey50", binwidth = 0.09) +
  geom_vline(xintercept = median(data_Ex3$FLAG %>% 
                                   subset(data_Ex3$FLAG != 0)),
             linetype = "dashed", color = "sienna", size = 0.7)+
  geom_vline(xintercept = median(data_Ex3$FLAG %>% 
                                   subset(data_Ex3$FLAG != 0)) * 10,
             linetype = "dashed", color = "darkblue", size = 0.7)+
  geom_vline(xintercept = median(data_Ex3$FLAG %>% 
                                   subset(data_Ex3$FLAG != 0)) * 0.1,
             linetype = "dashed", color = "darkblue", size = 0.7)+
  scale_x_log10(breaks=c(10^1,10^2,10^3,10^4),labels=expression(10^1,10^2,10^3,10^4)) +
  xlab("IP captured phage lib counts per million reads") +
  ylab("Frequency") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(color = "black", size = 12),
        axis.title.y = element_text(color = "black", size = 12),
        axis.text.x = element_text(color = "black", size = 12))

plot_Ex3B
