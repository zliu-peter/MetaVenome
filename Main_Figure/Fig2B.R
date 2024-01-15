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

### ========================== 2B
# 6*9 cairo

mature_vec <- c(data_2B_Mature_meta$aa_Length, rep(NA, nrow(data_2B_Parent_v2)-nrow(data_2B_Mature_meta)))
x <- data.frame(Parent=data_2B_Parent_v2$aa_Length, Mature=mature_vec)

data_2B <- tidyr::pivot_longer(x, everything(), names_to="library", values_to="aa_length") %>%
  as.data.frame() %>% dplyr::filter(!is.na(aa_length)) %>% dplyr::filter(!aa_length>1000)

data_2B$library <- factor(data_2B$library, levels=c("Parent","Mature"))

plot_2B <- ggplot(data_2B, aes(x=aa_length, fill=library, after_stat(count))) + 
  geom_density(alpha=0.5, color="gray60") +
  theme_classic() +
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
        plot.caption = element_text(hjust=0, face="italic"), 
        axis.title.x = element_text(size=16, color="black"), 
        axis.title.y = element_text(size=16, color="black"), 
        legend.text = element_text(size=14),
        legend.position = c(0.775, 0.3),
        axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        legend.title = element_blank()) +
  xlab("aa length") +
  ylab("# of Peptides") +
  scale_x_continuous(breaks=c(10,100,200,300,400,500,1000,2000)) +
  scale_fill_manual(labels=c(Parent='Parent (total)', Mature='Mature and active'), values=c(Parent='#F8766D80', Mature='#00BFC480'))

plot_2B <- plot_2B %+%
  geom_vline(xintercept = median(data_2B_Parent_v2$aa_Length),
             col = "#9E2620",
             lwd = 0.5,
             lty = 5) +
  annotate("text",     
           x = 180,
           y = 210,
           label = paste0(median(data_2B_Parent_v2$aa_Length)),
           col = "black",
           size = 5) +
  geom_vline(xintercept = median(data_2B_Mature_meta$aa_Length),
             col = "#1F4D7A",
             lwd = 0.5,
             lty = 5) +
  annotate("text",
           x = 10,
           y = 210,
           label = paste0(median(data_2B_Mature_meta$aa_Length)),
           col = "black",
           size = 5)

plot_2B
