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

### ========================== 1G
# 8*8 cairo

plot_pd <- ggplot(data=data_1G %>% arrange(desc(Status)), aes(x=cell_pulldown, y=beads_pulldown)) +
  geom_point(aes(fill=Status, color=Status, alpha=Status), pch=21, size=2.2) +
  labs(x=expression("log"[2]*"(FC, Cell-based)"),
       y=expression("log"[2]*"(FC, Bead-based)")) +
  geom_blank(aes(x=beads_pulldown, y=cell_pulldown)) +
  geom_vline(xintercept=c(log2(5)),lty=4,col="black",lwd=0.5) +
  geom_hline(yintercept=c(log2(5)),lty=4,col="black",lwd=0.5) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        #legend.text = element_text(size=12.5),
        legend.position = "none",
        axis.text.x = element_text(size=16, color="black"),
        axis.text.y = element_text(size=16, color="black"),
        axis.title.x = element_text(size=18, color="black"),
        axis.title.y = element_text(size=18, color="black"),
        aspect.ratio = 1,
        plot.margin = unit(c(1,3,1,3), "cm"),
        #legend.position = c(0.775,0.3),
        legend.background = element_rect(fill = "white", color = "white"),
        legend.title = element_blank())

plot_pd <- plot_pd + scale_fill_manual(values=c(`Hits`="#FFD700", `N`=NA, EGF="#DE1818", 
                                                `EGF spike-in`="#FF00FF", 'TGF\u03B1'="#6B00C2",
                                                `HB-EGF`="#3BA500", Betacellulin="#FF99B0", `U18-MYRTX-Mri1a`="#023DCF",
                                                `MIITX(02)-Mg1a`="#C49102", `EGF-like protein (MPXV)`="#90EE90",
                                                `IL8 spike-in`="black"), na.value=NA,
                                       breaks=c("Hits","EGF","EGF spike-in","TGF\u03B1","HB-EGF","Betacellulin","U18-MYRTX-Mri1a",
                                                "MIITX(02)-Mg1a","EGF-like protein (MPXV)","IL8 spike-in")) +
  scale_color_manual(values=c(`Hits`="#FFD700", `N`="grey60", EGF="#DE1818",
                              `EGF spike-in`="#FF00FF", 'TGF\u03B1'="#6B00C2",
                              `HB-EGF`="#3BA500", Betacellulin="#FF99B0", `U18-MYRTX-Mri1a`="#023DCF",
                              `MIITX(02)-Mg1a`="#C49102", `EGF-like protein (MPXV)`="#90EE90",
                              `IL8 spike-in`="black"),
                     breaks=c("Hits","EGF","EGF spike-in","TGF\u03B1","HB-EGF","Betacellulin","U18-MYRTX-Mri1a",
                              "MIITX(02)-Mg1a","EGF-like protein (MPXV)","IL8 spike-in")) + 
  scale_alpha_manual(values=c(`Hits`=0.5, `N`=0.3, EGF=1,
                              `EGF spike-in`=1, 'TGF\u03B1'=1,
                              `HB-EGF`=1, Betacellulin=1, `U18-MYRTX-Mri1a`=1,
                              `MIITX(02)-Mg1a`=1, `EGF-like protein (MPXV)`=1,
                              `IL8 spike-in`=1),
                     breaks=c("Hits","EGF","EGF spike-in","TGF\u03B1","HB-EGF","Betacellulin","U18-MYRTX-Mri1a",
                              "MIITX(02)-Mg1a","EGF-like protein (MPXV)","IL8 spike-in"))
plot_pd
