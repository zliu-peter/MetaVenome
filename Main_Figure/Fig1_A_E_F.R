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

### ========================== 1A
# 5.5x6.5, cairo
set.seed(35)
plot_1A <- ggplot(data=data_1A %>% arrange(desc(Status)), aes(x=log2FC, y=`-log10pvalue`)) +
  geom_point(aes(fill=Status, color=Status, alpha=Status), pch=21, size=2.2) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  geom_vline(xintercept=c(log2(5)),lty=4,col="black",lwd=0.5) +
  geom_hline(yintercept=-log10(0.001),lty=4,col="black",lwd=0.5) +
  labs(x=expression("log"[2]*"(fold change)"),
       y=expression("-log"[10]*"(p-value)")) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.text = element_text(size=12.5),
        axis.text.x = element_text(size=16, color="black"),
        axis.text.y = element_text(size=16, color="black"),
        axis.title.x = element_text(size=18, color="black"),
        axis.title.y = element_text(size=18, color="black"),
        aspect.ratio = 1,
        plot.margin = unit(c(1,3,1,3), "cm"),
        legend.position = c(0.775,0.3),
        legend.background = element_rect(fill = "white", color = "white"),
        legend.title = element_blank())

plot_1A <- plot_1A + scale_fill_manual(values=c(`Hits`="#58A944", `N`=NA, `IL8 spike-in`=NA), 
                           breaks=c("Hits"), na.value=NA) +
  scale_color_manual(values=c(`Hits`="#58A944", `N`="grey60", `IL8 spike-in`="grey60"),
                     breaks=c("Hits")) + 
  scale_alpha_manual(values=c(`Hits`=0.8, `N`=0.3, `IL8 spike-in`=1),
                     breaks=c("Hits"))

plot_1A

### ============================== 1E
# 7x8, cairo

set.seed(35)
plot_1E <- ggplot(data=data_1E %>% arrange(desc(Status)), aes(x=log2FC, y=`-log10pvalue`)) +
  geom_point(aes(fill=Status, color=Status, alpha=Status), pch=21, size=2.2) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  geom_vline(xintercept=c(log2(5)),lty=4,col="black",lwd=0.5) +
  geom_hline(yintercept=-log10(0.001),lty=4,col="black",lwd=0.5) +
  labs(x=expression("log"[2]*"(fold change)"),
       y=expression("-log"[10]*"(p-value)")) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.text = element_text(size=12.5),
        axis.text.x = element_text(size=16, color="black"),
        axis.text.y = element_text(size=16, color="black"),
        axis.title.x = element_text(size=18, color="black"),
        axis.title.y = element_text(size=18, color="black"),
        aspect.ratio = 1,
        plot.margin = unit(c(1,3,1,3), "cm"),
        legend.position = c(0.775,0.3),
        legend.background = element_rect(fill = "white", color = "white"),
        legend.title = element_blank())


plot_1E <- plot_1E + scale_fill_manual(values=c(`N`=NA, EGF="#DE1818", 
                                    `EGF spike-in`="#FF00FF", 'TGF\u03B1'="#6B00C2",
                                    `HB-EGF`="#3BA500", Betacellulin="#FF99B0", `U18-MYRTX-Mri1a`="#023DCF",
                                    `MIITX(02)-Mg1a`="#C49102", `EGF-like protein (MPXV)`="#90EE90",
                                    `IL8 spike-in`="black"), na.value=NA,
                           breaks=c("EGF","EGF spike-in","TGF\u03B1","HB-EGF","Betacellulin","U18-MYRTX-Mri1a",
                                    "MIITX(02)-Mg1a","EGF-like protein (MPXV)","IL8 spike-in")) +
  scale_color_manual(values=c(`N`="grey70", EGF="#DE1818",
                              `EGF spike-in`="#FF00FF", 'TGF\u03B1'="#6B00C2",
                              `HB-EGF`="#3BA500", Betacellulin="#FF99B0", `U18-MYRTX-Mri1a`="#023DCF",
                              `MIITX(02)-Mg1a`="#C49102", `EGF-like protein (MPXV)`="#90EE90",
                              `IL8 spike-in`="black"),
                     breaks=c("EGF","EGF spike-in","TGF\u03B1","HB-EGF","Betacellulin","U18-MYRTX-Mri1a",
                              "MIITX(02)-Mg1a","EGF-like protein (MPXV)","IL8 spike-in")) + 
  scale_alpha_manual(values=c(`N`=0.3, EGF=1,
                              `EGF spike-in`=1, 'TGF\u03B1'=1,
                              `HB-EGF`=1, Betacellulin=1, `U18-MYRTX-Mri1a`=1,
                              `MIITX(02)-Mg1a`=1, `EGF-like protein (MPXV)`=1,
                              `IL8 spike-in`=1),
                     breaks=c("EGF","EGF spike-in","TGF\u03B1","HB-EGF","Betacellulin","U18-MYRTX-Mri1a",
                              "MIITX(02)-Mg1a","EGF-like protein (MPXV)","IL8 spike-in"))
plot_1E

### ============================== 1F
# 7x8, cairo

set.seed(35)
plot_1F <- ggplot(data=data_1F %>% arrange(desc(Status)), aes(x=log2FC, y=`-log10pvalue`)) +
  geom_point(aes(fill=Status, color=Status, alpha=Status), pch=21, size=2.2) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  geom_vline(xintercept=c(log2(5)),lty=4,col="black",lwd=0.5) +
  geom_hline(yintercept=-log10(0.001),lty=4,col="black",lwd=0.5) +
  labs(x=expression("log"[2]*"(fold change)"),
       y=expression("-log"[10]*"(p-value)")) +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.text = element_text(size=12.5),
        axis.text.x = element_text(size=16, color="black"),
        axis.text.y = element_text(size=16, color="black"),
        axis.title.x = element_text(size=18, color="black"),
        axis.title.y = element_text(size=18, color="black"),
        aspect.ratio = 1,
        plot.margin = unit(c(1,3,1,3), "cm"),
        legend.position = c(0.775,0.3),
        legend.background = element_rect(fill = "white", color = "white"),
        legend.title = element_blank())

plot_1F <- plot_1F + scale_fill_manual(values=c(`Hits`="#D2691E", `N`=NA, `CXCL8 (28-99)`="#DE1818", 
                                    `IL8 spike-in`="#FF00FF", CXCL3="#6B00C2",
                                    CXCL7="#3BA500", CXCL1="#FF99B0", CXCL2="#023DCF",
                                    CXCL6="#C49102", `CXCL8 (23-95)`="#90EE90",`EGF spike-in`="black"), na.value=NA,
                           breaks=c("CXCL8 (28-99)","IL8 spike-in","CXCL3","CXCL7","CXCL1",
                                    "CXCL2","CXCL6","CXCL8 (23-95)","EGF spike-in")) +
  scale_color_manual(values=c(`Hits`="#D2691E", `N`="grey70", `CXCL8 (28-99)`="#DE1818", 
                              `IL8 spike-in`="#FF00FF", CXCL3="#6B00C2",
                              CXCL7="#3BA500", CXCL1="#FF99B0", CXCL2="#023DCF",
                              CXCL6="#C49102", `CXCL8 (23-95)`="#90EE90",`EGF spike-in`="black"),
                     breaks=c("CXCL8 (28-99)","IL8 spike-in","CXCL3","CXCL7","CXCL1",
                              "CXCL2","CXCL6","CXCL8 (23-95)","EGF spike-in")) + 
  scale_alpha_manual(values=c(`Hits`=0.9, `N`=0.3, `CXCL8 (28-99)`=1,`IL8 spike-in`=1, CXCL3=1,
                              CXCL7=1, CXCL1=1, CXCL2=1,CXCL6=1, `CXCL8 (23-95)`=1,`EGF spike-in`=1),
                     breaks=c("CXCL8 (28-99)","IL8 spike-in","CXCL3","CXCL7","CXCL1",
                              "CXCL2","CXCL6","CXCL8 (23-95)","EGF spike-in"))
plot_1F