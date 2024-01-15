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

### ========================== 5A
# 8*9 cairo

set.seed(30)
plot_5A <- ggplot(data=data_5A %>% arrange(desc(Status)), aes(x=log2FC, y=`-log10pvalue`)) +
  geom_point(aes(fill=Status, color=Status), pch=21, size=2.2) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                limits = c(10^(-2), 10^2)) +
  geom_vline(xintercept=c(log2(5)),lty=4,col="black",lwd=0.5) +
  geom_hline(yintercept=-log10(0.001),lty=4,col="black",lwd=0.5) +
  labs(x=expression("log"[2]*"(fold change)"),
       y=expression("-log"[10]*"(p-value)"))  +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.text = element_text(size=12.5),
        axis.text.x = element_text(size=16, color="black"),
        axis.text.y = element_text(size=16, color="black"),
        axis.title.x = element_text(size=18, color="black"),
        axis.title.y = element_text(size=18, color="black"),
        aspect.ratio = 1,
        plot.margin = unit(c(1,3,1,3), "cm"),
        legend.position = c(0.9, 0.2),
        legend.background = element_rect(fill = "white", color = "white"),
        legend.title = element_blank())

plot_5A <- plot_5A + scale_fill_manual(values=c(`N`=NA, ERR1712142_166567_105_166="#DE1818", 
                                    U3ID88_3063_3121="#FF00FF", ERR1712142_166567_109_172="#6B00C2",
                                    A0A218UJH8_3163_3220="#3BA500", A0A131XJ19_7_72="#FF99B0",
                                    SRR10938839_assembly.fas_219713400_7_70="#FFD900"), na.value=NA,
                           breaks=c("ERR1712142_166567_105_166","ERR1712142_166567_109_172",
                                    "U3ID88_3063_3121","A0A218UJH8_3163_3220","A0A131XJ19_7_72",
                                    "SRR10938839_assembly.fas_219713400_7_70"),
                           label=c(ERR1712142_166567_105_166="ERR1712142|105-166",
                                   ERR1712142_166567_109_172="ERR1712142|109-172",
                                   U3ID88_3063_3121="U3ID88",
                                   A0A218UJH8_3163_3220="A0A218UJH8",
                                   A0A131XJ19_7_72="A0A131XJ19|7-72",
                                   SRR10938839_assembly.fas_219713400_7_70="SRR10938839|7-70")) +
  scale_color_manual(values=c(`N`="grey70", ERR1712142_166567_105_166="#DE1818", 
                              U3ID88_3063_3121="#FF00FF", ERR1712142_166567_109_172="#6B00C2",
                              A0A218UJH8_3163_3220="#3BA500", A0A131XJ19_7_72="#FF99B0",
                              SRR10938839_assembly.fas_219713400_7_70="#FFD900"),
                     breaks=c("ERR1712142_166567_105_166","ERR1712142_166567_109_172",
                              "U3ID88_3063_3121","A0A218UJH8_3163_3220","A0A131XJ19_7_72",
                              "SRR10938839_assembly.fas_219713400_7_70"),
                     label=c(ERR1712142_166567_105_166="ERR1712142|105-166",
                             ERR1712142_166567_109_172="ERR1712142|109-172",
                             U3ID88_3063_3121="U3ID88",
                             A0A218UJH8_3163_3220="A0A218UJH8",
                             A0A131XJ19_7_72="A0A131XJ19|7-72",
                             SRR10938839_assembly.fas_219713400_7_70="SRR10938839|7-70")) + 
  scale_alpha_manual(values=c(`N`=0.3, ERR1712142_166567_105_166=1, 
                              U3ID88_3063_3121=1, ERR1712142_166567_109_172=1,
                              A0A218UJH8_3163_3220=1, A0A131XJ19_7_72=1,
                              SRR10938839_assembly.fas_219713400_7_70=1),
                     breaks=c("ERR1712142_166567_105_166","ERR1712142_166567_109_172",
                              "U3ID88_3063_3121","A0A218UJH8_3163_3220","A0A131XJ19_7_72",
                              "SRR10938839_assembly.fas_219713400_7_70"),
                     label=c(ERR1712142_166567_105_166="ERR1712142|105-166",
                             ERR1712142_166567_109_172="ERR1712142|109-172",
                             U3ID88_3063_3121="U3ID88",
                             A0A218UJH8_3163_3220="A0A218UJH8",
                             A0A131XJ19_7_72="A0A131XJ19|7-72",
                             SRR10938839_assembly.fas_219713400_7_70="SRR10938839|7-70"))
plot_5A