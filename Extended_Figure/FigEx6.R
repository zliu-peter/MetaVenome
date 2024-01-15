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

#data_S6A corresponds to supplemental 6A
#data_S6_blastp corresponds to supplemental 6B_E

### ========================== 6A
# 8*6 cario

plot_Ex6A <- 
  ggplot(data_Ex6A, aes(x=`Protein Length`, y=`Percentage with Blast Results`)) +
  geom_bar(stat="identity", color="black", fill="grey80", width=0.8) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), expand=c(0.02, 0)) +
  labs(x="Protein Length", y="Percentage with Blast Results") +
  theme_classic() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.text = element_text(size=10.5, color="black"),
        axis.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size=12, color="black"),
        axis.title.x = element_text(size=12.5, color="black"),
        axis.title.y = element_text(size=12.5, color="black"),
        aspect.ratio = 2/1.2) +
  geom_text(aes(label=paste0(`# in Results`, "/", `total #`)), vjust=4, size=3) +
  geom_text(aes(label=paste0(`Percentage with Blast Results`, "%")), vjust=1.5, size=3.2)
plot_Ex6A


# Coverage ----------- 8*6 cario
data = data_Ex6_blastp$qcovhsp
data_median = signif(median(data, na.rm=T), 3)
data_max = max(data, na.rm=T)
colname = "Query Coverage (%)"
data <- as.data.frame(data)
colnames(data) <- colname
xloc = 90
yloc = 3000

plot_Ex6B <- ggplot(data = data, aes(!!sym(colname))) +
  #geom_bar(fill = "darkgrey", color="grey50", alpha=0.8) +
  geom_histogram(fill="darkgrey", color="grey50", binwidth=1, alpha=0.8) +
  scale_x_continuous(breaks=seq(0,data_max,20)) +
  #scale_y_log10(expand=expansion(add=c(0.005))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                expand=expansion(add=c(0.005))) +
  xlab(colname) +
  ylab("Frequency") +
  theme_classic() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=18, color="black"),
        axis.title.y = element_text(size=18, color="black"),
        axis.text.x = element_text(size=14, color="black"),
        axis.text.y = element_text(size=14, color="black"))

plot_Ex6B <- plot_Ex6B %+%
  geom_vline(xintercept = data_median,      # Add line for median
             col = "black",
             lwd = 0.9,
             lty = 5) +
  annotate("text",                        # Add text for median
           x = xloc,
           y = yloc,
           label = paste("Median =", data_median),
           col = "black",
           size = 4)
plot_Ex6B


# bitscore -----------
data = data_Ex6_blastp$bitscore
data_median = signif(median(data, na.rm=T), 3)
data_max = max(data, na.rm=T)
colname = "Bitscore"
data <- as.data.frame(data)
colnames(data) <- colname
xloc = 130
yloc = 1400

plot_Ex6C <- ggplot(data = data, aes(!!sym(colname))) +
  geom_histogram(fill="darkgrey", color="grey50", binwidth=3, alpha=0.8) +
  scale_x_continuous(breaks=seq(0,data_max,25)) +
  #scale_y_log10(expand=expansion(add=c(0.005))) +
  scale_y_log10(breaks = c(1,10,100,1000),
                labels = trans_format("log10", math_format(10^.x)),
                expand=expansion(add=c(0.005))) +
  xlab(colname) +
  ylab("Frequency") +
  theme_classic() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=18, color="black"),
        axis.title.y = element_text(size=18, color="black"),
        axis.text.x = element_text(size=14, color="black"),
        axis.text.y = element_text(size=14, color="black"))


plot_Ex6C <- plot_Ex6C %+%
  geom_vline(xintercept = data_median,      # Add line for median
             col = "black",
             lwd = 0.9,
             lty = 5) +
  annotate("text",                        # Add text for median
           x = xloc,
           y = yloc,
           label = paste("Median =", data_median),
           col = "black",
           size = 4)
plot_Ex6C


# evalue -----------
data = data_Ex6_blastp$evalue
data_median = signif(median(data, na.rm=T), 3)
data_max = max(data, na.rm=T)
colname = "E-value"
data <- as.data.frame(data)
colnames(data) <- colname
xloc = 1e-35
yloc = 1200

plot_Ex6D <- ggplot(data = data, aes(!!sym(colname))) +
  geom_histogram(fill="darkgrey", color="grey50", binwidth=1, alpha=0.8) +
  scale_x_log10(breaks=c(1e-65,1e-50,1e-35,1e-20,1e-10,1e-03),labels=expression(10^-65,10^-50,10^-35,10^-20,10^-10,10^-3)) +
  #scale_y_log10(expand=expansion(add=c(0.001))) +
  scale_y_log10(breaks = c(1,10,100,1000),
                labels = trans_format("log10", math_format(10^.x)),
                expand=expansion(add=c(0.005))) +
  xlab(colname) +
  ylab("Frequency") +
  theme_classic() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=18, color="black"),
        axis.title.y = element_text(size=18, color="black"),
        axis.text.x = element_text(size=14, color="black"),
        axis.text.y = element_text(size=14, color="black"))


plot_Ex6D  <- plot_Ex6D %+%
  geom_vline(xintercept = data_median,      # Add line for median
             col = "black",
             lwd = 0.9,
             lty = 5) +
  annotate("text",                        # Add text for median
           x = xloc,
           y = yloc,
           label = paste("Median =", data_median),
           col = "black",
           size = 4)
plot_Ex6D


# pident -----------
data = data_Ex6_blastp$pident
data_median = signif(median(data, na.rm=T), 3)
data_max = max(data, na.rm=T)
colname = "Percent Identity (%)"
data <- as.data.frame(data)
colnames(data) <- colname
xloc = 75
yloc = 3000

plot_Ex6E <- ggplot(data = data, aes(!!sym(colname))) +
  geom_histogram(fill="darkgrey", color="grey50", binwidth=1, alpha=0.8) +
  scale_x_continuous(breaks=seq(0,data_max,10)) +
  #scale_y_log10(expand=expansion(add=c(0.005))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                expand=expansion(add=c(0.005))) +
  xlab(colname) +
  ylab("Frequency") +
  theme_classic() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=18, color="black"),
        axis.title.y = element_text(size=18, color="black"),
        axis.text.x = element_text(size=14, color="black"),
        axis.text.y = element_text(size=14, color="black"))

plot_Ex6E <- plot_Ex6E %+%
  geom_vline(xintercept = data_median,      # Add line for median
             col = "black",
             lwd = 0.9,
             lty = 5) +
  annotate("text",                        # Add text for median
           x = xloc,
           y = yloc,
           label = paste("Median =", data_median),
           col = "black",
           size = 4)
plot_Ex6E
