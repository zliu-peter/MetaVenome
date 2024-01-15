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

# Figure 2C ==============

colors <- colorRampPalette(c("rosybrown","#5969A4","#B2B6DC","#D28648","thistle",
                             "#938427","#A7A7A7","#3A4F60","cadetblue","sienna"))(22)

# Without labeling
plot_2C <- plot_ly(data=data_2C, labels = ~class, values = ~n, type = "pie",
                  hole = 0.8, textposition = "inside",
                  marker = list(colors = colors),
                  hoverinfo = "none",
                  textinfo = "none",
                  sort = FALSE, showlegend = FALSE
) %>%
  add_pie(data = data_2C, labels = ~phylum, values = ~n,
          hole = 0.3,
          marker = list(colors = colors),
          domain = list(y = c(0.15, 0.85)), name = "phylum",
          sort = FALSE,
          textinfo = "none")

# With labeling (figure was adjusted)
plot_2C <- plot_ly(data=data_2C, labels = ~class, values = ~n, type = "pie",
                  hole = 0.8, textposition = "inside",
                  marker = list(colors = colors),
                  hoverinfo = "label+value+percent",
                  sort = FALSE, showlegend = FALSE, text = ~class
) %>%
  add_pie(data = data_2C, labels = ~phylum, values = ~n,
          hole = 0.3,
          marker = list(colors = colors),
          domain = list(y = c(0.15, 0.85)), name = "phylum",
          sort = FALSE, text = ~phylum
  )

plot_2C
#htmlwidgets::saveWidget(plot_2C, "Fig2C.html")
