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

# Figure 3D ===============

colors <- grDevices::colorRampPalette(c("rosybrown","#5969A4","#B2B6DC","#D28648","thistle",
                             "#938427","#A7A7A7","#3A4F60","cadetblue","sienna"))(300)


# Without labels
plot_3D <- plot_ly(data = data_3D, labels = ~class, values = ~n, type = "pie",
               hole = 0.8, textposition = "inside",
               marker = list(colors = colors),
               hoverinfo = "none",
               textinfo = "none",
               sort = FALSE, showlegend = FALSE
) %>%
  plotly::add_pie(data = data_3D, labels = ~phylum, values = ~n,
          hole = 0.3,
          marker = list(colors = colors),
          domain = list(y = c(0.15, 0.85)), name = "phylum",
          sort = FALSE,
          textinfo = "none")



# With labels (figure was adjusted)
plot_3D <- plot_ly(data = data_3D, labels = ~class, values = ~n, type = "pie",
               hole = 0.7, textposition = "inside",
               marker = list(colors = colors),
               hoverinfo = "label+value+percent",
               sort = FALSE, showlegend = FALSE, text = ~class
) %>%
  plotly::add_pie(data = data_3D, labels = ~phylum, values = ~n,
          hole = 0.3,
          marker = list(colors = colors),
          domain = list(y = c(0.2, 0.8)), name = "phylum",
          sort = FALSE, text = ~phylum
  )

plot_3D
#htmlwidgets::saveWidget(plot_3D, "Fig3D.html")
