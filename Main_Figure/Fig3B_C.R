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

### ============================== 3B
### 6.59*5.71; 150%

plot_3B <- ggplot(data_3B, aes(n)) +
  geom_histogram(fill="darkgrey", color="gray50", binwidth=1) +
  scale_x_continuous(limits=c(0, 75), breaks=c(1, 5, 10, 15, 20, 30, 50, 75), expand = c(0.02,0.02)) +
  scale_y_continuous(expand=c(0.02,0.02)) +
  xlab("# of MV Lib Members per Animal Toxin") +
  ylab("Frequency") +
  theme_classic() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=16, color="black"),
        axis.title.y = element_text(size=16, color="black"),
        axis.text.x = element_text(size=14, color="black"),
        axis.text.y = element_text(size=14, color="black"))

plot_3B <- plot_3B %+%
  geom_vline(xintercept = median(data_3B$n),      # Add line for median
             col = "black",
             lwd = 0.8,
             lty = 5) +
  annotate("text",                                # Add text for median
           x = 17,
           y = 775,
           label = paste("Median=",median(data_3B$n)),
           col = "black",
           size = 5)

plot_3B


### ============================== 3C
# 5.00; 4.70

hp <- data_3C %>% select(venom_seq, meta_seq) %>%
  mutate(meta_numc=stringr::str_count(meta_seq, "C|c"),
         venom_numc=stringr::str_count(venom_seq, "C|c")) %>%
  mutate(meta_numc=ifelse(meta_numc>17, '17+', meta_numc),
         venom_numc=ifelse(venom_numc>17, '17+', venom_numc)) %>%
  distinct()
hp_count <- hp %>% group_by(meta_numc, venom_numc) %>%
  summarise(sum=n()) %>%
  ungroup() 
hp_matrix <- as.data.frame.matrix(table(hp %>% select(meta_numc, venom_numc))) # y: MV, x: AT
col_order <- c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","17+")
hp_matrix <- hp_matrix[, col_order]
hp_matrix_rev <- hp_matrix[rev(col_order),]

mat <- as.matrix(hp_matrix_rev)
my_palette <- colorRampPalette(c("white", "black", "#993333", "#FF6666"))(7)
plot_3C <- ComplexHeatmap::Heatmap(mat/1000, 
                        name = "Number of Cysteines between AT-Meta Pairs",
                        col = my_palette,
                        cluster_rows = FALSE, 
                        cluster_columns = FALSE, 
                        show_row_names = TRUE,
                        row_names_side = "left",
                        column_names_rot = 0,
                        row_names_gp = gpar(fontsize = 9),
                        column_names_gp = gpar(fontsize = 9),
                        heatmap_legend_param = list(title = bquote(x ~ 10^3), 
                                                    labels_gp = gpar(fontsize = 10),
                                                    at = seq(0, 15, by = 2), 
                                                    breaks = seq(0, 15, by = 2),
                                                    direction = "vertical"),
                        row_dend_reorder = FALSE,
                        column_title = "# of cysteines in AT within venom-meta pairs",
                        row_title = "# of cysteines in meta within AT-meta pairs",
                        column_title_side = "bottom",
                        column_title_gp = gpar(fontsize = 12),
                        row_title_gp = gpar(fontsize = 12),
                        rect_gp = gpar(col = "grey90", lwd=0.5))
plot_3C
