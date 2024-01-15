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

#anno_blast = fig_4B_2
#df_blast = fig_4B_1

### ============================== 4B

nodes <- anno_blast %>% select(uni_id, pep_id) %>%
  mutate(status=str_extract_all(.$uni_id, "parent|mature|AT_meta|secretome|spikin", simplify=T)) %>% 
  mutate(status=mgsub::mgsub(status, c("parent","mature","AT_meta","secretome","spikin"), c(rep("Animal toxin",2),"Metagenomic","Secretome","spike-in"))) %>%
  mutate(status=ifelse(grepl("EGF_HUMAN|TGFA_HUMAN|HBEGF_HUMAN|BTC_HUMAN|IL8_HUMAN|TX18A_MANRB|TX21A_MYRGU|A0A212QDB3_37_88", pep_id),pep_id,status)) %>%
  mutate(status=mgsub::mgsub(status, c("EGF_HUMAN","TGFA_HUMAN","HBEGF_HUMAN","BTC_HUMAN","TX18A_MANRB","TX21A_MYRGU","A0A212QDB3_37_88"), 
                             c("EGF","TGF\u03B1","HB-EGF","Betacellulin","U18-MYRTX-Mri1a","MIITX(02)-Mg1a","EGF-like protein (MPXV)"))) %>%
  
  mutate(color=ifelse(status=="Animal toxin", "#FFA07A", 
                      ifelse(status=="Metagenomic", "#7AC4F7",
                      ifelse(status=="Secretome", "#A4ACCB",
                      ifelse(status=="EGF", "#DE1818",
                      ifelse(status=="TGF\u03B1", "#6B00C2",
                      ifelse(status=="HB-EGF", "#3BA500",
                      ifelse(status=="Betacellulin", "#FF99B0",
                      ifelse(status=="IL8_HUMAN", "#893f45",
                      ifelse(status=="U18-MYRTX-Mri1a", "#023DCF",
                      ifelse(status=="MIITX(02)-Mg1a", "#C49102", 
                      ifelse(status=="EGF-like protein (MPXV)", "#90EE90", "#1F78B4")))))))))))) %>%
  select(uni_id, status, color)


links <- df_blast %>% select(c(1, 2, "bitscore")) %>%
  dplyr::rename(., "from"=1, "to"=2, "weight"=3)

# Check all links are unique
x <- nrow(links[, c("from", "to")])==nrow(unique(links[, c("from", "to")]))
print(x)

# Remove mutual connections
net <- igraph::graph_from_data_frame(d=links, vertices=nodes, directed=F)
net <- igraph::simplify(net, remove.multiple=T)                                     
sum(which_multiple(net))                                                            

deg <- igraph::degree(net, mode="all") %>% data.frame() %>%
  dplyr::rename(., connections=1) %>%
  tibble::rownames_to_column(., var="uni_id")     

nodes <- nodes %>% left_join(y=deg, by="uni_id") %>%
  mutate(size=ifelse(connections>=1 & connections<10, 1.5, 
              ifelse(connections>=10 & connections<30, 2,
              ifelse(connections>=30 & connections<50, 2.2, 
              ifelse(connections>=50, 2.3, 1)))))

# Node & Link dataframe ready for graphing
net2 <- igraph::graph_from_data_frame(d=links, vertices=nodes, directed=F)
net2 <- igraph::simplify(net2, remove.multiple=T)

# ------------------------ Setting color/size for vertices/nodes --------------------------
V(net2)$color <- V(net2)$color

# Size
V(net2)$size <- V(net2)$size*2.5
V(net2)$label.cex <- 0.4
V(net2)$label.dist <- 0.5
V(net2)$label.color <- "black"

# ------------------------ Setting width; edge.color for Edges/links ----------------------
E(net2)$width <- E(net2)$weight/50
E(net2)$edge.color <- "grey90"


# ------------------------ Plotting -------------------------------------------------------
set.seed(1)                                                            
e <- get.edgelist(net2, names=F)                                       
l <- qgraph.layout.fruchtermanreingold(e, vcount=vcount(net2),
                                       area=15*(vcount(net2)^2),
                                       repulse.rad=(3*vcount(net2)^3))                  

# Uncomment the next line to save the plot
#grDevices::cairo_pdf("./Fig4B.pdf",height=10,width=14)
plot(net2, vertex.label=NA, vertex.frame.color="#555555",              
     edge.lty=1, frame=F, layout=l, asp=1, margin=0.01)

lib_legend <- unique(V(net2)$status)[unique(V(net2)$status) %in% c("Animal toxin", "Metagenomic", "Secretome")]
lib_col <- unique(V(net2)$color)[unique(V(net2)$color) %in% c("#FFA07A","#7AC4F7","#A4ACCB")]
legend("topright", inset=c(-0.03,0), title="Library",
       legend=lib_legend, col=lib_col,
       pch=16, cex=1.3, xpd=T, box.col="white", title.adj = 0.08)

pro_legend <- unique(V(net2)$status)[!unique(V(net2)$status) %in% c("Animal toxin", "Metagenomic", "Secretome")]
pro_col <- unique(V(net2)$color)[!unique(V(net2)$color) %in% c("#FFA07A","#7AC4F7","#A4ACCB")]
legend("bottomright", inset=c(-0.03,0), title="Protein hits",
       legend = pro_legend, col=pro_col,
       pch=16, cex=1.3, xpd=T, box.col="white", title.adj = 0.08)

dev.off()