
# Set working directory
setwd("/Volume3/hnishio/R/SIG5_field3")

# Load packages
library(ggpubr)
library(tidyverse)
library(patchwork)
library(data.table)

# Create output directory
out <- "figures/"
if(file.exists(out)==F){
  dir.create(out, recursive=T)
}


## E
load("07_Embedding_local/07_Embedding_local_ggplot.RData")

# Integration of all plots into a figure
void <- ggplot() + theme_void()
g <- (g_CCA1[[1]] + labs(tag = "A")) + g_CCA1[[2]] + g_CCA1[[3]] + void +
  (g_SIG5[[1]] + labs(tag = "B")) + g_SIG5[[2]] + g_SIG5[[3]] + void +
  (g_BLRP[[1]] + labs(tag = "C")) + g_BLRP[[2]] + g_BLRP[[3]] + void +
  (g_CCA1_1 + labs(tag = "D")) + g_CCA1_2 + g_CCA1_3 + legend_CCA1_2 +
  (g_SIG5_1 + labs(tag = "E")) + g_SIG5_2 + g_SIG5_3 + legend_SIG5_2 +
  (g_BLRP_1 + labs(tag = "F")) + g_BLRP_2 + g_BLRP_3 + legend_BLRP_2 +
  plot_layout(ncol = 4, widths = c(3,3,3,1.2)) &
  theme(plot.tag = element_text(face = "bold", size = 10))

out <- "figures/"
ggsave(paste0(out, "Fig.S12_Embedding_231112.pdf"),
       g, width = 180, height = 230, units = "mm")

