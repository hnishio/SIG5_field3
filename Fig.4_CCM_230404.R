
# Set working directory
setwd("/Volume3/hnishio/R/SIG5_field2")

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


## Explanation of CCM
g_text <- ggplot() + theme_void() + 
  geom_segment(aes(x = -1.3, y = 0.1, xend = 0.95, yend = 0.1),
               arrow = arrow(length = unit(0.2, "cm")), size = 0.7) +
  geom_segment(aes(x = 0.95, y = -0.1, xend = -1.3, yend = -0.1),
               arrow = arrow(length = unit(0.2, "cm")), size = 0.7,
               col = "firebrick") +
  geom_rect(aes(xmin=-1.95, xmax=-1.45, ymin=-0.2, ymax=0.2), fill=NA, col="black") +
  geom_rect(aes(xmin=1.1, xmax=1.6, ymin=-0.2, ymax=0.2), fill=NA, col="black") +
  coord_cartesian(xlim = c(-1.5, 1.5), ylim = c(-1.5, 0.5), clip = "off") +
  annotate("text", x=-0.2, y=0.3, label = "Causality", size = 7/ggplot2::.pt) +
  annotate("text", x=-0.2, y=-0.28, label = "Prediction", size = 7/ggplot2::.pt, col = "firebrick") +
  annotate("text", x=-0.2, y=-0.48, label = "(or cross mapping)", size = 7/ggplot2::.pt, col = "firebrick") +
  annotate("text", x=-0.2, y=-0.68, label = "of present or past values", size = 7/ggplot2::.pt, col = "firebrick") +
  annotate("text", x=-0.2, y=-0.88, label = "is successful", size = 7/ggplot2::.pt, col = "firebrick") +
  annotate("text", x=-1.7, y=0, label = "X", size = 12/ggplot2::.pt) +
  annotate("text", x=1.35, y=0, label = "Y", size = 12/ggplot2::.pt)


## CCM
load("08_CCM_local/08_CCM_local.RData")
source("functions/EDM_local.R")
g_CCA1_SIG5_CCM <- CCMplot(ccm_res = ccm_CCA1_SIG5_spar0.5, x_name = 'AhgCCA1', y_name = 'AhgSIG5', spar = 0.5, outdir = out)
g_SIG5_BLRP_CCM <- CCMplot(ccm_res = ccm_SIG5_BLRP_spar0.5, x_name = 'AhgSIG5', y_name = 'AhgpsbD BLRP', spar = 0.5, outdir = out)


## Convergence
load("09_Convergence_local/09_Convergence_local.RData")
source("functions/EDM_local.R")
#tpy_CCA1_SIG5 <- ccm_CCA1_SIG5_spar0.5$lag_2[which.max(ccm_CCA1_SIG5_spar0.5$rho_2)]
#tpy_SIG5_BLRP <- ccm_SIG5_BLRP_spar0.5$lag_2[which.max(ccm_SIG5_BLRP_spar0.5$rho_2)]
#tpy_SIG5_BLRP2 <- ccm_SIG5_BLRP_spar0.5$lag_2[order(ccm_SIG5_BLRP_spar0.5$rho_2, decreasing = T)][2]

g_CCA1_SIG5_Conv <- Convergence_plot(conv_CCA1_SIG5_spar0.5, x_name = 'AhgCCA1', y_name = 'AhgSIG5', 
                                     tpx = NULL, tpy = tpy_CCA1_SIG5, spar = 0.5, outdir = out)
g_SIG5_BLRP_Conv <- Convergence_plot(conv_SIG5_BLRP_spar0.5, x_name = 'AhgSIG5', y_name = 'AhgpsbD BLRP', 
                                     tpx = NULL, tpy = tpy_SIG5_BLRP, spar = 0.5, outdir = out)
g_SIG5_BLRP2_Conv <- Convergence_plot(conv_SIG5_BLRP_spar0.5_2, x_name = 'AhgSIG5', y_name = 'AhgpsbD BLRP', 
                                      tpx = NULL, tpy = tpy_SIG5_BLRP2, spar = 0.5, outdir = out)


# Integration of all plots into a figure
void <- ggplot() + theme_void()
g <- (g_text + labs(tag = "A")) + void + 
  (g_CCA1_SIG5_CCM + labs(tag = "B")) + void + 
  (g_SIG5_BLRP_CCM + labs(tag = "C")) + void + 
  (g_CCA1_SIG5_Conv + labs(tag = "D")) + void + 
  (g_SIG5_BLRP_Conv + labs(tag = "E")) + void +
  (g_SIG5_BLRP2_Conv + labs(tag = "F")) + void +
  plot_layout(ncol=6, widths = c(1, 0.1, 1, 0.1, 1, 0.1)) &
  theme(plot.tag = element_text(face = "bold", size = 10))

out <- "figures/"
ggsave(paste0(out, "Fig.4_CCM_230404.pdf"),
       g, width = 95*3/2, height = 100, units = "mm")

