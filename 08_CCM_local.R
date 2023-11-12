
# Set working directory
setwd("/Volume3/hnishio/R/SIG5_field3")

# Load Embedding results
load("07_Embedding_local/07_Embedding_local.RData")

# Load packages
library(rEDM); packageVersion("rEDM") # v0.7.5
library(tidyverse)
library(reshape2)
library(patchwork)

# Create output directory
out <- "08_CCM_local/"
if(file.exists(out)==F){
  dir.create(out, recursive=T)
}


## Load function
source("functions/EDM_local.R")



### spar=NULL
ccm_CCA1_SIG5_sparNULL <- CCMwSurrogate(x = "CCA1", y = "SIG5", Ex = E_CCA1, Ey = E_SIG5, lag = -6:2, num_surr = 1000, spar = NULL)
g_CCA1_SIG5 <- CCMplot(ccm_res = ccm_CCA1_SIG5_sparNULL, x_name = "AhgCCA1", y_name = "AhgSIG5", spar = NULL, outdir = out)

ccm_SIG5_BLRP_sparNULL <- CCMwSurrogate(x = "SIG5", y = "BLRP", Ex = E_SIG5, Ey = E_BLRP, lag = -6:2, num_surr = 1000, spar = NULL)
g_SIG5_BLRP <- CCMplot(ccm_res = ccm_SIG5_BLRP_sparNULL, x_name = "AhgSIG5", y_name = "AhgpsbD BLRP", spar = NULL, outdir = out)

ccm_CCA1_BLRP_sparNULL <- CCMwSurrogate(x = "CCA1", y = "BLRP", Ex = E_CCA1, Ey = E_BLRP, lag = -6:2, num_surr = 1000, spar = NULL)
g_CCA1_BLRP <- CCMplot(ccm_res = ccm_CCA1_BLRP_sparNULL, x_name = "AhgCCA1", y_name = "AhgpsbD BLRP", spar = NULL, outdir = out)

# Integration of all plots into a figure
void <- ggplot() + theme_void()
g <- (g_CCA1_SIG5 + labs(tag = "A")) + void + 
  (g_SIG5_BLRP + labs(tag = "B")) + void +
  (g_CCA1_BLRP + labs(tag = "C")) + void +
  plot_layout(ncol=6, widths = c(1, 0.2, 1, 0.2, 1, 0.2)) &
  theme(plot.tag = element_text(size = 10))
ggsave(paste0(out, "CCM_sparNULL.pdf"), 
       g, height = 47, width = 145, units = "mm")



### spar
vec_spar <- c(0.2, 0.5, 0.8)

for(i in 1:3){
  # CCA1_SIG5
  eval(parse(text = paste0(
    "ccm_CCA1_SIG5_spar", vec_spar[i], " <- CCMwSurrogate(x = 'CCA1', y = 'SIG5', Ex = E_CCA1, Ey = E_SIG5, lag = -6:2, num_surr = 1000, spar = vec_spar[i])
     g_CCA1_SIG5 <- CCMplot(ccm_res = ccm_CCA1_SIG5_spar", vec_spar[i], ", x_name = 'AhgCCA1', y_name = 'AhgSIG5', spar = vec_spar[i], outdir = out)"
  )))
  # SIG5_BLRP
  eval(parse(text = paste0(
    "ccm_SIG5_BLRP_spar", vec_spar[i], " <- CCMwSurrogate(x = 'SIG5', y = 'BLRP', Ex = E_SIG5, Ey = E_BLRP, lag = -6:2, num_surr = 1000, spar = vec_spar[i])
     g_SIG5_BLRP <- CCMplot(ccm_res = ccm_SIG5_BLRP_spar", vec_spar[i], ", x_name = 'AhgSIG5', y_name = 'AhgpsbD BLRP', spar = vec_spar[i], outdir = out)"
  )))
  # CCA1_BLRP
  eval(parse(text = paste0(
    "ccm_CCA1_BLRP_spar", vec_spar[i], " <- CCMwSurrogate(x = 'CCA1', y = 'BLRP', Ex = E_CCA1, Ey = E_BLRP, lag = -6:2, num_surr = 1000, spar = vec_spar[i])
     g_CCA1_BLRP <- CCMplot(ccm_res = ccm_CCA1_BLRP_spar", vec_spar[i], ", x_name = 'AhgCCA1', y_name = 'AhgpsbD BLRP', spar = vec_spar[i], outdir = out)"
  )))

  # Integration of all plots into a figure
  void <- ggplot() + theme_void()
  g <- (g_CCA1_SIG5 + labs(tag = "A")) + void + 
    (g_SIG5_BLRP + labs(tag = "B")) + void +
    plot_layout(ncol=4, widths = c(1, 0.1, 1, 0.1)) &
    theme(plot.tag = element_text(size = 10))
  ggsave(paste0(out, "CCM_spar", vec_spar[i],".pdf"), 
         g, height = 47, width = 95, units = "mm")
}



### Save output
save.image(paste0(out, "08_CCM_local.RData"))

