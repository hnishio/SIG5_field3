
# Set working directory
setwd("/Volume3/hnishio/R/SIG5_field2")

# Load CCM results
load("08_CCM_local/08_CCM_local.RData")

# Load packages
library(rEDM); packageVersion("rEDM") # v0.7.5
library(tidyverse)
library(reshape2)
library(patchwork)

# Create output directory
out <- "09_Convergence_local/"
if(file.exists(out)==F){
  dir.create(out, recursive=T)
}


## Load function
source("functions/EDM_local.R")


###### Convergence #####

# spar
vec_spar <- c(0.2, 0.5, 0.8)

for(i in 1:3){
  # CCA1_SIG5
  tpy_CCA1_SIG5 <- ccm_CCA1_SIG5_spar0.5$lag_2[which.max(ccm_CCA1_SIG5_spar0.5$rho_2)]
  eval(parse(text = paste0(
    "conv_CCA1_SIG5_spar", vec_spar[i], " <- Convergence(data = d, x = 'CCA1', y = 'SIG5', 
                                                         Ex = E_CCA1, Ey = E_SIG5, tpx = NULL, tpy = tpy_CCA1_SIG5, 
                                                         num_surr = 1000, spar = vec_spar[i])
     g_CCA1_SIG5 <- Convergence_plot(conv_CCA1_SIG5_spar", vec_spar[i], ", x_name = 'AhgCCA1', y_name = 'AhgSIG5', 
                                     tpx = NULL, tpy = tpy_CCA1_SIG5, spar = vec_spar[i], outdir = out)"
  )))
  # SIG5_BLRP
  tpy_SIG5_BLRP <- ccm_SIG5_BLRP_spar0.5$lag_2[which.max(ccm_SIG5_BLRP_spar0.5$rho_2)]
  eval(parse(text = paste0(
    "conv_SIG5_BLRP_spar", vec_spar[i], " <- Convergence(data = d, x = 'SIG5', y = 'BLRP', 
                                                         Ex = E_SIG5, Ey = E_BLRP, tpx = NULL, tpy = tpy_SIG5_BLRP, 
                                                         num_surr = 1000, spar = vec_spar[i])
     g_SIG5_BLRP <- Convergence_plot(conv_SIG5_BLRP_spar", vec_spar[i], ", x_name = 'AhgSIG5', y_name = 'AhgpsbD BLRP', 
                                     tpx = NULL, tpy = tpy_SIG5_BLRP, spar = vec_spar[i], outdir = out)"
  )))
  # SIG5_BLRP2
  tpy_SIG5_BLRP2 <- ccm_SIG5_BLRP_spar0.5$lag_2[order(ccm_SIG5_BLRP_spar0.5$rho_2, decreasing = T)][2]
  eval(parse(text = paste0(
    "conv_SIG5_BLRP_spar", vec_spar[i], "_2 <- Convergence(data = d, x = 'SIG5', y = 'BLRP', 
                                                           Ex = E_SIG5, Ey = E_BLRP, tpx = NULL, tpy = tpy_SIG5_BLRP2, 
                                                           num_surr = 1000, spar = vec_spar[i])
     g_SIG5_BLRP2 <- Convergence_plot(conv_SIG5_BLRP_spar", vec_spar[i], "_2, x_name = 'AhgSIG5', y_name = 'AhgpsbD BLRP', 
                                      tpx = NULL, tpy = tpy_SIG5_BLRP2, spar = vec_spar[i], outdir = out)"
  )))
  
  # Integration of all plots into a figure
  void <- ggplot() + theme_void()
  g <- (g_CCA1_SIG5 + labs(tag = "C")) + void + 
    (g_SIG5_BLRP + labs(tag = "D")) + void +
    (g_SIG5_BLRP2 + labs(tag = "E")) + void +
    plot_layout(ncol=6, widths = c(1, 0.1, 1, 0.1, 1, 0.1)) &
    theme(plot.tag = element_text(size = 10))
  ggsave(paste0(out, "Convergence_spar", vec_spar[i],".pdf"), 
         g, height = 50, width = 95*3/2, units = "mm")
}


## Save output
save.image(paste0(out, "09_Convergence_local.RData"))

