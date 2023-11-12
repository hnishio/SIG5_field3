
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

# Load plot function
source("functions/Plot_functions.R")


# Load data
data <- read.csv("data/march_data.csv")
input <- "06_SSM_cyclic_MarSep_constant_input2/"


## SIG5

# Load output
out_name <- paste0("SSM_cyclic_MarSep_SIG5_Lagtemp0_Laglight0_LagCCA10")
df_natural <- as.data.frame(fread(paste0(input, out_name, "_pred_natural.csv")))
df_meantemp <- as.data.frame(fread(paste0(input, out_name, "_pred_meantemp.csv")))
df_meanlight <- as.data.frame(fread(paste0(input, out_name, "_pred_meanlight.csv")))
df_meanCCA1 <- as.data.frame(fread(paste0(input, out_name, "_pred_meanCCA1.csv")))

# Adjustment of data frame
df_natural <- as.data.frame(cbind(df_natural, unique(data$Time)))
names(df_natural)[ncol(df_natural)] <- c("time")
df_meantemp <- as.data.frame(cbind(df_meantemp, unique(data$Time)))
names(df_meantemp)[ncol(df_meantemp)] <- c("time")
df_meanlight <- as.data.frame(cbind(df_meanlight, unique(data$Time)))
names(df_meanlight)[ncol(df_meanlight)] <- c("time")
df_meanCCA1 <- as.data.frame(cbind(df_meanCCA1, unique(data$Time)))
names(df_meanCCA1)[ncol(df_meanCCA1)] <- c("time")

# Draw figures
g_SIG5_natural <- Pred_vis_cyclic_constant(df=df_natural, var="AhgSIG5", cond="natural")
g_SIG5_meantemp <- Pred_vis_cyclic_constant(df=df_meantemp, var="AhgSIG5", cond="mean temp.")
g_SIG5_meanlight <- Pred_vis_cyclic_constant(df=df_meanlight, var="AhgSIG5", cond="mean irrad.")
g_SIG5_meanCCA1 <- Pred_vis_cyclic_constant(df=df_meanCCA1, var="AhgSIG5", cond="mean AhgCCA1")



### Integration of all plots into a figure
glist_all <- c(g_SIG5_natural, g_SIG5_meantemp, g_SIG5_meanlight, g_SIG5_meanCCA1)

void <- ggplot() + theme_void()

g <- {
  (glist_all[[1]] + labs(tag = "A")) + 
    (glist_all[[2]] + labs(tag = "B")) + 
    (glist_all[[3]] + labs(tag = "C")) +
    (glist_all[[4]] + labs(tag = "D")) + 
    (glist_all[[5]] + labs(tag = "E")) + 
    (glist_all[[6]] + labs(tag = "F")) +
    (glist_all[[7]] + labs(tag = "G")) + 
    (glist_all[[8]] + labs(tag = "H")) + 
    plot_layout(ncol=2)
} /
  void /
  legend_MarSep1_dlm +
  plot_layout(heights = c(1, 0.05, 0.05)) #+ 
#plot_annotation(title = "Fig. S5") &
#theme(plot.tag = element_text(size = 10))

ggsave(paste0(out, "Fig.S9_SIG5const2_231104.pdf"),
       g, width = 110, height = 140, units = "mm")

