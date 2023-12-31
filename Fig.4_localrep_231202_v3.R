
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
source("functions/Plot_functions_231123_v3.R")




##### Local comparisons

### Temperature
local_environment_data <- read.csv("data/local_environment_data.csv")
local_environment_data$Condition = factor(local_environment_data$Condition, levels=c("Ambient Conditions", "Warm", "Chill", "Low light"))
local_environment_data <- local_environment_data[local_environment_data$Condition!=unique(local_environment_data$Condition)[4],]

local_environment_data$Time <- local_environment_data$Time + 6

#sunrise1_sep <- 5+41/60
sunset1_sep <- 18+13/60
sunrise2_sep <- 5+40/60 + 24
sunset2_sep <- 18+11/60 + 24

# plot code:
g_temp <- ggplot(local_environment_data, aes(x=Time, y=Temperature, group=Condition, color=Condition)) + 
  annotate("rect", xmin = sunset1_sep, xmax = sunrise2_sep, ymin = 13, ymax = 32, alpha = 0.3, fill = "gray50") +
  annotate("rect", xmin = sunset2_sep, xmax = 45, ymin = 13, ymax = 32, alpha = 0.3, fill = "gray50") +
  #geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  geom_line(aes(color=Condition), alpha = 1) +
  theme_classic(7) +
  theme(legend.position = "none",
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        plot.title = element_blank(),
        plot.tag = element_text(size = 10, face = "bold"))+
  scale_colour_manual(values=c("black", "orangered", "cyan3", "lavenderblush4"))+
  scale_fill_manual(values=c("black", "orangered", "cyan3", "lavenderblush4"))+
  scale_x_continuous(breaks=c(18, 24, 30, 36, 42), labels=c("18:00", "0:00", "6:00", "12:00", "18:00")) +
  scale_y_continuous(expand = c(0, 0), breaks=seq(15,30,5), limits = c(13, 32))+
  labs(
    #title = "Temperature", subtitle = "Multiple local treatments", 
    tag = "B", 
    x = "Local time (hh:mm)", 
    y = expression(atop("Temperature",  paste("(°C)" ))))



### Difference plot
input <- "03_STM_diff_local_rep/"
df_CCA1_local <- as.data.frame(fread(paste0(input, "STM_diff_local_CCA1rep.csv")))
df_SIG5_local <- as.data.frame(fread(paste0(input, "STM_diff_local_SIG5rep.csv")))
df_BLRP_local <- as.data.frame(fread(paste0(input, "STM_diff_local_BLRPrep.csv")))
df_CCA1_local$signif_diff2 <- df_CCA1_local$`diff2_2.5%` * df_CCA1_local$`diff2_97.5%` > 0
df_CCA1_local$signif_diff3 <- df_CCA1_local$`diff3_2.5%` * df_CCA1_local$`diff3_97.5%` > 0
df_SIG5_local$signif_diff2 <- df_SIG5_local$`diff2_2.5%` * df_SIG5_local$`diff2_97.5%` > 0
df_SIG5_local$signif_diff3 <- df_SIG5_local$`diff3_2.5%` * df_SIG5_local$`diff3_97.5%` > 0
df_BLRP_local$signif_diff2 <- df_BLRP_local$`diff2_2.5%` * df_BLRP_local$`diff2_97.5%` > 0
df_BLRP_local$signif_diff3 <- df_BLRP_local$`diff3_2.5%` * df_BLRP_local$`diff3_97.5%` > 0

glist_CCA1_local <- diff_vis_local(df = df_CCA1_local, var = "AhgCCA1")
glist_SIG5_local <- diff_vis_local(df = df_SIG5_local, var = "AhgSIG5")
glist_BLRP_local <- diff_vis_local(df = df_BLRP_local, var = "AhgpsbD BLRP")

# Integration of all plots into a figure
glist <- c(list(g_temp), glist_CCA1_local, glist_SIG5_local, glist_BLRP_local)

g <- (glist[[1]] + labs(tag = "A")) + (glist[[2]] + labs(tag = "B")) +
  legend_local2 + glist[[3]] +
  (glist[[4]] + labs(tag = "C")) + (glist[[6]] + labs(tag = "D")) +
  glist[[5]] + glist[[7]] +
  plot_layout(ncol=2) #+
  # plot_annotation(title = "Fig. 4") &
  # theme(plot.tag = element_text(size = 10))

ggsave(paste0(out, "Fig.4_localrep_231202_v3.pdf"),
       g, width = 130, height = 120, units = "mm")

