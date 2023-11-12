
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

# Load plot function
source("functions/Plot_functions.R")



##### March sun x shaded

### Irradiance
environment_data_march <- read.csv("data/environment_data_corrected_march.csv")
environment_data_march$Condition = factor(environment_data_march$Condition, levels=c("Sun", "Shade"))

# plot code:
irradiance_mar <- ggplot(environment_data_march, aes(x=Time, y=Irradiance, group=Condition, color=Condition)) + 
  annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 2500, alpha = 0.3, fill = "gray50")+
  #geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  geom_line(aes(color=Condition), alpha = 1) +
  theme_classic(7) +
  theme(legend.position = "none",
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        plot.title = element_text(size = 7),
        plot.tag = element_text(size = 10, face = "bold"))+
  scale_colour_manual(values=c("orange", "gray30"))+
  scale_fill_manual(values=c("orange", "gray30"))+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,2000,1000))+
  scale_x_continuous(breaks=seq(12,30,6))+
  labs(title = "Irradiance (March/Spring)", tag = "A", 
       x = "Time relative to initial dawn (h)", 
       y = expression(atop("Total irradiance", paste(~(mu~mol~m^{-2}~s^{-1})))))



### Temperature

# plot code:
temperature_mar <- ggplot(environment_data_march, aes(x=Time, y=Temperature, group=Condition, color=Condition)) + 
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 30, alpha = 0.3, fill = "gray50")+
  #geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  geom_line(aes(color=Condition), alpha = 1) +
  theme_classic(7) +
  theme(legend.position = "none",
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        plot.title = element_text(size = 7),
        plot.tag = element_text(size = 10, face = "bold"))+
  scale_colour_manual(values=c("orange", "gray30"))+
  scale_fill_manual(values=c("orange", "gray30"))+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,30,10), limits=c(-3,30))+
  scale_x_continuous(breaks=seq(12,30,6))+
  labs(title = "Temperature (March/Spring)", tag = "C", 
       x = "Time relative to initial dawn (h)", 
       y = expression(atop("Temperature",  paste("(°C)" ))))



##### September sun x shaded

### Irradiance
environment_data_september <- read.csv("data/environment_data_corrected_september.csv")
environment_data_september$Condition = factor(environment_data_september$Condition, levels=c("Sun", "Shade"))

# plot code:
irradiance_sep <- ggplot(environment_data_september, aes(x=Time, y=Irradiance, group=Condition, color=Condition)) + 
  annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 2500, alpha = 0.3, fill = "gray50")+
  #geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  geom_line(aes(color=Condition), alpha = 1) +
  theme_classic(7) +
  theme(legend.position = "none",
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        plot.title = element_text(size = 7),
        plot.tag = element_text(size = 10, face = "bold"))+
  scale_colour_manual(values=c("orange", "gray30"))+
  scale_fill_manual(values=c("orange", "gray30"))+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,2000,1000))+
  scale_x_continuous(breaks=seq(12,30,6))+
  labs(title = "Irradiance (September/Autumn)", tag = "B", 
       x = "Time relative to initial dawn (h)", 
       y = expression(atop("Total irradiance", paste(~(mu~mol~m^{-2}~s^{-1})))))



### Temperature

# plot code:
temperature_sep <- ggplot(environment_data_september, aes(x=Time, y=Temperature, group=Condition, color=Condition)) + 
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 30, alpha = 0.3, fill = "gray50")+
  #geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  geom_line(aes(color=Condition), alpha = 1) +
  theme_classic(7) +
  theme(legend.position = "none",
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        plot.title = element_text(size = 7),
        plot.tag = element_text(size = 10, face = "bold"))+
  scale_colour_manual(values=c("orange", "gray30"))+
  scale_fill_manual(values=c("orange", "gray30"))+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,30,10), limits=c(-3,30))+
  scale_x_continuous(breaks=seq(12,30,6))+
  labs(title = "Temperature (September/Autumn)", tag = "D", 
       x = "Time relative to initial dawn (h)", 
       y = expression(atop("Temperature",  paste("(°C)" ))))



### Difference plot
input <- "02_STM_diff_SunShade_rep/"
df_CCA1_Mar <- as.data.frame(fread(paste0(input, "STM_diff_Mar_CCA1rep.csv")))
df_SIG5_Mar <- as.data.frame(fread(paste0(input, "STM_diff_Mar_SIG5rep.csv")))
df_BLRP_Mar <- as.data.frame(fread(paste0(input, "STM_diff_Mar_BLRPrep.csv")))
df_CCA1_Sep <- as.data.frame(fread(paste0(input, "STM_diff_Sep_CCA1rep.csv")))
df_SIG5_Sep <- as.data.frame(fread(paste0(input, "STM_diff_Sep_SIG5rep.csv")))
df_BLRP_Sep <- as.data.frame(fread(paste0(input, "STM_diff_Sep_BLRPrep.csv")))
df_CCA1_Mar[,str_detect(names(df_CCA1_Mar), "diff")] <- df_CCA1_Mar[,str_detect(names(df_CCA1_Mar), "diff")] * -1
df_SIG5_Mar[,str_detect(names(df_SIG5_Mar), "diff")] <- df_SIG5_Mar[,str_detect(names(df_SIG5_Mar), "diff")] * -1
df_BLRP_Mar[,str_detect(names(df_BLRP_Mar), "diff")] <- df_BLRP_Mar[,str_detect(names(df_BLRP_Mar), "diff")] * -1
df_CCA1_Sep[,str_detect(names(df_CCA1_Sep), "diff")] <- df_CCA1_Sep[,str_detect(names(df_CCA1_Sep), "diff")] * -1
df_SIG5_Sep[,str_detect(names(df_SIG5_Sep), "diff")] <- df_SIG5_Sep[,str_detect(names(df_SIG5_Sep), "diff")] * -1
df_BLRP_Sep[,str_detect(names(df_BLRP_Sep), "diff")] <- df_BLRP_Sep[,str_detect(names(df_BLRP_Sep), "diff")] * -1

df_CCA1_Mar$signif_diff <- df_CCA1_Mar$`diff_2.5%` * df_CCA1_Mar$`diff_97.5%` > 0
df_SIG5_Mar$signif_diff <- df_SIG5_Mar$`diff_2.5%` * df_SIG5_Mar$`diff_97.5%` > 0
df_BLRP_Mar$signif_diff <- df_BLRP_Mar$`diff_2.5%` * df_BLRP_Mar$`diff_97.5%` > 0
df_CCA1_Sep$signif_diff <- df_CCA1_Sep$`diff_2.5%` * df_CCA1_Sep$`diff_97.5%` > 0
df_SIG5_Sep$signif_diff <- df_SIG5_Sep$`diff_2.5%` * df_SIG5_Sep$`diff_97.5%` > 0
df_BLRP_Sep$signif_diff <- df_BLRP_Sep$`diff_2.5%` * df_BLRP_Sep$`diff_97.5%` > 0

glist_CCA1_Mar <- diff_vis_SunShade(df = df_CCA1_Mar, var = "AhgCCA1", season = "Mar")
glist_SIG5_Mar <- diff_vis_SunShade(df = df_SIG5_Mar, var = "AhgSIG5", season = "Mar")
glist_BLRP_Mar <- diff_vis_SunShade(df = df_BLRP_Mar, var = "AhgpsbD BLRP", season = "Mar")
glist_CCA1_Sep <- diff_vis_SunShade(df = df_CCA1_Sep, var = "AhgCCA1", season = "Sep")
glist_SIG5_Sep <- diff_vis_SunShade(df = df_SIG5_Sep, var = "AhgSIG5", season = "Sep")
glist_BLRP_Sep <- diff_vis_SunShade(df = df_BLRP_Sep, var = "AhgpsbD BLRP", season = "Sep")

# Integration of all plots into a figure
glist <- c(list(temperature_mar, temperature_sep, 
                irradiance_mar, irradiance_sep),
           glist_CCA1_Mar, glist_CCA1_Sep, glist_SIG5_Mar,
           glist_SIG5_Sep, glist_BLRP_Mar, glist_BLRP_Sep)

g <- {(glist[[1]]+labs(tag = "A")) + (glist[[2]]+labs(tag = "B")) + 
    (glist[[3]]+labs(tag = "C")) + (glist[[4]]+labs(tag = "D")) +
    (glist[[5]]+labs(tag = "E")) + (glist[[7]]+labs(tag = "F")) + 
    glist[[6]] + glist[[8]] +
    (glist[[9]]+labs(tag = "G")) + (glist[[11]]+labs(tag = "H")) + glist[[10]] + glist[[12]] +
    (glist[[13]]+labs(tag = "I")) + (glist[[15]]+labs(tag = "J")) +
    glist[[14]] + glist[[16]] +
    plot_layout(ncol=2)} /
  legend_SunShade1 +
  plot_layout(heights = c(1, 0.05)) #+
#plot_annotation(title = "Fig. S3") &
#  theme(plot.tag = element_text(size = 10))

ggsave(paste0(out, "Fig.S3_SunShaderep_230402.pdf"),
       g, width = 130, height = 230, units = "mm")

