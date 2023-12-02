
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



##### Sun March x September

### Irradiance
seasons_environment_sun <- read.csv("data/seasons_environment_sun.csv")
seasons_environment_sun$Condition = factor(seasons_environment_sun$Condition, levels=c("March (Spring)", "September (Autumn)"))
seasons_environment_sun$Time <- seasons_environment_sun$Time + 6

#sunrise1_mar <- 5+56/60
sunset1_mar <- 18+18/60
sunrise2_mar <- 5+54/60 + 24
#sunrise1_sep <- 5+41/60
sunset1_sep <- 18+9/60
sunrise2_sep <- 5+41/60 + 24

ymin1 <- 0; ymax1 <- 2500

# plot code:
irradiance_sun <- ggplot(seasons_environment_sun, aes(x=Time, y=Irradiance, group=Condition, color=Condition)) + 
  # annotate("rect", xmin = sunset1_mar, xmax = sunrise2_mar, ymin = ymax1-(ymax1-ymin1)*0.1, ymax = ymax1, alpha = 0.6, fill = "gray50")+
  # annotate("rect", xmin = sunset1_sep, xmax = sunrise2_sep, ymin = ymax1-(ymax1-ymin1)*0.2, ymax = ymax1-(ymax1-ymin1)*0.1, alpha = 0.6, fill = "burlywood3")+ 
  #geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  geom_vline(xintercept = c(sunset1_mar, sunrise2_mar), linetype = "solid", linewidth = 0.2, col = "gray70") +
  geom_vline(xintercept = c(sunset1_sep, sunrise2_sep), linetype = "dashed", linewidth = 0.2, col = "gray70") +
  geom_line(aes(color=Condition), alpha = 1) +
  theme_classic(base_size = 7) +
  theme(legend.position = "none",
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        plot.title = element_text(size = 7),
        plot.tag = element_text(size = 10, face = "bold"))+
  scale_colour_manual(values=c("#FF1493", "#522A17"))+
  scale_fill_manual(values=c("#FF1493", "#522A17"))+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,2000,1000))+
  scale_x_continuous(breaks=c(18, 24, 30, 36), labels=c("18:00", "0:00", "6:00", "12:00")) +
  labs(title = "Irradiance (sun)", tag = "A", 
       x = "Local time (hh:mm)",
       y = expression(atop("Total irradiance", paste(~(mu~mol~m^{-2}~s^{-1})))))



### Temperature

ymin1 <- -3; ymax1 <- 30

# plot code:
temperature_sun <- ggplot(seasons_environment_sun, aes(x=Time, y=Temperature, group=Condition, color=Condition)) + 
  # annotate("rect", xmin = sunset1_mar, xmax = sunrise2_mar, ymin = ymax1-(ymax1-ymin1)*0.1, ymax = ymax1, alpha = 0.6, fill = "gray50")+
  # annotate("rect", xmin = sunset1_sep, xmax = sunrise2_sep, ymin = ymax1-(ymax1-ymin1)*0.2, ymax = ymax1-(ymax1-ymin1)*0.1, alpha = 0.6, fill = "burlywood3")+ 
  #geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  geom_vline(xintercept = c(sunset1_mar, sunrise2_mar), linetype = "solid", linewidth = 0.2, col = "gray70") +
  geom_vline(xintercept = c(sunset1_sep, sunrise2_sep), linetype = "dashed", linewidth = 0.2, col = "gray70") +
  geom_line(aes(color=Condition), alpha = 1) +
  theme_classic(base_size = 7) +
  theme(legend.position = "none",
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        plot.title = element_text(size = 7),
        plot.tag = element_text(size = 10, face = "bold"))+
  scale_colour_manual(values=c("#FF1493", "#522A17"))+
  scale_fill_manual(values=c("#FF1493", "#522A17"))+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,30,10), limits=c(-3,30))+
  scale_x_continuous(breaks=c(18, 24, 30, 36), labels=c("18:00", "0:00", "6:00", "12:00")) +
  labs(title = "Temperature (sun)", tag = "C", 
       x = "Local time (hh:mm)", 
       y = expression(atop("Temperature",  paste("(°C)" ))))





##### Shaded March x September

### Irradiance
seasons_environment_shaded <- read.csv("data/seasons_environment_shaded.csv")
seasons_environment_shaded$Condition = factor(seasons_environment_shaded$Condition, levels=c("March (Spring)", "September (Autumn)"))
seasons_environment_shaded$Time <- seasons_environment_shaded$Time + 6

ymin1 <- 0; ymax1 <- 2500

# plot code:
irradiance_shade <- ggplot(seasons_environment_shaded, aes(x=Time, y=Irradiance, group=Condition, color=Condition)) + 
  # annotate("rect", xmin = sunset1_mar, xmax = sunrise2_mar, ymin = ymax1-(ymax1-ymin1)*0.1, ymax = ymax1, alpha = 0.6, fill = "gray50")+
  # annotate("rect", xmin = sunset1_sep, xmax = sunrise2_sep, ymin = ymax1-(ymax1-ymin1)*0.2, ymax = ymax1-(ymax1-ymin1)*0.1, alpha = 0.6, fill = "burlywood3")+ 
  #geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  geom_vline(xintercept = c(sunset1_mar, sunrise2_mar), linetype = "solid", linewidth = 0.2, col = "gray70") +
  geom_vline(xintercept = c(sunset1_sep, sunrise2_sep), linetype = "dashed", linewidth = 0.2, col = "gray70") +
  geom_line(aes(color=Condition), alpha = 1) +
  theme_classic(base_size = 7) +
  theme(legend.position = "none",
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        plot.title = element_text(size = 7),
        plot.tag = element_text(size = 10, face = "bold"))+
  scale_colour_manual(values=c("#FF1493", "#522A17"))+
  scale_fill_manual(values=c("#FF1493", "#522A17"))+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,2000,1000))+
  scale_x_continuous(breaks=c(18, 24, 30, 36), labels=c("18:00", "0:00", "6:00", "12:00")) +
  labs(title = "Irradiance (shade)", tag = "B", 
       x = "Local time (hh:mm)", 
       y = expression(atop("Total irradiance", paste(~(mu~mol~m^{-2}~s^{-1})))))



### Temperature

ymin1 <- -3; ymax1 <- 30

# plot code:
temperature_shade <- ggplot(seasons_environment_shaded, aes(x=Time, y=Temperature, group=Condition, color=Condition)) + 
  # annotate("rect", xmin = sunset1_mar, xmax = sunrise2_mar, ymin = ymax1-(ymax1-ymin1)*0.1, ymax = ymax1, alpha = 0.6, fill = "gray50")+
  # annotate("rect", xmin = sunset1_sep, xmax = sunrise2_sep, ymin = ymax1-(ymax1-ymin1)*0.2, ymax = ymax1-(ymax1-ymin1)*0.1, alpha = 0.6, fill = "burlywood3")+ 
  #geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  geom_vline(xintercept = c(sunset1_mar, sunrise2_mar), linetype = "solid", linewidth = 0.2, col = "gray70") +
  geom_vline(xintercept = c(sunset1_sep, sunrise2_sep), linetype = "dashed", linewidth = 0.2, col = "gray70") +
  geom_line(aes(color=Condition), alpha = 1) +
  theme_classic(base_size = 7) +
  theme(legend.position = "none",
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        plot.title = element_text(size = 7),
        plot.tag = element_text(size = 10, face = "bold"))+
  scale_colour_manual(values=c("#FF1493", "#522A17"))+
  scale_fill_manual(values=c("#FF1493", "#522A17"))+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,30,10), limits=c(-3,30))+
  scale_x_continuous(breaks=c(18, 24, 30, 36), labels=c("18:00", "0:00", "6:00", "12:00")) +
  labs(title = "Temperature (shade)", tag = "D", 
       x = "Local time (hh:mm)", 
       y = expression(atop("Temperature",  paste("(°C)" ))))


### Difference plot
input <- "01_STM_diff_MarSep_rep/"
df_CCA1_Sun <- as.data.frame(fread(paste0(input, "STM_diff_Sun_CCA1rep.csv")))
df_SIG5_Sun <- as.data.frame(fread(paste0(input, "STM_diff_Sun_SIG5rep.csv")))
df_BLRP_Sun <- as.data.frame(fread(paste0(input, "STM_diff_Sun_BLRPrep.csv")))
df_CCA1_Shade <- as.data.frame(fread(paste0(input, "STM_diff_Shade_CCA1rep.csv")))
df_SIG5_Shade <- as.data.frame(fread(paste0(input, "STM_diff_Shade_SIG5rep.csv")))
df_BLRP_Shade <- as.data.frame(fread(paste0(input, "STM_diff_Shade_BLRPrep.csv")))
df_CCA1_Sun$signif_diff <- df_CCA1_Sun$`diff_2.5%` * df_CCA1_Sun$`diff_97.5%` > 0
df_SIG5_Sun$signif_diff <- df_SIG5_Sun$`diff_2.5%` * df_SIG5_Sun$`diff_97.5%` > 0
df_BLRP_Sun$signif_diff <- df_BLRP_Sun$`diff_2.5%` * df_BLRP_Sun$`diff_97.5%` > 0
df_CCA1_Shade$signif_diff <- df_CCA1_Shade$`diff_2.5%` * df_CCA1_Shade$`diff_97.5%` > 0
df_SIG5_Shade$signif_diff <- df_SIG5_Shade$`diff_2.5%` * df_SIG5_Shade$`diff_97.5%` > 0
df_BLRP_Shade$signif_diff <- df_BLRP_Shade$`diff_2.5%` * df_BLRP_Shade$`diff_97.5%` > 0

glist_CCA1_Sun <- diff_vis_MarSep(df = df_CCA1_Sun, var = "AhgCCA1", condition = "Sun")
glist_SIG5_Sun <- diff_vis_MarSep(df = df_SIG5_Sun, var = "AhgSIG5", condition = "Sun")
glist_BLRP_Sun <- diff_vis_MarSep(df = df_BLRP_Sun, var = "AhgpsbD BLRP", condition = "Sun")
glist_CCA1_Shade <- diff_vis_MarSep(df = df_CCA1_Shade, var = "AhgCCA1", condition = "Shade")
glist_SIG5_Shade <- diff_vis_MarSep(df = df_SIG5_Shade, var = "AhgSIG5", condition = "Shade")
glist_BLRP_Shade <- diff_vis_MarSep(df = df_BLRP_Shade, var = "AhgpsbD BLRP", condition = "Shade")

# Integration of all plots into a figure
glist <- c(list(temperature_sun, temperature_shade, 
                irradiance_sun, irradiance_shade),
           glist_CCA1_Sun, glist_CCA1_Shade, glist_SIG5_Sun,
           glist_SIG5_Shade, glist_BLRP_Sun, glist_BLRP_Shade)

g <- {(glist[[1]]+labs(tag = "A")) + (glist[[2]]+labs(tag = "B")) + 
    (glist[[3]]+labs(tag = "C")) + (glist[[4]]+labs(tag = "D")) +
    (glist[[5]]+labs(tag = "E")) + (glist[[7]]+labs(tag = "F")) + 
    glist[[6]] + glist[[8]] +
    (glist[[9]]+labs(tag = "G")) + (glist[[11]]+labs(tag = "H")) + glist[[10]] + glist[[12]] +
    (glist[[13]]+labs(tag = "I")) + (glist[[15]]+labs(tag = "J")) +
    glist[[14]] + glist[[16]] +
    plot_layout(ncol=2)} /
  legend_MarSep1 +
  plot_layout(heights = c(1, 0.05)) #+
  #plot_annotation(title = "Fig. 1") &
  #theme(plot.tag = element_text(size = 10))
#+plot_annotation(theme = theme(plot.margin = unit(c(1,1,1,1), "mm")))

ggsave(paste0(out, "Fig.2_MarSeprep_231123_v3.pdf"),
       g, width = 130, height = 220, units = "mm")

