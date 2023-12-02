
# Set working directory
setwd("/Volume3/hnishio/R/SIG5_field3")

# Load packages
library(ggpubr)
library(tidyverse)
library(patchwork)
library(plyr)

# Load plot function
source("functions/Plot_functions_231123_v3.R")

# Create output directory
out <- "figures/"
if(file.exists(out)==F){
  dir.create(out, recursive=T)
}



test <- read.csv("data/march_data.csv")
sampling_time <- unique(test$Time)



### Temperature

## Sun March x September
seasons_environment_sun <- read.csv("data/seasons_environment_sun.csv")
# subset sampling time
seasons_environment_sun_sampling <- 
  seasons_environment_sun[seasons_environment_sun$Time %in% c(sampling_time, 31.31, 8.10),]
seasons_environment_sun_sampling <- seasons_environment_sun_sampling[-2,]
seasons_environment_sun_sampling$Time

seasons_environment_sun_sampling$Condition = factor(seasons_environment_sun_sampling$Condition, levels=c("March (Spring)", "September (Autumn)"))
seasons_environment_sun_sampling$Time <- seasons_environment_sun_sampling$Time + 6

#sunrise1_mar <- 5+56/60
sunset1_mar <- 18+18/60
sunrise2_mar <- 5+54/60 + 24
#sunrise1_sep <- 5+41/60
sunset1_sep <- 18+9/60
sunrise2_sep <- 5+41/60 + 24

# plot code:
seasons_temperature_sun <- ggplot(seasons_environment_sun_sampling, aes(x=Time, y=Temperature, group=Condition, color=Condition)) + 
  # annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 30, alpha = 0.3, fill = "gray50")+
  #geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  geom_vline(xintercept = c(sunset1_mar, sunrise2_mar), linetype = "solid", linewidth = 0.2, col = "gray70") +
  geom_vline(xintercept = c(sunset1_sep, sunrise2_sep), linetype = "dashed", linewidth = 0.2, col = "gray70") +
  geom_line(aes(color=Condition), size= 1, alpha = 1) +
  theme_classic(base_size = 7) +
  theme(legend.position = "none",
        legend.text = element_text(size = 7, colour = "black"),
        legend.title = element_text(size = 7, colour = "black"),
        axis.text = element_text(size = 7, colour = "black"),
        axis.title = element_text(size = 7, colour = "black"),
        plot.title = element_text(size = 7, colour = "black"),
        plot.subtitle = element_text(size = 7, colour = "black"),
        plot.tag = element_text(size = 10, colour = "black", face = "bold"))+
  scale_colour_manual(values=c("#FF1493", "#522A17"))+
  scale_fill_manual(values=c("#FF1493", "#522A17"))+
  coord_cartesian(ylim = c(-3, 30), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,30,10), limits=c(-3,30))+
  scale_x_continuous(breaks=c(18, 24, 30, 36), labels=c("18:00", "0:00", "6:00", "12:00")) +
  labs(title = "Temperature (sun)", tag = "A", 
       x = "Local time (hh:mm)", 
       y = expression(atop("Temperature",  paste("(°C)" ))))


## Shade March x September
seasons_environment_shaded <- read.csv("data/seasons_environment_shaded.csv")

# subset sampling time
seasons_environment_shaded_sampling <- 
  seasons_environment_shaded[seasons_environment_shaded$Time %in% c(sampling_time, 31.31),]
seasons_environment_shaded_sampling$Time

seasons_environment_shaded_sampling$Condition = factor(seasons_environment_shaded_sampling$Condition, levels=c("March (Spring)", "September (Autumn)"))
seasons_environment_shaded_sampling$Time <- seasons_environment_shaded_sampling$Time + 6

# plot code:
seasons_temperature_shaded <- ggplot(seasons_environment_shaded_sampling, aes(x=Time, y=Temperature, group=Condition, color=Condition)) + 
  # annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 30, alpha = 0.3, fill = "gray50")+
  #geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  geom_vline(xintercept = c(sunset1_mar, sunrise2_mar), linetype = "solid", linewidth = 0.2, col = "gray70") +
  geom_vline(xintercept = c(sunset1_sep, sunrise2_sep), linetype = "dashed", linewidth = 0.2, col = "gray70") +
  geom_line(aes(color=Condition), size= 1, alpha = 1) +
  theme_classic(base_size = 7) +
  theme(legend.position = "none",
        legend.text = element_text(size = 7, colour = "black"),
        legend.title = element_text(size = 7, colour = "black"),
        axis.text = element_text(size = 7, colour = "black"),
        axis.title = element_text(size = 7, colour = "black"),
        plot.title = element_text(size = 7, colour = "black"),
        plot.subtitle = element_text(size = 7, colour = "black"),
        plot.tag = element_text(size = 10, colour = "black", face = "bold"))+
  scale_colour_manual(values=c("#FF1493", "#522A17"))+
  scale_fill_manual(values=c("#FF1493", "#522A17"))+
  coord_cartesian(ylim = c(-3, 30), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,30,10), limits=c(-3,30))+
  scale_x_continuous(breaks=c(18, 24, 30, 36), labels=c("18:00", "0:00", "6:00", "12:00")) +
  labs(title = "Temperature (shade)", tag = "B", 
       x = "Local time (hh:mm)", 
       y = expression(atop("Temperature",  paste("(°C)" ))))



### Irradiance
# plot code:
seasons_irradiance_sun <- ggplot(seasons_environment_sun_sampling, aes(x=Time, y=Irradiance, group=Condition, color=Condition)) + 
  # annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 2500, alpha = 0.3, fill = "gray50")+
  #geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  geom_vline(xintercept = c(sunset1_mar, sunrise2_mar), linetype = "solid", linewidth = 0.2, col = "gray70") +
  geom_vline(xintercept = c(sunset1_sep, sunrise2_sep), linetype = "dashed", linewidth = 0.2, col = "gray70") +
  geom_line(aes(color=Condition), size= 1, alpha = 1) +
  theme_classic(base_size = 7) +
  theme(legend.position = "none",
        legend.text = element_text(size = 7, colour = "black"),
        legend.title = element_text(size = 7, colour = "black"),
        axis.text = element_text(size = 7, colour = "black"),
        axis.title = element_text(size = 7, colour = "black"),
        plot.title = element_text(size = 7, colour = "black"),
        plot.subtitle = element_text(size = 7, colour = "black"),
        plot.tag = element_text(size = 10, colour = "black", face = "bold"))+
  scale_colour_manual(values=c("#FF1493", "#522A17"))+
  scale_fill_manual(values=c("#FF1493", "#522A17"))+
  coord_cartesian(ylim = c(0, 2500), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,2000,1000))+
  scale_x_continuous(breaks=c(18, 24, 30, 36), labels=c("18:00", "0:00", "6:00", "12:00")) +
  labs(title = "Irradiance (sun)", tag = "C", 
       x = "Local time (hh:mm)", 
       y = expression(atop("Total irradiance", paste(~(mu~mol~m^{-2}~s^{-1})))))

# plot code:
seasons_irradiance_shaded <- ggplot(seasons_environment_shaded_sampling, aes(x=Time, y=Irradiance, group=Condition, color=Condition)) + 
  # annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 2500, alpha = 0.3, fill = "gray50")+
  #geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  geom_vline(xintercept = c(sunset1_mar, sunrise2_mar), linetype = "solid", linewidth = 0.2, col = "gray70") +
  geom_vline(xintercept = c(sunset1_sep, sunrise2_sep), linetype = "dashed", linewidth = 0.2, col = "gray70") +
  geom_line(aes(color=Condition), size= 1, alpha = 1) +
  theme_classic(base_size = 7) +
  theme(legend.position = "none",
        legend.text = element_text(size = 7, colour = "black"),
        legend.title = element_text(size = 7, colour = "black"),
        axis.text = element_text(size = 7, colour = "black"),
        axis.title = element_text(size = 7, colour = "black"),
        plot.title = element_text(size = 7, colour = "black"),
        plot.subtitle = element_text(size = 7, colour = "black"),
        plot.tag = element_text(size = 10, colour = "black", face = "bold"))+
  scale_colour_manual(values=c("#FF1493", "#522A17"))+
  scale_fill_manual(values=c("#FF1493", "#522A17"))+
  coord_cartesian(ylim = c(0, 2500), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,2000,1000))+
  scale_x_continuous(breaks=c(18, 24, 30, 36), labels=c("18:00", "0:00", "6:00", "12:00")) +
  labs(title = "Irradiance (shade)", tag = "D", 
       x = "Local time (hh:mm)", 
       y = expression(atop("Total irradiance", paste(~(mu~mol~m^{-2}~s^{-1})))))



### CCA1

#Import and organize dataset
data <- read.csv("data/march_data.csv")
data_MarSun <- subset(data, Condition=="Sun") %>% mutate(Condition="March (Spring)")
data_MarShade <- subset(data, Condition=="Shade") %>% mutate(Condition="March (Spring)")
data <- read.csv("data/september_data.csv")
data_SepSun <- subset(data, Condition=="Sun") %>% mutate(Condition="September (Autumn)")
data_SepShade <- subset(data, Condition=="Shade") %>% mutate(Condition="September (Autumn)")

seasons_sun <- rbind(data_MarSun, data_SepSun)
seasons_shaded <- rbind(data_MarShade, data_SepShade)

seasons_sun$Condition = factor(seasons_sun$Condition, levels=c("March (Spring)", "September (Autumn)"))
seasons_shaded$Condition = factor(seasons_shaded$Condition, levels=c("March (Spring)", "September (Autumn)"))


# Function to calculate meand and standard deviation 
seasons_sun_CCA1 <- ddply(seasons_sun, .(Time, Condition), summarise, 
                             M = mean(CCA1), SE = sd(CCA1) / sqrt((length(CCA1))), 
                             SD = sd(CCA1))
seasons_sun_CCA1$Time <- seasons_sun_CCA1$Time + 6

# plot code:
g_seasons_sun_CCA1 <- ggplot(seasons_sun_CCA1, aes(x=Time, y=M, group=Condition, color=Condition)) + 
  # annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 25, alpha = 0.3, fill = "gray50")+
  # annotate("text", x = 22, y = 20, label = "*", size = 5, color = "black")+
  # annotate("text", x = 24, y = 20, label = "*", size = 5, color = "black")+
  # annotate("text", x = 26, y = 20, label = "*", size = 5, color = "black")+
  # annotate("text", x = 28, y = 20, label = "*", size = 5, color = "black")+
  geom_vline(xintercept = c(sunset1_mar, sunrise2_mar), linetype = "solid", linewidth = 0.2, col = "gray70") +
  geom_vline(xintercept = c(sunset1_sep, sunrise2_sep), linetype = "dashed", linewidth = 0.2, col = "gray70") +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width = 0.8, size = 0.8, 
                position=position_dodge(0.08)) +
  geom_line(aes(color=Condition), size= 1, alpha = 0.6) + 
  geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  theme_classic(base_size = 7) +
  theme(legend.position = "none",
        legend.text = element_text(size = 7, colour = "black"),
        legend.title = element_text(size = 7, colour = "black"),
        axis.text = element_text(size = 7, colour = "black"),
        axis.title = element_text(size = 7, colour = "black"),
        plot.title = element_text(size = 7, colour = "black"),
        plot.subtitle = element_text(size = 7, colour = "black"),
        plot.tag = element_text(size = 10, colour = "black", face = "bold"))+
  scale_colour_manual(values=c("#FF1493", "#522A17"))+
  scale_fill_manual(values=c("#FF1493", "#522A17"))+
  coord_cartesian(ylim = c(0, 25), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,20,10))+
  scale_x_continuous(breaks=c(18, 24, 30, 36), labels=c("18:00", "0:00", "6:00", "12:00")) +
  labs(title = expression(paste(italic(AhgCCA1), " (sun)", sep="")),
       tag = "E", 
       x = "Local time (hh:mm)", 
       y = expression(atop("Relative transcript",  paste("abundance"))))


# Function to calculate meand and standard deviation 
seasons_shaded_CCA1 <- ddply(seasons_shaded, .(Time, Condition), summarise, 
                             M = mean(CCA1), SE = sd(CCA1) / sqrt((length(CCA1))), 
                             SD = sd(CCA1))
seasons_shaded_CCA1$Time <- seasons_shaded_CCA1$Time + 6

# plot code:
g_seasons_shaded_CCA1 <- ggplot(seasons_shaded_CCA1, aes(x=Time, y=M, group=Condition, color=Condition)) + 
  # annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 25, alpha = 0.3, fill = "gray50")+
  # annotate("text", x = 22, y = 25, label = "*", size = 5, color = "black")+
  # annotate("text", x = 24, y = 25, label = "*", size = 5, color = "black")+
  # annotate("text", x = 26, y = 25, label = "*", size = 5, color = "black")+
  # annotate("text", x = 28, y = 25, label = "*", size = 5, color = "black")+
  geom_vline(xintercept = c(sunset1_mar, sunrise2_mar), linetype = "solid", linewidth = 0.2, col = "gray70") +
  geom_vline(xintercept = c(sunset1_sep, sunrise2_sep), linetype = "dashed", linewidth = 0.2, col = "gray70") +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width = 0.8, size = 0.8, 
                position=position_dodge(0.08)) +
  geom_line(aes(color=Condition), size= 1, alpha = 0.6) + 
  geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  theme_classic(base_size = 7) +
  theme(legend.position = "none",
        legend.text = element_text(size = 7, colour = "black"),
        legend.title = element_text(size = 7, colour = "black"),
        axis.text = element_text(size = 7, colour = "black"),
        axis.title = element_text(size = 7, colour = "black"),
        plot.title = element_text(size = 7, colour = "black"),
        plot.subtitle = element_text(size = 7, colour = "black"),
        plot.tag = element_text(size = 10, colour = "black", face = "bold"))+
  scale_colour_manual(values=c("#FF1493", "#522A17"))+
  scale_fill_manual(values=c("#FF1493", "#522A17"))+
  coord_cartesian(ylim = c(0, 25), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,20,10))+
  scale_x_continuous(breaks=c(18, 24, 30, 36), labels=c("18:00", "0:00", "6:00", "12:00")) +
  labs(title = expression(paste(italic(AhgCCA1), " (shade)", sep="")), 
       tag = "F", 
       x = "Local time (hh:mm)", 
       y = expression(atop("Relative transcript",  paste("abundance"))))



### SIG5

# Function to calculate mean and standard deviation 
seasons_sun_SIG5 <- ddply(seasons_sun, .(Time, Condition), summarise, 
                          M = mean(SIG5), SE = sd(SIG5) / sqrt((length(SIG5))), 
                          SD = sd(SIG5))
seasons_sun_SIG5$Time <- seasons_sun_SIG5$Time + 6

# plot code:
g_seasons_sun_SIG5 <- ggplot(seasons_sun_SIG5, aes(x=Time, y=M, group=Condition, color=Condition)) + 
  # annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 5, alpha = 0.3, fill = "gray50")+
  # annotate("text", x = 8, y = 3, label = "*", size = 5, color = "black")+
  # annotate("text", x = 10, y = 3, label = "*", size = 5, color = "black")+
  # annotate("text", x = 12, y = 3, label = "*", size = 5, color = "black")+
  # annotate("text", x = 14, y = 3, label = "*", size = 5, color = "black")+
  # annotate("text", x = 16, y = 3, label = "*", size = 5, color = "black")+
  geom_vline(xintercept = c(sunset1_mar, sunrise2_mar), linetype = "solid", linewidth = 0.2, col = "gray70") +
  geom_vline(xintercept = c(sunset1_sep, sunrise2_sep), linetype = "dashed", linewidth = 0.2, col = "gray70") +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width = 0.8, size = 0.8, 
                position=position_dodge(0.08)) +
  geom_line(aes(color=Condition), size= 1, alpha = 0.6) + 
  geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  theme_classic(base_size = 7) +
  theme(legend.position = "none",
        legend.text = element_text(size = 7, colour = "black"),
        axis.title = element_text(size = 7, colour = "black"),
        legend.title = element_text(size = 7, colour = "black"),
        axis.text = element_text(size = 7, colour = "black"),
        plot.title = element_text(size = 7, colour = "black"),
        plot.subtitle = element_text(size = 7, colour = "black"),
        plot.tag = element_text(size = 10, colour = "black", face = "bold"))+
  scale_colour_manual(values=c("#FF1493", "#522A17"))+
  scale_fill_manual(values=c("#FF1493", "#522A17"))+
  coord_cartesian(ylim = c(0, 5), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,5,2.5))+
  scale_x_continuous(breaks=c(18, 24, 30, 36), labels=c("18:00", "0:00", "6:00", "12:00")) +
  labs(title = expression(paste(italic(AhgSIG5), " (sun)", sep="")), 
       tag = "G", 
       x = "Local time (hh:mm)", 
       y = expression(atop("Relative transcript",  paste("abundance"))))

# Function to calculate meand and standard deviation 
seasons_shaded_SIG5 <- ddply(seasons_shaded, .(Time, Condition), summarise, 
                             M = mean(SIG5), SE = sd(SIG5) / sqrt((length(SIG5))), 
                             SD = sd(SIG5))
seasons_shaded_SIG5$Time <- seasons_shaded_SIG5$Time + 6

# plot code:
g_seasons_shaded_SIG5 <- ggplot(seasons_shaded_SIG5, aes(x=Time, y=M, group=Condition, color=Condition)) + 
  # annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 5, alpha = 0.3, fill = "gray50")+
  # annotate("text", x = 8, y = 4.5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 10, y = 4.5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 12, y = 4.5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 14, y = 4.5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 16, y = 4.5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 18, y = 4.5, label = "*", size = 5, color = "black")+
  geom_vline(xintercept = c(sunset1_mar, sunrise2_mar), linetype = "solid", linewidth = 0.2, col = "gray70") +
  geom_vline(xintercept = c(sunset1_sep, sunrise2_sep), linetype = "dashed", linewidth = 0.2, col = "gray70") +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width = 0.8, size = 0.8, 
                position=position_dodge(0.08)) +
  geom_line(aes(color=Condition), size= 1, alpha = 0.6) + 
  geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  theme_classic(base_size = 7) +
  theme(legend.position = "none",
        legend.text = element_text(size = 7, colour = "black"),
        legend.title = element_text(size = 7, colour = "black"),
        axis.text = element_text(size = 7, colour = "black"),
        axis.title = element_text(size = 7, colour = "black"),
        plot.title = element_text(size = 7, colour = "black"),
        plot.subtitle = element_text(size = 7, colour = "black"),
        plot.tag = element_text(size = 10, colour = "black", face = "bold"))+
  scale_colour_manual(values=c("#FF1493", "#522A17"))+
  scale_fill_manual(values=c("#FF1493", "#522A17"))+
  coord_cartesian(ylim = c(0, 5), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,5,2.5))+
  scale_x_continuous(breaks=c(18, 24, 30, 36), labels=c("18:00", "0:00", "6:00", "12:00")) +
  labs(title = expression(paste(italic(AhgSIG5), " (shade)", sep="")), 
       tag = "H", 
       x = "Local time (hh:mm)", 
       y = expression(atop("Relative transcript",  paste("abundance"))))



### BLRP

# Function to calculate meand and standard deviation 
seasons_sun_BLRP <- ddply(seasons_sun, .(Time, Condition), summarise, 
                          M = mean(BLRP), SE = sd(BLRP) / sqrt((length(BLRP))), 
                          SD = sd(BLRP))
seasons_sun_BLRP$Time <- seasons_sun_BLRP$Time + 6

# plot code:
g_seasons_sun_BLRP <- ggplot(seasons_sun_BLRP, aes(x=Time, y=M, group=Condition, color=Condition)) + 
  # annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 6.2, alpha = 0.3, fill = "gray50")+
  # annotate("text", x = 28, y = 4, label = "*", size = 5, color = "black")+
  geom_vline(xintercept = c(sunset1_mar, sunrise2_mar), linetype = "solid", linewidth = 0.2, col = "gray70") +
  geom_vline(xintercept = c(sunset1_sep, sunrise2_sep), linetype = "dashed", linewidth = 0.2, col = "gray70") +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width = 0.8, size = 0.8, 
                position=position_dodge(0.08)) +
  geom_line(aes(color=Condition), size= 1, alpha = 0.6) +
  geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  theme_classic(base_size = 7) +
  theme(legend.position = "none",
        legend.text = element_text(size = 7, colour = "black"),
        legend.title = element_text(size = 7, colour = "black"),
        axis.text = element_text(size = 7, colour = "black"),
        axis.title = element_text(size = 7, colour = "black"),
        plot.title = element_text(size = 7, colour = "black"),
        plot.subtitle = element_text(size = 7, colour = "black"),
        plot.tag = element_text(size = 10, colour = "black", face = "bold"))+
  scale_colour_manual(values=c("#FF1493", "#522A17"))+
  scale_fill_manual(values=c("#FF1493", "#522A17"))+
  coord_cartesian(ylim = c(0, 6.2), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,6,3), limits = c(0,6.2))+
  scale_x_continuous(breaks=c(18, 24, 30, 36), labels=c("18:00", "0:00", "6:00", "12:00")) +
  labs(title = expression(paste(italic("AhgpsbD BLRP"), " (sun)", sep="")), 
       tag = "I", 
       x = "Local time (hh:mm)", 
       y = expression(atop("Relative transcript",  paste("abundance"))))

# Function to calculate meand and standard deviation 
seasons_shaded_BLRP <- ddply(seasons_shaded, .(Time, Condition), summarise, 
                             M = mean(BLRP), SE = sd(BLRP) / sqrt((length(BLRP))), 
                             SD = sd(BLRP))
seasons_shaded_BLRP$Time <- seasons_shaded_BLRP$Time + 6

# plot code:
g_seasons_shaded_BLRP <- ggplot(seasons_shaded_BLRP, aes(x=Time, y=M, group=Condition, color=Condition)) + 
  # annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 6.2, alpha = 0.3, fill = "gray50")+
  # annotate("text", x = 10, y = 6.5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 28, y = 6.5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 30, y = 6.5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 32, y = 6.5, label = "*", size = 5, color = "black")+
  geom_vline(xintercept = c(sunset1_mar, sunrise2_mar), linetype = "solid", linewidth = 0.2, col = "gray70") +
  geom_vline(xintercept = c(sunset1_sep, sunrise2_sep), linetype = "dashed", linewidth = 0.2, col = "gray70") +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width = 0.8, size = 0.8, 
                position=position_dodge(0.08)) +
  geom_line(aes(color=Condition), size= 1, alpha = 0.6) + 
  geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  theme_classic(base_size = 7) +
  theme(legend.position = "none",
        legend.text = element_text(size = 7, colour = "black"),
        legend.title = element_text(size = 7, colour = "black"),
        axis.text = element_text(size = 7, colour = "black"),
        axis.title = element_text(size = 7, colour = "black"),
        plot.title = element_text(size = 7, colour = "black"),
        plot.subtitle = element_text(size = 7, colour = "black"),
        plot.tag = element_text(size = 10, colour = "black", face = "bold"))+
  scale_colour_manual(values=c("#FF1493", "#522A17"))+
  scale_fill_manual(values=c("#FF1493", "#522A17"))+
  coord_cartesian(ylim = c(0, 6.2), clip = 'off') +
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,6,3), limits = c(0,6.2))+
  scale_x_continuous(breaks=c(18, 24, 30, 36), labels=c("18:00", "0:00", "6:00", "12:00")) +
  labs(title = expression(paste(italic("AhgpsbD BLRP"), " (shade)", sep="")), 
       tag = "J", 
       x = "Local time (hh:mm)", 
       y = expression(atop("Relative transcript",  paste("abundance"))))



### Integration of all plots into a figure
glist <- list(seasons_temperature_sun, seasons_temperature_shaded, 
              seasons_irradiance_sun, seasons_irradiance_shaded,
              g_seasons_sun_CCA1, g_seasons_shaded_CCA1,
              g_seasons_sun_SIG5, g_seasons_shaded_SIG5, 
              g_seasons_sun_BLRP, g_seasons_shaded_BLRP)
g <- ((glist[[1]] | glist[[2]]) / (glist[[3]] | glist[[4]]) / 
        (glist[[5]] | glist[[6]]) / (glist[[7]] | glist[[8]]) /
        (glist[[9]] | glist[[10]])) /
  legend_MarSep1 +
  plot_layout(heights = c(rep(5, 5), 1.5))

ggsave("figures/Fig.S2_MarSep_original_231202_v3.pdf",
       g, width = 130, height = 170, units = "mm")

