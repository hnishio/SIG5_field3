
# Set working directory
setwd("/Volume3/hnishio/R/SIG5_field2")

# Load packages
library(ggpubr)
library(tidyverse)
library(patchwork)
library(plyr)

# Load plot function
source("functions/Plot_functions.R")

# Create output directory
out <- "figures/"
if(file.exists(out)==F){
  dir.create(out, recursive=T)
}



test <- read.csv("data/march_data.csv")
sampling_time <- unique(test$Time)



### Temperature

## March sun x shaded
environment_data_march <- read.csv("data/environment_data_corrected_march.csv")
# subset sampling time
environment_data_march_sampling <- 
  environment_data_march[environment_data_march$Time %in% c(sampling_time, 31.31),]
environment_data_march_sampling$Time

environment_data_march_sampling$Condition = factor(environment_data_march_sampling$Condition, levels=c("Sun", "Shade"))

# plotting
temperature_mar <- ggplot(environment_data_march_sampling, aes(x=Time, y=Temperature, group=Condition, color=Condition)) + 
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 30, alpha = 0.3, fill = "gray50")+
  #geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  geom_line(aes(color=Condition), size= 1, alpha = 1) +
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
  labs(title = "Temperature (March/Spring)", tag = "A", 
       x = "Time relative to initial dawn (h)", 
       y = expression(atop("Temperature",  paste("(°C)" ))))


## September sun x shaded
environment_data_september <- read.csv("data/environment_data_corrected_september.csv")
# subset sampling time
environment_data_september_sampling <- 
  environment_data_september[environment_data_september$Time %in% c(sampling_time, 31.31),]
environment_data_september_sampling <- rbind(environment_data_september[1,], environment_data_september_sampling)
environment_data_september_sampling$Time

environment_data_september_sampling$Condition = factor(environment_data_september_sampling$Condition, levels=c("Sun", "Shade"))

# plot code:
temperature_sep <- ggplot(environment_data_september_sampling, aes(x=Time, y=Temperature, group=Condition, color=Condition)) + 
  annotate("rect", xmin = 12, xmax = 24, ymin = -3, ymax = 30, alpha = 0.3, fill = "gray50")+
  #geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  geom_line(aes(color=Condition), size= 1, alpha = 1) +
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
  labs(title = "Temperature (September/Autumn)", tag = "B", 
       x = "Time relative to initial dawn (h)", 
       y = expression(atop("Temperature",  paste("(°C)" ))))



### Irradiance
# plotting
irradiance_mar <- ggplot(environment_data_march_sampling, aes(x=Time, y=Irradiance, group=Condition, color=Condition)) + 
  annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 2500, alpha = 0.3, fill = "gray50")+
  #geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  geom_line(aes(color=Condition), size= 1, alpha = 1) +
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
  labs(title = "Irradiance (March/Spring)", tag = "C", 
       x = "Time relative to initial dawn (h)", 
       y = expression(atop("Total irradiance", paste(~(mu~mol~m^{-2}~s^{-1})))))

# plot code:
irradiance_sep <- ggplot(environment_data_september_sampling, aes(x=Time, y=Irradiance, group=Condition, color=Condition)) + 
  annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 2500, alpha = 0.3, fill = "gray50")+
  #geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  geom_line(aes(color=Condition), size= 1, alpha = 1) +
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
  labs(title = "Irradiance (September/Autumn)", tag = "D", 
       x = "Time relative to initial dawn (h)", 
       y = expression(atop("Total irradiance", paste(~(mu~mol~m^{-2}~s^{-1})))))



### CCA1

#Import and organize dataset
march_data <- read.csv("data/march_data.csv")
march_data$Condition = factor(march_data$Condition, levels=c("Sun", "Shade"))

# Function to calculate mean and standard deviation 
march_data_CCA1 <- ddply(march_data, .(Time, Condition), summarise, 
              M = mean(CCA1), SE = sd(CCA1) / sqrt((length(CCA1))), 
              SD = sd(CCA1))

# plot code:
g_march_data_CCA1 <- ggplot(march_data_CCA1, aes(x=Time, y=M, group=Condition, color=Condition)) + 
  annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 25, alpha = 0.3, fill = "gray50")+
  # annotate("text", x = 20, y = 10, label = "*", size = 5, color = "black")+
  # annotate("text", x = 26, y = 10, label = "*", size = 5, color = "black")+
  # annotate("text", x = 28, y = 10, label = "*", size = 5, color = "black")+
  # annotate("text", x = 30, y = 10, label = "*", size = 5, color = "black")+
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width = 0.8, size = 0.8, 
                position=position_dodge(0.01)) +
  geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  geom_line(aes(color=Condition), size= 1, alpha = 0.6) +
  theme_classic(base_size = 7) +
  theme(legend.position = "none",
        legend.text = element_text(size = 7, colour = "black"),
        legend.title = element_text(size = 7, colour = "black"),
        axis.text = element_text(size = 7, colour = "black"),
        axis.title = element_text(size = 7, colour = "black"),
        plot.title = element_text(size = 7, colour = "black"),
        plot.subtitle = element_text(size = 7, colour = "black"),
        plot.tag = element_text(size = 10, colour = "black", face = "bold"))+
  scale_colour_manual(values=c("orange", "gray30"))+
  scale_fill_manual(values=c("orange", "gray30"))+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,20,10))+
  scale_x_continuous(breaks=seq(12,30,6))+
  labs(title = expression(paste(italic(AhgCCA1), " (March/Spring)", sep="")),
       tag = "E", 
       x = "Time relative to initial dawn (h)", 
       y = expression(atop("Relative transcript",  paste("abundance"))))

#Import and organize dataset
september_data <- read.csv("data/september_data.csv")
september_data$Condition = factor(september_data$Condition, levels=c("Sun", "Shade"))

# Function to calculate meand and standard deviation 
september_data_CCA1 <- ddply(september_data, .(Time, Condition), summarise, 
                             M = mean(CCA1), SE = sd(CCA1) / sqrt((length(CCA1))), 
                             SD = sd(CCA1))

# plot code:
g_september_data_CCA1 <- ggplot(september_data_CCA1, aes(x=Time, y=M, group=Condition, color=Condition)) + 
  annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 25, alpha = 0.3, fill = "gray50")+
  # annotate("text", x = 22, y = 23, label = "*", size = 5, color = "black")+
  # annotate("text", x = 24, y = 23, label = "*", size = 5, color = "black")+
  # annotate("text", x = 26, y = 23, label = "*", size = 5, color = "black")+
  # annotate("text", x = 28, y = 23, label = "*", size = 5, color = "black")+
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
  scale_colour_manual(values=c("orange", "gray30"))+
  scale_fill_manual(values=c("orange", "gray30"))+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,20,10))+
  scale_x_continuous(breaks=seq(12,30,6))+
  labs(title = expression(paste(italic(AhgCCA1), " (September/Autumn)", sep="")),
       tag = "F", 
       x = "Time relative to initial dawn (h)", 
       y = expression(atop("Relative transcript",  paste("abundance"))))



### SIG5

# Function to calculate mean and standard deviation 
march_data_SIG5 <- ddply(march_data, .(Time, Condition), summarise, 
                         M = mean(SIG5), SE = sd(SIG5) / sqrt((length(SIG5))), 
                         SD = sd(SIG5))

# plot code:
g_march_data_SIG5 <- ggplot(march_data_SIG5, aes(x=Time, y=M, group=Condition, color=Condition)) + 
  annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 5, alpha = 0.3, fill = "gray50")+
  # annotate("text", x = 12, y = 4.5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 14, y = 4.5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 30, y = 4.5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 32, y = 4.5, label = "*", size = 5, color = "black")+
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width = 0.8, size = 0.8, 
                position=position_dodge(0.01)) +
  geom_line(aes(color=Condition), size= 1, alpha = 0.6) +
  geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  theme_classic(base_size = 7) +
  theme(legend.position = "none",
        legend.text = element_text(size = 7, colour = "black"),
        axis.text = element_text(size = 7, colour = "black"),
        legend.title = element_text(size = 7, colour = "black"),
        axis.title = element_text(size = 7, colour = "black"),
        plot.title = element_text(size = 7, colour = "black"),
        plot.subtitle = element_text(size = 7, colour = "black"),
        plot.tag = element_text(size = 10, colour = "black", face = "bold"))+
  scale_colour_manual(values=c("orange", "gray30"))+
  scale_fill_manual(values=c("orange", "gray30"))+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,5,2.5))+
  scale_x_continuous(breaks=seq(12,30,6))+
  labs(title = expression(paste(italic(AhgSIG5), " (March/Spring)", sep="")),
       tag = "G", 
       x = "Time relative to initial dawn (h)", 
       y = expression(atop("Relative transcript",  paste("abundance"))))

# Function to calculate meand and standard deviation 
september_data_SIG5 <- ddply(september_data, .(Time, Condition), summarise, 
                             M = mean(SIG5), SE = sd(SIG5) / sqrt((length(SIG5))), 
                             SD = sd(SIG5))

# plot code:
g_september_data_SIG5 <- ggplot(september_data_SIG5, aes(x=Time, y=M, group=Condition, color=Condition)) + 
  annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 5, alpha = 0.3, fill = "gray50")+
  # annotate("text", x = 32, y = 3, label = "*", size = 5, color = "black")+
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
  scale_colour_manual(values=c("orange", "gray30"))+
  scale_fill_manual(values=c("orange", "gray30"))+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,5,2.5))+
  scale_x_continuous(breaks=seq(12,30,6))+
  labs(title = expression(paste(italic(AhgSIG5), " (September/Autumn)", sep="")),
       tag = "H", 
       x = "Time relative to initial dawn (h)", 
       y = expression(atop("Relative transcript",  paste("abundance"))))



### BLRP

# Function to calculate meand and standard deviation 
march_data_BLRP <- ddply(march_data, .(Time, Condition), summarise, 
                         M = mean(BLRP), SE = sd(BLRP) / sqrt((length(BLRP))), 
                         SD = sd(BLRP))

# plot code:
g_march_data_BLRP <- ggplot(march_data_BLRP, aes(x=Time, y=M, group=Condition, color=Condition)) + 
  annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 6.2, alpha = 0.3, fill = "gray50")+
  # annotate("text", x = 8, y = 5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 10, y = 5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 12, y = 5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 14, y = 5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 16, y = 5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 18, y = 5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 20, y = 5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 22, y = 5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 24, y = 5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 26, y = 5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 28, y = 5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 30, y = 5, label = "*", size = 5, color = "black")+
  # annotate("text", x = 32, y = 5, label = "*", size = 5, color = "black")+
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
  scale_colour_manual(values=c("orange", "gray30"))+
  scale_fill_manual(values=c("orange", "gray30"))+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,6,3), limits = c(0,6.2))+
  scale_x_continuous(breaks=seq(12,30,6))+
  labs(title = expression(paste(italic("AhgpsbD BLRP"), " (March/Spring)", sep="")),
       tag = "I", 
       x = "Time relative to initial dawn (h)", 
       y = expression(atop("Relative transcript",  paste("abundance"))))

# Function to calculate meand and standard deviation 
september_data_BLRP <- ddply(september_data, .(Time, Condition), summarise, 
                             M = mean(BLRP), SE = sd(BLRP) / sqrt((length(BLRP))), 
                             SD = sd(BLRP))

# plot code:
g_september_data_BLRP <- ggplot(september_data_BLRP, aes(x=Time, y=M, group=Condition, color=Condition)) + 
  annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 6.2, alpha = 0.3, fill = "gray50")+
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
  scale_colour_manual(values=c("orange", "gray30"))+
  scale_fill_manual(values=c("orange", "gray30"))+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,6,3), limits = c(0,6.2))+
  scale_x_continuous(breaks=seq(12,30,6))+
  labs(title = expression(paste(italic("AhgpsbD BLRP"), " (September/Autumn)", sep="")),
       tag = "J", 
       x = "Time relative to initial dawn (h)", 
       y = expression(atop("Relative transcript",  paste("abundance"))))



### Integration of all plots into a figure
glist <- list(temperature_mar, temperature_sep,
              irradiance_mar, irradiance_sep, 
              g_march_data_CCA1, g_september_data_CCA1, 
              g_march_data_SIG5, g_september_data_SIG5, 
              g_march_data_BLRP, g_september_data_BLRP)
g <- ((glist[[1]] | glist[[2]]) / (glist[[3]] | glist[[4]]) / 
  (glist[[5]] | glist[[6]]) / (glist[[7]] | glist[[8]]) /
  (glist[[9]] | glist[[10]])) /
     legend_SunShade1 +
  plot_layout(heights = c(rep(5, 5), 1.5))   # Adjust plot height by nrow

ggsave("figures/Fig.S5_SunShade_original_230517.pdf",
       g, width = 130, height = 170, units = "mm")

