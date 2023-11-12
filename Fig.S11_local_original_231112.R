
# Set working directory
setwd("/Volume3/hnishio/R/SIG5_field3")

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


test <- read.csv("data/local_data.csv")
sampling_time <- unique(test$Time)



### Temperature
local_environment_data <- read.csv("data/local_environment_data.csv")
# subset sampling time
local_environment_data_sampling <- 
  local_environment_data[local_environment_data$Time %in% c(sampling_time+0.03, 38.58),]

local_environment_data_sampling$Condition = factor(local_environment_data_sampling$Condition, 
                                                   levels=c("Ambient Conditions", "Warm", "Chill", "Low light"))
local_environment_data_sampling <- local_environment_data_sampling[local_environment_data_sampling$Condition!=unique(local_environment_data_sampling$Condition)[4],]

# plot code:
g_temp <- ggplot(local_environment_data_sampling, aes(x=Time, y=Temperature, group=Condition, color=Condition)) + 
  annotate("rect", xmin = 12, xmax = 24, ymin = 13, ymax = 32, alpha = 0.3, fill = "gray50")+
  annotate("rect", xmin = 36, xmax = 39, ymin = 13, ymax = 32, alpha = 0.3, fill = "gray50")+
  #geom_point(aes(color=Condition), shape=19, size=1, alpha = 0.6, stroke = 1.2)+
  geom_line(aes(color=Condition), size= 1, alpha = 1) +
  theme_classic(7) +
  theme(legend.position = "none",
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        plot.title = element_blank(),
        plot.tag = element_text(size = 10, face = "bold"))+
  scale_colour_manual(values=c("black", "orangered", "cyan3"))+
  scale_fill_manual(values=c("black", "orangered", "cyan3"))+
  scale_y_continuous(expand = c(0, 0), breaks=seq(15,30,5), limits = c(13, 32))+
  scale_x_continuous(breaks=seq(12,36,6), limits=c(11,39))+
  labs(
    #title = "Temperature", subtitle = "Multiple local treatments", 
    tag = "D", 
    x = "Time relative to initial dawn (h)", 
    y = expression(atop("Temperature",  paste("(°C)" ))))




### CCA1

#Import and organize dataset
local_data <- read.csv("data/local_data.csv")
local_data$Condition = factor(local_data$Condition, levels=c("Ambient Conditions", "Warm", "Chill", "Low light"))
local_data <- local_data[local_data$Condition!=unique(local_data$Condition)[4],]


# Function to calculate meand and standard deviation 
local_data_CCA1 <- ddply(local_data, .(Time, Condition), summarise, 
                             M = mean(CCA1), SE = sd(CCA1) / sqrt((length(CCA1))), 
                             SD = sd(CCA1))

# plot code:
g_local_data_CCA1 <- ggplot(local_data_CCA1, aes(x=Time, y=M, group=Condition, color=Condition)) + 
  annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 32, alpha = 0.3, fill = "gray50")+
  annotate("rect", xmin = 36, xmax = 39, ymin = 0, ymax = 32, alpha = 0.3, fill = "gray50")+
  # annotate("text", x = 25, y = 37, label = "*", size = 5, color = "orangered")+
  # annotate("text", x = 23, y = 21, label = "*", size = 5, color = "cyan3")+
  # annotate("text", x = 19, y = 8, label = "*", size = 5, color = "lavenderblush4")+
  # annotate("text", x = 21, y = 14, label = "*", size = 5, color = "lavenderblush4")+
  # annotate("text", x = 25, y = 33, label = "*", size = 5, color = "lavenderblush4")+
  # annotate("text", x = 27, y = 22, label = "*", size = 5, color = "lavenderblush4")+
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width = 2, size = 0.8, 
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
  scale_colour_manual(values=c("black", "orangered", "cyan3"))+
  scale_fill_manual(values=c("black", "orangered", "cyan3"))+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,30,10))+
  scale_x_continuous(breaks=seq(12,36,6))+
  labs(expression(italic(AhgCCA1)),
       tag = "E", 
       x = "Time relative to initial dawn (h)", 
       y = "Relative\ntranscript\nabundance")



### SIG5

# Function to calculate meand and standard deviation 
local_data_SIG5 <- ddply(local_data, .(Time, Condition), summarise, 
                         M = mean(SIG5), SE = sd(SIG5) / sqrt((length(SIG5))), 
                         SD = sd(SIG5))

# plot code:
g_local_data_SIG5 <- ggplot(local_data_SIG5, aes(x=Time, y=M, group=Condition, color=Condition)) + 
  annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 5.3, alpha = 0.3, fill = "gray50")+
  annotate("rect", xmin = 36, xmax = 39, ymin = 0, ymax = 5.3, alpha = 0.3, fill = "gray50")+
  # annotate("text", x = 27, y = 6, label = "*", size = 5, color = "cyan3")+
  # annotate("text", x = 29, y = 3.7, label = "*", size = 5, color = "cyan3")+
  # annotate("text", x = 33, y = 3.7, label = "*", size = 5, color = "cyan3")+
  # annotate("text", x = 35, y = 3.7, label = "*", size = 5, color = "cyan3")+
  # annotate("text", x = 37, y = 3.7, label = "*", size = 5, color = "cyan3")+
  # annotate("text", x = 39, y = 3.7, label = "*", size = 5, color = "cyan3")+
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width = 2, size = 0.8, 
                position=position_dodge(0.08)) +
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
  scale_colour_manual(values=c("black", "orangered", "cyan3"))+
  scale_fill_manual(values=c("black", "orangered", "cyan3"))+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,5,1))+
  scale_x_continuous(breaks=seq(12,36,6))+
  labs(expression(italic(AhgSIG5)),
       tag = "F", 
       x = "Time relative to initial dawn (h)", 
       y = "Relative\ntranscript\nabundance")


### BLRP

# Function to calculate meand and standard deviation 
local_data_BLRP <- ddply(local_data, .(Time, Condition), summarise, 
                         M = mean(BLRP, na.rm=T), SE = sd(BLRP, na.rm=T) / sqrt((length(BLRP))), 
                         SD = sd(BLRP, na.rm=T))

# plot code:
g_local_data_BLRP <- ggplot(local_data_BLRP, aes(x=Time, y=M, group=Condition, color=Condition)) + 
  annotate("rect", xmin = 12, xmax = 24, ymin = 0, ymax = 9, alpha = 0.3, fill = "gray50")+
  annotate("rect", xmin = 36, xmax = 39, ymin = 0, ymax = 9, alpha = 0.3, fill = "gray50")+
  # annotate("text", x = 25, y = 5, label = "*", size = 5, color = "orangered")+
  # annotate("text", x = 27, y = 5, label = "*", size = 5, color = "orangered")+
  # annotate("text", x = 29, y = 6, label = "*", size = 5, color = "orangered")+
  # annotate("text", x = 31, y = 7, label = "*", size = 5, color = "orangered")+
  # annotate("text", x = 33, y = 8, label = "*", size = 5, color = "orangered")+
  # annotate("text", x = 35, y = 11, label = "*", size = 5, color = "orangered")+
  # annotate("text", x = 37, y = 10, label = "*", size = 5, color = "orangered")+
  # annotate("text", x = 31, y = 6, label = "*", size = 5, color = "cyan3")+
  # annotate("text", x = 33, y = 7, label = "*", size = 5, color = "cyan3")+
  # annotate("text", x = 35, y = 10, label = "*", size = 5, color = "cyan3")+
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width = 2, size = 0.8, 
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
  scale_colour_manual(values=c("black", "orangered", "cyan3"))+
  scale_fill_manual(values=c("black", "orangered", "cyan3"))+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,8,2))+
  scale_x_continuous(breaks=seq(12,36,6))+
  labs(expression(italic("AhgpsbD BLRP")),
       tag = "G", 
       x = "Time relative to initial dawn (h)", 
       y = "Relative\ntranscript\nabundance")



### Integration of all plots into a figure
void <- ggplot() + theme_void()
glist <- list(g_temp, g_local_data_CCA1, 
              g_local_data_SIG5, g_local_data_BLRP)
g <- ((glist[[1]] | glist[[2]]) / 
        (glist[[3]] | glist[[4]])) / 
  legend_local1 +
  plot_layout(heights = c(5, 5, 1.5))

ggsave("figures/Fig.S11_local_original_231112.pdf",
       g, width = 130, height = 80, units = "mm")

