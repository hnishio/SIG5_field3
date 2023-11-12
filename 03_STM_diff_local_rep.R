
# Set working directory
setwd("/Volume3/hnishio/R/SIG5_field2")

# Load packages
library(bayesplot)
library(ggpubr)
library(tidyverse)
library(data.table)
library(patchwork)
library(cmdstanr)
set_cmdstan_path("~/cmdstan/")

# Create output directory
out <- "03_STM_diff_local_rep/"
if(file.exists(out)==F){
  dir.create(out, recursive=T)
}

# Load functions
source("functions/STM_diff_local_rep.R")



##### Lacal data #####

# Preparation of data
data <- read.csv("data/local_data.csv")
data1 <- subset(data, Condition=="Ambient Conditions")
data1former <- data1[data1$Time!=35&data1$Time!=37&data1$Time!=39,]
data1latter <- data1[data1$Time!=11&data1$Time!=13&data1$Time!=15,]
data1rep <- rbind(data1former, data1, data1latter)
data1rep$Time <- c(data1former$Time - 24, data1$Time, data1latter$Time + 24)

data2 <- subset(data, Condition=="Warm")
data2former <- data2[data2$Time!=35&data2$Time!=37&data2$Time!=39,]
data2latter <- data2[data2$Time!=11&data2$Time!=13&data2$Time!=15,]
data2rep <- rbind(data2former, data2, data2latter)
data2rep$Time <- c(data2former$Time - 24, data2$Time, data2latter$Time + 24)

data3 <- subset(data, Condition=="Chill")
data3former <- data3[data3$Time!=35&data3$Time!=37&data3$Time!=39,]
data3latter <- data3[data3$Time!=11&data3$Time!=13&data3$Time!=15,]
data3rep <- rbind(data3former, data3, data3latter)
data3rep$Time <- c(data3former$Time - 24, data3$Time, data3latter$Time + 24)

data4 <- subset(data, Condition=="Low light")
data4former <- data4[data4$Time!=35&data4$Time!=37&data4$Time!=39,]
data4latter <- data4[data4$Time!=11&data4$Time!=13&data4$Time!=15,]
data4rep <- rbind(data4former, data4, data4latter)
data4rep$Time <- c(data4former$Time - 24, data4$Time, data4latter$Time + 24)

# CCA1
STM_diff_local(data1 = data1rep, data2 = data2rep, data3 = data3rep, data4 = data4rep, 
               gene_idx = 2, ps = 7, 
               data_start = 13, data_end = 27)

# SIG5
STM_diff_local(data1 = data1rep, data2 = data2rep, data3 = data3rep, data4 = data4rep, 
               gene_idx = 3, ps = 7, 
               data_start = 13, data_end = 27)

# BLRP
STM_diff_local(data1 = na.omit(data1rep), data2 = na.omit(data2rep), data3 = na.omit(data3rep), data4 = na.omit(data4rep), 
               gene_idx = 4, ps = 7, 
               data_start = 13, data_end = 27)

# Check
as.data.frame(fread(
  paste0(out, "STM_diff_local_", names(data1)[2], "rep.csv")))

