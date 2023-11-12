
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
out <- "02_STM_diff_SunShade_rep/"
if(file.exists(out)==F){
  dir.create(out, recursive=T)
}

# Load functions
source("functions/STM_diff_rep.R")


##### March data #####

# Preparation of data
data <- read.csv("data/march_data.csv")
data1 <- subset(data, Condition=="Sun")
data1former <- data1[data1$Time!=32,]
data1latter <- data1[data1$Time!=8,]
data1rep <- rbind(data1former, data1, data1latter)
data1rep$Time <- c(data1former$Time - 24, data1$Time, data1latter$Time + 24)

data2 <- subset(data, Condition=="Shade")
data2former <- data2[data2$Time!=32,]
data2latter <- data2[data2$Time!=8,]
data2rep <- rbind(data2former, data2, data2latter)
data2rep$Time <- c(data2former$Time - 24, data2$Time, data2latter$Time + 24)

# CCA1
STM_diff(data1 = data1rep, data2 = data2rep, gene_idx = 2, 
         condition_name = "Mar", ps = 7, 
         data_start = 13, data_end = 25)

# SIG5
STM_diff(data1 = data1rep, data2 = data2rep, gene_idx = 3, 
         condition_name = "Mar", ps = 7, 
         data_start = 13, data_end = 25)

# BLRP
STM_diff(data1 = data1rep, data2 = data2rep, gene_idx = 4, 
         condition_name = "Mar", ps = 7, 
         data_start = 13, data_end = 25)





##### September data #####

# Preparation of data
data <- read.csv("data/september_data.csv")
data1 <- subset(data, Condition=="Sun")
data1former <- data1[data1$Time!=32,]
data1latter <- data1[data1$Time!=8,]
data1rep <- rbind(data1former, data1, data1latter)
data1rep$Time <- c(data1former$Time - 24, data1$Time, data1latter$Time + 24)

data2 <- subset(data, Condition=="Shade")
data2former <- data2[data2$Time!=32,]
data2latter <- data2[data2$Time!=8,]
data2rep <- rbind(data2former, data2, data2latter)
data2rep$Time <- c(data2former$Time - 24, data2$Time, data2latter$Time + 24)

# CCA1
STM_diff(data1 = data1rep, data2 = data2rep, gene_idx = 2, 
         condition_name = "Sep", ps = 7, 
         data_start = 13, data_end = 25)

# SIG5
STM_diff(data1 = data1rep, data2 = data2rep, gene_idx = 3, 
         condition_name = "Sep", ps = 7, 
         data_start = 13, data_end = 25)

# BLRP
STM_diff(data1 = data1rep, data2 = data2rep, gene_idx = 4, 
         condition_name = "Sep", ps = 7, 
         data_start = 13, data_end = 25)

