
# Set working directory
setwd("/Volume3/hnishio/R/SIG5_field3")

# Install rEDM
remotes::install_github("ha0ye/rEDM")

# Load packages
library(rEDM); packageVersion("rEDM") # v0.7.5
library(tidyverse)
library(patchwork)
library(ggpubr)
library(data.table)

# Create output directory
out <- "07_Embedding_local/"
if(file.exists(out)==F){
  dir.create(out, recursive=T)
}

# Function
scale_c <- function(x){(x-mean(x))/sd(x)}



### Preparation of data

# Load results of STM
input <- "03_STM_diff_local_rep/"
df_CCA1_local <- as.data.frame(fread(paste0(input, "STM_diff_local_CCA1rep.csv")))
df_SIG5_local <- as.data.frame(fread(paste0(input, "STM_diff_local_SIG5rep.csv")))
df_BLRP_local <- as.data.frame(fread(paste0(input, "STM_diff_local_BLRPrep.csv")))

# vec_CCA1 <- c(df_CCA1_local$`mu1_50%`, df_CCA1_local$`mu2_50%`, df_CCA1_local$`mu3_50%`)
# vec_SIG5 <- c(df_SIG5_local$`mu1_50%`, df_SIG5_local$`mu2_50%`, df_SIG5_local$`mu3_50%`)
# vec_BLRP <- c(df_BLRP_local$`mu1_50%`, df_BLRP_local$`mu2_50%`, df_BLRP_local$`mu3_50%`)

vec_CCA1 <- c(df_CCA1_local$data_A, df_CCA1_local$data_W, df_CCA1_local$data_C)
vec_SIG5 <- c(df_SIG5_local$data_A, df_SIG5_local$data_W, df_SIG5_local$data_C)
vec_BLRP <- c(df_BLRP_local$data_A, df_BLRP_local$data_W, df_BLRP_local$data_C)
vec_all <- cbind(vec_CCA1, vec_SIG5, vec_BLRP)


# Scaling
d <- apply(vec_all, 2, scale_c)

# Add NA
d <- rbind(d[1:15,], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
           d[16:30,], NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
           d[31:45,])
colnames(d) <- c("CCA1", "SIG5", "BLRP")
fwrite(d, file = paste0(out, "local_data_edm.csv"))



### Embedding

## Load data
d <- as.data.frame(fread(paste0(out, "local_data_edm.csv")))


## Load function
source("functions/EDM_local.R")


## Simplex projection
genes <- names(d)
lib_type_used <- "full"

for(i in 1:length(genes)){
  eval(parse(text = paste0(
    "E_", genes[i], " <- TestE(d[,", i, "], d_name='", genes[i], "', E_range = 1:10, 
                  lib_type = lib_type_used, E_only = T)"
  )))
}


## Update the dimensions
E_CCA1 <- 2
E_SIG5 <- 4

## Save output
save.image(paste0(out, "07_Embedding_local.RData"))


## Visualize E
genes <- names(d)
lib_type_used <- "full"

for(i in 1:length(genes)){
  eval(parse(text = paste0(
    "g_", genes[i], " <- Eplot(d[,", i, "], d_name='", genes[i], "', E_range = 1:10, 
                  lib_type = lib_type_used)
      ggsave(paste0(out, 'E_', genes[i], '_local.pdf'),
         g_", genes[i], ", height = 35, width = 120, units = 'mm')"
  )))
}





##### Plotting prediction #####

## Setting
genes <- names(d)
# lib_l <- matrix(c(1,15,21,35,41,55), ncol = 2, byrow = T) #when no. of NA is 5
lib_l <- matrix(c(1,15,28,42,55,69), ncol = 2, byrow = T) #when no. of NA is 12
pred_l <- lib_l


## CCA1
for(i in 1){
  # Legend
  df_legend <- data.frame(Time=1:10, 
                          Temperature=1:10, 
                          Condition=c(rep("Observed",2), rep("E = 2",2), rep("E = 8",2), rep("E = 10",4)))
  df_legend$Condition <- factor(df_legend$Condition, levels=c("Observed", "E = 2", "E = 8", "E = 10"))
  
  gl <- ggplot(df_legend, aes(x=Time, y=Temperature, group=Condition, color=Condition)) + 
    geom_line() +
    geom_point() +
    theme_classic(base_size = 7) +
    theme(legend.text = element_text(size = 7),
          legend.title = element_blank(),
          legend.key.size = unit(0.8, 'lines')) +
    scale_colour_manual(values=c("black", "black", "orange", "steelblue1"),
                        guide = guide_legend(override.aes = list(
                          linetype = c("blank", rep("solid",3)),
                          shape = c(16, rep(NA,3)))))
  
  legend_CCA1_1 <- as_ggplot(get_legend(gl + guides(color = guide_legend(nrow = 1))))
  legend_CCA1_2 <- as_ggplot(get_legend(gl))
  
  # Prediction
  simplex.out <- simplex(unclass(d[genes[i]])[[1]], lib_l, pred_l, E = 1:10, silent = T, stats_only = F)
  df_A <- simplex.out$model_output[[2]][1:14,1:3] %>% 
    mutate(time = 1:14,
           pred2 = simplex.out$model_output[[8]][1:14,"pred"],
           pred3 = simplex.out$model_output[[10]][1:14,"pred"])
  df_W <- simplex.out$model_output[[2]][16:29,1:3] %>% 
    mutate(time = 1:14,
           pred2 = simplex.out$model_output[[8]][1:14,"pred"],
           pred3 = simplex.out$model_output[[10]][1:14,"pred"])
  df_C <- simplex.out$model_output[[2]][31:44,1:3] %>% 
    mutate(time = 1:14,
           pred2 = simplex.out$model_output[[8]][1:14,"pred"],
           pred3 = simplex.out$model_output[[10]][1:14,"pred"])
  
  ymin <- min(c(c(as.matrix(df_A[,-1])), c(as.matrix(df_W[,-1])), c(as.matrix(df_C[,-1]))), na.rm = T)
  ymax <- max(c(c(as.matrix(df_A[,-1])), c(as.matrix(df_W[,-1])), c(as.matrix(df_C[,-1]))), na.rm = T)
  
  g_CCA1_1 <- ggplot(data = df_A) +
    geom_point(aes(x = time, y = obs), col="black") +
    geom_line(aes(x = time, y = pred), col="black") +
    geom_line(aes(x = time, y = pred2), col="orange") +
    geom_line(aes(x = time, y = pred3), col="steelblue1") +
    ylim(c(ymin, ymax)) +
    theme_test(base_size = 7) +
    theme(legend.position = "none",
          axis.text = element_text(size = 7),
          axis.title = element_text(size = 7),
          plot.title = element_text(size = 7),
          plot.margin = unit(c(1.5,1,1.5,1), "mm")) +
    labs(title = bquote(paste(italic(Ahg), italic(.(genes[i])), " (ambient)", sep = "")), 
         x = "Time points", y = "Transcript abundance")
  
  g_CCA1_2 <- ggplot(data = df_W) +
    geom_point(aes(x = time, y = obs), col="black") +
    geom_line(aes(x = time, y = pred), col="black") +
    geom_line(aes(x = time, y = pred2), col="orange") +
    geom_line(aes(x = time, y = pred3), col="steelblue1") +
    ylim(c(ymin, ymax)) +
    theme_test(base_size = 7) +
    theme(legend.position = "none",
          axis.text = element_text(size = 7),
          axis.title = element_text(size = 7),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 7),
          plot.margin = unit(c(1.5,1,1.5,1), "mm")) +
    labs(title = bquote(paste(italic(Ahg), italic(.(genes[i])), " (warm)", sep = "")), 
         x = "Time points")
  
  g_CCA1_3 <- ggplot(data = df_C) +
    geom_point(aes(x = time, y = obs), col="black") +
    geom_line(aes(x = time, y = pred), col="black") +
    geom_line(aes(x = time, y = pred2), col="orange") +
    geom_line(aes(x = time, y = pred3), col="steelblue1") +
    ylim(c(ymin, ymax)) +
    theme_test(base_size = 7) +
    theme(legend.position = "none",
          axis.text = element_text(size = 7),
          axis.title = element_text(size = 7),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 7),
          plot.margin = unit(c(1.5,1,1.5,1), "mm")) +
    labs(title = bquote(paste(italic(Ahg), italic(.(genes[i])), " (cool)", sep = "")), 
         x = "Time points")
  
  g <- g_CCA1_1 + g_CCA1_2 + g_CCA1_3 + legend_CCA1_2 + 
    plot_layout(ncol = 4, widths = c(3,3,3,1.2))
  ggsave(paste0(out, "pred_", genes[i],".pdf"),
         g, height = 40, width = 180, units = "mm")
}


## SIG5
for(i in 2){
  # Legend
  df_legend <- data.frame(Time=1:10, 
                          Temperature=1:10, 
                          Condition=c(rep("Observed",2), rep("E = 4",2), rep("E = 8",2), rep("E = 9",4)))
  df_legend$Condition <- factor(df_legend$Condition, levels=c("Observed", "E = 4", "E = 8", "E = 9"))
  
  gl <- ggplot(df_legend, aes(x=Time, y=Temperature, group=Condition, color=Condition)) + 
    geom_line() +
    geom_point() +
    theme_classic(base_size = 7) +
    theme(legend.text = element_text(size = 7),
          legend.title = element_blank(),
          legend.key.size = unit(0.8, 'lines')) +
    scale_colour_manual(values=c("black", "black", "orange", "steelblue1"),
                        guide = guide_legend(override.aes = list(
                          linetype = c("blank", rep("solid",3)),
                          shape = c(16, rep(NA,3)))))
  
  legend_SIG5_1 <- as_ggplot(get_legend(gl + guides(color = guide_legend(nrow = 1))))
  legend_SIG5_2 <- as_ggplot(get_legend(gl))
  
  # Prediction
  simplex.out <- simplex(unclass(d[genes[i]])[[1]], lib_l, pred_l, E = 1:10, silent = T, stats_only = F)
  df_A <- simplex.out$model_output[[4]][1:14,1:3] %>% 
    mutate(time = 1:14,
           pred2 = simplex.out$model_output[[8]][1:14,"pred"],
           pred3 = simplex.out$model_output[[9]][1:14,"pred"])
  df_W <- simplex.out$model_output[[4]][16:29,1:3] %>% 
    mutate(time = 1:14,
           pred2 = simplex.out$model_output[[8]][1:14,"pred"],
           pred3 = simplex.out$model_output[[9]][1:14,"pred"])
  df_C <- simplex.out$model_output[[4]][31:44,1:3] %>% 
    mutate(time = 1:14,
           pred2 = simplex.out$model_output[[8]][1:14,"pred"],
           pred3 = simplex.out$model_output[[9]][1:14,"pred"])
  
  ymin <- min(c(c(as.matrix(df_A[,-1])), c(as.matrix(df_W[,-1])), c(as.matrix(df_C[,-1]))), na.rm = T)
  ymax <- max(c(c(as.matrix(df_A[,-1])), c(as.matrix(df_W[,-1])), c(as.matrix(df_C[,-1]))), na.rm = T)
  
  g_SIG5_1 <- ggplot(data = df_A) +
    geom_point(aes(x = time, y = obs), col="black") +
    geom_line(aes(x = time, y = pred), col="black") +
    geom_line(aes(x = time, y = pred2), col="orange") +
    geom_line(aes(x = time, y = pred3), col="steelblue1") +
    ylim(c(ymin, ymax)) +
    theme_test(base_size = 7) +
    theme(legend.position = "none",
          axis.text = element_text(size = 7),
          axis.title = element_text(size = 7),
          plot.title = element_text(size = 7),
          plot.margin = unit(c(1.5,1,1.5,1), "mm")) +
    labs(title = bquote(paste(italic(Ahg), italic(.(genes[i])), " (ambient)", sep = "")), 
         x = "Time points", y = "Transcript abundance")
  
  g_SIG5_2 <- ggplot(data = df_W) +
    geom_point(aes(x = time, y = obs), col="black") +
    geom_line(aes(x = time, y = pred), col="black") +
    geom_line(aes(x = time, y = pred2), col="orange") +
    geom_line(aes(x = time, y = pred3), col="steelblue1") +
    ylim(c(ymin, ymax)) +
    theme_test(base_size = 7) +
    theme(legend.position = "none",
          axis.text = element_text(size = 7),
          axis.title = element_text(size = 7),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 7),
          plot.margin = unit(c(1.5,1,1.5,1), "mm")) +
    labs(title = bquote(paste(italic(Ahg), italic(.(genes[i])), " (warm)", sep = "")), 
         x = "Time points")
  
  g_SIG5_3 <- ggplot(data = df_C) +
    geom_point(aes(x = time, y = obs), col="black") +
    geom_line(aes(x = time, y = pred), col="black") +
    geom_line(aes(x = time, y = pred2), col="orange") +
    geom_line(aes(x = time, y = pred3), col="steelblue1") +
    ylim(c(ymin, ymax)) +
    theme_test(base_size = 7) +
    theme(legend.position = "none",
          axis.text = element_text(size = 7),
          axis.title = element_text(size = 7),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 7),
          plot.margin = unit(c(1.5,1,1.5,1), "mm")) +
    labs(title = bquote(paste(italic(Ahg), italic(.(genes[i])), " (cool)", sep = "")), 
         x = "Time points")
  
  g <- g_SIG5_1 + g_SIG5_2 + g_SIG5_3 + legend_SIG5_2 + 
    plot_layout(ncol = 4, widths = c(3,3,3,1.2))
  ggsave(paste0(out, "pred_", genes[i],".pdf"),
         g, height = 40, width = 180, units = "mm")
}


## BLRP
for(i in 3){
  # Legend
  df_legend <- data.frame(Time=1:10, 
                          Temperature=1:10, 
                          Condition=c(rep("Observed",2), rep("E = 1",2), rep("E = 2",2), rep("E = 4",4)))
  df_legend$Condition <- factor(df_legend$Condition, levels=c("Observed", "E = 1", "E = 2", "E = 4"))
  
  gl <- ggplot(df_legend, aes(x=Time, y=Temperature, group=Condition, color=Condition)) + 
    geom_line() +
    geom_point() +
    theme_classic(base_size = 7) +
    theme(legend.text = element_text(size = 7),
          legend.title = element_blank(),
          legend.key.size = unit(0.8, 'lines')) +
    scale_colour_manual(values=c("black", "black", "orange", "steelblue1"),
                        guide = guide_legend(override.aes = list(
                          linetype = c("blank", rep("solid",3)),
                          shape = c(16, rep(NA,3)))))
  
  legend_BLRP_1 <- as_ggplot(get_legend(gl + guides(color = guide_legend(nrow = 1))))
  legend_BLRP_2 <- as_ggplot(get_legend(gl))
  
  # Prediction
  simplex.out <- simplex(unclass(d[genes[i]])[[1]], lib_l, pred_l, E = 1:10, silent = T, stats_only = F)
  df_A <- simplex.out$model_output[[1]][1:14,1:3] %>% 
    mutate(time = 1:14,
           pred2 = simplex.out$model_output[[2]][1:14,"pred"],
           pred3 = simplex.out$model_output[[4]][1:14,"pred"])
  df_W <- simplex.out$model_output[[1]][16:29,1:3] %>% 
    mutate(time = 1:14,
           pred2 = simplex.out$model_output[[2]][1:14,"pred"],
           pred3 = simplex.out$model_output[[4]][1:14,"pred"])
  df_C <- simplex.out$model_output[[1]][31:44,1:3] %>% 
    mutate(time = 1:14,
           pred2 = simplex.out$model_output[[2]][1:14,"pred"],
           pred3 = simplex.out$model_output[[4]][1:14,"pred"])
  
  ymin <- min(c(c(as.matrix(df_A[,-1])), c(as.matrix(df_W[,-1])), c(as.matrix(df_C[,-1]))), na.rm = T)
  ymax <- max(c(c(as.matrix(df_A[,-1])), c(as.matrix(df_W[,-1])), c(as.matrix(df_C[,-1]))), na.rm = T)
  
  g_BLRP_1 <- ggplot(data = df_A) +
    geom_point(aes(x = time, y = obs), col="black") +
    geom_line(aes(x = time, y = pred), col="black") +
    geom_line(aes(x = time, y = pred2), col="orange") +
    geom_line(aes(x = time, y = pred3), col="steelblue1") +
    ylim(c(ymin, ymax)) +
    theme_test(base_size = 7) +
    theme(legend.position = "none",
          axis.text = element_text(size = 7),
          axis.title = element_text(size = 7),
          plot.title = element_text(size = 7),
          plot.margin = unit(c(1.5,1,1.5,1), "mm")) +
    labs(title = bquote(paste(italic(AhgpsbD ), italic(.(genes[i])), " (ambient)", sep = "")), 
         x = "Time points", y = "Transcript abundance")
  
  g_BLRP_2 <- ggplot(data = df_W) +
    geom_point(aes(x = time, y = obs), col="black") +
    geom_line(aes(x = time, y = pred), col="black") +
    geom_line(aes(x = time, y = pred2), col="orange") +
    geom_line(aes(x = time, y = pred3), col="steelblue1") +
    ylim(c(ymin, ymax)) +
    theme_test(base_size = 7) +
    theme(legend.position = "none",
          axis.text = element_text(size = 7),
          axis.title = element_text(size = 7),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 7),
          plot.margin = unit(c(1.5,1,1.5,1), "mm")) +
    labs(title = bquote(paste(italic(AhgpsbD ), italic(.(genes[i])), " (warm)", sep = "")), 
         x = "Time points")
  
  g_BLRP_3 <- ggplot(data = df_C) +
    geom_point(aes(x = time, y = obs), col="black") +
    geom_line(aes(x = time, y = pred), col="black") +
    geom_line(aes(x = time, y = pred2), col="orange") +
    geom_line(aes(x = time, y = pred3), col="steelblue1") +
    ylim(c(ymin, ymax)) +
    theme_test(base_size = 7) +
    theme(legend.position = "none",
          axis.text = element_text(size = 7),
          axis.title = element_text(size = 7),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 7),
          plot.margin = unit(c(1.5,1,1.5,1), "mm")) +
    labs(title = bquote(paste(italic(AhgpsbD ), italic(.(genes[i])), " (cool)", sep = "")), 
         x = "Time points")
  
  g <- g_BLRP_1 + g_BLRP_2 + g_BLRP_3 + legend_BLRP_2 + 
    plot_layout(ncol = 4, widths = c(3,3,3,1.2))
  ggsave(paste0(out, "pred_", genes[i],".pdf"),
         g, height = 40, width = 180, units = "mm")
}


## Update the dimensions
E_CCA1 <- 2
E_SIG5 <- 4

## Save output
save.image(paste0(out, "07_Embedding_local_ggplot.RData"))

