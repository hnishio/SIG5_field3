
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


# Load data
data <- read.csv("data/march_data.csv")
data_MarSun <- subset(data, Condition=="Sun")
data_MarShade <- subset(data, Condition=="Shade")
data <- read.csv("data/september_data.csv")
data_SepSun <- subset(data, Condition=="Sun")
data_SepShade <- subset(data, Condition=="Shade")


### SIG5

# Load output
df <- as.data.frame(fread("04_SSM_cyclic_MarSep_SIG5/SSM_cyclic_MarSep_SIG5_Lagtemp0_Laglight0_LagCCA10.csv"))

# Adjustment of data frame
df_alpha_MarSun <- df %>% filter(str_starts(par, "alpha_MarSun")) %>% select(c("2.5%", "50%", "97.5%"))
df_alpha_MarShade <- df %>% filter(str_starts(par, "alpha_MarShade")) %>% select(c("2.5%", "50%", "97.5%"))
df_alpha_SepSun <- df %>% filter(str_starts(par, "alpha_SepSun")) %>% select(c("2.5%", "50%", "97.5%"))
df_alpha_SepShade <- df %>% filter(str_starts(par, "alpha_SepShade")) %>% select(c("2.5%", "50%", "97.5%"))
df_b_temp <- df %>% filter(str_starts(par, "b_temp")) %>% select(c("2.5%", "50%", "97.5%"))
df_b_light <- df %>% filter(str_starts(par, "b_light")) %>% select(c("2.5%", "50%", "97.5%"))
df_b_CCA1 <- df %>% filter(str_starts(par, "b_CCA1")) %>% select(c("2.5%", "50%", "97.5%"))
df_mu <- df %>% filter(str_starts(par, "mu\\.")) %>% select(c("2.5%", "50%", "97.5%"))

df_b <- rbind(df_b_temp, df_b_light, df_b_CCA1)
df_b <- df_b %>% mutate(var = c("b_temp", "b_light", "b_CCA1"))
df_SIG5 <- as.data.frame(cbind(df_alpha_MarSun, df_alpha_MarShade, df_alpha_SepSun, df_alpha_SepShade, df_mu, unique(data$Time)))
names(df_SIG5) <- c("alpha_MarSun_2.5", "alpha_MarSun_50", "alpha_MarSun_97.5", 
                    "alpha_MarShade_2.5", "alpha_MarShade_50", "alpha_MarShade_97.5",
                    "alpha_SepSun_2.5", "alpha_SepSun_50", "alpha_SepSun_97.5", 
                    "alpha_SepShade_2.5", "alpha_SepShade_50", "alpha_SepShade_97.5",
                    "mu_2.5", "mu_50", "mu_97.5",
                    "time")

df_SIG5_rev <- cbind(df_SIG5,
                     data.frame(data_MarSun = tapply(data_MarSun$SIG5, data_MarSun$Time, mean),
                                data_MarShade = tapply(data_MarShade$SIG5, data_MarShade$Time, mean),
                                data_SepSun = tapply(data_SepSun$SIG5, data_SepSun$Time, mean),
                                data_SepShade = tapply(data_SepShade$SIG5, data_SepShade$Time, mean)))

# Draw figures
glist_AhgSIG5_MarSep <- Pred_vis_cyclic(df1=df_SIG5_rev, df2=df_b, var="AhgSIG5")



### BLRP
# Load output
df <- as.data.frame(fread("05_SSM_cyclic_MarSep_BLRP/SSM_cyclic_MarSep_BLRP_Lagtemp0_Laglight0_LagSIG50.csv"))

# Adjustment of data frame
df_alpha_MarSun <- df %>% filter(str_starts(par, "alpha_MarSun")) %>% select(c("2.5%", "50%", "97.5%"))
df_alpha_MarShade <- df %>% filter(str_starts(par, "alpha_MarShade")) %>% select(c("2.5%", "50%", "97.5%"))
df_alpha_SepSun <- df %>% filter(str_starts(par, "alpha_SepSun")) %>% select(c("2.5%", "50%", "97.5%"))
df_alpha_SepShade <- df %>% filter(str_starts(par, "alpha_SepShade")) %>% select(c("2.5%", "50%", "97.5%"))
df_b_temp <- df %>% filter(str_starts(par, "b_temp")) %>% select(c("2.5%", "50%", "97.5%"))
df_b_light <- df %>% filter(str_starts(par, "b_light")) %>% select(c("2.5%", "50%", "97.5%"))
df_b_SIG5 <- df %>% filter(str_starts(par, "b_SIG5")) %>% select(c("2.5%", "50%", "97.5%"))
df_mu <- df %>% filter(str_starts(par, "mu\\.")) %>% select(c("2.5%", "50%", "97.5%"))

df_b <- rbind(df_b_temp, df_b_light, df_b_SIG5)
df_b <- df_b %>% mutate(var = c("b_temp", "b_light", "b_SIG5"))
df_BLRP <- as.data.frame(cbind(df_alpha_MarSun, df_alpha_MarShade, df_alpha_SepSun, df_alpha_SepShade, df_mu, unique(data$Time)))
names(df_BLRP) <- c("alpha_MarSun_2.5", "alpha_MarSun_50", "alpha_MarSun_97.5", 
                    "alpha_MarShade_2.5", "alpha_MarShade_50", "alpha_MarShade_97.5",
                    "alpha_SepSun_2.5", "alpha_SepSun_50", "alpha_SepSun_97.5", 
                    "alpha_SepShade_2.5", "alpha_SepShade_50", "alpha_SepShade_97.5",
                    "mu_2.5", "mu_50", "mu_97.5",
                    "time")

df_BLRP_rev <- cbind(df_BLRP,
                     data.frame(data_MarSun = tapply(data_MarSun$BLRP, data_MarSun$Time, mean),
                                data_MarShade = tapply(data_MarShade$BLRP, data_MarShade$Time, mean),
                                data_SepSun = tapply(data_SepSun$BLRP, data_SepSun$Time, mean),
                                data_SepShade = tapply(data_SepShade$BLRP, data_SepShade$Time, mean)))

# Draw figures
glist_AhgBLRP_MarSep <- Pred_vis_cyclic(df1=df_BLRP_rev, df2=df_b, var="AhgpsbD BLRP")



### Integration of all plots into a figure
glist_all <- c(glist_AhgSIG5_MarSep, glist_AhgBLRP_MarSep)

void <- ggplot() + theme_void()

g <- {
  {(glist_all[[1]] + labs(tag = "A")) + 
      (glist_all[[2]] + labs(tag = "B")) + 
      (glist_all[[3]] + labs(tag = "C")) +
      ((glist_all[[7]] + labs(tag = "D")) /
         (glist_all[[4]] + glist_all[[5]] + glist_all[[6]] + plot_layout(ncol=3)) + 
         plot_layout(heights = c(0.1, 1))) +
      void + void + plot_layout(heights = c(1, 1, 0.3), ncol = 2)} /
    
    {(glist_all[[8]] + labs(tag = "E")) + 
        (glist_all[[9]] + labs(tag = "F")) + 
        (glist_all[[10]] + labs(tag = "G")) +
        ((glist_all[[14]] + labs(tag = "H")) /
           (glist_all[[11]] + glist_all[[12]] + glist_all[[13]] + plot_layout(ncol=3)) + 
           plot_layout(heights = c(0.1, 1))) +
        void + void + plot_layout(heights = c(1, 1, 0.3), ncol = 2)}
} /
  legend_MarSep1_dlm +
  plot_layout(heights = c(1, 1, 0.05)) #+ 
  # plot_annotation(title = "Fig. S3") &
  # theme(plot.tag = element_text(size = 10))

ggsave(paste0(out, "Fig.3_LLMX_231202_v3.pdf"),
       g, width = 110, height = 140, units = "mm")

