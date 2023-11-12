
# Set working directory
setwd("/Volume3/hnishio/R/SIG5_field3")

# Load packages
library(tidyverse)
library(data.table)

# Create output directory
out <- "06_SSM_cyclic_MarSep_constant_input2/"
if(file.exists(out)==F){
  dir.create(out, recursive=T)
}


### Preparation of data

# Load data of March and September
data <- read.csv("data/march_data.csv")
data_MarSun <- subset(data, Condition=="Sun")
data_MarShade <- subset(data, Condition=="Shade")
data <- read.csv("data/september_data.csv")
data_SepSun <- subset(data, Condition=="Sun")
data_SepShade <- subset(data, Condition=="Shade")
environment_data_march <- read.csv("data/environment_data_corrected_march.csv")
environment_data_march$Condition = factor(environment_data_march$Condition, levels=c("Sun", "Shade"))
environment_data_march <- environment_data_march %>%
  arrange(Condition, Time)
environment_data_september <- read.csv("data/environment_data_corrected_september.csv")
environment_data_september$Condition = factor(environment_data_september$Condition, levels=c("Sun", "Shade"))
environment_data_september <- environment_data_september %>%
  arrange(Condition, Time)
env_MarSun <- environment_data_march[environment_data_march$Condition=="Sun",]
env_MarShade <- environment_data_march[environment_data_march$Condition=="Shade",]
env_SepSun <- environment_data_september[environment_data_september$Condition=="Sun",]
env_SepShade <- environment_data_september[environment_data_september$Condition=="Shade",]

env_MarSun2 <- NULL; env_MarShade2 <- NULL
env_SepSun2 <- NULL; env_SepShade2 <- NULL
for(i in 1:length(unique(data_MarSun$Time))){
  env_MarSun2 <- rbind(env_MarSun2, env_MarSun[env_MarSun$Time == ts(unique(data_MarSun$Time))[i],])
  env_MarShade2 <- rbind(env_MarShade2, env_MarShade[env_MarShade$Time == ts(unique(data_MarShade$Time))[i],])
  env_SepSun2 <- rbind(env_SepSun2, env_SepSun[env_SepSun$Time == ts(unique(data_SepSun$Time))[i],])
  env_SepShade2 <- rbind(env_SepShade2, env_SepShade[env_SepShade$Time == ts(unique(data_SepShade$Time))[i],])
}

env_MarSun2 <- rbind(env_MarSun2, env_MarSun[nrow(env_MarSun),])
env_MarShade2 <- rbind(env_MarShade2, env_MarShade[nrow(env_MarShade),])
env_SepSun2 <- rbind(env_SepSun2, env_SepSun[nrow(env_SepSun),])
env_SepShade2 <- rbind(env_SepShade2, env_SepShade[nrow(env_SepShade),])
env_SepSun2 <- rbind(env_SepSun[1,], env_SepSun2)

# CCA1
CCA1_MarSun = data_MarSun %>%
  group_by(Time) %>%
  summarize(Mean_CCA1 = mean(CCA1))
CCA1_MarShade = data_MarShade %>%
  group_by(Time) %>%
  summarize(Mean_CCA1 = mean(CCA1))
CCA1_SepSun = data_SepSun %>%
  group_by(Time) %>%
  summarize(Mean_CCA1 = mean(CCA1))
CCA1_SepShade = data_SepShade %>%
  group_by(Time) %>%
  summarize(Mean_CCA1 = mean(CCA1))

# SIG5
SIG5_MarSun = data_MarSun %>%
  group_by(Time) %>%
  summarize(Mean_SIG5 = mean(SIG5))
SIG5_MarShade = data_MarShade %>%
  group_by(Time) %>%
  summarize(Mean_SIG5 = mean(SIG5))
SIG5_SepSun = data_SepSun %>%
  group_by(Time) %>%
  summarize(Mean_SIG5 = mean(SIG5))
SIG5_SepShade = data_SepShade %>%
  group_by(Time) %>%
  summarize(Mean_SIG5 = mean(SIG5))


## Functions
quantile99 <- function(x){
  quantile(x, probs = c(0.005, 0.025, 0.5, 0.975, 0.995), names = TRUE)
}

pred_SIG5 <- function(mcmc, st, en, temp, light, gene, cond){
  alpha_mcmc <- NULL
  for(i in 1:nrow(mcmc)){
    alpha <- mcmc[i, st:en] + 
      mcmc$b_temp[i] * temp + mcmc$b_light[i] * light + mcmc$b_CCA1[i] * gene
    alpha_mcmc <- rbind(alpha_mcmc, alpha)
  }
  df_alpha_q <- as.data.frame(round(t(apply(alpha_mcmc, 2, quantile99)), digits = 4))
  names(df_alpha_q) <- paste0("alpha_", cond, "_", str_replace(names(df_alpha_q), "%", ""))
  return(df_alpha_q)
}

pred_BLRP <- function(mcmc, st, en, temp, light, gene, cond){
  alpha_mcmc <- NULL
  for(i in 1:nrow(mcmc)){
    alpha <- mcmc[i, st:en] + 
      mcmc$b_temp[i] * temp + mcmc$b_light[i] * light + mcmc$b_SIG5[i] * gene
    alpha_mcmc <- rbind(alpha_mcmc, alpha)
  }
  df_alpha_q <- as.data.frame(round(t(apply(alpha_mcmc, 2, quantile99)), digits = 4))
  names(df_alpha_q) <- paste0("alpha_", cond, "_", str_replace(names(df_alpha_q), "%", ""))
  return(df_alpha_q)
}



### Preparation of variables
temp_MarSun = env_MarSun2$Temperature
temp_MarShade = env_MarShade2$Temperature
temp_SepSun = env_SepSun2$Temperature
temp_SepShade = env_SepShade2$Temperature
light_MarSun = env_MarSun2$Irradiance
light_MarShade = env_MarShade2$Irradiance
light_SepSun = env_SepSun2$Irradiance
light_SepShade = env_SepShade2$Irradiance
CCA1_MarSun = CCA1_MarSun$Mean_CCA1
CCA1_MarShade = CCA1_MarShade$Mean_CCA1
CCA1_SepSun = CCA1_SepSun$Mean_CCA1
CCA1_SepShade = CCA1_SepShade$Mean_CCA1
SIG5_MarSun = SIG5_MarSun$Mean_SIG5
SIG5_MarShade = SIG5_MarShade$Mean_SIG5
SIG5_SepSun = SIG5_SepSun$Mean_SIG5
SIG5_SepShade = SIG5_SepShade$Mean_SIG5





##### SIG5 #####

## Preparation of variables
input <- "04_SSM_cyclic_MarSep_SIG5/"
out_name <- paste0("SSM_cyclic_MarSep_SIG5_Lagtemp0_Laglight0_LagCCA10")
mcmc <- as.data.frame(fread(paste0(input, out_name, "_MCMC.csv")))
st <- which(str_ends(names(mcmc), "mu.1"))
en <- which(str_ends(names(mcmc), "mu.13"))


## Mean values
temp_mean <- mean(c(temp_MarSun, temp_MarShade, temp_SepSun, temp_SepShade))
light_mean <- mean(c(light_MarSun, light_MarShade, light_SepSun, light_SepShade))
CCA1_mean <- mean(c(CCA1_MarSun, CCA1_MarShade, CCA1_SepSun, CCA1_SepShade))
SIG5_mean <- mean(c(SIG5_MarSun, SIG5_MarShade, SIG5_SepSun, SIG5_SepShade))


## Prediction using natural variables
# Calculation
df_alpha_MarSun <- pred_SIG5(mcmc = mcmc, st = st, en = en, cond = "MarSun",
                             temp = temp_MarSun, light = light_MarSun, gene = CCA1_MarSun)
df_alpha_MarShade <- pred_SIG5(mcmc = mcmc, st = st, en = en, cond = "MarShade",
                               temp = temp_MarShade, light = light_MarShade, gene = CCA1_MarShade)
df_alpha_SepSun <- pred_SIG5(mcmc = mcmc, st = st, en = en, cond = "SepSun",
                             temp = temp_SepSun, light = light_SepSun, gene = CCA1_SepSun)
df_alpha_SepShade <- pred_SIG5(mcmc = mcmc, st = st, en = en, cond = "SepShade",
                               temp = temp_SepShade, light = light_SepShade, gene = CCA1_SepShade)
# Save output
df_alpha_all <- cbind(df_alpha_MarSun, df_alpha_MarShade, df_alpha_SepSun, df_alpha_SepShade)
fwrite(df_alpha_all, file = paste0(out, out_name, "_pred_natural.csv"))


## Prediction using mean temperature
# Calculation
df_alpha_MarSun <- pred_SIG5(mcmc = mcmc, st = st, en = en, cond = "MarSun",
                             temp = temp_mean, light = light_MarSun, gene = CCA1_MarSun)
df_alpha_MarShade <- pred_SIG5(mcmc = mcmc, st = st, en = en, cond = "MarShade",
                               temp = temp_mean, light = light_MarShade, gene = CCA1_MarShade)
df_alpha_SepSun <- pred_SIG5(mcmc = mcmc, st = st, en = en, cond = "SepSun",
                             temp = temp_mean, light = light_SepSun, gene = CCA1_SepSun)
df_alpha_SepShade <- pred_SIG5(mcmc = mcmc, st = st, en = en, cond = "SepShade",
                               temp = temp_mean, light = light_SepShade, gene = CCA1_SepShade)
# Save output
df_alpha_all <- cbind(df_alpha_MarSun, df_alpha_MarShade, df_alpha_SepSun, df_alpha_SepShade)
fwrite(df_alpha_all, file = paste0(out, out_name, "_pred_meantemp.csv"))


## Prediction using mean irradiance
# Calculation
df_alpha_MarSun <- pred_SIG5(mcmc = mcmc, st = st, en = en, cond = "MarSun",
                             temp = temp_MarSun, light = light_mean, gene = CCA1_MarSun)
df_alpha_MarShade <- pred_SIG5(mcmc = mcmc, st = st, en = en, cond = "MarShade",
                               temp = temp_MarShade, light = light_mean, gene = CCA1_MarShade)
df_alpha_SepSun <- pred_SIG5(mcmc = mcmc, st = st, en = en, cond = "SepSun",
                             temp = temp_SepSun, light = light_mean, gene = CCA1_SepSun)
df_alpha_SepShade <- pred_SIG5(mcmc = mcmc, st = st, en = en, cond = "SepShade",
                               temp = temp_SepShade, light = light_mean, gene = CCA1_SepShade)
# Save output
df_alpha_all <- cbind(df_alpha_MarSun, df_alpha_MarShade, df_alpha_SepSun, df_alpha_SepShade)
fwrite(df_alpha_all, file = paste0(out, out_name, "_pred_meanlight.csv"))


## Prediction using mean CCA1
# Calculation
df_alpha_MarSun <- pred_SIG5(mcmc = mcmc, st = st, en = en, cond = "MarSun",
                             temp = temp_MarSun, light = light_MarSun, gene = CCA1_mean)
df_alpha_MarShade <- pred_SIG5(mcmc = mcmc, st = st, en = en, cond = "MarShade",
                               temp = temp_MarShade, light = light_MarShade, gene = CCA1_mean)
df_alpha_SepSun <- pred_SIG5(mcmc = mcmc, st = st, en = en, cond = "SepSun",
                             temp = temp_SepSun, light = light_SepSun, gene = CCA1_mean)
df_alpha_SepShade <- pred_SIG5(mcmc = mcmc, st = st, en = en, cond = "SepShade",
                               temp = temp_SepShade, light = light_SepShade, gene = CCA1_mean)
# Save output
df_alpha_all <- cbind(df_alpha_MarSun, df_alpha_MarShade, df_alpha_SepSun, df_alpha_SepShade)
fwrite(df_alpha_all, file = paste0(out, out_name, "_pred_meanCCA1.csv"))





##### BLRP #####

## Preparation of variables
input <- "05_SSM_cyclic_MarSep_BLRP/"
out_name <- paste0("SSM_cyclic_MarSep_BLRP_Lagtemp0_Laglight0_LagSIG50")
mcmc <- as.data.frame(fread(paste0(input, out_name, "_MCMC.csv")))
st <- which(str_ends(names(mcmc), "mu.1"))
en <- which(str_ends(names(mcmc), "mu.13"))


## Prediction using natural variables
# Calculation
df_alpha_MarSun <- pred_BLRP(mcmc = mcmc, st = st, en = en, cond = "MarSun",
                             temp = temp_MarSun, light = light_MarSun, gene = SIG5_MarSun)
df_alpha_MarShade <- pred_BLRP(mcmc = mcmc, st = st, en = en, cond = "MarShade",
                               temp = temp_MarShade, light = light_MarShade, gene = SIG5_MarShade)
df_alpha_SepSun <- pred_BLRP(mcmc = mcmc, st = st, en = en, cond = "SepSun",
                             temp = temp_SepSun, light = light_SepSun, gene = SIG5_SepSun)
df_alpha_SepShade <- pred_BLRP(mcmc = mcmc, st = st, en = en, cond = "SepShade",
                               temp = temp_SepShade, light = light_SepShade, gene = SIG5_SepShade)
# Save output
df_alpha_all <- cbind(df_alpha_MarSun, df_alpha_MarShade, df_alpha_SepSun, df_alpha_SepShade)
fwrite(df_alpha_all, file = paste0(out, out_name, "_pred_natural.csv"))


## Prediction using mean temperature
# Calculation
df_alpha_MarSun <- pred_BLRP(mcmc = mcmc, st = st, en = en, cond = "MarSun",
                             temp = temp_mean, light = light_MarSun, gene = SIG5_MarSun)
df_alpha_MarShade <- pred_BLRP(mcmc = mcmc, st = st, en = en, cond = "MarShade",
                               temp = temp_mean, light = light_MarShade, gene = SIG5_MarShade)
df_alpha_SepSun <- pred_BLRP(mcmc = mcmc, st = st, en = en, cond = "SepSun",
                             temp = temp_mean, light = light_SepSun, gene = SIG5_SepSun)
df_alpha_SepShade <- pred_BLRP(mcmc = mcmc, st = st, en = en, cond = "SepShade",
                               temp = temp_mean, light = light_SepShade, gene = SIG5_SepShade)
# Save output
df_alpha_all <- cbind(df_alpha_MarSun, df_alpha_MarShade, df_alpha_SepSun, df_alpha_SepShade)
fwrite(df_alpha_all, file = paste0(out, out_name, "_pred_meantemp.csv"))


## Prediction using mean irradiance
# Calculation
df_alpha_MarSun <- pred_BLRP(mcmc = mcmc, st = st, en = en, cond = "MarSun",
                             temp = temp_MarSun, light = light_mean, gene = SIG5_MarSun)
df_alpha_MarShade <- pred_BLRP(mcmc = mcmc, st = st, en = en, cond = "MarShade",
                               temp = temp_MarShade, light = light_mean, gene = SIG5_MarShade)
df_alpha_SepSun <- pred_BLRP(mcmc = mcmc, st = st, en = en, cond = "SepSun",
                             temp = temp_SepSun, light = light_mean, gene = SIG5_SepSun)
df_alpha_SepShade <- pred_BLRP(mcmc = mcmc, st = st, en = en, cond = "SepShade",
                               temp = temp_SepShade, light = light_mean, gene = SIG5_SepShade)
# Save output
df_alpha_all <- cbind(df_alpha_MarSun, df_alpha_MarShade, df_alpha_SepSun, df_alpha_SepShade)
fwrite(df_alpha_all, file = paste0(out, out_name, "_pred_meanlight.csv"))


## Prediction using mean SIG5
# Calculation
df_alpha_MarSun <- pred_BLRP(mcmc = mcmc, st = st, en = en, cond = "MarSun",
                             temp = temp_MarSun, light = light_MarSun, gene = SIG5_mean)
df_alpha_MarShade <- pred_BLRP(mcmc = mcmc, st = st, en = en, cond = "MarShade",
                               temp = temp_MarShade, light = light_MarShade, gene = SIG5_mean)
df_alpha_SepSun <- pred_BLRP(mcmc = mcmc, st = st, en = en, cond = "SepSun",
                             temp = temp_SepSun, light = light_SepSun, gene = SIG5_mean)
df_alpha_SepShade <- pred_BLRP(mcmc = mcmc, st = st, en = en, cond = "SepShade",
                               temp = temp_SepShade, light = light_SepShade, gene = SIG5_mean)
# Save output
df_alpha_all <- cbind(df_alpha_MarSun, df_alpha_MarShade, df_alpha_SepSun, df_alpha_SepShade)
fwrite(df_alpha_all, file = paste0(out, out_name, "_pred_meanSIG5.csv"))

