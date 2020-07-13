# Load necessary libraries
library(rlang)
library(skimr)
library(dplyr)

# Load necessary data
env <- read.csv("data/environmental_parameters/KI_env_all_KI_Platy.csv")

# Remove spurious readings
env["23",]$ysi_salinity_mean_1m <- NA # Abnormally high reading, suspect bubble 
env["23",]$ysi_salinity_se_1m <- NA # Abnormally high reading, suspect bubble 

# Choose parameters to include
include <- c("ysi_salinity_mean_1m",
              "ysi_DO_mean_1m","ysi_pH_mean_1m", 
              "npp_mean_sat","npp_max_sat","wave_mean_sat")

# Summarize mean and sd
env.summ3 <- env %>%
  group_by(fpressure) %>%
  summarise_at(vars(include), funs(mean(., na.rm=TRUE), sd(., na.rm=TRUE)))

# Write out data
write.csv(x=env.summ3,file="figures/Extended_Data/Extended_Data_Table_3/env_summ_KI_Platy_alltimes.csv",row.names = FALSE)