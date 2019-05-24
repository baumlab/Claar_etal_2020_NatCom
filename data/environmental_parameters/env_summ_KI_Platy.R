rm(list=ls())

library(rlang)
library(skimr)
library(dplyr)

env <- read.csv("data/environmental_parameters/KI_env_all_KI_Platy.csv")

# env <- env[env$field_season=="2014"| env$field_season=="2015b",]

env["23",]$ysi_salinity_mean_1m <- NA # Abnormally high reading, suspect bubble 
env["23",]$ysi_salinity_se_1m <- NA # Abnormally high reading, suspect bubble 

names(env)

skim(env)

env_skimmed <- env %>%
  dplyr::group_by(site) %>%
  skim()

env_skimmed 

str(env_skimmed)

env_skimmed2 <- env %>%
  dplyr::group_by(site) %>%
  skim_to_list()
env_skimmed3 <- env %>%
  dplyr::group_by(fpressure) %>%
  skim_to_list()

env_skimmed4 <- env %>%
  dplyr::group_by(fpressure) %>%
  skim_to_wide()

env_skimmed2[["numeric"]]

env_skimmed2[["numeric"]] %>%  
  dplyr::select(site, variable, mean, sd) %>%
  dplyr::filter(variable == "nitrate_plus_nitrite_uM_mean")
env_skimmed3[["numeric"]] %>%  
  dplyr::select(fpressure, variable, mean, sd) %>%
  dplyr::filter(variable == "nitrate_plus_nitrite_uM_mean")

env_skimmed2[["numeric"]] %>%  
  dplyr::select(site, variable, mean, sd) %>%
  dplyr::filter(variable == "visibility_m")
env_skim_vis <- env_skimmed2[["numeric"]] %>%  
  dplyr::select(site, variable, mean, sd) %>%
  dplyr::filter(variable == "visibility_m")
env_skim_vis2 <- env_skimmed3[["numeric"]] %>%  
  dplyr::select(fpressure, variable, mean, sd) %>%
  dplyr::filter(variable == "visibility_m")
# write.csv(x=env_skim_vis2,file="data/environmental_parameters/env_skim_vis.csv",row.names = FALSE)


env_skimmed2[["numeric"]] %>%  
  dplyr::select(site, variable, mean, sd) %>%
  dplyr::filter(variable == "ysi_salinity_mean_1m")
env_skim_sal <- env_skimmed3[["numeric"]] %>%  
  dplyr::select(fpressure, variable, mean, sd) %>%
  dplyr::filter(variable == "ysi_salinity_mean_1m")
# write.csv(x=env_skim_sal,file="data/environmental_parameters/env_skim_sal.csv",row.names = FALSE)


env_skimmed3[["numeric"]] %>%  
  dplyr::select(fpressure, variable, mean, sd) %>%
  dplyr::filter(variable == "turb_mean_aq")

env_skimmed3[["numeric"]] %>%  
  dplyr::select(fpressure, variable, mean, sd) %>%
  dplyr::filter(variable == "phosphate_uM_mean")

env_skimmed3[["numeric"]] %>%  
  dplyr::select(fpressure, variable, mean, sd) %>%
  dplyr::filter(variable == "prod_mean_f")

env_skimmed3[["numeric"]] %>%  
  dplyr::select(fpressure, variable, mean, sd) %>%
  dplyr::filter(variable == "prod_mean_aq")

env_skimmed3[["numeric"]] %>%  
  dplyr::select(fpressure, variable, mean, sd) %>%
  dplyr::filter(variable == "npp_mean_sat")

env_skimmed3[["numeric"]] %>%  
  dplyr::select(fpressure, variable, mean, sd) %>%
  dplyr::filter(variable == "npp_max_sat")

env_skimmed3[["numeric"]] %>%  
  dplyr::select(fpressure, variable, mean, sd) %>%
  dplyr::filter(variable == "ysi_DO_mean_1m")

env_skim_all <- env_skimmed3[["numeric"]] %>%  
  dplyr::select(fpressure, variable, mean, sd)
# write.csv(x=env_skim_all,file="data/environmental_parameters/env_skim_all.csv",row.names = FALSE)
write.csv(x=env_skim_all,file="data/environmental_parameters/env_skim_all_alltimes.csv",row.names = FALSE)

## Set which metrics to include in PCA
include <- c("visibility_m", "nitrate_plus_nitrite_uM_mean", 
             "phosphate_uM_mean", "prod_mean_aq", "turb_mean_aq", 
             "ysi_DO_mean_1m")

env.summ <- env %>%
  group_by(site) %>%
  summarise_at(vars(include), funs(mean(., na.rm=TRUE)))

env.summ2 <- env %>%
  group_by(fpressure) %>%
  summarise_at(vars(include), funs(mean(., na.rm=TRUE)))

include2 <- c("visibility_m", "nitrate_plus_nitrite_uM_mean", 
             "phosphate_uM_mean", "prod_mean_aq", "prod_mean_f",
             "turb_mean_aq", "ysi_temp_mean_1m", "ysi_salinity_mean_1m",
             "ysi_DO_mean_1m","ysi_pH_mean_1m", 
             "npp_mean_sat","npp_max_sat","wave_mean_sat")

include3 <- c("visibility_m", "ysi_temp_mean_1m", "ysi_salinity_mean_1m",
              "ysi_DO_mean_1m","ysi_pH_mean_1m", 
              "npp_mean_sat","npp_max_sat","wave_mean_sat")

env.summ3 <- env %>%
  group_by(fpressure) %>%
  summarise_at(vars(include3), funs(mean(., na.rm=TRUE), sd(., na.rm=TRUE)))
# write.csv(x=env.summ3,file="data/environmental_parameters/env_summ_KI_Platy.csv",row.names = FALSE)
write.csv(x=env.summ3,file="data/environmental_parameters/env_summ_KI_Platy_alltimes.csv",row.names = FALSE)