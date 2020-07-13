# Analyses and figures for Claar et al. 2020


### Figures

# FIGURE 1
source("figures/Figure_1/Figure_1_new.R")
# LOAD 'figures/Figure_1/KI_map_platysites_villages.jpg' # KI map
# LOAD 'figures/Figure_1/photos/site15_jan15_DanielleClaar_IMG_4926_2.jpg' 
# LOAD 'figures/Figure_1/photos/highdisturbance_KI.jpg' # image
# LOAD "analyses/2020_analyses/ASV_ordination/samplelist_Fig1.csv"
# LOAD "data/KI_Platy_f_coral_grouped_ASVs.RData"
# MAKE "figures/Figure_1/Figure1_fpenta_CAP.jpg"
# MAKE "figures/Figure_1/Figure1_fpenta_CAP.pdf"
# MAKE "figures/Figure_1/Figure1_platy_CAP.jpg"
# MAKE "figures/Figure_1/Figure1_platy_CAP.pdf"
# LOAD "Platy_Favites_LogisticPlots.RData" #Logistic regressions
# MAKE "figures/Figure_1/Figure1_platy_reg.jpg"
# MAKE "figures/Figure_1/Figure1_platy_reg.pdf"
# MAKE "figures/Figure_1/Figure1_fpenta_reg.pdf"
# All these intermediate files were manually panelled in Illustrator

# Map for Figure 1
source("figures/Figure_1/ki_map_files/KI_maps_platyms.R")
# LOAD 'figures/Figure_1/ki_map_files/ki_sites_updated2019.csv'
# SOURCE "figures/Figure_1/ki_map_files/KI_base_B&W_bigger.R"
# MAKE "figures/Figure_1/KI_map_platysites_villages.jpg"
# MAKE "figures/Figure_1/KI_map_platysites_villages.pdf"

# Figure 2
source("figures/Figure_2/Figure_2_new.R")
# LOAD "data/temperature/KI_SB_temp_DHW_NOAAMMM_minOffsetnoEN.RData"
# LOAD "data/temperature/KI_SB_temp_1hr.RData"
# MAKE "figures/Figure_2/Figure2_a.pdf"
# LOAD "Platy_Favites_LogisticPlots.RData"
# MAKE "figures/Figure_2/Figure2_platy_reg1.pdf"
# MAKE "figures/Figure_2/Figure2_fpenta_reg1.pdf"
# MAKE "figures/Figure_2/Figure2_platy_reg2.pdf"
# MAKE "figures/Figure_2/Figure2_fpenta_reg2.pdf"
# All these intermediate files were manually panelled in Illustrator
 
# Figure 3
source("figures/Figure_3/Figure3_qpcr.R")
# LOAD load(file="data/Coralphoto__Metadata/KI_Platy_metadataSH.RData")
# LOAD load("data/temperature/KI_SB_temp_DHW.RData")
# LOAD load("data/temperature/KI_SB_temp_DHW_allsites.RData")
# MAKE "figures/Figure_3/Figure3a.pdf"
source("figures/Figure_3/Bleaching_versus_DurusdiniumPlots.R")
# Sam please fill this in
# LOAD
# MAKE 
# All these intermediate files were manually panelled in Illustrator

# Extended Data Figure 2 - map
source("figures/Extended_Data/Extended_Data_Figure_2/supp_map/KI_maps_platy_regions_supp.R")
# LOAD 'figures/Figure_1/ki_map_files/ki_sites_updated2019.csv'
# SOURCE "figures/Figure_1/ki_map_files/KI_base_B&W_bigger.R"
# SOURCE "figures/Figure_1/ki_map_files/KI_base_inset_forbigger.R"
# MAKE "figures/Extended_Data/Extended_Data_Figure_2/supp_map/KI_map_platy_regions_inset_bigger.pdf"
# MAKE "figures/Extended_Data/Extended_Data_Figure_2/supp_map/KI_map_platy_regions_inset_bigger.jpg"

# Extended Data Table 3
source("figures/Extended_Data/Extended_Data_Table_3/env_summ_KI_Platy.R")
# READ "data/environmental_parameters/KI_env_all_KI_Platy.csv"
# MAKE "figures/Extended_Data/Extended_Data_Table_3/env_summ_KI_Platy_alltimes.csv"

# Supplementary Table 3
source("figures/Supplementary_Materials/Supp_Table_3_env_logistic/EnvironmentalVariables.R")
# LOAD "data/Updated_Starko/LogisticData.xlsx"
# LOAD "data/environmental_parameters/KI_env_all.xlsx"
# Manually extract model outputs

# Supplementary Table 6
source("figures/Supplementary_Materials/Supp_Tables_2and8/fpenta_lib_cons.R")
# LOAD "analyses/2020_analyses/ASV_ordination/samplelist_Fig1.csv"
# LOAD "data/KI_Platy_f_coral_grouped_ASVs.RData"
# Manually extract model outputs
source("figures/Supplementary_Materials/Supp_Tables_2and8/platy_lib_cons.R")
# LOAD "analyses/2020_analyses/ASV_ordination/samplelist_Fig1.csv"
# LOAD "data/KI_Platy_f_coral_grouped_ASVs.RData"
v