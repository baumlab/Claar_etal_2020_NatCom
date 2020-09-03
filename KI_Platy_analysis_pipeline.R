# Analyses and figures for Claar et al. 2020

# dada2
source("analyses/dada2/dada2.R")
# LOAD sequences in "data/Bioinf/sequences/KI_Platy_sequences/"
## Need to download sequences separately from NCBI and put them in this folder
# LOAD "analyses/dada2/ITS2db_trimmed_derep_dada.fasta"
# MAKE "analyses/2019_analyses/dada2/KI_Platy_dada.RData"

# Filter dada2 output
source("analyses/dada2/KI_Platy_filter_samples_dada.R")
# LOAD "analyses/dada2/KI_Platy_dada.RData"
# MAKE intermediate tree files in "data/Bioinf/tree"
# MAKE "data/KI_Platy_f_coral_grouped_ASVs.RData"
# MAKE "data/KI_Platy_phyASVfcp_ASVs.RData"

# Process metadata
source("data/Coralphoto__Metadata/process_coral_metadata.R")
# LOAD "data/qPCR/qPCR.RData"
# LOAD "data/Coralphoto__Metadata/KI_Coralphoto_Metadata_through2017_ALLDONE.csv"
# LOAD "data/Coralphoto__Metadata/mapping_file.txt"
# LOAD "data/Coralphoto__Metadata/mapping_file2.txt"
# MAKE "data/Coralphoto__Metadata/KI_Platy_metadata.csv"
# MAKE "data/Coralphoto__Metadata/KI_Platy_metadataSH.csv"
# MAKE "data/Coralphoto__Metadata/KI_Platy_metadataSH.tsv"
# MAKE "data/Coralphoto__Metadata/KI_Platy_metadataSH.RData"

# Temperature data
source("data/temperature/NOAA_5km_extract.R")
# LOAD "data/temperature/disturbance_latlons.csv"
# READ "data/temperature/NOAA_5km/" 
       # All NOAA 5km files need to be in this folder
# MAKE "data/temperature/NOAA_5km.RData"
source("data/temperature/create_KI_allsites_DHW.R")
# LOAD "data/temperature/KI_SB_temp_DHW.RData"
# MAKE "data/temperature/KI_SB_temp_DHW_allsites.RData"


### Figures

# FIGURE 1
source("figures/Figure_1/Figure_1_new.R")
# LOAD 'figures/Figure_1/KI_map_platysites_villages.jpg' # KI map
# LOAD 'figures/Figure_1/photos/site15_jan15_DanielleClaar_IMG_4926_2.jpg' 
# LOAD 'figures/Figure_1/photos/highdisturbance_KI.jpg' # image
# LOAD "figures/Figure_1/samplelist_Fig1.csv"
# LOAD "data/KI_Platy_f_coral_grouped_ASVs.RData"
# MAKE "figures/Figure_1/Figure1_fpenta_CAP.jpg"
# MAKE "figures/Figure_1/Figure1_fpenta_CAP.pdf"
# MAKE "figures/Figure_1/Figure1_platy_CAP.jpg"
# MAKE "figures/Figure_1/Figure1_platy_CAP.pdf"
# LOAD "analyses/logistic_regressions/Platy_Favites_LogisticPlots.RData" 
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
# LOAD "analyses/logistic_regressions/Platy_Favites_LogisticPlots.RData"
# MAKE "figures/Figure_2/Figure2_platy_reg1.pdf"
# MAKE "figures/Figure_2/Figure2_fpenta_reg1.pdf"
# MAKE "figures/Figure_2/Figure2_platy_reg2.pdf"
# MAKE "figures/Figure_2/Figure2_fpenta_reg2.pdf"
# All these intermediate files were manually panelled in Illustrator
 
# Figure 3
source("figures/Figure_3/Figure_3_Bleaching_versus_DurusdiniumPlots.R")
# Sam please fill this in
# LOAD read_excel("data/Logistic_regression_data/LogisticData.xlsx")
# MAKE "figures/Figure_3/Figure_3_new_platy.pdf
# MAKE  "figures/Figure_3/Figure_3_new_fpenta.pdf"
# All these intermediate files were manually panelled in Illustrator

# Figure 4
source("figures/Figure_4/Figure_4_qpcr.R")
# LOAD load(file="data/Coralphoto__Metadata/KI_Platy_metadataSH.RData")
# LOAD load("data/temperature/KI_SB_temp_DHW.RData")
# LOAD load("data/temperature/KI_SB_temp_DHW_allsites.RData")
# MAKE "figures/Figure_4/Figure4.pdf"

# Figure 5
source("figures/Figure_5/HeatmapFigureScript.R")
# LOAD "data/Heatmap_datasheets/HM_data.csv"
# Plots exported manually, edited and combined elsewhere

# Supplementary Figure 2 - map
source("figures/Supplementary_Materials/Supplementary_Figure_2/supp_map/Supplementary_Figure_2_KI_maps_platy_regions_supp.R")
# LOAD 'figures/Figure_1/ki_map_files/ki_sites_updated2019.csv'
# SOURCE "figures/Figure_1/ki_map_files/KI_base_B&W_bigger.R"
# SOURCE "figures/Figure_1/ki_map_files/KI_base_inset_forbigger.R"
# MAKE "figures/Supplementary_Figures/Supplementary_Figure_2/supp_map/KI_map_platy_regions_inset_bigger.pdf"
# MAKE "figures/Supplementary_Figures/Supplementary_Figure_2/supp_map/KI_map_platy_regions_inset_bigger.jpg"

# Supplementary Figure 2 - daily variation
source("figures/Supplementary_Materials/Supplementary_Figure_2/Fig_S2bc_temperature_variation.R")
# LOAD "data/temperature/KI_SB_temp_wKim_1hr.RData"
# MAKE "figures/ESupplementary_Materials/Supplementary_Figure_2/Fig_S2b_KI_insitu_daily_var_nonEN.pdf"
# MAKE "figures/Supplementary_Materials/Supplementary_Figure_2/Fig_S2c_KI_insitu_daily_var_EN.pdf"

# Supplementary Figure 2 - in situ illustration
source("figures/Supplementary_Materials/Supplementary_Figure_2/Fig_S2de_plot_insitu_illustration.R")
# LOAD "data/temperature/KI_SB_temp_wKim_1d.RData"
# MAKE "figures/Supplementary_Materials/Supplementary_Figure_2/Fig_S2e_KI_insitu_ill_EN.pdf"
# MAKE "figures/Supplementary_Materials/Supplementary_Figure_2/Fig_S2d_KI_insitu_SST_ill_nonEN.pdf"

# Supplementary Figure 2 - in situ
source("figures/Supplementary_Materials/Supplementary_Figure_2/Fig_S2f_plot_insitu_wKim.R")
# LOAD "data/temperature/KI_SB_temp_wKim_1d.RData"
# LOAD "data/temperature/NOAA_CoralTemp_2011_2018.RData"
# MAKE "figures/Supplementary_Materials/Supplementary_Figure_2/Fig_S2f_KI_insitu_wKim.pdf"


# Supplementary Figure 2 - NOAA DHWs
source("figures/Supplementary_Materials/Supplementary_Figure_2/Fig_S2g_plot_dhws_platy.R")
# LOAD "data/temperature/DHW_all.RData"
# LOAD "data/temperature/NOAA_DHW_5km.RData"
# MAKE "figures/Supplementary_Materials/Supplementary_Figure_2/Fig_S2g_KI_DHWs.pdf"

# Supplementary Figure 3 - ITS2 Profile barplots
source("figures/Supplementary_Materials/Supplementary_Figure_3/ITS2_Profile_Figures_ED/ITS2ProfileFigures.R")
# LOAD "data/SymPortal/Metadata.csv"
# LOAD "data/SymPortal/By_genus.csv"
# LOAD "data/SymPortal/RawRead_matrix.csv"
# LOAD "data/SymPortal/tags.csv"
# LOAD "data/SymPortal/Colour_scheme.csv"
# MAKE "figures/Supplementary_Materials/Supplementary_Figure_3/ITS2_Profile_Figures_ED/Platygyra_ITS2Profiles.pdf"
# MAKE "figures/Supplementary_Materials/Supplementary_Figure_3/ITS2_Profile_Figures_ED/Favites_ITS2Profiles.pdf"

# Supplementary Figure 4
source("figures/Supplementary_Materials/Supplementary_Figure_4/EnvironmentalVariables_Fig_S4.R")
# LOAD "data/environmental_parameters/KI_env_all.xlsx"
# LOAD "data/Logistic_regression_data/LogisticData.xlsx"
# manually export figures 

# Supplementary Figure 6
source("figures/Supplementary_Materials/Supplementary_Figure_6/Supplementary_Figure_6.R")
# LOAD "data/Coralphoto__Metadata/KI_Platy_metadataSH.RData"
# MAKE "figures/Supplementary_Materials/Supplementary_Figure_6/Supplementary_Figure_6.pdf"


# Supplementary Figure 7
source("figures/Supplementary_Materials/Supplementary_Figure_7/Figure_S7.R")
# LOAD "data/Coralphoto__Metadata/KI_Platy_metadataSH.RData"
# MAKE "figures/Supplementary_Materials/Supplementary_Figure_7/Figure_S7.jpg
# MAKE "figures/Supplementary_Materials/Supplementary_Figure_7/Figure_S7.tiff"
# MAKE "figures/Supplementary_Materials/Supplementary_Figure_7/Figure_S7.pdf"

# Supplementary Figure 8
source("figures/Supplementary_Materials/Supplementary_Figure_8/Supplementary_Figure_8_bleaching.R")
# READ "data/Bleaching/KI_2015d_Merulinidae.csv"
# READ "data/Bleaching/data_Supplementary_Figure_8.csv"
# MAKE "figures/Supplementary_Materials/Supplementary_Figure_8/Supplementary_Figure_8.pdf"

# Supplementary Table 2
source("figures/Supplementary_Materials/Supplementary_Table_2/env_summ_KI_Platy.R")
# READ "data/environmental_parameters/KI_env_all_KI_Platy.csv"
# MAKE "figures/Supplementary_Materials/Supplementary_Table_2/env_summ_KI_Platy_alltimes.csv"

# Supplementary Table 6
source("figures/Supplementary_Materials/Supp_Table_6_env_logistic/EnvironmentalVariables.R")
# LOAD "data/Updated_Starko/LogisticData.xlsx"
# LOAD "data/environmental_parameters/KI_env_all.xlsx"
# Manually extract model outputs

# Supplementary Table 3 and 13
source("figures/Supplementary_Materials/Supp_Table_3and13_survival_ord/fpenta_lib_cons.R")
# LOAD "figures/Figure_1/samplelist_Fig1.csv"
# LOAD "data/KI_Platy_f_coral_grouped_ASVs.RData"
# Manually extract model outputs
source("figures/Supplementary_Materials/Supp_Table_3and13_survival_ord//platy_lib_cons.R")
# LOAD "figures/Figure_1/samplelist_Fig1.csv"
# LOAD "data/KI_Platy_f_coral_grouped_ASVs.RData"


