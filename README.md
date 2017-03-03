# KI PLATY MANUSCRIPT  
Repository for the "KI Platy" Symbiodinium analyses and manuscript  
  
Data source: Kiritimati Field Seasons 2014, 2015Jan, 2015May, 2015July, 2016March  
Sequencing Runs: First two runs completed by Amy at HIMB 1) "Platy" run, 2) "Compartment Study" run  
Data Subset: Included only coral data from these runs (water and sediment data are currently in KI_seqs repo)  
  
Initial data processing: SymITS2 repo (https://github.com/jrcunning/SymITS2)  
  
Basic Analysis Scripts:  
 1) KI_Platy_analysis_PhyloseqCleanup.R  
    This script takes filtered RData object from output of filter_notsym.R script (from SymITS2 pipeline) and conducts basic clean up and filtering:  
       - Specifying disturbance levels for each site  
       - Making a column in phyloseq tax_table for Symbiodinium clade  
       - Subsetting coral by taxa  
       - Calculating proportional abundance for downstream analyses that need the data in this form  
       - Subsetting coral by site  
       - Subsetting coral by field season  
       - Calculating "principal" (>1% overall abundance) and "background" (<1% overall abundance) Symbiodinium types  
       - Calculating "core" (found in >75% of samples), "common" (found in 25-75% of samples), and "rare" (found in <25% of samples) Symbiodinium types  
    Saves as C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/KI_seqs_f_coral_grouped.RData to be used downstream.  
 2) KI_Platy_analysis_OTUSummary.Rmd  
    This script takes cleaned data from KI_Platy_analysis_PhyloseqCleanup.R and conducts basic summary analyses and creates summary figures.  
       - Calculate OTU and clade diversity by taxa, and create barplot  
       - Calculate OTU and clade diversity by site, and create barplot  
       - Calculate OTU and clade diversity by field season, and create barplot  
       - Define and list principal and background symbiont names, create barplot  
       - Define and list core, common, and rare symbiont names, create barplot  
       - Calculates abundance and prevalence for all OTUs, and create scatter plot  
    Saves as C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/otus_97/KI_seqs_f_coral_grouped_OTUSummary.RData and knits to html for easier viewing.

