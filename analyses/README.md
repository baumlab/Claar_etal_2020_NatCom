#Started July 19, 2016 as our README.md file for our R data analyses of our KI Platy manuscript
  
FOLDERS  
CSEE - Rmd, html, and image files of plots created for CSEE presentation, can be adapted for manuscript figures  
	- All figures are with Platygyra samples only (no other species are included here)
  
dep - depreciated scripts, kept for potential future reference  
   WGCNA - Folder for WGCNA analyses  
   KI_Platy_analysis_DiversityModels.html & .Rmd - RMarkdown and knitted html documents with initial alpha diversity analyses. **I am not confident in these outputs yet - will need to edit/confirm to actually use**.  
   KI_Platy_analysis_PlotExploration.Rmd Takes C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/otus_97/KI_Platy_f_grouped.RData from output of KI_Platy_analysis_PhyloseqCleanup.Rmd and conducts many exploratory plots using phyloseq
  
KI_Platy_analysis_ViolinPlots_Clades.Rmd & .html - Make violin plots to see variability in abundance among different field seasons, species, and alive/dead

KI_Platy_analysis_OTUSummary_cache and KI_Platy_analysis_OTUSummary_files - cache and figure files for knitted OTUSummary.Rmd  
  


FILES
KI_Platy_analysis_PhyloseqCleanup.R Takes C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/otus_97/KI_Platy_f.RData from output of filter_notsym.R script and 
	a. Characterizes sites by disturbance level
	b. Use merge_taxa to collapse OTUS with equivalent names
	c. Use merge_taxa to collapse OTUS by clade
Saves file C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/otus_97/KI_Platy_f_grouped.RData
  
KI_Platy_analysis_OTUSummary.html and .Rmd - RMarkdown and knitted html documents with basic OTU summary statistics including  
	a. Alpha diversity by coral species  
	b. Alpha diversity by Site  
	c. Alpha diversity by Field Season  
	d. Principal (>0.01% abundance) and Background (<0.01% abundance) Symbiodinium types in coral  
	e. Core, Common, and Rare Symbiodinium types in coral  
	f. OTU Prevalence vs. Abundance in coral

KI_Platy_filter_samples.R

KI_Platy_Ordination.Rmd and .html - RMarkdown and knitted html documents wtih initial ordination analyses
