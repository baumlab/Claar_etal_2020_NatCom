#Started July 19, 2016 as our README.md file for our R data analyses of our KI Platy manuscript


R Scripts
1. KI_Platy_analysis_PhyloseqCleanup.Rmd Takes C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/otus_97/KI_Platy_f.RData from output of filter_notsym.R script and 
	a. Characterizes sites by disturbance level
	b. Use merge_taxa to collapse OTUS with equivalent names
	c. Use merge_taxa to collapse OTUS by clade
Saves file C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/otus_97/KI_Platy_f_grouped.RData

2. KI_Platy_analysis_PlotExploration.Rmd Takes C:/Users/Dani/Documents/Data_Analysis/KI_Platy/data/otus_97/KI_Platy_f_grouped.RData from output of KI_Platy_analysis_PhyloseqCleanup.Rmd and conducts many exploratory plots using phyloseq


