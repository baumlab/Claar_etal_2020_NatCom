#Started July 19, 2016 as the README.md file for our KI Platy Manuscript - DATA Folder  
  
FOLDERS  
   
Bioinf - Contains:  
	1) clust - all clustering files
	2) sequences - this is only on my linux machine (too large for github). Includes all raw sequence data. Will be deposited elsewhere after publication
	3) tree - includes script for phylogenetic tree building and all associated tree-building files
	4) Config files

Coralphoto_Metadata - Contains:  
	1) Raw KI_Coralphoto_Metadata_*date*.xlsx. Current version is "Jan_to_Apr_2017_23March". This is the metadata extracted from the individual coral photos  
	2) Processed KI_Coralphoto_metadata.csv - from .xlsx file above (1) with standardized column formatting. File is created using mapping files from KI_Platy/data/ 
	3) DClaar_SamplesforIllumina_19April2016_PrioritySet.xlsx - sample metadata that was sent to HIMB for processing (by Amy) This is the "Priority Set", also called "Platy run"
	4) KI_Platy_metadata.csv and .tsv - processed metadata - to use downstream
	5) process_coral_metadata.R - script to concatenate and process coral metadata based on mapping file
  
dep - depreciated scripts, kept for potential future reference. Includes:  
otus_97 - output files from running SymITS2 scripts for 97% OTU (all 	sequences combined)  
otus_97_bysample - output files from running SymITS2 scripts for 97% OTU (each sample clustered separately)  
otus_100 - output files from running SymITS2 scripts for 100% OTU (all sequences combined) This collapses down to unique sequences across the data set.  
  
fasta - Contains: 
	1) USEARCH61 chimeras
	2) combined_seqs.fna - all sequences for all samples, before chimera checking
	3) combined_seqs_chimera_filtered.fasta - after chimera checking
	4) combined seqs trimmed.fasta

merge - Contains: individual files for each paired end merged coral sample.

  
temperature - Data output from DCC+JJS heat stress Matlab script (from Claar et al. 2017 El Niño meta-analysis)  
	1) histout_kiritimati.csv - timeseries of DHW, during the timeperiod provided by timevec_kiritimati.csv  
	2) thistout_kiritimati.csv - timeseries of satellite sea surface temperature, during the timeperiod provided by timevec_kiritimati.csv  
	3) timevec_kiritimati.csv - corresponding time series for satellite data
	4) format_KI_satellite_heat.R  - formatting and prepping satellite heat data
	5) KI_satellite_heat.RData - satellite temperature data after format_KI_satellite_heat.R
	6) KI_SB_temp_DHW.RData - processed DHW from in situ temperature data from Sea-Bird 56s

FILES  

ITS2db_trimmed_derep.fasta & ITS2db_trimmed_notuniques_otus.txt - from database preparation, created by script SymITS2/prep_ITS2db.sh **The original file needed to create these files is missing - ITS2db_raw.fasta need to get!**  
  

KI_seqs_f_coral_grouped.RData - current phyloseq file used in downstream analysis. Based on 97% OTU by sample. Created from script /analyses/KI_Platy_filter_samples.Rmd  
  
mapping_file.txt - qiime-formatted mapping file for KI Platy sequencing run  
  
otu_table.tsv - otu table for phylogenetically-informed diversity. From phy97.f.c (filtered phyloseq object, before tree is added)

tax_table.txt - tax table for phylogenetically-informed diversity. From phy97.f.c (filtered phyloseq object, before tree is added)




