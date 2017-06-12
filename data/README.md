#Started July 19, 2016 as the README.md file for our KI Platy Manuscript - DATA Folder  
  
FOLDERS  
  
Coralphoto_Metadata - Contains:  
	1) Raw KI_Coralphoto_Metadata_*date*.xlsx. Current version is "Jan 26" **NEED TO DOUBLE CHECK THAT THIS IS THE MOST UP-TO-DATE version**. This is the metadata extracted from the individual coral photos  
	2) Processed KI_metadata.csv - from .xlsx file above (1) with standardized column formatting. File is created also using mapping files from KI_Platy/data/ and KI_Compartment/data to provide additional metadata  
	3) DClaar_SamplesforIllumina_19April2016_PrioritySet.xlsx - sample metadata that was sent to HIMB for processing (by Amy) This is the "Priority Set", also called "Platy run"
  
otus_97 - output files from running SymITS2 scripts for 97% OTU (all sequences combined)  
  
otus_97_bysample - output files from running SymITS2 scripts for 97% OTU (each sample clustered separately)  
   
otus_100 - output files from running SymITS2 scripts for 100% OTU (all sequences combined) This collapses down to unique sequences across the data set.  
  
temperature - Data output from DCC+JJS heat stress Matlab script (from Claar et al. 2017 El Niño meta-analysis)  
	1) histout_kiritimati.csv - timeseries of DHW, during the timeperiod provided by timevec_kiritimati.csv  
	2) thistout_kiritimati.csv - timeseries of satellite sea surface temperature, during the timeperiod provided by timevec_kiritimati.csv  

dep - depreciated scripts, kept for potential future reference  
  
FILES  
  
ITS2db_trimmed_derep.fasta & ITS2db_trimmed_notuniques_otus.txt - from database preparation, created by script SymITS2/prep_ITS2db.sh **The original file needed to create these files is missing - ITS2db_raw.fasta need to get!**  
  
KI_seqs_f_coral_grouped.RData - current phyloseq file used in downstream analysis. Based on 97% OTU by sample. Created from script /analyses/KI_Platy_analysis_PhyloseqCleanup.Rmd  
  
mapping_file.txt - qiime-formatted mapping file for KI Platy sequencing run  
  



