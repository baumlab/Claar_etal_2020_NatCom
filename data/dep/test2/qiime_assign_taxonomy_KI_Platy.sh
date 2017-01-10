#!/bin/bash

source /macqiime/configs/bash_profile.txt

assign_taxonomy.py -i /Users/Danielle/Documents/Data_Analysis/KI_Platy/data/test2/KI14FSYM004_otus_out/KI14FSYM004_rep_set.fasta -r /Users/Danielle/Documents/Data_Analysis/Resources/ITS2_Database_04_23_13.fas -m blast -e 1e-20 -t /Users/Danielle/Documents/Research/HIMB_2014/AmSam/AmSam_Data_Files/ReferenceData/id_to_taxonomy_subtype_mod.txt -o /Users/Danielle/Documents/Data_Analysis/KI_Platy/data/test2/KI14FSYM004_otus_out/

