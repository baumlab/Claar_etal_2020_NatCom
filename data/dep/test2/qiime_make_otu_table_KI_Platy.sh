#!/bin/bash

source /macqiime/configs/bash_profile.txt

make_otu_table.py -i /Users/Danielle/Documents/Data_Analysis/KI_Platy/data/test2/KI14FSYM004_otus_out/KI14FSYM004_otus.txt -t /Users/Danielle/Documents/Data_Analysis/KI_Platy/data/test2/KI14FSYM004_otus_out/KI14FSYM004_rep_set_tax_assignments.txt -e /Users/Danielle/Documents/Data_Analysis/KI_Platy/data/test2/KI14FSYM004_otus_out/KI14FSYM004_no_hits.txt -o /Users/Danielle/Documents/Data_Analysis/KI_Platy/data/test2/KI14FSYM004_otus_out/KI14FSYM004_otu_table.biom

