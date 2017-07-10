#!/bin/bash

find . -type f -exec gunzip {} + # Unzip these files

# They are going to get really big! And this may take quite some time!

python make_configs.py # Make the Illumina-utils config files for the Bokulich QC method

python boku_qc.py # Run Bokulich QC method on all applicable files
chmod u+x boku_qc_KI_Platy.


sh # Give permissions so .sh file will run
source ~/virtual-envs/illumina-utils-v2.0.0/bin/activate
./boku_qc_KI_Platy.sh # Run .sh file that was just created 

python make_merge_configs.py # Make the Illumina-utils config files for merging pairs

python iu_merge_pairs.py # Merge pairs using Illumina-utils
chmod u+x iu-merge-pairs_KI_Platy.sh # Give permissions so .sh file will run
./iu-merge-pairs_KI_Platy.sh # Run .sh file that was just created 
# find . -maxdepth 1 -name # To check how far along the merging is (because it takes a long time to do hundreds of samples)

python iu_filter_merged_reads.py # Filter merged reads (MAX-MISMATCH=3) using Illumina-utils
chmod u+x iu-filter-merged-reads_KI_Platy.sh # Give permissions so .sh file will run
./iu-filter-merged-reads_KI_Platy.sh # Run .sh file that was just created 

python rename.py # Rename merged files for downstream processing

