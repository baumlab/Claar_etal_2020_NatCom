#!/bin/bash

find . -type f -exec gunzip {} + # Unzip these files
# They are going to get really big! And this may take quite some time!

python make_configs.py # Make the Illumina-utils config files for the Bokulich QC method

python boku_qc.py # Run Bokulich QC method on all applicable files

python make_merge_configs.py # Make the Illumina-utils config files for merging pairs

python iu_merge_pairs.py # Merge pairs using Illumina-utils

python iu_filter_merged_reads.py # Filter merged reads (MAX-MISMATCH=3) using Illumina-utils

python rename.py # Rename merged files for downstream processing