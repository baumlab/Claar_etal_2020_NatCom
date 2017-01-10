#!/bin/bash
set -e
set -u
set -o pipefail

iu-filter-merged-reads -m 3 /Users/Danielle/Desktop/test2/./KI14FSYM004-39355724/Data/Intensities/BaseCalls/OR4_S333_L001-QUALITY_PASSED_MERGED
iu-filter-merged-reads -m 3 /Users/Danielle/Desktop/test2/./KI14FSYM019-39355798/Data/Intensities/BaseCalls/OR19_S307_L001-QUALITY_PASSED_MERGED
