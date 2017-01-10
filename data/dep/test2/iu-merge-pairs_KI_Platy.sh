#!/bin/bash
set -e
set -u
set -o pipefail

iu-merge-pairs -o OR19_S307_L001-QUALITY_PASSED --enforce-Q30-check OR19_S307_L001-QUALITY_PASSED_merge_config
iu-merge-pairs -o OR4_S333_L001-QUALITY_PASSED --enforce-Q30-check OR4_S333_L001-QUALITY_PASSED_merge_config
