#!/bin/bash
set -e
set -u
set -o pipefail

iu-filter-quality-bokulich ./OR19_S307_L001_config
iu-filter-quality-bokulich ./OR4_S333_L001_config
