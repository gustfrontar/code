#!/bin/bash
DATE=$1

# Activate conda environment
source activate pm

# Main directories
export DATADIR='/vol0302/data/hp150019/u01172/data/OBS/radar_qc/20191010/'

# 1) MAX. DBZ MOSAIC
python -u calc_mdbz_mosaic.py $DATE

