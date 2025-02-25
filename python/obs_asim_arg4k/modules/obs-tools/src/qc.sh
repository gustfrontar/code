#!/bin/bash 

tini=$(date +'%s')
STATUS="OK"

$CONDADIR/conda run -n $CONDAOBS python -u ${SRCDIR}/qc/QC_surface.py
STATUS=$(job_check_proc "QC_surface.py" $?)
log_tabular "main_$PROC" $tini $STATUS "QC_surface"
