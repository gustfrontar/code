#!/bin/bash 

### LOAD CONFIGURATION
for f in ../../utils/sh-lib/*.sh; do [[ -f "$f" ]] && . "$f";  done
export PROC=$(job_get_proc_name "$0")
job_reply $PROC "ini"
job_load_config_exp

### LOG MAIN AND PROCESS
log_split "main" "ini" "2.\\t${PROC^^}"
log_init "main_$PROC"
log_header "main_$PROC"

### SET DIRECTORIES
export REPODIR=$H_OBSDIR/REPO
export MONITDIR=$H_MONITDIR
export QCDIR=$H_OBSDIR/QC

### SET SLURM PARAMETERS
QNAME=OBS.QC
QPROC=$QCPROC
QTHREADS=$QCTHREADS
QARRAY=""
QWALLTIME=$QCWALLTIME
QEXCLU=$QCEXCLU

### CREATE SLURM SCRIPT
cd $SLURMDIR
read -r -d '' QSCRIPTCMD << "EOF"

### RUN QC
. $SRCDIR/qc.sh 

EOF

### QUEUE SCRIPT
queue

### GET DATE
DATE=$(get_date $START_TIME $START_HOUR $START_MIN $START_SEC)

### LOGGING
read STATUS EXIT_CODE < <(job_check_main "main_$PROC" $QCMAXFAIL $QARRAY)

### PROCESS EXIT
log_split "main" "end" "$STATUS\\t$INFO"
log_sync_to_hist $DATE
job_reply $PROC $STATUS $INFO

