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
export REPODIR=$H_OBSDIR/REPO/
export QCDIR=$H_OBSDIR/QC
mkdir -p $H_OBSDIR $H_MONITDIR

### SET SLURM PARAMETERS
QNAME=OBS.PROCESS
QPROC=$OBSPROC
QTHREADS=$OBSTHREADS
export NSOURCES=$(echo $SOURCES | awk -F',' '{print NF}')
QARRAY=0
QWALLTIME=$OBSWALLTIME
QEXCLU=$OBSEXCLU

### CREATE SLURM SCRIPT
cd $SLURMDIR
read -r -d '' QSCRIPTCMD << "EOF"

#IVAR=$(($ARRAYID % $NSOURCES))
#export ISTEP=$(($ARRAYID / $NSOURCES))

#export SRC=$(echo ${SOURCES} | cut -d ',' -f$(($IVAR + 1)))
#export STEP=$(($ISTEP * $WLENGTH))

### RUN PROCESS
. $SRCDIR/process.sh 

EOF

### QUEUE SCRIPT
queue
### GET DATE
DATE=$(get_date $START_TIME $START_HOUR $START_MIN $START_SEC)

### LOGGING
read STATUS EXIT_CODE < <(job_check_main "main_process" $OBSMAXFAIL $QARRAY)

### PROCESS EXIT
log_split "main" "end" "$STATUS\\t$INFO"
log_sync_to_hist $DATE
job_reply $PROC $STATUS $INFO

