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

### SET DATES
DATE=$(get_date $START_TIME $START_HOUR $START_MIN $START_SEC)
read -r y m d H M S <<< $(get_date $START_TIME $START_HOUR $START_MIN $START_SEC "%Y %m %d %H %M %S")
VARS="y m d H M S"
export eval $VARS

### SET DIRECTORIES
export REPODIR=$H_OBSDIR/REPO
mkdir -p $REPODIR

### RUN DOWNLOAD
. $SRCDIR/download.sh 

### LOGGING
read STATUS EXIT_CODE < <(job_check_main "main_download" $DOWNMAXFAIL $SOURCES)

### PROCESS EXIT
log_split "main" "end" "$STATUS\\t$INFO"
log_sync_to_hist $DATE
job_reply $PROC $STATUS $INFO

