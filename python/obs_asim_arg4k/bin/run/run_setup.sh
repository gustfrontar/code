#!/bin/bash 

### LOAD CONFIGURATION
export TOOLSDIR=$(realpath $(pwd)/../../modules/obs-tools)
for f in $TOOLSDIR/utils/sh-lib/*.sh; do [[ -f "$f" ]] && . "$f";  done
export PROC=$(job_get_proc_name "$0")
job_reply $PROC "ini"
job_load_config "../config"
job_load_config_exp

### MOVE PREVIOUS LOGS TO HIST
DATE=$(get_date $START_TIME $START_HOUR $START_MIN $START_SEC)
[[ ! "$START_TIME" == "1990/01/01" ]] && log_sync_to_hist $DATE "rm"

### UPDATE DATE
TIMEINC=$(get_timeinc_from_wait)

read -r START_TIME START_HOUR START_MIN START_SEC <<< $(get_date $START_TIME $START_HOUR $START_MIN $START_SEC+$TIMEINC "%Y/%m/%d %H %M %S")
VARS="START_TIME START_HOUR START_MIN START_SEC"
file_edit "$EXPDIR/experiment.conf" $VARS
DATE=$(get_date $START_TIME $START_HOUR $START_MIN $START_SEC)

### LOGGING
log_init "main" "RUNNING DATE: $START_TIME $START_HOUR:$START_MIN:$START_SEC \n"
log_header "main"
log_split "main" "ini" "1.\\tSETUP"

### LOGGING
log_split "main" "end" $S_OK
log_sync_to_hist $DATE
job_reply $PROC $S_OK

