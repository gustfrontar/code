#!/bin/bash 
for f in $TOOLSDIR/utils/sh-lib/*.sh; do [[ -f "$f" ]] && . "$f";  done

echo $REPODIR

### SET DATE
read -r y m d H M S <<< $(get_date $START_TIME $START_HOUR $START_MIN $START_SEC-$STEP "%Y %m %d %H %M %S")
echo $MY_DATA_SOURCE
echo $y/$m/$d $H:$M:$S

tini=$(date +'%s')
STATUS="OK"
echo Running the following script: $SRCDIR/process/process_${MY_DATA_SOURCE}.py $y $m $d $H $M $S
python -u $SRCDIR/process/process_${MY_DATA_SOURCE}.py $y $m $d $H $M $S
STATUS=$(job_check_proc "${MY_DATA_SOURCE}.py" $?)
log_tabular "main_process" $tini $STATUS ${MY_DATA_SOURCE}_S${ISTEP} 0

