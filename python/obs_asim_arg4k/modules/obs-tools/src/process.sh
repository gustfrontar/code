#!/bin/bash 

echo $REPODIR

### SET DATE
read -r y m d H M S <<< $(get_date $START_TIME $START_HOUR $START_MIN $START_SEC-$STEP "%Y %m %d %H %M %S")
echo $SRC
echo $y/$m/$d $H:$M:$S

tini=$(date +'%s')
STATUS="OK"

$CONDADIR/conda run -n $CONDAOBS python -u $SRCDIR/process/process_${SRC}.py $y $m $d $H $M $S
STATUS=$(job_check_proc "${SRC}.py" $?)
log_tabular "main_process" $tini $STATUS ${SRC}_S${ISTEP} $ARRAYID

