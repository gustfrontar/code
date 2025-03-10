#!/bin/bash 
#SET VARIABLES
logsdir=$LOGSDIR/OBS.DOWNLOAD
mkdir -p $logsdir

export OBSDIRIN=/data/OBS

### DOWNLOAD OBSERVATION SOURCES
IFS=","
for SRC in $SOURCES
do
   echo -n "Downloading ${SRC}..."
   tini=$(date +'%s')
   STATUS="OK"

   $CONDADIR/conda run -n $CONDAOBS python -u $SRCDIR/download/download_${SRC}.py $y $m $d $H $M $S > $logsdir/${SRC}.out 2> $logsdir/${SRC}.err
   STATUS=$(job_check_proc "${SRC}.py" $?)
   log_tabular "main_download" $tini $STATUS $SRC
   echo "done"
done
unset IFS

