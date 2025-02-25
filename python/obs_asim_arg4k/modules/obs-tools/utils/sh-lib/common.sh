#!/bin/bash 

CDIR=$(dirname -- $BASH_SOURCE)
. ${CDIR}/settings
[[ -f ${CDIR}/model ]] && . ${CDIR}/model

########
# JOBS #
########
function job_run_from_tool {

   TOOLSDIR=$1
   PROC=$2
   ARGS=$3
 
   ### SET TOOL DIRECTORIES
   export SRCDIR=$TOOLSDIR/src
   export UTILSDIR=$TOOLSDIR/utils

   ### LOAD CONFIGURATION
   job_load_config_exp

   ### RUN FROM TOOL
   cd $TOOLSDIR/bin/run
   . run_${PROC}.sh $ARGS
 
}

function job_get_proc_name {
   FILE=$1

   IFS="_,." read -ra PROC <<< $(basename "$FILE")
   echo ${PROC[1]}
}

function job_get_alias {

   nmem=$1
   model=${2:-"WRF"}

   if [[ $nmem -eq 1 ]];then
      [[ $model == 'WRF' ]] && alias=$WRF_DET || alias=$GFS_DET
   elif [[ ! -z $BATCH || $DTYPE == 'asim' ]];then
      alias=$WRF_ANA
   else
      [[ $model == 'WRF' ]] && alias=$WRF_ENS || alias=$GFS_ENS
   fi

   echo $alias
}

function job_load_config {

   ENVCONFIG=$1
   [[ ! -f $ENVCONFIG ]] && job_reply $PROC $S_ERROR "File not found: $ENVCONFIG" || source $ENVCONFIG

}

function job_load_config_exp {

   # Experiment directories
   if [[ -f "$RUNDIR/experiment.dirs" ]]; then
      file=$RUNDIR/experiment.dirs
   elif [[ -f "$RUNDIR/$EXPNAME/experiment.dirs" ]];then
      file=$RUNDIR/$EXPNAME/experiment.dirs
   else
      job_reply $PROC $S_ERROR "File not found: experiment.dirs"
   fi
   source $file

   # Experiment configuration
   [[ ! -d $EXPDIR ]] && job_reply $PROC $S_ERROR "Directory not found: $EXPDIR"
   EXPS="experiment.conf experiment.procs"
   for exp in ${EXPS[@]}
   do
      [[ ! -f $EXPDIR/$exp ]] && job_reply $PROC $S_ERROR "File not found: $EXPDIR/$exp" || source $EXPDIR/$exp
   done

}

function job_reply {
   
   PROC=$1
   TYPE=$2
   MSG=$3

   K="\e[0m"
   R="\e[1;31m"
   G="\e[1;32m"
   Y="\e[1;33m"

   [[ ! -z $MSG ]] && MSG="-> $MSG"
   [[ $TYPE == ${S_OK} ]] && COLOR=$G && EC=$EC_OK
   [[ $TYPE == ${S_ERROR} ]] && COLOR=$R && EC=$EC_ERROR
   [[ $TYPE == ${S_WARNING} ]] && COLOR=$Y && EC=$EC_WARNING

   [[ $TYPE == "ini" ]] && STR="LAUNCH ${PROC^^}" || STR="FINISH ${PROC^^} $COLOR$TYPE$K"
   echo -e "[$(date '+%Y-%m-%d %H:%M:%S')] $STR $MSG"
   [[ ! -z $EC ]] && exit $EC 
}

function job_check_main {

   LOGFILE=$1
   MAXFAIL=$2
   ARRAY=$3

   # Get number of total arrays
   NARRAY=1
   if [[ ! -z $ARRAY ]];then
      NARRAY=$(echo $ARRAY | awk -F',' '{print NF}')
      [[ $NARRAY -eq 1 ]] && IFS="-,%" read -ra NARRAY <<< $ARRAY && NARRAY=$(( ${NARRAY[1]} - ${NARRAY[0]} + 1 )); unset IFS
   fi

   # Get number of process for each status
   LIST=$(cut -f5 < $LOGSDIR/${LOGFILE}.log)
   nerr=$(echo -n $LIST | grep -Fo $S_ERROR | wc -l)
   nwarn=$(echo -n $LIST | grep -Fo $S_WARNING | wc -l)
   nok=$(echo -n $LIST | grep -Fo $S_OK | wc -l)

   # Update number of errors
   nmiss=$(( $NARRAY - $nok - $nerr - $nwarn))
   nerr=$(( $nerr + $nmiss ))

   # Classify job
   if [[ $nok -eq $NARRAY ]];then
      STATUS=$S_OK 
   else
      if [[ $MAXFAIL -lt 0 ]];then
         [[ $nerr -eq $NARRAY ]] && STATUS=$S_ERROR || STATUS=$S_WARNING
      else
         [[ $nerr -gt $MAXFAIL ]] && STATUS=$S_ERROR || STATUS=$S_WARNING
      fi
   fi

   # TODO: get column info if error or warning
   INFO=

   echo $STATUS $INFO

}

function job_check_proc {

   PROC=$1
   EC=$2

   # Set default values
   [[ $EC -eq $EC_OK ]] && STATUS=$S_OK || STATUS="${S_ERROR}\\t$PROC"

   # Get proccess file extension
   [[ "$PROC" == *".py"* ]] && [[ $EC -eq $EC_WARNING ]] && STATUS="${S_WARNING}\\t$PROC"

   echo $STATUS
   
}

########
# LOGS #
########
function log_sync_to_hist {

   DATE=$1
   ACTION=$2

   local H_LOGSDIR=$H_LOGSDIR/$DATE/$EXPNAME
   [[ ! -d $H_LOGSDIR ]] && mkdir -p $H_LOGSDIR
   [[ ! -z "$( ls -A "$LOGSDIR")" ]] && rsync -a $LOGSDIR/ $H_LOGSDIR 
   if [[ ! -z $ACTION && $ACTION == "rm" ]]; then
      rm -fr $LOGSDIR/*
   fi
 
}

function log_init {

   LOGFILE=$1
   STR=$2

   file=$LOGSDIR/${LOGFILE}.log
   [[ -e $file ]] && rm -f $file
   touch $file
   [[ ! -z $STR ]] && echo -e $STR >> $file

}

function log_header {

   LOGFILE=$1

   HEADER="ARRAY\\tID\\tINI_TIMESTAMP\\tEND_TIMESTAMP\\tSTATUS\\tINFO"
   [[ "$LOGFILE" == "main" ]] && HEADER="STEP\\tPROCESS\\tINI_TIMESTAMP\\tEND_TIMESTAMP\\tSTATUS\\tINFO"

   echo -e $HEADER >> $LOGSDIR/${LOGFILE}.log
}

function log_split {

   LOGFILE=$1
   STAGE=$2
   STR=$3

   timestamp=$(date +'%s')
   if [[ "$STAGE" == "ini" ]];then
      FLAG="ne"
      STR="$STR\\t$timestamp"
   elif [[ "$STAGE" == "end" ]];then
      FLAG="e"
      STR="\\t$timestamp\\t$STR"
   fi
   echo -$FLAG $STR >> $LOGSDIR/${LOGFILE}.log

}

function log_tabular {

   LOGFILE=$1
   TINI=$2
   STATUS=$3
   ID=${4:-"None"}
   ARRAY=${5:-"0000"}

   # Set string format 
   ARRAY=$(printf "%04g" $ARRAY)
   STR="$ARRAY\\t$ID\\t$TINI\\t$(date +'%s')\\t$STATUS"
   echo -e $STR >> $LOGSDIR/${LOGFILE}.log

}

#########
# DATES #
#########
function get_date {
   date=$1
   hour=$2
   min=$3
   sec=$4
   fmt=${5:-$DATEFOLDER_fmt} 
   echo $(date -u -d "$date UTC +$((10#$hour)) hours +$((10#$min)) minutes + $((10#$sec)) seconds" +"$fmt")

}

function get_date_diff {

  d1=$1
  d2=$2

  # Convert to seconds
  d1=$(date -u -d "${d1:0:8} ${d1:9:2}:${d1:11:2}:${d1:13:2}" +%s)
  d2=$(date -u -d "${d2:0:8} ${d2:9:2}:${d2:11:2}:${d2:13:2}" +%s)

  # Get difference in seconds 
  echo $(($d1-$d2)) # | sed 's/-//'

}

function get_date_diff2 () {

   local DATE1=$1
   local DATE2=$2

   cy1=`echo $DATE1 | cut -c1-4`
   cm1=`echo $DATE1 | cut -c5-6`
   cd1=`echo $DATE1 | cut -c7-8`
   ch1=`echo $DATE1 | cut -c9-10`
   cn1=`echo $DATE1 | cut -c11-12`
   cs1=`echo $DATE1 | cut -c13-14`
   seconds1=`date +%s -d"$cy1-$cm1-$cd1 $ch1:$cn1:$cs1 UTC"`

   cy2=`echo $DATE2 | cut -c1-4`
   cm2=`echo $DATE2 | cut -c5-6`
   cd2=`echo $DATE2 | cut -c7-8`
   ch2=`echo $DATE2 | cut -c9-10`
   cn2=`echo $DATE2 | cut -c11-12`
   cs2=`echo $DATE2 | cut -c13-14`
   seconds2=`date +%s -d"$cy2-$cm2-$cd2 $ch2:$cn2:$cs2 UTC"`

   echo ` expr $seconds1 - $seconds2 `

}


function get_date_from_cycle {

  cycle=$1 
  [[ ${#cycle} -eq 2 ]] && cycle=${cycle}0000
  u_buffer=${2:-0}
  l_buffer=$((86400-$u_buffer))

  CD_date=$(date -u "+%Y%m%d")
  CD=$(date -u "+%Y%m%d%H%M%S")
  FD=$(date -u "+%Y%m%d")$cycle

  diff=$(get_date_diff2 $CD $FD)

  inc=0
  [[ $diff -gt 0 && $diff -gt $l_buffer ]] && inc=1 
  [[ $diff -lt 0 && ${diff#-} -gt $u_buffer ]] && inc=-1 

  echo $(date -u -d "$CD_date + $inc days" +"%Y/%m/%d")

}

#########
# FILES #
#########
function file_replace_str {
   FILE=$1
   IN=$2
   OUT=$3
   STR="s|$IN|$OUT|g"
   sed -i -e "$STR" $FILE
}

function file_edit {

   FILE=$1
   shift
   arr=${@}

   # Loop over variables to set
   for key in ${arr[@]}
   do
      if set | grep -q "^$key="; then eval val="\$$key"; fi
      if [[ ! -z $val ]];then
         STR="s|$key|$val|g"
         [[ "$FILE" == *"namelist"* ]] && STR="s|__${key}__|$val|g"
         [[ "$FILE" == *"experiment"* || "$FILE" == *"config"* ]] && STR="/export $key=/c\\export $key=$val"
         sed -i -e "$STR" $FILE
      fi
   done
}

#########
# PASES #
#########
function file_replace_exp {

   FILE_RUN=$1
   FILE_CONF=$2
   shift 2
   KEEP_VARS=${@}

   source $FILE_RUN
   cp $FILE_RUN ${FILE_RUN}.ori
   cp $FILE_CONF $FILE_RUN

   file_edit $FILE_RUN $KEEP_VARS
   EC=$?

   [[ $EC -eq 0 ]] && rm ${FILE_RUN}.ori
   [[ $EC -ne 0 ]] && mv ${FILE_RUN}.ori $FILE_RUN && echo "[ERROR] Replacing values on the file failed. Restoring the original file"

}
