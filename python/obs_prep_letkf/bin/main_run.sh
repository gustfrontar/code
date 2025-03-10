##!bin/bash 
source $HOME/.bashrc
source activate obs-tools

export TEMPLATE="RADAR5MIN"  #Which configuration are we going to use?

source ../config_exp/$TEMPLATE/experiment.dirs
### START JOB
echo "Procesing observations for the template: " $TEMPLATE
echo "----------------------------------------------"
echo "[$(date '+%Y-%m-%d %H:%M')] START JOB         "
echo "----------------------------------------------"
### LOAD CONFIGURATION
for f in $TOOLSDIR/utils/sh-lib/*.sh; do [[ -f "$f" ]] && . "$f";  done
ln -sf $CONFIGDIR/catalog_process.py $CONFIGDIR/catalog_obs.py 
export PROC=$(job_get_proc_name "$0")
job_load_config_exp

#Loop over times
export CDATE=$START_DATE
while [ $(date -d "$CDATE" +"%Y%m%d%H%M%S") -le $(date -d "$END_DATE" +"%Y%m%d%H%M%S") ] 
do
   echo Processing the date: $CDATE
   #Loop over different data types.
   for MY_DATA_SOURCE in "${SOURCES[@]}"
   do
     echo Processing: $MY_DATA_SOURCE
     export $MY_DATA_SOURCE
     if [ $DOWNLOAD -eq 1 ]
 

     fi

     python -u $SRCDIR/process/process_${MY_DATA_SOURCE}.py $(date -d "$CDATE" +"%Y%m%d%H%M%S")
   done 

   export CDATE=$(date -u -d "$CDATE UTC + $WLENGTH seconds" +"%Y-%m-%d %T")


done

echo "----------------------------------------------------"
echo "[$(date '+%Y-%m-%d %H:%M')] FINISH JOB              "
echo "----------------------------------------------------"



