##!bin/bash 
export OBSPROC=40
source $HOME/.bashrc
source activate obs-tools

export TEMPLATE="RADARwVR"  #Which configuration are we going to use?
source ../config_exp/$TEMPLATE/experiment.dirs
source ../config_exp/$TEMPLATE/experiment.conf
### START JOB
echo "Procesing observations for the template: " $TEMPLATE
echo "----------------------------------------------"
echo "[$(date '+%Y-%m-%d %H:%M')] START JOB         "
echo "----------------------------------------------"
### LOAD CONFIGURATION
#for f in $TOOLSDIR/utils/sh-lib/*.sh; do [[ -f "$f" ]] && . "$f";  done
ln -sf $CONFIGDIR/catalog_process.py $CONFIGDIR/catalog_obs.py 
export PROC=$(job_get_proc_name "$0")
#job_load_config_exp

echo ${SOURCES}
#Loop over times

echo "Downloading data"
export CDATE=$START_DATE
while [ $(date -d "$CDATE" +"%Y%m%d%H%M%S") -le $(date -d "$END_DATE" +"%Y%m%d%H%M%S") ] 
do
   echo Processing the date: $CDATE
   #Loop over different data types.
   for MY_DATA_SOURCE in "${SOURCES[@]}"
   do
     export $MY_DATA_SOURCE
     echo $DOWNLOAD $MY_DATA_SOURCE
     if [ $DOWNLOAD -eq 1 ] 
     then
       echo Downloading: $MY_DATA_SOURCE
     #  python -u $SRCDIR/download/download_${MY_DATA_SOURCE}.py $(date -d "$CDATE" +"%Y%m%d%H%M%S")

     fi
   done
   export CDATE=$(date -u -d "$CDATE UTC + $WLENGTH seconds" +"%Y-%m-%d %T")
done 

#Perform QC for automatic weather stations (ADPAUT) if requested.
if [[ " ${SOURCES[*]} " =~ [[:space:]]"ADPAUT"[[:space:]] ]] ; then
   if [ $QCSURF -eq 1 ] ; then 
      echo QC: $MY_DATA_SOURCE
      python -u $SRCDIR/qc/QC_surface.py $(date -d "$END_DATE" +"%Y%m%d%H%M%S")
   fi
fi


echo "Processing the data"
export CDATE=$START_DATE
while [ $(date -d "$CDATE" +"%Y%m%d%H%M%S") -le $(date -d "$END_DATE" +"%Y%m%d%H%M%S") ] 
do
   echo Processing the date: $CDATE
   #Loop over different data types.
   for MY_DATA_SOURCE in "${SOURCES[@]}"
   do
     echo Processing: $MY_DATA_SOURCE
     export $MY_DATA_SOURCE
     python -u $SRCDIR/process/process_${MY_DATA_SOURCE}.py $(date -d "$CDATE" +"%Y%m%d%H%M%S")
   done 

   export CDATE=$(date -u -d "$CDATE UTC + $WLENGTH seconds" +"%Y-%m-%d %T")
done

echo "----------------------------------------------------"
echo "[$(date '+%Y-%m-%d %H:%M')] FINISH JOB              "
echo "----------------------------------------------------"



