#!/bin/bash

#Este script usa el comando curl para descargar los pronosticos retrospectivos del NCEP del AWS en formato grib2. 
ITIME='2000-01-01 00:00:00' 
ETIME='2019-12-31 00:00:00'
INT=86400

CURL="curl"
GRIBPATH="/home/jruiz/datosmunin3/datos/DATOS_REFORECAST/"
VAR="apcp_sfc"
MEMBER="c00"
SIMDOWN=10


mkdir ${GRIBPATH}/${VAR}/
CTIME=$ITIME
while [ $(date -u -d "$CTIME UTC" +"%Y%m%d%H") -le $(date -u -d "$ETIME UTC" +"%Y%m%d%H") ]
do

NDOWN=0

   while [ $NDOWN -le $SIMDOWN  ]
   do 
     echo "Voy a bajar el GDAS correspondiente a la fecha: $CTIME"
     ANIO=$(date -u -d "$CTIME UTC" +"%Y")
     FECHA=$(date -u -d "$CTIME UTC" +"%Y%m%d%H")
     curl https://noaa-gefs-retrospective.s3.amazonaws.com/GEFSv12/reforecast/${ANIO}/${FECHA}/${MEMBER}/Days%3A1-10/${VAR}_${FECHA}_${MEMBER}.grib2 -o  ${GRIBPATH}/${VAR}/${VAR}_${FECHA}_${MEMBER}.grib2  &

     CTIME=$(date -u -d "$CTIME UTC +$INT seconds" +"%Y-%m-%d %T")

     NDOWN=$(( $NDOWN + 1 ))

   done

   time wait

done
