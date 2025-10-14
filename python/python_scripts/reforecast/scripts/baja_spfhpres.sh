#!/bin/bash

#ESTE SCRIPT USA EL PROGRAMA CURL Y LAS HERRAMIENTAS DE LOS SERVIDORES DEL NCEP PARA BAJAR EFICIENTEMENTE UN SECTOR DE LA CORRIDA DEL GFS EN ALTA RESOLUCION PARA ANIDAR EL WRF.

ITIME=1984120100
ETIME=2010123118
INT=24

#PROXY="proxy.fcen.uba.ar:8080"
#BOTTOMLAT="-80"
#TOPLAT="10"
#LEFTLON="-110"
#RIGTHLON="-20"
#TODAY=`date +%Y%m%d`
CURL="/usr/bin/curl"
#GRIBPATH=./

CTIME=$ITIME
while [ $CTIME -le $ETIME ]
do
echo "Voy a bajar el GDAS correspondiente a la fecha: $CTIME"
FECHA=`echo $CTIME | cut -c1-8`
ANIO=`echo $CTIME | cut -c1-4`
MES=`echo $CTIME | cut -c5-6`
DIA=`echo $CTIME | cut -c7-8`

curl ftp://ftp.cdc.noaa.gov/Projects/Reforecast2/${ANIO}/${ANIO}${MES}/${FECHA}00/mean/latlon/spfh_pres_${FECHA}00_mean.grib2 -o /home/jruiz/DATOS_REFORECAST/spfh_pres/spfh_pres_${FECHA}00_mean.grib2
CTIME=`sh ndate.sh $CTIME $INT`
echo $CTIME
done
