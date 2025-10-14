#!/bin/bash

#ESTE SCRIPT USA EL PROGRAMA CURL Y LAS HERRAMIENTAS DE LOS SERVIDORES DEL NCEP PARA BAJAR EFICIENTEMENTE UN SECTOR DE LA CORRIDA DEL GFS EN ALTA RESOLUCION PARA ANIDAR EL WRF.

ITIME=1984120100
ETIME=2012010100
INT=24
MEMBER=c00   #c00, p01.... , mean,sprd

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

echo  ftp://ftp.cdc.noaa.gov/Projects/Reforecast2/${ANIO}/${ANIO}${MES}/${FECHA}00/${MEMBER}/latlon/tmp_2m_${FECHA}00_${MEMBER}.grib2

curl ftp://ftp.cdc.noaa.gov/Projects/Reforecast2/${ANIO}/${ANIO}${MES}/${FECHA}00/${MEMBER}/latlon/tmp_2m_${FECHA}00_${MEMBER}.grib2 -o /home/jruiz/datosmate/DATOS_REFORECAST/tmp_2m/tmp_2m_${FECHA}00_${MEMBER}.grib2
CTIME=`sh ndate.sh $CTIME $INT`
echo $CTIME
done
