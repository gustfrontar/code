#!/bin/bash

#ESTE SCRIPT USA EL PROGRAMA CURL Y LAS HERRAMIENTAS DE LOS SERVIDORES DEL NCEP PARA BAJAR EFICIENTEMENTE UN SECTOR DE LA CORRIDA DEL GFS EN ALTA RESOLUCION PARA ANIDAR EL WRF.

#PROXY="proxy.fcen.uba.ar:8080"
BOTTOMLAT="-90"
TOPLAT="90"
LEFTLON="0"
RIGTHLON="360"
CURL="/home/wrf/curl/src/curl"
VAR="HGT"
INTERVALO="6"
FECHAINI=1979030418
FECHAFIN=1979030418
GRIBPATH=../DATA/REA2NCEP/HGTVPO/


source ./timeinc.sh
#cd $GRIBPATH

PLAZO=$PLAZO_INI


FECHA=$FECHAINI
while [ $FECHA -le $FECHAFIN ]
do

idir=`echo ${FECHA} |  cut -c1-6`

echo "http://nomad1.ncep.noaa.gov/pub/reanalysis-2/6hr/pgb/pgb.${idir}.inv"
echo "Get the following fields d=${FECHA}:${VAR}:500 mb:|d=${FECHA}:${VAR}:850 mb:"

./get_inv.pl http://nomad1.ncep.noaa.gov/pub/reanalysis-2/6hr/pgb/pgb.${idir}.inv | egrep "(d=${FECHA}:${VAR}:500 mb:|d=${FECHA}:${VAR}:850 mb:)" | ./get_grib.pl http://nomad1.ncep.noaa.gov/pub/reanalysis-2/6hr/pgb/pgb.${idir}   ${GRIBPATH}/REA2NCEP_${VAR}_${FECHA}.grib

#$CURL --proxy $PROXY "http://nomads.ncep.noaa.gov/cgi-bin/filter_fnl.pl?file=gdas1.t${icycle}z.pgrbanl.grib2&lev_500_mb=on&lev_850_mb=on&var_${VAR}=on&leftlon=${LEFTLON}&rightlon=${RIGTHLON}&toplat=${TOPLAT}&bottomlat=${BOTTOMLAT}&dir=%2Fgdas.${idate}" -o ${GRIBPATH}/GDAS_${VAR}_${FECHA}.grib2 

echo $FECHA
FECHA=`timeinc6hr $FECHA`
echo $FECHA

done




