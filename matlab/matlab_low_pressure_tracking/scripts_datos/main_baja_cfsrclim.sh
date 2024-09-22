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
FECHAINI=197901
FECHAFIN=197901
GRIBPATH=../DATA/CFSR/HGTCLIM/


source ./timeincmonth.sh
#cd $GRIBPATH

PLAZO=$PLAZO_INI


FECHA=$FECHAINI
while [ $FECHA -le $FECHAFIN ]
do
#USE OPTION --proxy for proxy especification
curl  "http://nomad1.ncep.noaa.gov/cgi-bin/var/g2subset_cfsr_monh_pgb.pl?file=pgb.gdas.${FECHA}.grb2&lev_500_mb=on&var_HGT=on&dir=" -o $GRIBPATH/HGT500MONTHLYMEAN$FECHA.grib2

echo $FECHA
FECHA=`timeincmonth $FECHA`
echo $FECHA

done




