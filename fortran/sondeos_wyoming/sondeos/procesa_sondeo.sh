#!/bin/bash
#La ide de este script es bajar un sondeo de wyoming, y procesarlo con el script de matlab para analizar la estabilidad del sondeo.
#sondeos de Wyoming.
# -- Juan Ruiz 2006 ---
#Parametros de entrada:
# $1 la fecha en formato yyyymmddhh
# $2 el id de estacion
date=$1
estacion=$2             #Id de estacion
shave="10x190"          #Geometria de reduccion de la imagen.
composite="x40+10+30" #Geometria del composite del logo.
nfigs=8                 #Numero total de figuras generadas por el scripts.
proxy=$3
spath=$4
matlabpath=$5
webpath=$6
logopath=$7

year=`echo ${date} | cut -c1-4`    #Año de comienzo de la corrida.
month=`echo ${date} | cut -c5-6`   #Mes de comienzo de la corrida.
day=`echo ${date} | cut -c7-8`     #Dia de comienzo de la corrida.
hora=`echo ${date} | cut -c9-10`     #Dia de comienzo de la corrida.


http_proxy=$proxy
export http_proxy

rm -f ${spath}sondeo.input
rm -f ${spath}/matlab/input.txt
rm -f ${spath}/matlab/out*.png

#Primera parte del script, uso wget para bajar el sondeo en cuestion en modo texto.  
  wget -o ${spath}/wgetsond.log --proxy=on  "http://weather.uwyo.edu/cgi-bin/sounding?region=samer&TYPE=TEXT%3ALIST&YEAR=${year}&MONTH=${month}&FROM=${day}${hora}&TO=${day}${hora}&STNM=${estacion}" -O ${spath}/sondeo.input

#Segundo usando el programa lee_sondeo.exe obtenemos un archivo con P(hpa),T(c) y Hr(%) para el script de matlab.

  ${spath}/lee_sondeo.exe
#La salida de este programa se llama input.txt que coincide con el nombre de la entrada del script de Matlab.
  
#Tercera etapa, vamos a llamar al script de matlab para generar los graficos.

    export DISPLAY="" #Borramos la variable display para correr matlab en batch.
cd  ${spath}/matlab/
    ${matlabpath}/matlab < sondeo.m > ${spath}/matlab/log_matlab.txt

#Cuarta etapa movemos los graficos al directorio de imagenes para publicacion en web.
#Tambiéhacemos los composites 

for ((contador=2 ; contador <= $nfigs ; contador++))
do
    mv ${spath}/matlab/out${contador}.png ${webpath}/${estacion}fig${contador}.png
    #convert ${webpath}/${estacion}fig${contador}.png -shave $shave ${webpath}/${estacion}fig${contador}.png
    composite -geometry $composite ${logopath}/logonuevo.gif ${webpath}/${estacion}fig${contador}.png ${webpath}/${estacion}fig${contador}.png
done

exit




