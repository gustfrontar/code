#!/bin/bash
#La ide de este script es bajar un sondeo de wyoming, y procesarlo con el script de matlab para analizar la estabilidad del sondeo.
#sondeos de Wyoming.
# -- Juan Ruiz 2006 ---
#Parametros de entrada:
# $1 la fecha en formato yyyymmddhh
# $2 el id de estacion
date=$1
estacion=$2    #Id de estacion
shave="10x190" #Geometria de reduccion de la imagen.
composite="x40+120+107" #Geometria del composite del logo.
nfigs=8                 #Numero total de figuras generadas por el scripts.

thispath=`pwd`
path=${thispath}/
matlabpath=""
webpath=${thispath}/jpg/
#logopath=/WRFV2/wrfsi/domains/operativo/www/imagenes

http_proxy=""
export http_proxy

rm -f ${path}sondeo.input
rm -f ${path}/matlab/input.txt
rm -f ${path}/matlab/out*.png

#Primera parte del script, uso wget para bajar el sondeo en cuestion en modo texto.  
  wget -o ${path}/wgetsond.log --proxy=on  "http://weather.uwyo.edu/cgi-bin/sounding?OUTPUT=TEXT&TYPE=stuve&TIME=${date}&TIME=${date}&STNM=${estacion}" -O ${path}/sondeo.input

#Segundo usando el programa lee_sondeo.exe obtenemos un archivo con P(hpa),T(c) y Hr(%) para el script de matlab.

  ${path}/lee_sondeo.exe
#La salida de este programa se llama input.txt que coincide con el nombre de la entrada del script de Matlab.
  
#Tercera etapa, vamos a llamar al script de matlab para generar los graficos.

    export DISPLAY="" #Borramos la variable display para correr matlab en batch.
cd  ${path}/matlab/
    ${matlabpath}matlab < sondeo.m > ${path}/matlab/log_matlab.txt

#Cuarta etapa movemos los graficos al directorio de imagenes para publicacion en web.
#Tambiéhacemos los composites 

for ((contador=2 ; contador <= $nfigs ; contador++))
do
    mv ${path}/matlab/out${contador}.png ${webpath}/${estacion}date${date}fig${contador}.png
#    convert ${webpath}/${estacion}fig${contador}.png -shave $shave ${webpath}/${estacion}fig${contador}.png
#    composite -geometry $composite ${logopath}/logonuevo.gif ${webpath}/${estacion}fig${contador}.png ${webpath}/${estacion}fig${contador}.png
done

exit




