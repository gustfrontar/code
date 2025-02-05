#!/bin/bash
#La ide de este script es bajar un sondeo de wyoming, y procesarlo con el script de matlab para analizar la estabilidad del sondeo.
#sondeos de Wyoming.
# -- Juan Ruiz 2006 ---
#Parametros de entrada:
# $1 la fecha en formato yyyymmddhh
# $2 el id de estacion
date=$1
estacion=$2    #Id de estacion

path=/mnt/windows/sondeos_GPDCAO
webpath=/mnt/windows/sondeos_GPDCAO/jpg

http_proxy="http://proxy.uba.ar:8080"
export http_proxy

rm -f ${path}sondeo.input
rm -f ${path}/matlab/input.txt
rm -f ${path}/matlab/out*.jpg

#Primera parte del script, uso wget para bajar el sondeo en cuestion en modo texto.  
  wget -o ${path}/wgetsond.log --proxy=on  "http://weather.uwyo.edu/cgi-bin/sounding?OUTPUT=TEXT&TYPE=stuve&TIME=${date}&TIME=${date}&STNM=${estacion}" -O ${path}/sondeo.input

#Segundo usando el programa lee_sondeo.exe obtenemos un archivo con P(hpa),T(c) y Hr(%) para el script de matlab.

  ${path}/lee_sondeo.exe
#La salida de este programa se llama input.txt que coincide con el nombre de la entrada del script de Matlab.
  
#Tercera etapa, vamos a llamar al script de matlab para generar los graficos.

    export DISPLAY="" #Borramos la variable display para correr matlab en batch.
cd  ${path}/matlab/
    matlab < sondeo.m >> ${path}/matlab/log_matlab.txt

#Cuarta etapa movemos los graficos al directorio de imagenes para publicacion en web.
#Tambiéhacemos los composites 


    mv ${path}/matlab/out1.jpg ${webpath}/${estacion}fig1.jpg
    mv ${path}/matlab/out2.jpg ${webpath}/${estacion}fig2.jpg
    mv ${path}/matlab/out3.jpg ${webpath}/${estacion}fig3.jpg
    mv ${path}/matlab/out4.jpg ${webpath}/${estacion}fig4.jpg
    mv ${path}/matlab/out5.jpg ${webpath}/${estacion}fig5.jpg
    mv ${path}/matlab/out6.jpg ${webpath}/${estacion}fig6.jpg
    mv ${path}/matlab/out7.jpg ${webpath}/${estacion}fig7.jpg

convert ${webpath}/${estacion}fig1.jpg -shave 10x190 ${webpath}/${estacion}fig1.jpg
composite -geometry x40+100+53 ${webpath}/logonuevo.gif ${webpath}/${estacion}fig1.jpg ${webpath}/${estacion}fig1.jpg

convert ${webpath}/${estacion}fig2.jpg -shave 10x190 ${webpath}/${estacion}fig2.jpg
composite -geometry x40+93+33 ${webpath}/logonuevo.gif ${webpath}/${estacion}fig2.jpg ${webpath}/${estacion}fig2.jpg

convert ${webpath}/${estacion}fig3.jpg -shave 10x190 ${webpath}/${estacion}fig3.jpg
composite -geometry x40+93+33 ${webpath}/logonuevo.gif ${webpath}/${estacion}fig3.jpg ${webpath}/${estacion}fig3.jpg

convert ${webpath}/${estacion}fig4.jpg -shave 10x190 ${webpath}/${estacion}fig4.jpg
composite -geometry x40+93+33 ${webpath}/logonuevo.gif ${webpath}/${estacion}fig4.jpg ${webpath}/${estacion}fig4.jpg

convert ${webpath}/${estacion}fig5.jpg -shave 10x190 ${webpath}/${estacion}fig5.jpg
composite -geometry x40+93+33 ${webpath}/logonuevo.gif ${webpath}/${estacion}fig5.jpg ${webpath}/${estacion}fig5.jpg

convert ${webpath}/${estacion}fig6.jpg -shave 10x190 ${webpath}/${estacion}fig6.jpg
composite -geometry x40+93+33 ${webpath}/logonuevo.gif ${webpath}/${estacion}fig6.jpg ${webpath}/${estacion}fig6.jpg

convert ${webpath}/${estacion}fig7.jpg -shave 10x190 ${webpath}/${estacion}fig7.jpg
composite -geometry x40+93+33 ${webpath}/logonuevo.gif ${webpath}/${estacion}fig7.jpg ${webpath}/${estacion}fig7.jpg

exit




