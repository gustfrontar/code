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


convert ${webpath}/${estacion}fig1.png -shave 1x180 ${webpath}/${estacion}fig1.png
composite -geometry x40+85+27 ${webpath}/logonuevo.gif ${webpath}/${estacion}fig1.png ${webpath}/${estacion}fig1.png


exit




