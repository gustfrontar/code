********************AEROPARQUE***********************************************
reinit
set ylint 2
set clopts 1 0.2
set xlopts 1 0.2
set ylopts 1 0.2
set grads off

open ../datos/sabe.ctl
set xlint 6

set vrange 15 30

set y -1 1
set t 1 37
set gxout barb
set cthick 7
set ccolor 11
set cstyle 1
d u;v


close 1


open ../wrf/dominio3.ctl
set vrange 15 30
set xlint 6

set t 1 37
set lat -34.61 -34.59
set lon -58.42


set gxout barb
set cthick 7
set ccolor 2
set cstyle 1
d u;v


close 1

open ../rams/rams-comp-g3.ctl
set vrange 15 30
set xlint 6

set t 1 19
set lat -34.61 -34.59
set lon -58.42

set gxout barb
set cthick 7
set ccolor 7
set cstyle 1
d u;v


draw title Viento en Aeroparque
printim uvaero.gif gif white
