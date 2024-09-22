#!/bin/bash

F90=ifort
F90FLAGS='-g'

$F90 $F90FLAGS -c minimun_laplaciano_tools.f90
$F90 $F90FLAGS -c find_minimun_laplaciano.f90
$F90 $F90FLAGS -o find_minimun_laplaciano_forecast minimun_laplaciano_tools.o find_minimun_laplaciano.o

rm find_minimun_laplaciano.o minimun_laplaciano_tools.o minimun_tools.mod



