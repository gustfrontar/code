#!/bin/bash

F90=ifort
F90FLAGS='-g'

$F90 $F90FLAGS -c minimun_tools2.0.f90
$F90 $F90FLAGS -c find_minimun2.0.f90
$F90 $F90FLAGS -o find_minimun2.0 minimun_tools2.0.o find_minimun2.0.o

rm find_minimun2.0.o minimun_tools2.0.o minimun_tools.mod



