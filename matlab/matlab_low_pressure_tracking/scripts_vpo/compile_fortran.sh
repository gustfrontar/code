#!/bin/bash

F90=ifort
F90FLAGS='-g'

$F90 $F90FLAGS -c minimun_tools.f90
$F90 $F90FLAGS -c find_minimun.f90
$F90 $F90FLAGS -o find_minimun minimun_tools.o find_minimun.o

rm find_minimun.o minimun_tools.o minimun_tools.mod



