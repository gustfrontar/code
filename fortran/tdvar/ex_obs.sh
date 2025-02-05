#!/bin/sh
compiler=$1
set -e
case ${compiler} in
 1) ifort  -o ex_obs ex_obs.f90;;
 2) gfortran -fno-range-check  -static-libgfortran  -o ex_obs ex_obs.f90;;
 3) pgf90 -Mbyteswapio -o ex_obs ex_obs.f90;;
 *) echo "Bad compiler number --> ${compiler} <--  EXITING!!..."; exit 0;;
esac


./ex_obs
rm ex_obs

