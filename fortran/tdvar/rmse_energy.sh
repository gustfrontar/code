#!/bin/sh
EXP=$1
ANALDIR=/home/$USER/SPEEDY_DA/DAS_result/3dvar/$EXP/analf
TRUEDIR=/home/$USER/SPEEDY_DA/truth
RMSEDIR=/home/$USER/SPEEDY_DA/DAS_result/3dvar/$EXP/rmse
#COmpiler: 1)Ifort 2)Gfortran 3)PG
compiler=1
export GFORTRAN_CONVERT_UNIT="big_endian"
export F_UFMTENDIAN=big


ln -s $ANALDIR analf
ln -s $TRUEDIR truef

case ${compiler} in
 1) ifort -w -assume byterecl -o rmse_energy mt19937ar.f90 common.f90 common_speedy.f90 rmse_energy.f90;;
 2) gfortran -fno-range-check  -static-libgfortran -o rmse_energy mt19937ar.f90 common.f90 common_speedy.f90 rmse_energy.f90;;
 3) pgf90 -Mbyteswapio -o rmse_energy mt19937ar.f90 common.f90 common_speedy.f90 rmse_energy.f90;;
 *) echo "Bad compiler number --> ${compiler} <--  EXITING!!..."; exit 0;;
esac

rm -f *.mod
./rmse_energy
rm -f analf
rm -f truef
rm -f rmse_energy

mkdir -p $RMSEDIR
mv rmse_energy.grd $RMSEDIR
cp rmse_energy.ctl $RMSEDIR 
