#!/bin/sh
# for 3dvar cycle
set -e



#
export F_UFMTENDIAN=big
export GFORTRAN_CONVERT_UNIT="big_endian"
EXP=$1                                                  #Experiment name
#COmpiler 1) ifort 2) gfortran 3) pg
compiler=1
TDVAR=/home/${USER}/SPEEDY_DA/tdvar                     #Script path
DATA=/home/${USER}/SPEEDY_DA/DAS_result/3dvar/${EXP}    #Output folder
ini=/home/${USER}/SPEEDY_DA/tdvar/initial          #Initial gues folder

OBSDIR=/home/${USER}/SPEEDY_DA/obs
GUESDIR=$DATA/gues
ANALDIR=$DATA/anal
ANALFDIR=$DATA/analf
mkdir -p $ANALFDIR
mkdir -p $ANALDIR
mkdir -p $GUESDIR

cp ${ini}/ctl/*.ctl ${DATA}/


cd $TDVAR
source timeinc.sh

# Initial date of analysis cycle
IYYYY=1982
IMM=01
IDD=01
IHH=00
# Final date of analysis cycle
EYYYY=1982
EMM=01
EDD=01
EHH=00


# 1111 for p-level output
FORT2=1111

cat ../model/update/cls_instep.h

#
# 3DVAR (Observation setting and build 3DVAR)
#
cd $TDVAR

#
# Observation coverage, the current setting is rawinsonde network. By changing 
# "msw_real=.TRUE." to "msw_real=.FALSE." and "msw_dnsobs=.FALSE." to "msw_dnsobs=.TRUE.",
# the observation network is changed from rawinsonde to dense observation coverage, which has one observation every two grid points.
#
sh ex_obs.sh $compiler
case ${compiler} in
 1) ifort -assume byterecl -o tdvar mt19937ar.f90 common.f90 common_speedy.f90 lbfgs.f minimizelib.f90 tdvar_tools.f90 tdvar.f90;;
 2) gfortran -fno-range-check  -static-libgfortran -o tdvar mt19937ar.f90 common.f90 common_speedy.f90 lbfgs.f minimizelib.f90 tdvar_tools.f90 tdvar.f90;;
 3) pgf90 -Mbyteswapio -o tdvar mt19937ar.f90 common.f90 common_speedy.f90 lbfgs.f minimizelib.f90 tdvar_tools.f90 tdvar.f90;;
 *) echo "Bad compiler number --> ${compiler} <--  EXITING!!..."; exit 0;;
esac
rm -f *.mod
#
# SPEEDY
#
TMPDIR=../model/tmp

mkdir -p $TMPDIR
rm -f $TMPDIR/*

cd $TMPDIR
#
# BUILD SPEEDY MODEL
#
echo '>>>BEGIN BUILDING SPEEDY MODEL'
cp ../../model/source/*.h ./
cp ../../model/source/*.f ./
cp ../../model/source/*.s ./

mv par_horres_t30.h atparam.h
mv par_verres.h atparam1.h

cp ../../model/ver32.input/cls_*.h ./
cp ../../model/ver32.input/inpfiles.s ./


case $compiler in
 1) cp ../../model/update/makefile.ifort       ./makefile;;
 2) cp ../../model/update/makefile.gfortran    ./makefile;;
 3) cp ../../model/update/makefile.pg          ./makefile;;
 *) echo "Bad Compiler number !!!";exit          ;;
esac

cp ../../model/update/*.h ./
cp ../../model/update/*.f ./

make imp.exe


sh inpfiles.s t30
cd ../

echo '>>>END BUILDING SPEEDY MODEL'
#
# MAIN LOOP
#

cp -f ${ini}/anal/$IYYYY$IMM$IDD$IHH.grd $ANALDIR
while test $IYYYY$IMM$IDD$IHH -le $EYYYY$EMM$EDD$EHH
do

echo ">>>BEGIN COMPUTATION OF $IYYYY/$IMM/$IDD/$IHH"

cd ../model/tmp

echo $FORT2 > fort.2
echo $IYYYY >> fort.2
echo $IMM >> fort.2
echo $IDD >> fort.2
echo $IHH >> fort.2

ln -fs ${ANALDIR}/$IYYYY$IMM$IDD$IHH.grd fort.90
time ./imp.exe > out.lis
mv $IYYYY$IMM$IDD$IHH*.grd ${ANALFDIR}





ITIME=`timeinc6hr $IYYYY $IMM $IDD $IHH`
IYYYY=`echo $ITIME | cut -c1-4`
IMM=`echo $ITIME | cut -c5-6`
IDD=`echo $ITIME | cut -c7-8`
IHH=`echo $ITIME | cut -c9-10`
#mv out.lis ${GUESDIR}/$IYYYY$IMM$IDD$IHH.lis
mv $IYYYY$IMM$IDD$IHH*.grd ${GUESDIR}


#
# 3DVAR
#
cd ../../tdvar
ln -fs $OBSDIR/$IYYYY$IMM$IDD$IHH.grd obs.grd
ln -fs $GUESDIR/$IYYYY$IMM$IDD$IHH.grd gues.grd
cp $GUESDIR/$IYYYY$IMM$IDD$IHH.grd  $DATA/gues.grd

time ./tdvar
mv anal.grd $DATA/analysis.grd
rm -f gues.grd

done

rm -f tdvar
rm -fr $GUESDIR $ANALDIR $ANALFDIR

echo "NORMAL END"

