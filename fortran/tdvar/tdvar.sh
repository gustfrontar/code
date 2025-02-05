#!/bin/sh
# for 3dvar cycle
set -e
#
export F_UFMTENDIAN=big
export GFORTRAN_CONVERT_UNIT="big_endian"
EXP=$1   
                                                        #Experiment name
#Compiler 1) ifort 2) gfortran 3)pg
compiler=1
store=0                                                 #Store=1 save all files store=0 save less files.
TDVAR=/home/${USER}/SPEEDY_DA/tdvar                     #Script path
data=/home/${USER}/SPEEDY_DA/DAS_result/3dvar/${EXP}    #Output folder
ini=/home/${USER}/SPEEDY_DA/tdvar/initial               #Initial gues folder
model=../../model

#Obs timing = 1 sup observations at 06 and 18 utc, upper air at 00 and 12
#Obs timing = 0 upper air at 00 06 12 18
timing_obs=0


OBSDIR=/home/${USER}/SPEEDY_DA/obs
GUESDIR=$data/gues
ANALDIR=$data/anal
ANALFDIR=$data/analf
mkdir -p $ANALFDIR
mkdir -p $ANALDIR
mkdir -p $GUESDIR

cp ${ini}/ctl/*.ctl ${ANALFDIR}/
cp ${ini}/ctl/*.ctl ${ANALDIR}/
cp ${ini}/ctl/*.ctl ${GUESDIR}/



cd $TDVAR
source timeinc.sh

# Initial date of analysis cycle
IYYYY=1982
IMM=01
IDD=01
IHH=00
# Final date of analysis cycle
EYYYY=1982
EMM=02
EDD=28
EHH=18


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
rm -f *.mod *.o

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
cp ${model}/source/*.h ./
cp ${model}/source/*.f ./
cp ${model}/source/*.s ./

mv par_horres_t30.h atparam.h
mv par_verres.h atparam1.h

cp ${model}/ver32.input/cls_*.h ./
cp ${model}/ver32.input/inpfiles.s ./

case $compiler in
 1) cp ${model}/update/makefile.ifort       ./makefile;;
 2) cp ${model}/update/makefile.gfortran    ./makefile;;
 3) cp ${model}/update/makefile.pg          ./makefile;;
 *) echo "Bad Compiler number !!!";exit          ;;
esac
cp ${model}/update/*.h ./
cp ${model}/update/*.f ./

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

if test $store -eq 1
then
ln -fs ${ANALDIR}/$IYYYY$IMM$IDD$IHH.grd fort.90
time ./imp.exe > out.lis
mv $IYYYY$IMM$IDD$IHH*.grd ${ANALFDIR}
else
mv ${ANALDIR}/$IYYYY$IMM$IDD$IHH.grd fort.90
time ./imp.exe > out.lis
mv $IYYYY$IMM$IDD${IHH}_p.grd ${ANALFDIR}

fi





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
if  test $store -eq 1
then
ln -fs $OBSDIR/$IYYYY$IMM$IDD$IHH.grd obs.grd
ln -fs $GUESDIR/$IYYYY$IMM$IDD$IHH.grd gues.grd
else
ln -fs $OBSDIR/$IYYYY$IMM$IDD$IHH.grd obs.grd
mv $GUESDIR/$IYYYY$IMM$IDD$IHH.grd gues.grd
fi


if [ "$timing_obs" != "0" ]
 then
   if test $IHH -eq 00
   then
   echo "Full obs"
   cp fort.22.full fort.22
   fi
   if test $IHH -eq 12
   then
   echo "Full obs"
   cp fort.22.full fort.22
   fi
   if test $IHH -eq 06
   then
   echo "Sup obs"
   cp fort.22.sup fort.22
   fi
   if test $IHH -eq 18
   then
   echo "Sup obs"
   cp fort.22.sup fort.22
   fi

fi

time ./tdvar
mv anal.grd $ANALDIR/$IYYYY$IMM$IDD$IHH.grd
rm -f gues.grd

done

rm -f tdvar

echo "NORMAL END"

