#!/bin/sh
# for forecast cycle
set -e

#
export F_UFMTENDIAN=big
export GFORTRAN_CONVERT_UNIT="big_endian"
#Compiler 1) ifort 2) gfortran 3)pg
compiler=3
#Set the PATH for the analysis
EXP=../../DAS_result/3dvar/$1
analysis=${EXP}/analf
forecast=${EXP}/forecast
model=../../model
scripts=./

mkdir -p $forecast


source ${scripts}/timeinc.sh

# Initial date of analysis cycle
IYYYY=1982
IMM=01
IDD=01
IHH=00
# Final date of analysis cycle
EYYYY=1982
EMM=01
EDD=31
EHH=18

# 1111 for p-level output
FORT2=1111

#
# SPEEDY
#
TMPDIR=${model}/tmp

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

#Replace cls_instep to perform a 96h forecast
cp cls_instep48hforecast.h cls_instep.h

make imp.exe

sh inpfiles.s t30

echo '>>>END BUILDING SPEEDY MODEL'
#
# MAIN LOOP
#
while test $IYYYY$IMM$IDD$IHH -le $EYYYY$EMM$EDD$EHH
do

echo ">>>BEGIN COMPUTATION OF $IYYYY/$IMM/$IDD/$IHH"

cd ${model}/tmp

echo $FORT2 > fort.2
echo $IYYYY >> fort.2
echo $IMM >> fort.2
echo $IDD >> fort.2
echo $IHH >> fort.2

ln -fs ${analysis}/$IYYYY$IMM$IDD$IHH.grd fort.90


time ./imp.exe > out.lis

#
# Outputs
#
ftime[1]=00
ftime[2]=06
ftime[3]=12
ftime[4]=18
ftime[5]=24
ftime[6]=30
ftime[7]=36
ftime[8]=42
ftime[9]=48

IYYYY2=$IYYYY
IMM2=$IMM
IDD2=$IDD
IHH2=$IHH
for ((contador = 1 ; contador <= 8 ; contador++))
do
#This do loop is to rename the output files according to the forecast time
rm -f ${TMPDIR}/$IYYYY2$IMM2$IDD2$IHH2.grd ${TMPDIR}/$IYYYY2$IMM2$IDD2${IHH2}_p.grd
ITIME2=`timeinc6hr $IYYYY2 $IMM2 $IDD2 $IHH2`
IYYYY2=`echo $ITIME2 | cut -c1-4`
IMM2=`echo $ITIME2 | cut -c5-6`
IDD2=`echo $ITIME2 | cut -c7-8`
IHH2=`echo $ITIME2 | cut -c9-10`
done
#We only store the last time of the forecast.
mkdir -p  ${forecast}/F${ftime[$contador]}
mv ${TMPDIR}/$IYYYY2$IMM2$IDD2$IHH2.grd ${forecast}/F${ftime[$contador]}/$IYYYY2$IMM2$IDD2${IHH2}.grd
mv ${TMPDIR}/$IYYYY2$IMM2$IDD2${IHH2}_p.grd ${forecast}/F${ftime[$contador]}/$IYYYY2$IMM2$IDD2${IHH2}_p.grd


#Move to the next forecast!!!
ITIME=`timeinc6hr $IYYYY $IMM $IDD $IHH`
IYYYY=`echo $ITIME | cut -c1-4`
IMM=`echo $ITIME | cut -c5-6`
IDD=`echo $ITIME | cut -c7-8`
IHH=`echo $ITIME | cut -c9-10`
mv out.lis ${forecast}/$IYYYY$IMM$IDD$IHH.lis
done


echo "NORMAL END"

