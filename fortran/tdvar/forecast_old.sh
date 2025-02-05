#!/bin/sh
# for forecast cycle
set -e

#
export F_UFMTENDIAN=big
#Set the PATH for the analysis
EXP=/home/juan/datos/DAS_result/3dvar/EXPnoq
analysis=${EXP}/anal
forecast=${EXP}/forecast
model=/home/juan/docencia/workshop2008/EKF/DAS/model
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
cp ${model}/source/makefile ./
cp ${model}/source/*.h ./
cp ${model}/source/*.f ./
cp ${model}/source/*.s ./

mv par_horres_t30.h atparam.h
mv par_verres.h atparam1.h

cp ${model}/ver32.input/cls_*.h ./
cp ${model}/ver32.input/inpfiles.s ./

cp ${model}/update/makefile ./
cp ${model}/update/*.h ./
cp ${model}/update/*.f ./

#Replace cls_instep to perform a 96h forecast
cp cls_instep96hforecast.h cls_instep.h

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
ftime[10]=54
ftime[11]=60
ftime[12]=66
ftime[13]=72
ftime[14]=78
ftime[15]=84
ftime[16]=90
ftime[17]=96

IYYYY2=$IYYYY
IMM2=$IMM
IDD2=$IDD
IHH2=$IHH
for ((contador = 1 ; contador <= 17 ; contador++))
do
#This do loop is to rename the output files according to the forecast time
mv ${TMPDIR}/$IYYYY2$IMM2$IDD2$IHH2.grd ${forecast}/$IYYYY2$IMM2$IDD2${IHH2}F${ftime[$contador]}.grd
mv ${TMPDIR}/$IYYYY2$IMM2$IDD2${IHH2}_p.grd ${forecast}/$IYYYY2$IMM2$IDD2${IHH2}F${ftime[$contador]}_p.grd

ITIME2=`timeinc6hr $IYYYY2 $IMM2 $IDD2 $IHH2`
IYYYY2=`echo $ITIME2 | cut -c1-4`
IMM2=`echo $ITIME2 | cut -c5-6`
IDD2=`echo $ITIME2 | cut -c7-8`
IHH2=`echo $ITIME2 | cut -c9-10`
done

#Move to the next forecast!!!
ITIME=`timeinc6hr $IYYYY $IMM $IDD $IHH`
IYYYY=`echo $ITIME | cut -c1-4`
IMM=`echo $ITIME | cut -c5-6`
IDD=`echo $ITIME | cut -c7-8`
IHH=`echo $ITIME | cut -c9-10`
mv out.lis ${forecast}/$IYYYY$IMM$IDD$IHH.lis
done


echo "NORMAL END"

