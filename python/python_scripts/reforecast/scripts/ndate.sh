#!/bin/bash
sdate=$1
inc=$2
iyear=`echo ${sdate} |  cut -c1-4`
imonth=`echo ${sdate}|  cut -c5-6`
iday=`echo ${sdate}  |  cut -c7-8`
ihour=`echo ${sdate} |  cut -c9-10`
case ${imonth} in
 01) cmon=Jan;;
 02) cmon=Feb;;
 03) cmon=Mar;;
 04) cmon=Apr;;
 05) cmon=May;;
 06) cmon=Jun;;
 07) cmon=Jul;;
 08) cmon=Aug;;
 09) cmon=Sep;;
 10) cmon=Oct;;
 11) cmon=Nov;;
 12) cmon=Dec;;
 *) echo "Bad Month --> ${imonth} <--  EXITING!!..."; exit 8;;
esac
                                                                                                                            
enddate2=`date --date="${inc}  hours ${ihour}:00 ${iday} ${cmon} ${iyear}" +%Y%m%d%H`

#Para tratar de salvar los casos en los que cambia la fecha.
if [ $? -eq 1 ]
then
inc=`expr $inc -1`
ihour=`expr $ihour + 1`
fi


echo $enddate2
