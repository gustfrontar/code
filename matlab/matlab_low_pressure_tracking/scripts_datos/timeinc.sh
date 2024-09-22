#!/bin/bash
set -e

function timeinc6hr
{


YYYY=`echo ${1} |  cut -c1-4`
MM=`echo ${1}   |  cut -c5-6`
DD=`echo ${1}   |  cut -c7-8`
HH=`echo ${1}   |  cut -c9-10`   


# Increment date
HH=`expr $HH + 6`
if test $HH -lt 10
then
  HH=0$HH
elif test $HH -gt 23
then
  HH=00
  DD=`expr $DD + 1`
  if test $DD -lt 10
  then
    DD=0$DD
  elif test $DD -eq 29
  then
    ITMP=`expr $YYYY % 4`
    if test $MM -eq 02 -a $ITMP -ne 0
    then
      DD=01
      MM=03
    fi
  elif test $DD -eq 30
  then
    ITMP=`expr $YYYY % 4`
    if test $MM -eq 02 -a $ITMP -eq 0
    then
      DD=01
      MM=03
    fi
  elif test $DD -eq 31
  then
    if test $MM -eq 04 -o $MM -eq 06
    then
      DD=01
      MM=`expr $MM + 1`
      MM=0$MM
    elif test $MM -eq 09 -o $MM -eq 11
    then
      DD=01
      MM=`expr $MM + 1`
    fi
  elif test $DD -eq 32
  then
    DD=01
    MM=`expr $MM + 1`
    if test $MM -lt 10
    then
      MM=0$MM
    fi
  fi
  if test $MM -gt 12
  then
    MM=01
    YYYY=`expr $YYYY + 1`
  fi
fi
#
# Outputs
#
echo $YYYY$MM$DD$HH

}
