#!/bin/bash
set -e

function timeincmonth
{


YYYY=`echo ${1} |  cut -c1-4`
MM=`echo ${1}   |  cut -c5-6`


# Increment date
MM=`expr $MM + 1`
if test $MM -lt 10
   MM=0$MM
fi
if test $MM -gt 12
   MM=01
   YYYY=`expr $YYYY + 1`
fi

#
# Outputs
#
echo $YYYY$MM

}
