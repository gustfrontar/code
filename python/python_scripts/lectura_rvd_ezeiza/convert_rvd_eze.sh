#!/bin/bash

# 240 km
data=`echo $1 | awk '{print substr($1,13,13)}'`
# 120 km
#data=`echo $file1 | awk '{print substr($1,11,19)}'`

# Extracts the Header
coman=`dd if=$1 ibs=512 count=4 | tr -d '\000' > header.240km.z.$data`

# Extracts the Volume Scan
coman=` dd if=$1 ibs=2048 skip=1 | gzip -d > volscan.240km.z.$data`
