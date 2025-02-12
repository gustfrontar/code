#!/bin/sh
# for 3dvar cycle
set -e

TDVAR=~/workshop_2007/DAS/tdvar

#
# All input/output operations perform conversion from big-endian to little-endian on READ and from little-endian to big-endian on WRITE.
#

export F_UFMTENDIAN=big

pgf90 -Mbyteswapio -o  test mt19937ar.f90 common.f90 common_speedy.f90 test.f90 

./test

echo "NORMAL END"

