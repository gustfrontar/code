#!/bin/bash
#####################################################################
# Run python script in this directory and save output to log folder #
#####################################################################
SCRIPT=$1 
NP=$2
LEV=$3
VERT=z
EXP=RMA1_d2_2km_scalebdy_radar_init18_4D_CTRL

export PYTHONPATH="$PWD/.."
source activate mpi
MPIRUN="$(dirname $(which python3))/mpiexec"

RUNSCRIPT=${SCRIPT}.py
DATADIR="../../../../DATA/EXPS/CORDOBA_20181110_OFP/$EXP/ctl"

gues_mean=$DATADIR/guesg${VERT}_mean.ctl
gues_sprd=$DATADIR/guesg${VERT}_sprd.ctl

$MPIRUN -n $NP python3 $RUNSCRIPT $mem
#time $MPIRUN -n $NP python3 $RUNSCRIPT 2>&1 > $LOGFILE

sed -i '/tdef/c\tdef  36 linear 18:05Z10Nov2018 5mn' $gues_mean
sed -i '/^edef/d' $gues_mean
sed -i '/^0001/d' $gues_mean

sed -i '/tdef/c\tdef  36 linear 18:05Z10Nov2018 5mn' $gues_sprd
sed -i '/^edef/d' $gues_sprd
sed -i '/^0001/d' $gues_sprd

sed -i -e "1 c\\" -e "dset ^../../%y4%m2%d2%h2%n200/guesg${VERT}_${LEV}/mean.grd" $gues_mean
sed -i -e "1 c\\" -e "dset ^../../%y4%m2%d2%h2%n200/guesg${VERT}_${LEV}/sprd.grd" $gues_sprd

# Move grd and ctl files
find $DATADIR/../2*  -type d -name "guesg${VERT}" | while read f; do path=($(dirname $f)\/guesg${VERT}_${LEV}); echo "f -> $path"; mv $f $path; done

mkdir -p $DATADIR/guesg${VERT}_${LEV}
mv $DATADIR/guesg${VERT}*ctl $DATADIR/guesg${VERT}_${LEV}

