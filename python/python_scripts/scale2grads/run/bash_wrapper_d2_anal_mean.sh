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

anal_mean=$DATADIR/analg${VERT}_mean.ctl
anal_sprd=$DATADIR/analg${VERT}_sprd.ctl

$MPIRUN -n $NP python3 $RUNSCRIPT $mem

sed -i '/tdef/c\tdef  36 linear 18:05Z10Nov2018 5mn' $anal_mean
sed -i '/^edef/d' $anal_mean
sed -i '/^0001/d' $anal_mean

sed -i '/tdef/c\tdef  36 linear 18:05Z10Nov2018 5mn' $anal_sprd
sed -i '/^edef/d' $anal_sprd
sed -i '/^0001/d' $anal_sprd

sed -i -e "1 c\\" -e "dset ^../../%y4%m2%d2%h2%n200/analg${VERT}_${LEV}/mean.grd" $anal_mean
sed -i -e "1 c\\" -e "dset ^../../%y4%m2%d2%h2%n200/analg${VERT}_${LEV}/sprd.grd" $anal_sprd

# Move grd and ctl files
find $DATADIR/../2*  -type d -name "analg${VERT}" | while read f; do path=($(dirname $f)\/analg${VERT}_${LEV}); echo "f -> $path"; mv $f $path; done

mkdir -p $DATADIR/analg${VERT}_${LEV}
mv $DATADIR/analg${VERT}*ctl $DATADIR/analg${VERT}_${LEV}

