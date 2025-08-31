#!/bin/bash
#####################################################################
# Run python script in this directory and save output to log folder #
#####################################################################
SCRIPT=$1
NP=$2
LEV=$3
VERT=z
EXP=RMA1_d2_2km_scalebdy_radar_init18_3D_5min

export PYTHONPATH="$PWD/.."
source activate mpi
MPIRUN="$(dirname $(which python3))/mpiexec"

RUNSCRIPT=${SCRIPT}.py
DATADIR="../../../../DATA/EXPS/CORDOBA_20181110_OFP/$EXP/ctl"

for mem in {1..60}; do

   member=$(printf "%04d" $mem)

   anal_mem=$DATADIR/analg${VERT}_$member.ctl

   # Run python script
   $MPIRUN -n $NP python3 $RUNSCRIPT $mem

   if [[ $mem -eq 1 ]]; then
      mv $DATADIR/analg${VERT}.ctl $anal_mem

      sed -i '/tdef/c\tdef  36 linear 18:05Z10Nov2018 5mn' $anal_mem
      sed -i '/^edef/d' $anal_mem
      sed -i '/^0001/d' $anal_mem

   else
      cp $DATADIR/analg${VERT}_0001.ctl $anal_mem
   fi

   sed -i -e "1 c\\" -e "dset ^../../%y4%m2%d2%h2%n200/analg${VERT}_${LEV}/$member.grd" $anal_mem

done

# Move grd and ctl files
find $DATADIR/../2*  -type d -name "analg${VERT}" | while read f; do echo $f; path=($(dirname $f)\/analg${VERT}_${LEV}); echo "f -> $path"; mv $f $path; done

mkdir -p $DATADIR/analg${VERT}_${LEV} 
mv $DATADIR/analg${VERT}*ctl $DATADIR/analg${VERT}_${LEV}
