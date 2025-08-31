#!/bin/bash
#####################################################################
# Run python script in this directory and save output to log folder #
#####################################################################
SCRIPT=$1 
NP=$2
VERT=$3

export PYTHONPATH="$PWD/.."
source activate mpi
MPIRUN="$(dirname $(which python3))/mpiexec"

RUNSCRIPT=${SCRIPT}.py
DATADIR="../../DATA/EXPS/CORDOBA_20181110_OFP/RMA1_d1_10km_wrfbdy/ctl"

for mem in {1..60}; do

   member=$(printf "%04d" $mem)

   anal_mem=$DATADIR/analg${VERT}_$member.ctl
   gues_mem=$DATADIR/guesg${VERT}_$member.ctl

   $MPIRUN -n $NP python3 $RUNSCRIPT $mem
   #time $MPIRUN -n $NP python3 $RUNSCRIPT 2>&1 > $LOGFILE

   if [[ $mem -eq 1 ]]; then
      mv $DATADIR/analg${VERT}.ctl $anal_mem
      mv $DATADIR/guesg${VERT}.ctl $gues_mem

      sed -i '/tdef/c\tdef  28 linear 19:00Z09Nov2018 60mn' $anal_mem
      sed -i '/tdef/c\tdef  28 linear 19:00Z09Nov2018 60mn' $gues_mem

      sed -i '/^edef/d' $anal_mem
      sed -i '/^edef/d' $gues_mem

      sed -i '/^0001/d' $anal_mem
      sed -i '/^0001/d' $gues_mem 

   else
      cp $DATADIR/analg${VERT}_0001.ctl $anal_mem
      cp $DATADIR/guesg${VERT}_0001.ctl $gues_mem
   fi

   sed -i -e "1 c\\" -e "dset ^../%y4%m2%d2%h2%n200/analg${VERT}/$member.grd" $anal_mem
   sed -i -e "1 c\\" -e "dset ^../%y4%m2%d2%h2%n200/guesg${VERT}/$member.grd" $gues_mem 

done
