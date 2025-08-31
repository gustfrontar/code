#!/bin/bash
#####################################################################
# Run python script in this directory and save output to log folder #
#####################################################################
SCRIPT=$1
NP=$2
LEV=lev
VERT=z
EXP=RMA1_d2_2km_scalebdy_radar_init18_4D_CTRL
FINI=20181110203000

export PYTHONPATH="$PWD/.."
source activate mpi
MPIRUN="$(dirname $(which python3))/mpiexec"

RUNSCRIPT=${SCRIPT}.py
DATADIR="../../../../DATA/EXPS/CORDOBA_20181110_OFP/$EXP/ctl/$FINI"

for mem in {1..60}; do

   member=$(printf "%04d" $mem)

   anal_mem=$DATADIR/fcstg${VERT}_$member.ctl

   $MPIRUN -n $NP python3 $RUNSCRIPT $mem
   #time $MPIRUN -n $NP python3 $RUNSCRIPT 2>&1 > $LOGFILE

   if [[ $mem -eq 1 ]]; then
      mv $DATADIR/fcstg${VERT}.ctl $anal_mem

      sed -i '/tdef/c\tdef  13 linear 20:30Z10Nov2018 5mn' $anal_mem

      sed -i '/^edef/d' $anal_mem
      sed -i '/^options/d' $anal_mem
      sed -i '/^0001/d' $anal_mem

   else
      cp $DATADIR/fcstg${VERT}_0001.ctl $anal_mem
   fi

   sed -i -e "1 c\\" -e "dset ^../../$FINI/fcstg${VERT}/$member.grd" $anal_mem

done
