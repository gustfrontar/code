#!/bin/bash
#####################################################################
# Run python script in this directory and save output to log folder #
#####################################################################
SCRIPT=$1 
NP=$2

export PYTHONPATH="$PWD/.."
source activate mpi
MPIRUN="$(dirname $(which python3))/mpiexec"

RUNSCRIPT=${SCRIPT}.py
$MPIRUN -n $NP python3 $RUNSCRIPT 
#time $MPIRUN -n $NP python3 $RUNSCRIPT 2>&1 > $LOGFILE

