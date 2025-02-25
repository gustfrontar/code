#!/bin/bash

### LOAD CONFIGURATION
MAINDIR='../'
ENVCONFIG=../bin/config
[[ ! -f $ENVCONFIG ]] && "[ERROR] File not found: $ENVCONFIG" && exit 1
source $ENVCONFIG
source $MODULESDIR/obs-tools/lib/common.sh
job_load_config "$EXPCONF"

### REPLACE FILES IN RUNs DIRECTORY
rm $RUNDIR/experiment.queue
cp $CONFIGDIR/experiment.procs $RUNDIR

### UPDATE TIME VARIABLES IN EXPERIMENT FILES
VARS="START_TIME START_HOUR START_MIN START_SEC"
file_replace_exp $RUNDIR/experiment.conf $CONFIGDIR/experiment.conf $VARS


### RENAME EXPERIMENT FOLDER
source $MAINDIR/RUNs/experiment.dirs
EXPDIR=$(echo $EXPDIR | sed -e 's/asimilacion/SAP.SMN-ANA/')
LOGSDIR=$(echo $LOGSDIR | sed -e 's/asimilacion/SAP.SMN-ANA/')
SLURMDIR=$(echo $SLURMDIR | sed -e 's/asimilacion/SAP.SMN-ANA/')

VARS="EXPDIR LOGSDIR SLURMDIR"
file_edit $MAINDIR/RUNs/experiment.dirs $VARS

ASIMDIR=$(readlink -f ${MAINDIR}/..)
NEW_ASIMDIR=$(echo $ASIMDIR | sed -e 's/asimilacion/SAP.SMN-ANA/')

mv $ASIMDIR $NEW_ASIMDIR
cd $NEW_ASIMDIR/obs_asim_arg4k/pases
