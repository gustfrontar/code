#!/bin/bash
### HELP 
read -r -d '' HELP << EOF
  Use:
     $0 < exp name > < global history directory path >
EOF
: ${1?"$HELP"}

### INPUT PARAMETERS
export EXPNAME=$1
export HISTGDIR=$2

### LOAD CONFIGURATION
ENVCONFIG=../config
[[ ! -f $ENVCONFIG ]] && "[ERROR] File not found: $ENVCONFIG" && exit 1
source $ENVCONFIG
LIBDIR=$MODULESDIR/obs-tools/utils/sh-lib
source $LIBDIR/common_install.sh

### CREATE EXPERIMENT 
EXPDIR=$RUNDIR
create_exp $EXPDIR

### CREATE CONDA ENVIRONMENT 
[[ ! -z $CONDAENVS ]] && create_conda_envs

### LINK FILES IN LIB SUBMODULE
FILE=$LIBDIR/util_obs-tools
NEWF=$LIBDIR/util.sh
[[ -f $NEWF ]] && rm $NEWF
[[ -f $FILE ]] && ln -s $FILE $NEWF


