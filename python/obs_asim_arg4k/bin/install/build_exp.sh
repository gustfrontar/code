#!/bin/bash

### COPY MAIN FILES
cp $CONFIGDIR/*.py $EXPDIR

### EXPERIMENT TO PROCESS
export EXPNAME_OBS=$(echo ${EXPNAME#*_})
sed -i -e "s|__EXPNAME_OBS__|$EXPNAME_OBS|g" $EXPDIR/experiment.dirs
