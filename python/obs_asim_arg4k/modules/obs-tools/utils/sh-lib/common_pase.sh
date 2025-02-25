#!/bin/bash 

CDIR=$(dirname -- $BASH_SOURCE)
. ${CDIR}/settings
[[ -f ${CDIR}/model ]] && . ${CDIR}/model
. ${CDIR}/common.sh

###########
## PASES ##
###########

function update_config {

   VARS=${@} #Extra variables to edit in experiment.dirs

   job_load_config_exp
   cp $CONFIGDIR/experiment.* $RUNDIR
   cp $CONFIGDIR/*.py $RUNDIR

   file_edit $RUNDIR/experiment.dirs "EXPDIR EXPNAME ${VARS}"

}
