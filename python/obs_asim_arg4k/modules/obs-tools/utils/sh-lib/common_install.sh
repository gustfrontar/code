#!/bin/bash 

CDIR=$(dirname -- $BASH_SOURCE)
. ${CDIR}/settings
. ${CDIR}/util_build-tars

function get_global_histdir {

   if [[ "$USER" == "prod" ]]; then
      HISTGDIR=$HIST_PROD
   elif [[ "$USER" == "testing" ]]; then
      HISTGDIR=$HIST_TESTING
   elif [[ "$USER" == "desa" ]]; then
      HISTGDIR=$HIST_DESA
   elif [[ ! -z $HISTGDIR ]];then
      HISTGDIR=$HISTGDIR
   else
      HISTGDIR=$HISTDIR
   fi

   echo $HISTGDIR

}

function create_conda_envs {

   [[ $(which conda) != $CONDADIR ]] && eval "$(/home/opt/conda/bin/conda shell.bash hook)"
   IFS=","
   for env in $CONDAENVS
   do

      echo -n "Creating env..."
      if [[ $(${CONDADIR}/conda env list) == *"$env"* ]];then
         echo "skipping ($env already exist)"
      else
         echo ""
         $CONDADIR/conda env create -f $TOOLSDIR/templates/conda_${env}.yml
         echo "done"
      fi

   done
   unset IFS
}

function create_tars {

   TARS=$1

   IFS=","
   for tar in $TARS
   do

      if [[ ${tar^^} == *"WRF"* ]]; then
         FILE=WRF
         OUTDIR=$FCSTDIR 
      elif [[ ${tar^^} == *"WPS"* ]]; then
         FILE=WPS
         OUTDIR=$PREPDIR
      elif [[ ${tar^^} == *"LETKF"* ]]; then
         FILE=LETKF
         OUTDIR=$LETKFDIR 
      else
         echo "[ERROR] Executable not coded" && exit 1
      fi

      # Create tar
      echo -n "Creating tar ${tar}..."
      if  [[ ! -f $TARDIR/${tar}.tar ]]; then
         build_tar_${FILE,,} $tar
         echo "done"
      else
          echo "skipping (already exist)" 
      fi

      # Copy to experiment directory
      mkdir -p $OUTDIR
      cp $TARDIR/${tar}.tar $OUTDIR/${FILE}.tar

   done
   unset IFS

}

function create_exp {

   DIR=$1

   # CREATE EXPERIMENT
   echo -n "Creating exp..."
   [[ -d $DIR ]] && rm -fr $DIR
   mkdir -p $DIR

   # Copy basic configuration files
   cp $CONFIGDIR/exp* $DIR

   # Get global HIST directory
   HISTGDIR=$(get_global_histdir)

   # Create directories
   mkdir -p $DIR/LOGS $DIR/SLURM
   [[ ! -d $HISTGDIR ]] && mkdir -p $HISTGDIR
   [[ -d $HISTDIR ]] && echo "[ERROR] HIST directory exist" && exit 1
   [[ $HISTGDIR != $HISTDIR ]] && ln -s $HISTGDIR $HISTDIR

   # Get realpath of directories
   EXPDIR=$(realpath $EXPDIR)
   HISTGDIR=$(realpath $HISTGDIR)

   # Update experiment directories
   sed -i -e "s|__EXPNAME__|$EXPNAME|g" $DIR/experiment.dirs
   sed -i -e "s|__EXPDIR__|$EXPDIR|g" $DIR/experiment.dirs
   sed -i -e "s|__HISTDIR__|$HISTGDIR|g" $DIR/experiment.dirs

   # Specific actions
   FILE="$MAINDIR/bin/install/build_exp.sh"
   [[ -f $FILE ]] && . $FILE
   echo "done"

}
