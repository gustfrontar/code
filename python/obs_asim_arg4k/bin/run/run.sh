#!/bin/bash 
### HELP
read -r -d '' HELP << EOF
   Use:
      $0 < exp name > < main dir > 
EOF
: ${1:?"$HELP"} 

### INPUT PARAMETER
EXPNAME=$1
MAINDIR=$2

### SET MAINDIR
[[ ! -z $MAINDIR ]] && cd $MAINDIR/$EXPNAME/bin/run

### START JOB
echo "----------------------------------------------"
echo "[$(date '+%Y-%m-%d %H:%M')] START JOB $EXPNAME"
echo "----------------------------------------------"

### RUN SETUP
echo ""
./run_setup.sh 
exit_code=$?
[[ $exit_code -eq 1 ]] && exit $exit_code

### RUN PROCESS
echo ""
./run_process.sh
exit_code=$?
[[ $exit_code -eq 1 ]] && exit $exit_code

echo -e "\n-----------------------------------------------"
echo "[$(date '+%Y-%m-%d %H:%M')] FINISH JOB $EXPNAME"
echo "----------------------------------------------------"
