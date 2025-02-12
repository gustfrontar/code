#!/bin/bash
#source /home/aarruti/.bash_profile
#source /home/aarruti/.bashrc

LOGPATH=./logmv


SCRIPT=./run_vet.py

ulimit -s unlimited

export OMP_NUM_THREADS=12

radar="RMA1"                #Radar
array1=("20190210")         #Fecha
array2=("20190210_100749")  #Fecha y hora de inicio
array3=("20190210_154621")  #Fecha y hora de fin


length=${#array1[@]}


for ((i=0;i<$length;i++)); do
        export radar=$radar
        export conf_experiment="vet"
        export date=${array1[$i]}
        export date_start=${array2[$i]}
        export date_end=${array3[$i]}
        time  python -u  $SCRIPT  #2>&1 > ${LOGPATH}/vet_RMA1_${i}_${conf_experiment}.log
done



