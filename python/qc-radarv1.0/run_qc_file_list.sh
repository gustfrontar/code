source activate qc-radar2
export OMP_NUM_THREADS=12
export GOMP_STACKSIZE=512m
export OMP_STACKSIZE=512m
ulimit -s unlimited
python  qc_file_list.py
source deactivate 

