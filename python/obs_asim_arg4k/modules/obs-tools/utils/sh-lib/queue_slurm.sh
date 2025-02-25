queue () {

   ### SET VARIABLES
   logsdir=$LOGSDIR/${QNAME}
   mkdir -p $logsdir
   log_out=$logsdir/ARR0000.out
   log_err=$logsdir/ARR0000.err
   test $QARRAY && log_out=$logsdir/ARR%4a.out && log_err=$logsdir/ARR%4a.err
   nproc=$(($QPROC*$QTHREADS))			# Total number of processes
   nnode=$(((${nproc}+${ICORE}-1)/${ICORE}))	# Total number of nodes
   ppn=$(((${QPROC}+${nnode}-1)/${nnode}))	# Total number of processes per node

   ### START SLURM SCRIPT
   qfile=${QNAME}.SLURM
   rm -f $qfile
   touch $qfile

   ### SLURM HEADER
   echo "#!/bin/bash" >> $qfile   
   echo "#SBATCH -W" >> $qfile  		# Use blocking queue
   echo "#SBATCH --mail-type=NONE" >> $qfile	# Email job status [default: never]

   echo "#SBATCH -J $QNAME" >> $qfile		# Job name
   echo "#SBATCH -p $QPARTITION" >> $qfile     	# Slurm partition name
   echo "#SBATCH -o $log_out" >> $qfile		# Standard output file
   echo "#SBATCH -e $log_err" >> $qfile		# Standard error file
   echo "#SBATCH -t $QWALLTIME" >> $qfile	# Max. running time

   echo "#SBATCH -N $nnode" >> $qfile 		# Number of nodes
   echo "#SBATCH -n $QPROC" >> $qfile 		# Number of mpi processes
   echo "#SBATCH -c $QTHREADS" >> $qfile	# Number of threads per process
   echo "#SBATCH --ntasks-per-node=$ppn" >> $qfile 		# Number of max. processes per node
   [[ $QEXCLU -eq 1 ]] && echo "#SBATCH --exclusive" >> $qfile	# Use nodes exclusively
   test $QARRAY && echo "#SBATCH -a $QARRAY:1" >> $qfile	# Use Slurm arrays
   echo "" >> $qfile						

   ### SCRIPT HEADER
   echo "export OMP_NUM_THREADS=$QTHREADS" >> $qfile 
   echo "export MPIEXE='/home/opt/intel/compilers_and_libraries_2019.1.144/linux/mpi/intel64/bin/mpirun -bootstrap slurm -n ${QPROC}'" >> $qfile 
   test $QARRAY && echo 'export ARRAYID=${1:-$SLURM_ARRAY_TASK_ID}' >> $qfile 
   test $QARRAY && echo 'export ARRAYCNT=${1:-$SLURM_ARRAY_TASK_COUNT}' >> $qfile 

   echo "MAINDIR=$MAINDIR" >> $qfile
   echo "source $MAINDIR/bin/config" >> $qfile
   echo "source $UTILSDIR/sh-lib/common.sh" >> $qfile
   echo "job_load_config_exp" >> $qfile
   test $SRCDIR && echo "export SRCDIR=$SRCDIR" >> $qfile
   test $UTILSDIR && echo "export UTILSDIR=$UTILSDIR" >> $qfile

   ### SCRIPT BODY
   echo "" >> $qfile
   echo "${QSCRIPTCMD}" >> $qfile 
   echo "" >> $qfile

   ### SUBMIT JOB
   chmod 755 $qfile
   [[ $SUBMIT -eq 1 ]] && /usr/bin/sbatch $qfile || echo "Job scripts created"

}
