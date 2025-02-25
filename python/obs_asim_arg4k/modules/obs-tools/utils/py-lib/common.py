import os

########
# LOAD #
########
def load_config_exp(expdir=None):

   import shlex
   from subprocess import Popen, PIPE
   from datetime import datetime

   if expdir is None: expdir = os.environ['EXPDIR']
   maindir = os.environ['MAINDIR']
   files = [f'{expdir}/experiment.conf', f'{expdir}/experiment.dirs', f'{expdir}/experiment.procs']
   
   envvars = dict()
   for filename in files:
      cmd = shlex.split(f"env -i bash -c 'source {filename} && env'")
      proc = Popen(cmd, stdout=PIPE)
      for line in proc.stdout:
         (key, _, value) = line.decode().partition("=")
         envvars[key] = value.partition('\n')[0]
      proc.communicate()

   # Add dates with folder and file format
   DATE = datetime.strptime(f'{envvars["START_TIME"]}{envvars["START_HOUR"]}{envvars["START_MIN"]}{envvars["START_SEC"]}', '%Y/%m/%d%H%M%S')
   envvars['DATEFILE'] = DATE.strftime(os.environ['DATEFILE_fmt'])
   envvars['DATEFOLDER'] = DATE.strftime(os.environ['DATEFOLDER_fmt'])

   return envvars

def load_cipi_table(path=None):

   from pandas import read_csv
   
   if path is None: path = os.environ['UTILSDIR']

   # Load CIPI file
   CIPIS = read_csv(f'{path}/cipi/tables/CIPI.csv')
   TIPO = read_csv(f'{path}/cipi/tables/Tipo.csv')
   PINT = read_csv(f'{path}/cipi/tables/PInteres.csv', encoding = 'latin-1')

   df = PINT.merge(TIPO, on='tipo').merge(CIPIS, on='cipi')
   df = df.set_index(['cipi'])

   return df

########
# DASK #
########
def dask_client_start(workers, memory=None):

   from dask import config
   from dask.distributed import Client, LocalCluster
   import tempfile

   # Set worker directory
   tmpdir = tempfile.mkdtemp()
   config.set({'temporary_directory': tmpdir})

   if 'MALLOC_TRIM_THRESHOLD_' in os.environ:
      config.set({'distributed.nanny.pre-spawn-environ.MALLOC_TRIM_THRESHOLD_': int(os.environ['MALLOC_TRIM_THRESHOLD_'])})

   # Set variables
   if memory is None: memory = f'{80/workers}Gib'

   # Set cluster
   cluster = LocalCluster(n_workers = workers, threads_per_worker = 1, memory_limit = memory)
   client = Client(cluster, direct_to_workers = True)

   return client, cluster


def dask_client_close(client, cluster):

   from dask import config
   from shutil import rmtree

   # Close dask client and cluster
   try:
      client.close()
      cluster.close()
   except:
      pass

   # Remove worker temprorary directory
   tmpdir = config.get('temporary_directory')
   if os.path.isdir(tmpdir): rmtree(tmpdir)

   return
