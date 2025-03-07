import os

########
# LOAD #
########
def load_config_exp(configdir=None):

   import shlex
   from subprocess import Popen, PIPE
   from datetime import datetime

   if configdir is None: configdir = os.environ['CONFIGDIR']
   maindir = os.environ['MAINDIR']
   files = [f'{configdir}/experiment.conf', f'{configdir}/experiment.dirs', f'{configdir}/experiment.procs']
   
   envvars = dict()
   for filename in files:
      cmd = shlex.split(f"env -i bash -c 'source {filename} && env'")
      #print( cmd )
      proc = Popen(cmd, stdout=PIPE)
      for line in proc.stdout:
         (key, _, value) = line.decode().partition("=")
         envvars[key] = value.partition('\n')[0]
      proc.communicate()

   # Add dates with folder and file format
   print( os.environ['CDATE'] )
   DATE = datetime.strptime(os.environ['CDATE'],'%Y-%m-%d %H:%M:%S')
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
def dask_client_start(workers, memory=None, threads = 1):

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
   cluster = LocalCluster(n_workers = workers, threads_per_worker = threads, memory_limit = memory)
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


############
# CATALOGS #
############

def merge_catalog(ctlg, ctlg_type, var_name):

   from importlib import import_module

   ctlg_common = import_module(f'catalogs.catalog_{ctlg_type}')

   return ctlg | getattr(ctlg_common, var_name)
