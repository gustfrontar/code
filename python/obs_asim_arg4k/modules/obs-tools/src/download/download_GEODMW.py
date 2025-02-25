import sys, os, s3fs, glob
sys.path.append(os.environ['EXPDIR'])
import catalog_sources as src
import util as ut

import numpy as np
import pandas as pd
from multiprocessing.dummy import Pool

def get_file(fs, file_):
   fs.get(file_, file_.split('/')[-1])


def main(args):

   # Download date
   ana_date = ut.parse_date(args)
   # Analysis window (considering slots)
   ini, end = ut.get_awin_dates(ana_date)
   pd_ini = pd.to_datetime(ini).floor('1h')
   pd_end = pd.to_datetime(end).floor('1h')
   dates = pd.date_range(pd_ini, pd_end, freq = 'h')

   # Output directory
   pathout = '{}/GEODMW'.format(os.environ['REPODIR'])
   os.makedirs(pathout, exist_ok = True)
   os.chdir(pathout)

   # Use the anonymous credentials to access public data
   fs = s3fs.S3FileSystem(anon = True)

   data_folders = ['ABI-L2-DMWF', 'ABI-L2-DMWVF']
   #ABI-L2-DMWF - Advanced Baseline Imager Level 2 Derived Motion Winds Full Disk
   #ABI-L2-DMWVF - L2+ Derived Motion Winds - Vapor Full Disk (clear sky)

   files = []
   for win_date in dates:
      for folder in data_folders:
          data_path = f'noaa-goes16/{folder}/{win_date:%Y/%j/%H}/'
          if fs.exists(data_path): # newer versions of s3fs raise an exception if path doesn't exists
              # List contents of GOES-16 FULLDISK data of DMW
              files += fs.ls(data_path)

   files_in_repo = glob.glob('OR_ABI-L2-LV*')
   files_in_repo = [filename for filename in files_in_repo if os.stat(filename).st_size != 0] # Remove empty files in repo
   files2download = [filename for filename in files if (filename.split('/')[-1] not in files_in_repo)]

   if files2download:
       DOWNPROC = int(os.environ['DOWNPROC'])
       # Download files, and rename it the same name (without the directory structure)
       args = [(fs, filename) for filename in files2download]
       with Pool(min(DOWNPROC, len(args))) as pool:
          pool.starmap(get_file, args)


if __name__ == '__main__':

   ut.set_walltime(src.GEODMW['WALLTIME'])
   try:
      main(sys.argv[1:])
   except TimeoutError as e:
      print(e, file = sys.stderr)
      sys.exit(ut.EC_ERROR)


