import sys, os
sys.path.append(os.environ['EXPDIR'])
import catalog_sources as src
import util as ut

import glob, re, shutil
import pandas as pd
from datetime import datetime, timedelta

def get_files(path, ini, end):

   # Load files in pd.DataFrame
   files = sorted(glob.glob('{}/cfrad.s*.nc'.format(path)))
   res = pd.DataFrame(files, columns=['Path'])

   # Get start and end dates   
   sdates, edates = [], []
   for filename in res.Path:
      sdates.append(datetime.strptime(re.search(r's\d{8}_\d{6}', filename).group()[1:], '%Y%m%d_%H%M%S'))
      edates.append(datetime.strptime(re.search(r'e\d{8}_\d{6}', filename).group()[1:], '%Y%m%d_%H%M%S'))

   res['StartDate'] = sdates
   res['EndDate'] = edates

   # Drop out of time interval
   res.drop(res[(res.StartDate > end) | (res.EndDate < ini)].index, axis=0, inplace=True)
   res.reset_index(drop=True, inplace=True)

   return res


def main(args):

   # Parse input parameters into date
   path = f'{os.environ["OBSDIRIN"]}/qc-radar/asimilacion'
   ana_date = ut.parse_date(args)

   # Analysis window (considering slots)
   ini, end = ut.get_awin_dates(ana_date)
   #ini -= timedelta(minutes=st.OBSFREC/2.)
   #end += timedelta(minutes=st.OBSFREC/2.)

   # Output directory
   pathout = '{}/{}'.format(os.environ['REPODIR'], src.RADARC['NAME'])
   os.makedirs(pathout, exist_ok=True)

   # Get files
   files = get_files(path, ini, end)

   # Copy files
   for f in files['Path']: 
      shutil.copy(f, pathout)

if __name__ == '__main__':

   ut.set_walltime(src.RADARC['WALLTIME'])
   try:
      main(sys.argv[1:])
   except TimeoutError as e:
      print(e, file = sys.stderr)
      sys.exit(ut.EC_ERROR)

