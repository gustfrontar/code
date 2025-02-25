import sys, os
sys.path.append(os.environ['EXPDIR'])
import catalog_sources as src
import util as ut

import glob, re, shutil
import numpy as np
import pandas as pd
from datetime import datetime, timedelta

def get_files(path, ini, end):

   # Load files in pd.DataFrame
   files = sorted(glob.glob('{}/asm_temp_*'.format(path)))
   res = pd.DataFrame(files, columns=['Path'])

   # Get start and end dates   
   dates = []
   for filename in res.Path:
      try:
         dates.append(datetime.strptime(re.search(r'\d{12}', filename).group(), '%Y%m%d%H%M'))
      except:
         dates.append(end + timedelta(seconds=60))
   res['StartDate'] = dates

   # Drop out of time interval
   res.drop(res[(res.StartDate > end) | (res.StartDate < ini)].index, axis=0, inplace=True)
   res.reset_index(drop=True, inplace=True)

   return res

def main(args):

   # Parse input parameters into date
   path = f'{os.environ["OBSDIRIN"]}/desarrollo'
   ana_date = ut.parse_date(args)

   # Analysis window (considering slots)
   ini, end = ut.get_awin_dates(ana_date)
   #ini -= timedelta(minutes=st.OBSFREC/2.)
   #end += timedelta(minutes=st.OBSFREC/2.)

   # Output directory
   pathout = '{}/{}'.format(os.environ['REPODIR'], src.ADPUPA['NAME'])
   os.makedirs(pathout, exist_ok=True)

   # Get files
   files = get_files(path, ini, end)

   # Copy files
   for f in files['Path']: 
      shutil.copy(f, pathout)

if __name__ == '__main__':

   ut.set_walltime(src.ADPUPA['WALLTIME'])
   try:
      main(sys.argv[1:])
   except TimeoutError as e:
      print(e, file = sys.stderr)
      sys.exit(ut.EC_ERROR)

