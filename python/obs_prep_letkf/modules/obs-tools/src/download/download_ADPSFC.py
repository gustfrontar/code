import sys, os
sys.path += [os.environ['RUNDIR'], f'{os.environ["UTILSDIR"]}/py-lib']
import catalog_download as ctlg_download
import common_obs
import common

import glob, re, shutil
import pandas as pd
from datetime import datetime, timedelta

ctlg = common.merge_catalog(ctlg_download.adpsfc, 'obs', 'adpsfc')

def get_files(path, ini, end, base_filename = 'asm_synop'):

   # Load files in pd.DataFrame
   files = sorted(glob.glob('{}/{}_*'.format(path, base_filename)))
   res = pd.DataFrame(files, columns=['Path'])

   # Get start and end dates   
   dates = []
   for filename in res.Path:
      dates.append(datetime.strptime(re.search(r'\d{12}', filename).group(), '%Y%m%d%H%M'))
   res['StartDate'] = dates

   # Drop out of time interval
   res.drop(res[(res.StartDate > end) | (res.StartDate < ini)].index, axis=0, inplace=True)
   res.reset_index(drop=True, inplace=True)

   return res


def main(args):
  
   # Parse input parameters into date
   path = f'{os.environ["OBSDIRIN"]}/desarrollo'
   ana_date = common_obs.parse_date(args)

   # Analysis window (considering slots)
   ini, end = common_obs.get_awin_dates(ana_date)
   #ini -= timedelta(minutes=st.OBSFREC/2.)
   #end += timedelta(minutes=st.OBSFREC/2.)

   # Output directory
   pathout = '{}/{}'.format(os.environ['REPODIR'], ctlg['name'])
   os.makedirs(pathout, exist_ok=True)

   # Get files
   files = get_files(path, ini, end)

   # Copy files
   for f in files['Path']: 
      shutil.copy(f, pathout)

   # Copy minmax files for calib
   ini = ana_date - timedelta(days = 1)
   end = ana_date
   files = get_files(path, ini, end, 'diario')
   for f in files['Path']:
      shutil.copy(f, pathout)


if __name__ == '__main__':

   common_obs.set_walltime(ctlg['walltime'])
   try:
      main(sys.argv[1:])
   except TimeoutError as e:
      print(e, file = sys.stderr)
      sys.exit(common_obs.EC_ERROR)
 
