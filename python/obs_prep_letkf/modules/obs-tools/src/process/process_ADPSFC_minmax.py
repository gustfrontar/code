# -*- coding: utf-8 -*-
import sys, os, glob
from datetime import datetime, timedelta
sys.path += [os.environ['RUNDIR'], f'{os.environ["UTILSDIR"]}/py-lib']
import common
ENVVARS = common.load_config_exp()

import common_obs
import catalog_process as ctlg_process
from ADPSFC import get_files
import pandas as pd

ctlg = common.merge_catalog(ctlg_process.adsfc, 'obs', 'adpsfc')

def read_data(filename):

   #Column names
   names_diario = ['DateTime', 'ID', 'lat', 'lon', 't2x', 't2x 12',
                   't2x absoluta', 't2n 00', 't2n', 't2n absoluta', 'PP', 'Dummy']

   try:
      data = pd.read_csv(filename, sep = ',', header = None, names = names_diario,
                         usecols = [0, 1, 4, 8], na_values = ['-99.0', '99.0', '99', '-99', '#####'],
                         parse_dates = ['DateTime'])
   except:
      print(' ERROR reading {}'.format(filename))
      return None

   try:
      data[['t2x', 't2n']] = data[['t2x', 't2n']] + 273.15
      data['Tipo'] = 'ESTACION'
      data = data.astype({'ID': str})
      data.set_index(['DateTime', 'Tipo', 'ID'], inplace = True)
   except:
      print(' ERROR parsing {}'.format(filename))
      data = None

   return data


def main(args):

   EXPNAME_BASE = ENVVARS['EXPNAME_BASE']
   REPODIR = os.environ['REPODIR']
   OBSDIR = ENVVARS['H_OBSDIR']

   # Parse input parameters into date
   OBS_DATE = common_obs.parse_date(args)

   # Set variables
   pathobs = f'{REPODIR}/{ctlg["name"]}'

   ini = OBS_DATE - timedelta(days = 1)
   end = OBS_DATE
   files = get_files(pathobs, ini, end, 'diario')
   if files.empty: return

   for filename in files['Path']:
      date_file = datetime.strptime(filename, f'{pathobs}/diario_%Y%m%d%H00.lst')

      pathout = f'{OBSDIR}/{date_file:{os.environ["DATEFOLDER_fmt"]}}/{EXPNAME_BASE}'
      os.makedirs(pathout, exist_ok=True)

      fileout = f'{pathout}/{ctlg["name"]}_minmax_{date_file:{os.environ["DATEFILE_fmt"]}}.csv'
      if os.path.exists(fileout):
         continue

      # Read data
      data = read_data(filename)
      if data is None: return pd.DataFrame(), 0
      nin = data.shape[0]

      if date_file.hour == 12:
         data = data['t2n'].dropna()
      else:
         data = data['t2x'].dropna()

      data.to_csv(fileout, index = True)


if __name__ == '__main__':

   print('-----------------------------------')
   print('Hello from {}'.format(os.path.basename(__file__)))
   print('-----------------------------------')
   time = common_obs.measure_time(main, sys.argv[1:])
   print('Execution Time: {} seconds'.format(time))

