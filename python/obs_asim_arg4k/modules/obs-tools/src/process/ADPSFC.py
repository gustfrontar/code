# -*- coding: utf-8 -*-
import sys, os
import util as ut
import glob, re, time
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
import multiprocessing as mp
sys.path.append(f'{os.environ["UTILSDIR"]}/py-lib')
import common
ENVVARS = common.load_config_exp()

def get_files(path, ini, end, base_filename = 'asm_synop'):

   # Load files in pd.DataFrame
   files = sorted(glob.glob('{}/{}_*'.format(path, base_filename)))
   res = pd.DataFrame(files, columns=['Path'])

   # Get start dates
   sdates = []
   for filename in res['Path']:
      sdates.append(datetime.strptime(re.search(r'\d{12}', filename).group(), '%Y%m%d%H%M'))
   res['StartDate'] = sdates

   # Drop out of time interval
   res.drop(res[(res.StartDate > end) | (res.StartDate < ini)].index, axis=0, inplace=True)
   res.reset_index(drop=True, inplace=True)

   return res

def read_data(filename):

   # Set variables
   column_input = ['Date', 'Time', 'ID', 'Lat', 'Lon', 'Lev', 't2', 'td2', 'psfc', 'slp', 'wdir10', 'wspd10', 'rh2']

   # Read file
   try:
      data = pd.read_csv(filename, sep=',', names=column_input)
   except Exception as err:
      print(err, file = sys.stderr)
      raise RuntimeError('ERROR reading {}'.format(filename))

   # Parse data
   try:
      data['DateTime'] = pd.to_datetime(data['Date'].astype(str) + data['Time'].astype(str), format = '%Y%m%d%H')

      # Wind
      data.wspd10 *= ut.KT_MS
      data['u10'], data['v10'] = ut.calc_wind_components(data['wspd10'], data['wdir10'])

      # RHum: if missing compute fromt T, Td (if possible)
      if data.rh2.isna().any():
         data = ut.calc_relhum_from_dewpoint_ifmissing(data)

      # Standard units
      data.loc[data.Lon < 0., 'Lon'] = data.Lon + 360. 
      data.t2 += ut.K_C
      data.td2 += ut.K_C

   except Exception as err:
      print(err, file = sys.stderr)
      raise RuntimeError('ERROR parsing {}'.format(filename))

   return data 

def proc_filename(filename, source, ini, end, slot, monit_file):

   code_error = 0

   # Read data
   try:
      data = read_data(filename)
   except RuntimeError as err:
      print(err, file = sys.stderr)
      code_error = 1
      return pd.DataFrame(), 0, code_error

   nin = data.shape[0]

   # Filter data outside slot 
   if ENVVARS['DEBUG']: print('', filename)
   tmp = data.shape[0]
   data = ut.filter_time(data, ini, end)
   if data.empty: return pd.DataFrame(), nin, code_error

   # Filter data
   tmp = data.shape[0]
   data = ut.apply_filters(source, data, ['Lon', 'Lat', 'Lev', 'DateTime'])
   if data.empty: return pd.DataFrame(), nin, code_error

   # Write raw data for monitoring
   if monit_file is not None:
      ut.do_monit(data, source, slot, 'REPO', monit_file)

   return data, nin, code_error

def get_data(source, ini, end, files, slot, monit_file):

   OBSPROC = int(ENVVARS['OBSPROC'])

   nin = 0
   code_error = 0
   arg_list = [(filename, source, ini, end, slot, monit_file) for filename in files['Path']]
   with mp.Pool(min(OBSPROC, len(arg_list))) as pool:
      pool_out = pool.starmap(proc_filename, arg_list)

   df_list = []
   for df_file, nin_file, code_error_file in pool_out:
      nin += nin_file
      code_error += code_error_file
      df_list.append(df_file)

   # Concatenate data
   dataout = pd.concat(df_list, ignore_index = True, sort = False)

   return dataout, nin, code_error

