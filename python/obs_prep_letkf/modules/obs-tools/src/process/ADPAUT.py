# -*- coding: utf-8 -*-
import sys, os, glob, re
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
import multiprocessing as mp
sys.path.append(f'{os.environ["UTILSDIR"]}/py-lib')
import common
import common_obs
ENVVARS = common.load_config_exp()

def get_files(path, ini, end):

   # Load files in pd.DataFrame
   files = sorted(glob.glob('{}/prop*'.format(path)))
   res = pd.DataFrame(files, columns=['Path'])

   # Get start and end dates   
   sdates, edates = [], []
   for filename in res['Path']:
      sdates.append(datetime.strptime(re.search(r's\d{14}', filename).group(), 's%Y%m%d%H%M%S'))
      edates.append(datetime.strptime(re.search(r'e\d{14}', filename).group(), 'e%Y%m%d%H%M%S'))
   res['StartDate'] = sdates
   res['EndDate'] = edates

   # Drop out of time interval
   res.drop(res[(res.StartDate > end) | (res.EndDate < ini)].index, axis=0, inplace=True)
   res.reset_index(drop=True, inplace=True)

   return res

def read_data(filename):

   # Set variables
   column_keep = ['idEstacion', 'idPropietario', 'fechaHora', 'temperatura', 'humedad', 'presion', 'dirViento', 'velViento']
   column_input = ['ID', 'Prop', 'DateTime', 't2', 'rh2', 'psfc', 'wdir10', 'wspd10']
   column_stations = ['ID', 'Prop', 'Name', 'Lat', 'Lon', 'Lev', 'City', 'IDCity', 'Country', 'State', 'Running', 'Period', 'Brand', 'Model', 'TimeZone', 'Lev_ori', 'QC_wspd10', 'QC_t2', 'QC_psfc', 'QC_rh2']
   column_owners = ['Prop', 'Prop_Name']

   # Read variables and satations files
   try:
      data = pd.read_json(filename)
   except Exception as err:
      print(err, file = sys.stderr)
      raise RuntimeError(' ERROR Reading {}'.format(filename))

   if data.empty: 
      print(' WARNING: Empty file {}'.format(filename), file = sys.stderr)
      return data

   filename_stations = '{}/surface/stations.QC'.format(os.environ['QCDIR'])
   stations = pd.read_json(filename_stations)
   if stations.empty: 
      raise Exception(' WARNING: Empty file {}'.format(filename_stations))

   filename_owners = '{}/owners.json'.format(os.path.dirname(filename))
   owners = pd.read_json(filename_owners)
   if owners.empty: 
      raise Exception(' WARNING: Empty file {}'.format(filename_owners))

   # Parse data
   try:
      data.drop(data.columns.difference(column_keep), axis=1, inplace=True)
      data.reset_index(drop=True, inplace=True)
      data.rename(columns=dict(zip(column_keep, column_input)), inplace=True)

      # Stations
      stations.columns = column_stations 

      # Owners
      owners.columns = column_owners

      # Merge pd.DataFrames 
      data = data.merge(stations, how='inner', on=['ID', 'Prop'])  
      data = data.merge(owners, how = 'inner', on = ['Prop'])

      # Time
      data['DateTime'] = pd.to_datetime(data.DateTime, format='%Y-%m-%dT%H:%M:%S')

      # Wind
      if np.isin(['wspd10', 'wdir10'], data.columns).all(): # There are stations without wind observations
         data.wspd10 /= common_obs.KMH_MS
         data['u10'], data['v10'] = common_obs.calc_wind_components(data['wspd10'], data['wdir10'])

      if 't2' in data.columns:
         data.t2 += common_obs.K_C

      # Standard units
      data.loc[data.Lon < 0., 'Lon'] = data.Lon + 360. 

   except Exception as err:
      print(err, file = sys.stderr)
      raise RuntimeError(' ERROR parsing {}'.format(filename))

   return data 


def proc_filename(filename, ctlg, ini, end, slot, monit_file):

   code_error = 0

   if os.stat(filename).st_size == 0:
        return pd.DataFrame(), 0

   # Read data
   try:
      data = read_data(filename)
   except RuntimeError as err:
      print(err, file = sys.stderr)
      code_error = 1
      return pd.DataFrame(), 0, code_error

   if data.empty: return pd.DataFrame(), 0, code_error
   nin = data.shape[0]

   # Filter data outside slot 
   if ENVVARS['DEBUG']: print('', filename)
   data = common_obs.filter_time(data, ini, end)
   if data.empty: return pd.DataFrame(), nin, code_error

   # Filter data
   tmp = data.shape[0]
   data = common_obs.apply_filters(ctlg, data, ['Lon', 'Lat', 'Lev', 'DateTime'])
   if data.empty: return pd.DataFrame(), nin, code_error

   # Write raw data for monitoring
   if monit_file is not None:
      common_obs.do_monit(data, ctlg, slot, 'REPO', monit_file, data['Prop_Name'].unique()[0])

   # Additional filters
   # 1) QC
   common_cols = [x for x in ctlg['vars'] if x in data.columns]
   obs_in = data.dropna(subset = common_cols, how = 'all').shape[0]
   for var, thrs in ctlg['constraints']['qc_levels'].items():
      index_QC = data.where(~data[f'QC_{var}'].isin(thrs)).dropna(subset=[f'QC_{var}']).index
      if var == 'wspd10':
         data.loc[index_QC, ['u10', 'v10']] = np.nan
      else:
         data.loc[index_QC, var] = np.nan

   obs_out = data.dropna(subset = common_cols, how = 'all').shape[0]
   if ENVVARS['DEBUG']: print('   Filter QC', obs_in - obs_out)
   if obs_out == 0: return pd.DataFrame(), nin, code_error

   return data, nin, code_error


def get_data(ctlg, ini, end, files, slot, monit_file):

   nin = 0
   code_error = 0
   arg_list = [(filename, ctlg, ini, end, slot, monit_file) for filename in files['Path']]
   with mp.Pool(min(ctlg['procs'], len(files))) as pool:
      pool_out = pool.starmap(proc_filename, arg_list)

   df_list = []
   for df_file, nin_file, code_error_file in pool_out:
      nin += nin_file
      code_error += code_error_file
      df_list.append(df_file)

   # Concatenate data
   dataout = pd.concat(df_list, ignore_index = True, sort = False)

   return dataout, nin, code_error

if __name__ == '__main__':

    pass
