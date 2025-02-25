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

dict_airports = {'SAAR': [-32.90, 299.22, 25],
                 'SABE': [-34.57, 301.58, 6],
                 'SACO': [-31.30, 295.78, 495],
                 'SADF': [-34.45, 301.42, 5],
                 'SANT': [-26.83, 294.88, 450],
                 'SARI': [-25.73, 305.52, 270],
                 'SASA': [-24.85, 294.52, 1221],
                 'SASJ': [-24.38, 294.90, 907],
                 'SAVC': [-45.80, 292.53, 46],
                 'SAVT': [-43.22, 294.72, 43],
                 'SAWE': [-53.78, 292.23, 22],
                 'SAWH': [-54.83, 291.70, 57],
                 'SAZB': [-38.72, 297.83, 83]}

df_latlonlev = pd.DataFrame(dict_airports, index = ['Lat', 'Lon', 'Lev']).T

def get_files(path, ini, end):

   # Load files in pd.DataFrame
   files = sorted(glob.glob('{}/awos_*'.format(path)))
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

   data = pd.read_csv(filename, parse_dates = ['DateTime'])
   data = data.drop('Location', axis = 1)

   #Elimino los datos que no tienen definida la variable.
   data = data.dropna(subset = ['Variable', 'Group_type'])

   data.set_index(['Variable', 'Unit', 'Group_type', 'Group_period', 'DateTime', 'ID'], inplace = True)
   data.sort_index(inplace = True)

   data = data.groupby(data.index.names).mean()

   obs_list = []
   try:
      #Wind (promedio los datos de cabeceras y medio de la pista)
      wind_speed = data.loc[('WIND_SPEED', 'METRES_PER_SECOND', 'MEAN', 'PT2M'), 'Value'].astype(float)
      wind_dir = data.loc[('WIND_DIRECTION', 'DEGREES', 'MEAN', 'PT2M'), 'Value'].astype(float)

      if (not wind_speed.empty) and (not wind_dir.empty):
         Uwind, Vwind = ut.calc_wind_components(wind_speed, wind_dir)
         Uwind.name = 'u10'
         Vwind.name = 'v10'
         obs_list.append(Uwind)
         obs_list.append(Vwind)
   except:
      print(f'El archivo {filename} no tiene variables de viento', file = sys.stderr)


   try:
      #Temp (No tiene posicion)
      Temp = data.loc[('AIR_TEMPERATURE', 'DEGREES_CELSIUS', 'VALUE'), 'Value'].astype(float)
      Temp = Temp + ut.K_C
      Temp.name = 't2'
      Temp.index = Temp.index.droplevel('Group_period')
      obs_list.append(Temp)
   except:
      print(f'El archivo {filename} no tiene variables de temperatura', file = sys.stderr)

   try:
      #Pres 
      Pres = data.loc[('AIR_PRESSURE_QFE', 'HECTO_PASCALS', 'VALUE'), 'Value'].astype(float)
      Pres.name = 'psfc'
      Pres.index = Pres.index.droplevel('Group_period')
      obs_list.append(Pres)
   except:
      print(f'El archivo {filename} no tiene variables de presion', file = sys.stderr)

   try:
      #Rhum 
      RHum = data.loc[('RELATIVE_HUMIDITY', 'PERCENT', 'VALUE'), 'Value'].astype(float)
      RHum.name = 'rh2'
      RHum.index = RHum.index.droplevel('Group_period')
      obs_list.append(RHum)
   except:
      print(f'El archivo {filename} no tiene variables de humedad', file = sys.stderr)

   # Catch error at concatenation if obs_list is empty
   if obs_list:
      data_awos = pd.concat(obs_list, axis = 1)
      data_awos = data_awos.reset_index()
      data_awos = data_awos.join(df_latlonlev, on = 'ID')
   else:
      data_awos = pd.DataFrame()

   return data_awos


def proc_filename(filename, source, ini, end, slot, monit_file):

   code_error = 0

   # Read data
   data = read_data(filename)
   if data.dropna().empty: return pd.DataFrame(), 0, code_error
   nin = data.shape[0]

   # Filter data outside slot 
   if ENVVARS['DEBUG']: print('', filename)
   tmp = data.shape[0]
   data = ut.filter_time(data, ini, end)
   if data.empty: return pd.DataFrame(), nin, code_error

   # Filter data
   data = ut.apply_filters(source, data, ['Lon', 'Lat', 'Lev', 'DateTime'])
   if data.empty: return pd.DataFrame(), nin, code_error

   # Write raw data for monitoring
   if monit_file is not None:
      ut.do_monit(data, source, slot, 'REPO', monit_file)

   if ENVVARS['DEBUG']: print(' File In Out', tmp, data.shape[0])

   data = data.set_index('ID')

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

   dataout = pd.concat(df_list)

   return dataout.reset_index(), nin, code_error


