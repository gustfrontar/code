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

def get_files_hour(path, hora):

   files = glob.glob(f'{path}/{hora:%Y/%m/%d/%H}/C*')

   res_tmp = pd.DataFrame(columns = ['Path', 'StartDate', 'EndDate'])
   res_tmp['Path'] = files

   if files:
      # Get start and end dates   
      sdates, edates = [], []
      for n, filename in enumerate(res_tmp['Path']):
         try:
            awos = pd.read_csv(filename, sep = ';', header = 2, usecols = [1], names = ['Date'], parse_dates = ['Date'])
            awos['Date'] = awos['Date'].dt.tz_convert('UTC')
         except:
            print(' ERROR parsing {}'.format(filename), file = sys.stderr)
            continue

         res_tmp['StartDate'].iat[n] = awos['Date'].min()
         res_tmp['EndDate'].iat[n] = awos['Date'].max()

         res_tmp['StartDate'].iat[n] = res_tmp['StartDate'].iat[n].to_pydatetime()
         res_tmp['EndDate'].iat[n] = res_tmp['EndDate'].iat[n].to_pydatetime()

         res_tmp['StartDate'].iat[n] = res_tmp['StartDate'].iat[n].replace(tzinfo = None)
         res_tmp['EndDate'].iat[n] = res_tmp['EndDate'].iat[n].replace(tzinfo = None)

   return res_tmp


def get_files(path, ini, end):

   OBSPROC = int(ENVVARS['OBSPROC'])

   dates = pd.date_range(datetime(ini.year, ini.month, ini.day, ini.hour),
                         datetime(end.year, end.month, end.day, end.hour), freq = 'h')

   arg_list = [(path, hora) for hora in dates]
   with mp.Pool(min(OBSPROC, len(arg_list))) as pool:
      res_list = pool.starmap(get_files_hour, arg_list)

   res = pd.concat(res_list, ignore_index = True)

   res.dropna(subset = ['StartDate', 'EndDate'], inplace = True)

   # Drop out of time interval
   res.drop(res[(res.StartDate > end) | (res.EndDate < ini)].index, axis = 0, inplace = True)
   res.reset_index(drop = True, inplace = True)

   return res

def get_location(df):

    if 'MID' in df['Long_var']:
        df['Location'] = 'MID'
    elif 'RWY' in df['Long_var']:
        df['Location'] = 'RWY'
    else:
        df['Location'] = 'No_Loc'

    return df


def read_data(filename):

   column_input = ['Airport', 'DateTime', 'Long_var', 'Variable', 'Unit', 'Value', '????', 'Group_type', 'Group_period']

   data = pd.read_csv(filename, sep = ';', header = 2, names = column_input, parse_dates = ['DateTime'])

   #La primera linea arranca con un caracter raro, lo elimino y me fijo en todas las filas por las dudas
   data['Airport'] = data['Airport'].apply(lambda x: re.sub('(\ufeff)|(")', '', x))
   data['DateTime'] = data['DateTime'].dt.tz_convert('UTC').dt.tz_localize(None)

   #Elimino los datos que no tienen definida la variable.
   data = data.dropna(subset = ['Long_var', 'Variable', 'Group_type'])

   airport = data['Airport'].unique()[0]

   #Obtengo del Long_var si el dato es de cabecera de la pista o del medio
   data = data.apply(get_location, axis = 1)


   data.set_index(['Variable', 'Unit', 'Group_type', 'Group_period', 'DateTime'], inplace = True)
   data.sort_index(inplace = True)

   try:
      #Wind (promedio los datos de cabeceras y medio de la pista)
      wind_speed = data.loc[('WIND_SPEED', 'METRES_PER_SECOND', 'MEAN', 'PT2M'), 'Value'].astype(float)
      wind_dir = data.loc[('WIND_DIRECTION', 'DEGREES', 'MEAN', 'PT2M'), 'Value'].astype(float)

      if (not wind_speed.empty) and (not wind_dir.empty):
         Uwind, Vwind = ut.calc_wind_components(wind_speed, wind_dir)
         Uwind.name = 'u10'
         Vwind.name = 'v10'
         Uwind = Uwind.groupby('DateTime').mean()
         Vwind = Vwind.groupby('DateTime').mean()
   except:
      Uwind = pd.Series(data = np.nan, index = data.index.get_level_values('DateTime').unique(), name = 'u10')
      Vwind = pd.Series(data = np.nan, index = data.index.get_level_values('DateTime').unique(), name = 'v10')
      print(f'El archivo {filename} no tiene variables de viento', file = sys.stderr)


   try:
      #Temp (No tiene posicion)
      Temp = data.loc[('AIR_TEMPERATURE', 'DEGREES_CELSIUS', 'VALUE'), 'Value'].astype(float)
      Temp = Temp + ut.K_C
      Temp.name = 't2'
      Temp.index = Temp.index.droplevel('Group_period')
      Temp = Temp.groupby('DateTime').mean()
   except:
      Temp = pd.Series(data = np.nan, index = data.index.get_level_values('DateTime').unique(), name = 't2')
      print(f'El archivo {filename} no tiene variables de temperatura', file = sys.stderr)

   try:
      #Pres (promedio los datos de cabeceras y medio de la pista)
      Pres = data.loc[('AIR_PRESSURE_QFE', 'HECTO_PASCALS', 'VALUE'), 'Value'].astype(float)
      Pres.name = 'psfc'
      Pres.index = Pres.index.droplevel('Group_period')
      Pres = Pres.groupby('DateTime').mean()
   except:
      Pres = pd.Series(data = np.nan, index = data.index.get_level_values('DateTime').unique(), name = 'psfc')
      print(f'El archivo {filename} no tiene variables de presion', file = sys.stderr)

   try:
      #Rhum (No tiene posicion)
      RHum = data.loc[('RELATIVE_HUMIDITY', 'PERCENT', 'VALUE'), 'Value'].astype(float)
      RHum.name = 'rh2'
      RHum.index = RHum.index.droplevel('Group_period')
      RHum = RHum.groupby('DateTime').mean()
   except:
      RHum = pd.Series(data = np.nan, index = data.index.get_level_values('DateTime').unique(), name = 'rh2')
      print(f'El archivo {filename} no tiene variables de humedad', file = sys.stderr)

   data_awos = pd.concat([Uwind, Vwind, Temp, Pres, RHum], axis = 1)

   data_awos['ID'] = airport

   data_awos = data_awos.reset_index()

   data_awos = data_awos.join(df_latlonlev, on = 'ID')

   return data_awos


def proc_filename(filename, source, ini, end, slot, monit_file):

   # Read data
   data = read_data(filename)
   if data.dropna().empty: return pd.DataFrame(), 0
   nin = data.shape[0]

   # Filter data outside slot 
   if ENVVARS['DEBUG']: print('', filename)
   tmp = data.shape[0]
   data = ut.filter_time(data, ini, end)
   if data.empty: return pd.DataFrame(), nin

   # Filter data
   data = ut.apply_filters(source, data, ['Lon', 'Lat', 'Lev', 'DateTime'])
   if data.empty: return pd.DataFrame(), nin

   # Write raw data for monitoring
   if monit_file is not None:
      ut.do_monit(data, source, slot, 'REPO', monit_file)

   if ENVVARS['DEBUG']: print(' File In Out', tmp, data.shape[0])

   data = data.set_index('ID')

   return data, nin

def get_data(source, ini, end, files, slot, monit_file):

   OBSPROC = int(ENVVARS['OBSPROC'])

   nin = 0
   arg_list = [(filename, source, ini, end, slot, monit_file) for filename in files['Path']]
   with mp.Pool(min(OBSPROC, len(arg_list))) as pool:
      pool_out = pool.starmap(proc_filename, arg_list)

   df_list = []
   for df_file, nin_file in pool_out:
      nin += nin_file
      df_list.append(df_file)

   dataout = pd.concat(df_list)

   return dataout.reset_index(), nin


