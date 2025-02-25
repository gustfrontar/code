# -*- coding: utf-8 -*-
import sys, os
sys.path.append(f'{os.environ["UTILSDIR"]}/py-lib')
import common
ENVVARS = common.load_config_exp()

sys.path.append(ENVVARS['EXPDIR'])
import catalog_sources as src
import util as ut
from superobbing import so_th

import glob, re, time
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from netCDF4 import Dataset 
import multiprocessing as mp

def get_files(path, ini, end):

   # Load files in pd.DataFrame
   files = sorted(glob.glob('{}/ascat_*'.format(path)))
   res = pd.DataFrame(files, columns=['Path'])

   # Get start and end dates   
   sdates, edates = [], []
   for filename in res['Path']:
      sdates.append(datetime.strptime(re.search(r'\d{8}_\d{6}', filename).group(), '%Y%m%d_%H%M%S'))
      edates.append(sdates[-1] + timedelta(minutes=102))
      
   res['StartDate'] = sdates
   res['EndDate'] = edates

   # Drop out of time interval
   res.drop(res[(res.StartDate > end) | (res.EndDate < ini)].index, axis=0, inplace=True)
   res.reset_index(drop=True, inplace=True)

   return res

def read_data(filename):

   # Set variables
   column_input = ['Lon', 'Lat', 'Time', 'wdir10', 'wspd10', 'QCflag']
   variables = ['lon', 'lat', 'time', 'wind_dir', 'wind_speed', 'wvc_quality_flag']

   # Read file
   try:
      fv, data = dict(), dict()
      ncid = Dataset(filename, 'r')
      for col, var in zip(column_input, variables):

         # Get FillValues
         fv[col] = ncid.variables[var]._FillValue

         # Get data
         data[col] = ncid.variables[var][:].ravel()
         if var == 'time':
            epoch = (' ').join(ncid.variables[var].units.split(' ')[-2:])

      # Close file
      ncid.close()

   except Exception as err:
      print(err, file = sys.stderr)
      raise RuntimeError(' ERROR reading {}'.format(filename))

   # Parse data
   try:
      # Store data in pd.DataFrame
      data = pd.DataFrame.from_dict(data, orient='columns')
      data = data.apply(ut.to_numeric)

      # Replace FillValue with NaN
      data.replace(fv, np.nan, inplace=True)

      # Lev
      data['Lev'] = 10

      # Time
      data['DateTime'] = pd.to_datetime(epoch) + pd.to_timedelta(data.Time, unit='s')

      # Wind
      data['u10'], data['v10'] = ut.calc_wind_components(data['wspd10'], data['wdir10'], conv='ocean')

      # Standard units
      data.loc[data.Lon < 0., 'Lon'] = data.Lon + 360.

   except Exception as err:
      print(err, file = sys.stderr)
      raise RuntimeError(' ERROR parsing {}'.format(filename))

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
   print('', filename)
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

   # Additional filters
   # 1) Bad QC (flag != 0)
   obs_in = data.shape[0]
   data.drop(data[data.QCflag != 0.].index, axis=0, inplace=True)
   if ENVVARS['DEBUG']: print('   Filter QC', obs_in - data.shape[0])
   if data.empty: return pd.DataFrame(), nin, code_error

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


###############
# ASIMILACION #
###############

def main_asim(args, source):

   exit_code = 0

   EXPNAME_OBS = ENVVARS['EXPNAME_OBS']
   REPODIR = os.environ['REPODIR']
   OBSDIR = ENVVARS['H_OBSDIR']
   MODEL = ENVVARS['MODEL']

   # Parse input parameters into date
   ANA_DATE = ut.parse_date(args)
   print('ANALYSIS DATE:', ANA_DATE)

   # Set variables
   column_write = ['Lon', 'Lat', 'Lev'] + source['VARS']
   pathobs = f'{REPODIR}/{source["NAME"]}'
   pathout = f'{OBSDIR}/{ANA_DATE:{os.environ["DATEFOLDER_fmt"]}}/{EXPNAME_OBS}'
   os.makedirs(pathout, exist_ok=True)

   # Set variables for monitoring
   monit_file = None
   if ENVVARS['MONIT']:
      MONITDIR = ENVVARS['H_MONITDIR']
      monit_path = f'{MONITDIR}/{ANA_DATE:{os.environ["DATEFOLDER_fmt"]}}/{EXPNAME_OBS}/'
      os.makedirs(monit_path, exist_ok=True)

      # Create files 
      monit_file = ut.monit_create_csv(source, monit_path, ANA_DATE)

   # Get files in analysis window (considering slots)
   ini, end = ut.get_awin_dates(ANA_DATE) 
   files = get_files(pathobs, ini, end)

   # Get slots to process
   slots = source['SLOTS']
   if not slots:
      slots = ut.get_slots(source['SLOTS'])

   # Process observations for each slot
   for slot in slots:
      slot_date, sini, send = ut.get_slot_dates(slot, ANA_DATE)
      print('')
      print('*** SLOT {}: {} ***'.format(int(slot), slot_date))

      # Get files for current slot
      sfiles = files.drop(files[(files.StartDate > send) | (files.EndDate < sini)].index, axis=0, inplace=False)
      sfiles.reset_index(drop=True, inplace=True)
      if sfiles.empty: continue
      #print(sfiles)

      # Get data
      data, nin, exit_code_slot = get_data(source, sini, send, sfiles, slot, monit_file) 
      exit_code += exit_code_slot
      if data.empty: continue

      # Temporal superobbing
      obs_in = data.shape[0]
      gp = data.groupby(['Lon', 'Lat', 'Lev']).mean(numeric_only=True)
      data = gp.reset_index(inplace=False)
      if ENVVARS['DEBUG']: print('   Filter Temporal SO', obs_in - data.shape[0])
      if data.empty: continue

      print(' File In Out', obs_in, data.shape[0])

      # Apply filters
      obs_in = data.shape[0]
      data = ut.filter_duplicates(data, ['Lon', 'Lat', 'Lev'])
      if ENVVARS['DEBUG']: print('   Filter Duplicates', obs_in - data.shape[0])
      if data.empty: continue

      print(' All Files In Out', nin, data.shape[0])

      # Superobbing
      if source['SO/TH']:
         DOMAIN = eval(ENVVARS['DOMAIN'])
         data = so_th(source, data, [DOMAIN['lat_s'], DOMAIN['lat_n']], [DOMAIN['lon_w'], DOMAIN['lon_e']], [DOMAIN['bottom'], DOMAIN['top']])

      # Write processed data for monitoring
      if monit_file:
         ut.do_monit(data, source, slot, 'PROC', monit_file)

      # Standard data format
      data = ut.standard_data(data, column_write)

      # Write data to LETKF binary format
      fileout = f'{pathout}/{MODEL}_{source["NAME"]}_{slot_date:%Y%m%d%H%M%S}.dat'
      ut.write_letkf_dat(source, slot_date, data, fileout, ENVVARS['MODEL'])

      print('OBS IN OUT: {} {}'.format(nin, data.shape[0]))

   return exit_code

def main(args, source):

    OBSTYPE = ENVVARS['OBSTYPE']
    func = eval(f'main_{OBSTYPE}')
    exit_code = func(args, source)

    if exit_code != 0: sys.exit(ut.EC_WARNING)

if __name__ == '__main__':

   print('-----------------------------------')
   print('Hello from {}'.format(os.path.basename(__file__)))
   print('-----------------------------------')
   time = ut.measure_time(main, sys.argv[1:], src.ASCATW)
   print('Execution Time: {} seconds'.format(time))
