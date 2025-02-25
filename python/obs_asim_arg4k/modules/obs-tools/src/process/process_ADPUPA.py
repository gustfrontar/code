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
import metpy.calc as mcalc
from metpy.units import units
import multiprocessing as mp

def get_files(path, ini, end):

   # Load files in pd.DataFrame
   files = sorted(glob.glob('{}/asm_temp_*'.format(path)))
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
   column_input = ['Date', 'Time', 'ID', 'Lat', 'Lon', 'Lev', 't', 'td', 'z', 'wdir', 'wspd']

   # Read file
   try:
      data = pd.read_csv(filename, sep=',', names=column_input)
   except Exception as err:
      print(err, file = sys.stderr)
      raise RuntimeError(' ERROR reading {}'.format(filename))

   # Parse data
   try:
      data['DateTime'] = pd.to_datetime(data['Date'].astype(str) + data['Time'].astype(str).str.zfill(6), format = '%d%m%Y%H%M%S')
      data = data.apply(ut.to_numeric)

      # Get variables for each observation point
      data = data.groupby(['DateTime', 'ID', 'Lat', 'Lon', 'Lev']).first().reset_index()

      # RHum
      data['rh'] = mcalc.relative_humidity_from_dewpoint(data.t.values*units.degC, data.td.values*units.degC)

      # Wind
      data.wspd *= ut.KT_MS
      data['u'], data['v'] = ut.calc_wind_components(data['wspd'], data['wdir'])

      # Standard units
      data.loc[data.Lon < 0., 'Lon'] = data.Lon + 360. 
      data.t += ut.K_C
      data.rh *= 100.

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

   return data, nin, code_error

def get_data(source, ini, end, files, slot, monit_file):

   OBSPROC = int(ENVVARS['OBSPROC'])

   nin = 0
   code_error = 0
   arg_list = [(filename, source, ini, end, slot, monit_file) for filename in files['Path']]
   with mp.Pool(min(OBSPROC, len(files))) as pool:
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
      sfiles = files.drop(files[(files.StartDate > send) | (files.StartDate < sini)].index, axis=0, inplace=False)
      sfiles.reset_index(drop=True, inplace=True)
      if sfiles.empty: continue
      #print(sfiles)

      # Get data
      data, nin, exit_code_slot = get_data(source, sini, send, sfiles, slot, monit_file) 
      exit_code += exit_code_slot
      if data.empty: continue

      # Temporal superobbing
      obs_in = data.shape[0]
      gp = data.groupby(['ID', 'Lon', 'Lat', 'Lev']).mean(numeric_only=True)
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

### MAIN SCRIPT ###
def main(args, source):

    OBSTYPE = ENVVARS['OBSTYPE']
    func = eval(f'main_{OBSTYPE}')
    exit_code = func(args, source)

    if exit_code != 0: sys.exit(ut.EC_WARNING)

if __name__ == '__main__':

   print('-----------------------------------')
   print('Hello from {}'.format(os.path.basename(__file__)))
   print('-----------------------------------')
   time = ut.measure_time(main, sys.argv[1:], src.ADPUPA)
   print('Execution Time: {} seconds'.format(time))

