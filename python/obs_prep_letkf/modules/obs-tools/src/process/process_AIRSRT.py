# -*- coding: utf-8 -*-
import sys, os, glob, re, time
from datetime import datetime, timedelta
sys.path += [os.environ['RUNDIR'], f'{os.environ["UTILSDIR"]}/py-lib']
import common
ENVVARS = common.load_config_exp()

import common_obs
import catalog_process as ctlg_process
from superobbing import so_th

import numpy as np
import pandas as pd
from netCDF4 import Dataset
import leapsec93 as ls93
from collections import defaultdict
import multiprocessing as mp

ctlg = common.merge_catalog(ctlg_process.airsrt, 'obs', 'airsrt')

def get_files(path, ini, end):

   # Load files in pd.DataFrame
   files = sorted(glob.glob('{}/AIRS.*v6*'.format(path)))
   res = pd.DataFrame(files, columns=['Path'])

   # Get start and end dates   
   sdates, edates = [], []
   for filename in res['Path']:
      (year, month, day, granule) = re.search(r'\d{4}.\d{2}.\d{2}.\d{3}', filename).group().split('.')
      sdate = datetime(int(year), int(month), int(day)) + timedelta(minutes=int(granule)*6)
      sdates.append(sdate - ls93.dTAI_UTC_from_utc(sdate)) 
      edates.append(sdate + timedelta(minutes=6))
   res['StartDate'] = sdates
   res['EndDate'] = edates

   # Drop out of time interval
   res.drop(res[(res.StartDate > end) | (res.EndDate < ini)].index, axis=0, inplace=True)
   res.reset_index(drop=True, inplace=True)

   return res

def reshape_data(data, shape):

   (ndim1, ndim2, ndim3) = shape

   if data.ndim == 3:
      res = data
   elif data.ndim == 2:
      res = np.repeat(data[:,:,np.newaxis], ndim3, axis=2)
   elif data.ndim == 1 and data.size == ndim3:
      res = np.repeat(np.repeat(data[np.newaxis,:], ndim2, axis=0)[np.newaxis,:,:], ndim1, axis=0)
   else:
      #print('Option not available')
      res = None

   return res.ravel()

def read_data(filename, ctlg):

   # Read data
   try:
      # Variables 
      variables = ['Longitude', 'Latitude', 'Time', 'PSurfStd', 'TAirStd', 'TAirStdErr', 'StdPressureLev:L2_Standard_atmospheric&surface_product', 'TAirStd_QC', 'H2OMMRStd', 'H2OMMRStdErr', 'H2OPressureLay:L2_Standard_atmospheric&surface_product', 'H2OMMRStd_QC'] 
      keys = ['Lon', 'Lat', 'DateTime', 'psfc', 't', 'tErr', 'tLev', 'tQC', 'q', 'qErr', 'qLev', 'qQC']
      
      # Open file
      ncid = Dataset(filename)

      # Get data 
      fv, data = dict(), dict()
      for key, var in zip(keys, variables):
  
         # Get FillValues
         fv[key] = ncid.variables[var][:].fill_value

         # Get data
         data[key] = ncid.variables[var][:].data
         if var == 'Time':
            data[key] = common_obs.tai2utc(data[key])

#         # Convert to little endian
#         if data[key].dtype.byteorder == '>':
#            #data[key] = data[key].byteswap().newbyteorder()
#            data[key] = data[key].view(data[key].dtype.newbyteorder('<'))
            
      # Close file
      ncid.close()

   except Exception as err:
      print(err, file = sys.stderr)
      raise RuntimeError(' ERROR reading {}'.format(filename))
#      return None

   # Parse data
   try:
      # Loop over variables
      varfv, vardata = defaultdict(dict), defaultdict(dict)
      for var in ctlg['vars']:
         varfv[var] = dict()
         vardata[var] = dict()
         keys = ['Lon', 'Lat', 'Lev', 'DateTime', var, '{}Err'.format(var), 'QC', 'psfc']

         # Reshape data to 3D array
         for key in keys:
            tmpkey = key 
            if key == 'Lev' or key == 'QC':
               tmpkey = '{}{}'.format(var, key)
            varfv[var][key] = fv[tmpkey]
            vardata[var][key] = reshape_data(data[tmpkey], data[var].shape)

         # Store data in pd.DataFrame
         vardata[var] = pd.DataFrame.from_dict(vardata[var], orient='columns')

         # Replace FillValue with NaN
         vardata[var].replace(varfv[var], np.nan, inplace=True)

      # Store data in pd.DataFrame
      data = pd.concat([vardata['t'], vardata['q']], join='outer', axis=0, ignore_index=True, sort=False)

      # Standard units
      data.loc[data.Lon < 0., 'Lon'] = data.Lon + 360.
      data.q *= common_obs.GKG_KGKG
      data.qErr *= common_obs.GKG_KGKG

   except Exception as err:
      print(err, file = sys.stderr)
      raise RuntimeError(' ERROR parsing {}'.format(filename))
#      data = None

   return data 

def proc_filename(filename, ctlg, ini, end, slot, monit_file):

   code_error = 0

   # Read data
   try:
      data = read_data(filename, ctlg)
   except RuntimeError as err:
      print(err, file = sys.stderr)
      code_error = 1
      return pd.DataFrame(), 0, code_error

   nin = data.shape[0]

   # Filter data outside slot 
   print('', filename)
   tmp = data.shape[0]
   data = common_obs.filter_time(data, ini, end)
   if data.empty: return pd.DataFrame(), nin, code_error

   # Filter data
   print('', filename)
   tmp = data.shape[0]
   data = common_obs.apply_filters(ctlg, data, ['Lon', 'Lat', 'Lev', 'DateTime'])
   if data.empty: return pd.DataFrame(), nin, code_error

   # Write raw data for monitoring
   if monit_file is not None:
      common_obs.do_monit(data, ctlg, slot, 'REPO', monit_file)

   # Additional filters
   # 1) QC
   obs_in = data.shape[0]
   data.drop(data[data.QC != 0].index, axis=0, inplace=True)
   if ENVVARS['DEBUG']: print('   Filter QC', obs_in - data.shape[0])
   if data.empty: return pd.DataFrame(), nin, code_error
   # 2) Below surface 
   obs_in = data.shape[0]
   data.drop(data[data.Lev > data.psfc].index, axis=0, inplace=True)
   if ENVVARS['DEBUG']: print('   Filter Below Surface', obs_in - data.shape[0])
   if data.empty: return pd.DataFrame(), nin, code_error
   # 2) Above pressure level
   obs_in = data.shape[0]
   data.drop(data[data.Lev < ctlg['constraints']['max_lev']].index, axis=0, inplace=True)
   if ENVVARS['DEBUG']: print('   Filter Above Level', obs_in - data.shape[0])
   if data.empty: return pd.DataFrame(), nin, code_error

   return data, nin, code_error

def get_data(ctlg, ini, end, files, slot, monit_file):

   nin = 0
   code_error = 0
   arg_list = [(filename, ctlg, ini, end, slot, monit_file) for filename in files['Path']]
   with mp.Pool(min(ctlg['procs'], len(arg_list))) as pool:
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

def main_asim(args):

   exit_code = 0

   EXPNAME_BASE = ENVVARS['EXPNAME_BASE']
   REPODIR = os.environ['REPODIR']
   OBSDIR = ENVVARS['H_OBSDIR']
   MODEL = ENVVARS['MODEL']

   # Parse input parameters into date
   ANA_DATE = common_obs.parse_date(args)
   print('ANALYSIS DATE:', ANA_DATE)

   # Set variables
   column_write = ['Lon', 'Lat', 'Lev'] + ctlg['vars'] + [s + 'Err' for s in ctlg['vars']]
   pathobs = f'{REPODIR}/{ctlg["name"]}'
   pathout = f'{OBSDIR}/{ANA_DATE:{os.environ["DATEFOLDER_fmt"]}}/{EXPNAME_BASE}'
   os.makedirs(pathout, exist_ok=True)

   # Set variables for monitoring
   monit_file = None
   if ENVVARS['MONIT']:
      MONITDIR = ENVVARS['H_MONITDIR']
      monit_path = f'{MONITDIR}/{ANA_DATE:{os.environ["DATEFOLDER_fmt"]}}/{EXPNAME_BASE}/'
      os.makedirs(monit_path, exist_ok=True)

      # Create files 
      monit_file = common_obs.monit_create_csv(ctlg, monit_path, ANA_DATE)

   # Get files in analysis window (considering slots)
   ini, end = common_obs.get_awin_dates(ANA_DATE)
   files = get_files(pathobs, ini, end)

   # Get slots to process
   slots = ctlg['slots']
   if not slots:
      slots = common_obs.get_slots(ctlg['slots'])

   # Process observations for each slot
   for slot in slots:
      slot_date, sini, send = common_obs.get_slot_dates(slot, ANA_DATE)
      print('')
      print('*** SLOT {}: {} ***'.format(int(slot), slot_date))

      # Get files for current slot
      sfiles = files.drop(files[(files.StartDate > send) | (files.EndDate < sini)].index, axis=0, inplace=False)
      sfiles.reset_index(drop=True, inplace=True)
      if sfiles.empty: continue
      #print(sfiles)

      # Get data
      data, nin, exit_code_slot = get_data(ctlg, sini, send, sfiles, slot, monit_file) 
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
      data = common_obs.filter_duplicates(data, ['Lon', 'Lat', 'Lev'])
      if ENVVARS['DEBUG']: print('   Filter Duplicates', obs_in - data.shape[0])
      if data.empty: continue

      print(' All Files In Out', nin, data.shape[0])

      # Superobbing
      if ctlg['so/th']:
         DOMAIN = eval(ENVVARS['DOMAIN'])
         data = so_th(ctlg, data, [DOMAIN['lat_s'], DOMAIN['lat_n']], [DOMAIN['lon_w'], DOMAIN['lon_e']], [DOMAIN['bottom'], DOMAIN['top']])

      # Write processed data for monitoring
      if monit_file:
         common_obs.do_monit(data, ctlg, slot, 'PROC', monit_file)

      # Standard data format
      data = common_obs.standard_data(data, column_write)

      # Write data to LETKF binary format
      fileout = f'{pathout}/{MODEL}_{ctlg["name"]}_{slot_date:%Y%m%d%H%M%S}.dat'
      common_obs.write_letkf_dat(ctlg, slot_date, data, fileout, ENVVARS['MODEL'])

      print('OBS IN OUT: {} {}'.format(nin, data.shape[0]))

   return exit_code


def main(args):

    OBSTYPE = ENVVARS['OBSTYPE']
    func = eval(f'main_{OBSTYPE}')
    exit_code = func(args)

    if exit_code != 0: sys.exit(common_obs.EC_WARNING)

if __name__ == '__main__':

   print('-----------------------------------')
   print('Hello from {}'.format(os.path.basename(__file__)))
   print('-----------------------------------')
   time = common_obs.measure_time(main, sys.argv[1:])
   print('Execution Time: {} seconds'.format(time))
