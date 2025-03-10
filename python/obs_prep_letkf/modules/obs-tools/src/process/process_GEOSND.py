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
import multiprocessing as mp
import xarray as xr

ctlg = common.merge_catalog(ctlg_process.geosnd, 'obs', 'geosnd')

def get_files(path, ana_date, slots):

   # Load files in pd.DataFrame
   files = sorted(glob.glob('{}/OR_ABI-L2-*-M?_G??_*'.format(path)))
   df_files = pd.DataFrame(files, columns=['Path'])

   # Get start and end dates   
   sdates, edates = [], []
   for filename in df_files['Path']:
      sdates.append(datetime.strptime(re.search(r's\d{14}', filename).group()[1:], '%Y%j%H%M%S%f'))
      edates.append(datetime.strptime(re.search(r'e\d{14}', filename).group()[1:], '%Y%j%H%M%S%f'))

   df_files['StartDate'] = sdates
   df_files['EndDate'] = edates

   files_slots = []
   for slot in slots:
      slot_date, sini, send = common_obs.get_slot_dates(slot, ana_date)
      # Drop out of time interval
      df_slot = df_files.drop(df_files[(df_files.EndDate > send) | (df_files.EndDate < sini)].index, axis=0) # Use EndDate in both conditions because that is the date in data
      df_slot['slot'] = slot
      files_slots.append(df_slot)

   files2proc = pd.concat(files_slots, ignore_index = True)

   return files2proc

def read_data(filename):

   # Set variables
   column_input = ['Lon', 'Lat', 'Lev', 'DateTime', 'QC']
   variables = ['Lon', 'Lat', 'pressure', 't', 'DQF_Overall']
   drop_coords = ['y_image', 'x_image', 'pressure_image', 'retrieval_local_zenith_angle', 'quantitative_local_zenith_angle', 'solar_zenith_angle', 'latitude']

   # Read file
   try:
      ds = xr.open_dataset(filename)
      if 'LVM' in ds.data_vars:
         data_var = 'rh'
         column_input.append('rh')
         variables.append('LVM')
         var2scale = 'LVM'
         scale_value = 100
      else:
         data_var = 't'
         column_input.append('t')
         variables.append('LVT')
         var2scale = 'LVT'
         scale_value = 1

      lats, lons = get_latlon(ds)
      ds['Lat'] = lats
      ds['Lon'] = lons
      
      ds = ds[variables]
      ds[var2scale] = ds[var2scale] * scale_value
      ds = ds.rename({old: new for new, old in zip(column_input, variables)})
      ds = ds.drop_vars(drop_coords)
      # Close file
      ds.close()

   except:
      raise RuntimeError(' ERROR reading {}'.format(filename))

   # Parse data
   try:

      ds_stack = ds.stack({'xy': ['x', 'y']}, create_index = False)
      ds_stack = ds_stack.where(ds_stack['QC'] == 0, drop = True)
      ds_stack['Lon'] = ds_stack['Lon'].where(ds_stack['Lon'] >= 0, ds_stack['Lon'] + 360)
      data = ds_stack.to_dataframe().reset_index()
   except:
      raise RuntimeError(' ERROR parsing {}'.format(filename))

   return data, data_var

def get_latlon(ds):

   proj_info = ds['goes_imager_projection']
   lon_origin = proj_info.longitude_of_projection_origin
   H = proj_info.perspective_point_height+proj_info.semi_major_axis
   r_eq = proj_info.semi_major_axis
   r_pol = proj_info.semi_minor_axis
   # Data info
   lat_rad_1d = ds['x']
   lon_rad_1d = ds['y']
   # create meshgrid filled with radian angles
   lat_rad, lon_rad = xr.broadcast(lat_rad_1d,lon_rad_1d)
   # lat/lon calc routine from satellite radian angle vector
   lambda_0 = (lon_origin*np.pi)/180.0
   a_var = np.power(np.sin(lat_rad),2.0) + (np.power(np.cos(lat_rad),2.0)*(np.power(np.cos(lon_rad),2.0)+(((r_eq*r_eq)/(r_pol*r_pol))*np.power(np.sin(lon_rad),2.0))))
   b_var = -2.0*H*np.cos(lat_rad)*np.cos(lon_rad)
   c_var = (H**2.0)-(r_eq**2.0)
   r_s = (-1.0*b_var - np.sqrt(np.absolute((b_var**2)-(4.0*a_var*c_var))))/(2.0*a_var)
   s_x = r_s*np.cos(lat_rad)*np.cos(lon_rad)
   s_y = - r_s*np.sin(lat_rad)
   s_z = r_s*np.cos(lat_rad)*np.sin(lon_rad)
   lat = (180.0/np.pi)*(np.arctan(((r_eq*r_eq)/(r_pol*r_pol))*((s_z/np.sqrt(((H-s_x)*(H-s_x))+(s_y*s_y))))))
   lon = (lambda_0 - np.arctan(s_y/(H-s_x)))*(180.0/np.pi)

   return lat.T, lon.T

def proc_filename(file_data, ctlg, monit_file):

   code_error = 0

   # Read data
   try:
      data, var = read_data(file_data['Path'])
   except RuntimeError as err:
      print(err, file = sys.stderr)
      code_error = 1
      return pd.DataFrame(), 0, code_error

   nin = data.shape[0]

   # Filter data
   tmp = data.shape[0]
   data = common_obs.apply_filters(ctlg, data, ['Lon', 'Lat', 'Lev', 'DateTime'])
   if data.empty: return pd.DataFrame(), nin, code_error

   # Additional filters
   # 1) Above pressure level
   obs_in = data.shape[0]
   data.drop(data[data.Lev < ctlg['constraints']['max_lev']].index, axis=0, inplace=True)
   if ENVVARS['DEBUG']: print('   Filter Above Level', obs_in - data.shape[0])
   if data.empty: return pd.DataFrame(), nin, code_error

   # 2) Under pressure level
   obs_in = data.shape[0]
   data.drop(data[data.Lev > ctlg['constraints']['min_lev']].index, axis=0, inplace=True)
   if ENVVARS['DEBUG']: print('   Filter Above Level', obs_in - data.shape[0])
   if data.empty: return pd.DataFrame(), nin, code_error

   # Write raw data for monitoring
   if monit_file is not None:
      common_obs.do_monit(data, ctlg, file_data['slot'], 'REPO', monit_file)

   data = data.set_index(['DateTime', 'Lat', 'Lon', 'Lev'])
   data = data[[var]]

   return data, nin, code_error


def get_data(ctlg, files, monit_file):

   nin = 0
   code_error = 0
   arg_list = [(row, ctlg, monit_file) for index, row in files.iterrows()]
   with mp.Pool(min(ctlg['procs'], len(arg_list))) as pool:
      pool_out = pool.starmap(proc_filename, arg_list)

   df_list = []
   for df_file, nin_file, code_error_file in pool_out:
      nin += nin_file
      code_error += code_error_file
      df_list.append(df_file)

   # Concatenate data
   dataout = pd.concat(df_list)

   return dataout, nin, code_error

def proc_slot(ctlg, slot_data, pathout, slot, slot_date, monit_file):

   column_write = ['Lon', 'Lat', 'Lev'] + ctlg['vars']
   MODEL = ENVVARS['MODEL']

   # Temporal superobbing
   obs_in = slot_data.shape[0]
   gp = slot_data.groupby(['Lon', 'Lat', 'Lev']).mean(numeric_only=True)
   slot_data = gp.reset_index(inplace=False)
   if ENVVARS['DEBUG']: print('   Filter Temporal SO', obs_in - slot_data.shape[0])
   if slot_data.empty: return

   # Apply filters
   obs_in = slot_data.shape[0]
   slot_data = common_obs.filter_duplicates(slot_data, ['Lon', 'Lat', 'Lev'])
   if ENVVARS['DEBUG']: print('   Filter Duplicates', obs_in - slot_data.shape[0])
   if slot_data.empty: return

   # Superobbing
   if ctlg['so/th']:
      DOMAIN = eval(ENVVARS['DOMAIN'])
      slot_data = so_th(ctlg, slot_data, [DOMAIN['lat_s'], DOMAIN['lat_n']], [DOMAIN['lon_w'], DOMAIN['lon_e']], [DOMAIN['bottom'], DOMAIN['top']])

   # Write processed data for monitoring
   if monit_file:
      common_obs.do_monit(slot_data, ctlg, slot, 'PROC', monit_file)

   # Standard data format
   slot_data = common_obs.standard_data(slot_data, set(column_write).intersection(slot_data.columns))

   # Write data to LETKF binary format
   fileout = f'{pathout}/{MODEL}_{ctlg["name"]}_{slot_date:%Y%m%d%H%M%S}.dat'
   common_obs.write_letkf_dat(ctlg, slot_date, slot_data, fileout, MODEL)

###############
# ASIMILACION #
###############

def main_asim(args):

   exit_code = 0

   EXPNAME_BASE = ENVVARS['EXPNAME_BASE']
   REPODIR = os.environ['REPODIR']
   OBSDIR = ENVVARS['H_OBSDIR']

   # Parse input parameters into date
   ANA_DATE = common_obs.parse_date(args)
   print('ANALYSIS DATE:', ANA_DATE)

   # Set variables
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

   # Get slots to process
   slots = ctlg['slots']
   if not slots:
      slots = common_obs.get_slots(ctlg['slots'])

   # Get files in analysis window (considering slots)
   files = get_files(pathobs, ANA_DATE, slots)
   if files.empty: return exit_code

   # Get data
   data, nin, exit_code_slot = get_data(ctlg, files, monit_file) 
   if data.empty: return exit_code

   slot_list = []
   for slot in slots:
      slot_date, sini, send = common_obs.get_slot_dates(slot, ANA_DATE)

      slot_data = data.loc[slice(sini, send)]
      if slot_data.empty: continue
      slot_list.append((ctlg, slot_data, pathout, slot, slot_date, monit_file))

   with mp.Pool(min(ctlg['procs'], len(slot_list))) as pool:
      pool_out = pool.starmap(proc_slot, slot_list)

   return exit_code

### MAIN SCRIPT ###
def main(args):

    OBSTYPE = ENVVARS['OBSTYPE']
    func = eval(f'main_{OBSTYPE}')
    exit_code = func(args)

    if exit_code != 0: sys.exit(2)

if __name__ == '__main__':

   print('-----------------------------------')
   print('Hello from {}'.format(os.path.basename(__file__)))
   print('-----------------------------------')
   time = common_obs.measure_time(main, sys.argv[1:])
   print('Execution Time: {} seconds'.format(time))
