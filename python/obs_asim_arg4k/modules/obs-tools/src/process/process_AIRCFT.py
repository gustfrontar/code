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
import multiprocessing as mp

#############
### AMDAR ###
#############
def AMDAR_read_data(filename):

   # Set variables
   column_input = ['ID', 'Lat', 'Lon', 'Date', 'Time', 'FPhase', 'Lev', 't', 'Wind', 'FLevel']

   # Read file
   try: 
      data = ut.parse_fileFS(filename, r'\s', column_input, skipH=2)
      if data.empty: 
         print(' WARNING: Empty file {}'.format(filename), file = sys.stderr)
         return pd.DataFrame()
   except Exception as err:
      print(err, file = sys.stderr)
      raise RuntimeError(' ERROR reading {}'.format(filename))

   # Parse data
   try:
      # Replace FillValue with NaN
      data.Lon = data.Lon.replace(-0.00, np.nan)
      data.Lat = data.Lat.replace(-0.00, np.nan)
      data.Wind = data.Wind.replace('///////', np.nan)

      # Time
      data['DateTime'] = pd.to_datetime(data.Date + ' ' + data.Time, format='%Y-%m-%d %H:%M:%S')

      # Wind
      data[['wdir','wspd']] = data.Wind.str.split('/', expand=True)
      data = data.apply(ut.to_numeric)
      data['u'], data['v'] = ut.calc_wind_components(data['wspd'], data['wdir'])

      # Standard units
      data = standard_units(data)
      
   except Exception as err:
      print(err, file = sys.stderr)
      raise RuntimeError(' ERROR reading {}'.format(filename))

   return data

##########
### AA ###
##########
def FLevel2Pressure(FLevel):
    Psup = 1013.25
    h = FLevel * 100 * 0.3048
    return np.round(Psup * (1 - 0.0000226 * h)**5.255)

def AA_read_data(filename, date):
   # Set variables
   regex = r'^.+?(?:AG|AGA|AGU).*\s(\d+?(?:N|S|,))\s+?(\d+(?:E|W|,)).*(\d{6}).*F\s*(\d+?)\s+((?:PS|MS)\s*\d+)\s+,?(.+)=.*'
   column_input = ['Lat', 'Lon', 'Time', 'FLevel', 't', 'Wind']

   # Read file
   try:
      data = ut.parse_fileRE(filename, regex, column_input)
   except Exception as err:
      print(err, file = sys.stderr)
      raise RuntimeError(' ERROR reading {}'.format(filename))

   # Parse data 
   try:
      # Time
      data['DateTime'] = [i.replace(date.year, date.month) for i in pd.to_datetime(data.Time, format='%d%H%M')]

      # Longitude
      sign = (data.Lon.str.contains('E').astype(int))
      sign[sign == 0] = -1
      data.Lon = data.Lon.str[:-1].astype(float)/100 * sign

      # Latitude
      sign = (data.Lat.str.contains('N').astype(int))
      sign[sign == 0] = -1
      data.Lat = data.Lat.str[:-1].astype(float)/100 * sign

      # FLevel to Pressure
      data.FLevel = ut.to_numeric(data.FLevel)
      data['Lev'] = FLevel2Pressure(data.FLevel)

      # Temperature
      sign = (data.t.str.contains('PS').astype(int))
      sign[sign == 0] = -1
      data.t = data.t.str[2:].astype(float)/10 * sign

      # Wind
      data[['wdir','wspd']] = data.Wind.str.split('/', expand=True)
      data = data.apply(ut.to_numeric)

      if not np.issubdtype(data.wspd, np.int64):
         try:
            data['wspd'] = [int(i[:-1]) for i in data.wspd.values]
         except:
            data['wspd'] = [int(i[:-2]) for i in data.wspd.values] 
 
      data['u'], data['v'] = ut.calc_wind_components(data['wspd'], data['wdir'])

      # Standard units
      data = standard_units(data)

   except Exception as err:
      print(err, file = sys.stderr)
      raise RuntimeError(' ERROR parsing {}'.format(filename))

   return data

##############
### COMMON ###
##############
def standard_units(data):
   data.loc[data.Lon < 0., 'Lon'] = data.Lon + 360.
   data.t += ut.K_C
   data.u *= ut.KT_MS
   data.v *= ut.KT_MS

   return data

def get_files(path, ini, end):

   # Load files in pd.DataFrame
   allfiles = sorted(glob.glob('{}/AMDAR/amdar*'.format(path))) + sorted(glob.glob('{}/AA/*.txt'.format(path)))
   res = pd.DataFrame(columns=['Path', 'ID', 'StartDate'])

   # Get source ID and start dates 
   files, ids, sdates = [], [], []
   for filename in allfiles:
      id_ = re.search(r'AMDAR|AA', filename).group()
      time_re = re.search(r'\d{8}_\d{2}_\d{2}|\d{12}', filename)
      if time_re is None:
         continue
      time = time_re.group()
      if id_ == 'AMDAR': fmt = '%d%m%Y_%H_%M'
      if id_ == 'AA': fmt = '%Y%m%d%H%M'
      files.append(filename)
      ids.append(id_)
      sdates.append(datetime.strptime(time, fmt))
   res['Path'] = files
   res['ID'] = ids
   res['StartDate'] = sdates

   # Drop out of time interval
   res.drop(res[(res.StartDate > end) | (res.StartDate < ini)].index, axis=0, inplace=True)
   res.reset_index(drop=True, inplace=True)

   return res

def read_data(filename, id_, date):

   if id_.upper() == 'AMDAR':
      data = AMDAR_read_data(filename)
   elif id_.upper() == 'AA':
      data = AA_read_data(filename, date)
   else:
      raise RuntimeError('Aircraft type not coded yet')

   return data

def proc_filename(filename, source, ini, end, slot, monit_file, id_, date):

   code_error = 0

   # Read data
   try:
      data = read_data(filename, id_, date)
   except RuntimeError as err:
      print(err, file = sys.stderr)
      code_error = 1
      return pd.DataFrame(), 0, code_error

   if data.empty: return pd.DataFrame(), 0, code_error
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
      ut.do_monit(data, source, slot, 'REPO', monit_file, id_.upper())

   return data, nin, code_error

def get_data(source, ini, end, files, slot, monit_file, id_, date):

   OBSPROC = int(ENVVARS['OBSPROC'])

   nin = 0
   code_error = 0
   arg_list = [(filename, source, ini, end, slot, monit_file, id_, date) for filename in files['Path']]
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
   dataout = pd.DataFrame(columns=column_write)
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
   ini, end = ut.get_awin_dates(ANA_DATE) #, full=True)
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

     ntot = 0
     for id_ in ['AMDAR', 'AA']:

        print('# {} #'.format(id_.upper()))

        sfiles_id = sfiles[sfiles.ID == id_.upper()].reset_index(drop=True)
        if sfiles_id.empty: continue
        # Get data
        data, nin, exit_code_slot = get_data(source, sini, send, sfiles_id, slot, monit_file, id_, slot_date) 
        exit_code += exit_code_slot
        if data.empty: continue
        ntot += nin 

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
           ut.do_monit(data, source, slot, 'PROC', monit_file, id_.upper())

        # Standard data format
        data = ut.standard_data(data, column_write)

        # Concatenate data
        dataout = pd.concat([dataout, data], ignore_index=True, sort=False)

     # Write data to LETKF binary format
     fileout = f'{pathout}/{MODEL}_{source["NAME"]}_{slot_date:%Y%m%d%H%M%S}.dat'
     ut.write_letkf_dat(source, slot_date, dataout, fileout, ENVVARS['MODEL'])

     print('OBS IN OUT: {} {}'.format(ntot, data.shape[0]))

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
   time = ut.measure_time(main, sys.argv[1:], src.AIRCFT)
   print('Execution Time: {} seconds'.format(time))
