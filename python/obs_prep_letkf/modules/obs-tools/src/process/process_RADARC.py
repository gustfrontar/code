# -*- coding: utf-8 -*-
import sys, os, glob, re, time
from datetime import datetime, timedelta
os.environ['PYART_QUIET'] = ''
import pyart
sys.path += [os.environ['CONFIGDIR'], f'{os.environ["UTILSDIR"]}/py-lib']
import common
ENVVARS = common.load_config_exp()

import common_obs
import catalog_obs as ctlg_process
from superobbing import superobbing_radar

import numpy as np
import pandas as pd
from netCDF4 import Dataset, chartostring
import multiprocessing as mp

ctlg = ctlg_process.radarc

def get_files(path, ini, end):

   # Load files in pd.DataFrame
   files = sorted(glob.glob('{}/cfrad.*'.format(path)))
   res = pd.DataFrame(files, columns=['Path'])

   # Get radar ID, start and end dates 
   ids, sdates, edates = [], [], []
   for filename in res['Path']:
      ids.append(re.search(r'(RMA\d+|PAR|ANG|PER)+', filename).group()) 
      sdates.append(datetime.strptime(re.search(r's\d{8}_\d{6}', filename).group()[1:], '%Y%m%d_%H%M%S'))
      edates.append(datetime.strptime(re.search(r'e\d{8}_\d{6}', filename).group()[1:], '%Y%m%d_%H%M%S'))

   res['ID'] = ids
   res['StartDate'] = sdates
   res['EndDate'] = edates

   # Drop files out of time interval
   res.drop(res[(res.StartDate > end) | (res.EndDate < ini)].index, axis=0, inplace=True)
   res.reset_index(drop=True, inplace=True)

   return res

def read_data(ctlg, filename):


   # Read data
   print('Reading :', filename )
   try:
      radar = pyart.io.read(filename)
      process_doppler = False
      if 'nyquist_velocity' in radar.instrument_parameters.keys() :
         nyquist = radar.instrument_parameters['nyquist_velocity']['data'] 
         #TODO: This should be changed to a method in which the nyquist for each 
         #elevation can be considered
         if all( nyquist >= ctlg['constraints']['min_nyquist_velocity'] ) :
            process_doppler = True
            print('We will process Doppler data for this volume. Nyquist V.:', np.max( nyquist ) )

   except Exception as err:
      print(err, file = sys.stderr)
      print(' ERROR reading file {}'.format(filename))
      return None, None

   # Parse parse
   try:
   #if True :
      # Radar location
      radar_loc = [radar.longitude['data'].data, radar.latitude['data'].data, radar.altitude['data'].data]
 
      # Radar georeference variables
      data = dict()
      data['Lon'] = radar.gate_longitude['data'].ravel()
      data['Lat'] = radar.gate_latitude['data'].ravel()
      data['Lev'] = radar.gate_altitude['data'].ravel()
      data['Time'] = np.repeat(radar.time['data'].data[:,np.newaxis], radar.ngates, axis=1).ravel()
      data['Rang'] = np.repeat(radar.range['data'].data[np.newaxis,:], radar.nrays, axis=0).ravel()
      data['Azim'] = np.repeat(radar.azimuth['data'].data[:,np.newaxis], radar.ngates, axis=1).ravel()
      data['Elev'] = np.repeat(radar.elevation['data'].data[:,np.newaxis], radar.ngates, axis=1).ravel()

      # Radar meteorological variables
      for var in ctlg['vars']:
         if 'dbz' == var: ncvar = 'cref'
         if 'Vr' == var:  ncvar = 'cv'
         try:
            if ( var == 'dbz' ) or ( ( var == 'Vr' ) and  process_doppler ) :
               data[var] = radar.fields[ncvar]['data'].data
               data[var][data[var] == radar.fields[ncvar]['data'].fill_value] = np.nan
               data[var] = data[var].ravel()
         except:
            #print('Variable not found:', var) 
            continue

      # Store data in pd.DataFrame
      data = pd.DataFrame.from_dict(data, orient='columns')

      # Time
      epoch = datetime.strptime(radar.time['units'].split(' ')[2], '%Y-%m-%dT%H:%M:%SZ')
      data['DateTime'] = epoch + pd.to_timedelta(data.Time, unit='s')
      #print(data.DateTime.min(), data.DateTime.max())

      # Standard units
      data.loc[data.Lon < 0., 'Lon'] = data.Lon + 360.
      if 'dbz' in data.keys():
         data.dbz = np.power(10, data.dbz/10.)

      #Remove data wich is outside the valid range.
      for var in ctlg['vars'] :
         if var in data.keys() : 
            data[var][ data[( data[var] < ctlg['constraints']['valid_range'][var][0] ) |
                         ( data[var] > ctlg['constraints']['valid_range'][var][1] ) ].index ] = np.nan
   except Exception as err:
      print(err, file = sys.stderr)
      print(' ERROR parsing {}'.format(filename))
      data = None
      radar_loc = None

   return data, radar_loc 


def get_data(ctlg, ini, end, files, slot, monit_file):

   # Set variables
   dataout = pd.DataFrame() #columns=['Lon', 'Lat', 'Lev', 'Rang', 'Azim', 'Elev'] + ctlg['VARS'])
   valid_radar_loc = None

   # Organize data into pd.DataFrame
   nin = 0
   for filename in files['Path']:
  
      # Read data
      data, radar_loc = read_data(ctlg, filename)
      if data is None: continue
      valid_radar_loc = radar_loc # Will fail in superobbing if last radar_loc is None for error in reading. Save the location of valid data
      nin += data.shape[0]

      # Filter data outside slot 
      #print('', filename)
      tmp = data.shape[0]
      data = common_obs.filter_time(data, ini, end)
      if data.empty: continue

      # Filter data
      tmp = data.shape[0]
      data = common_obs.apply_filters(ctlg, data, ['Lon', 'Lat', 'Lev', 'Rang', 'Azim', 'Elev', 'DateTime'])

      if data.empty: continue

      # Write raw data for monitoring
      if monit_file is not None:
         radar_id = re.search(r'(RMA\d+|PAR|ANG|PER)+', filename).group()
         common_obs.do_monit(data, ctlg, slot, 'REPO', monit_file, radar_id)

      # Concatenate data
      dataout = pd.concat([dataout, data], ignore_index=True, sort=False)
      
   return dataout, nin, valid_radar_loc

def proc_radar(radar, ctlg, files, sini, send, slot, slot_date, monit_file, column_write, pathout, MODEL):

   # Get files for current radar
   sfiles = files.drop(files[(files.ID != radar)].index, axis=0, inplace=False)
   sfiles.reset_index(drop=True, inplace=True)
   if sfiles.empty: return pd.DataFrame(), 0

   # Get data
   data, nin, radar_loc = get_data(ctlg, sini, send, sfiles, slot, monit_file)
   if data.empty: return pd.DataFrame(), nin

   # Temporal superobbing
   obs_in = data.shape[0]
   gp = data.groupby(['Lon', 'Lat', 'Lev', 'Rang', 'Azim', 'Elev']).mean(numeric_only=True)
   data = gp.reset_index(inplace=False)
   if ENVVARS['DEBUG']: print('   Filter Temporal SO', obs_in - data.shape[0])
   if data.empty: pd.DataFrame(), nin

   print(' File In Out', obs_in, data.shape[0])

   # Apply filters
   obs_in = data.shape[0]
   data = common_obs.filter_duplicates(data, ['Lon', 'Lat', 'Lev', 'Azim', 'Elev', 'Rang'])
   if ENVVARS['DEBUG']: print('   Filter Duplicates', obs_in - data.shape[0])
   if data.empty: return pd.DataFrame(), nin

   print(' All Files In Out', nin, data.shape[0])

   # Superobbing
   if ctlg['so/th']:
      data = superobbing_radar(ctlg, data, radar_loc, [240., 15.])

      # Set boxes with few observations or high variance to NaN.
      for var in ctlg['vars'] :
          if var in data.keys()  :
             data[var][ data['NObs_'+var] < ctlg['constraints']['min_nobs'] ]  = np.nan
             data[var][ data['STD_'+var] > ctlg['constraints']['maximum_so_var'][var] ] = np.nan
      #data.drop(data[data.NObs < ctlg['constraints']['min_nobs']].index, axis=0, inplace=True)

   if data.empty: return pd.DataFrame(), nin

   # Write processed data for monitoring
   if monit_file:
      common_obs.do_monit(data, ctlg, slot, 'PROC', monit_file, radar)

   # Standard data format
   data = common_obs.standard_data(data, column_write)

   # Write data to LETKF binary format
   fileout = f'{pathout}/{MODEL}_{ctlg["name"]}_{radar}_{slot_date:%Y%m%d%H%M%S}.dat'
   common_obs.write_letkf_dat_radar(ctlg, slot_date, radar_loc, data, fileout, MODEL)

   print('OBS IN OUT: {} {}'.format(nin, data.shape[0]))



###############
# ASIMILACION #
###############

def main_asim(args):

   exit_code = 0

   REPODIR = os.environ['REPODIR'] 
   OBSDIR = ENVVARS['OBSDIR']
   MODEL = ENVVARS['MODEL']

   # Parse input parameters into date
   ANA_DATE = common_obs.parse_date( args[0] )
   print('ANALYSIS DATE:', ANA_DATE)

   # Set variables
   MODEL = ENVVARS['MODEL']
   if MODEL == 'WRF':
      column_write = ['Azim', 'Elev', 'Rang'] + ctlg['vars'] 
   elif MODEL == 'SCALE':
      column_write = ['Lon', 'Lat', 'Lev'] + ctlg['vars']
   else:
      print('NWP model not coded yet')
      sys.exit(common_obs.EC_ERROR)

   pathobs = f'{REPODIR}/{ctlg["name"]}/'
   pathout = f'{OBSDIR}/{ctlg["name"]}/'
   os.makedirs(pathout, exist_ok=True)

   # Set variables for monitoring
   monit_file = None
   if ENVVARS['MONIT']:
      MONITDIR = ENVVARS['MONITDIR']
      monit_path = f'{MONITDIR}/{ctlg["name"]}/MONIT/'
      os.makedirs(monit_path, exist_ok=True)

      # Create files 
      monit_file = common_obs.monit_create_csv(ctlg, monit_path, ANA_DATE)

   # Get files in analysis window
   ini, end = common_obs.get_awin_dates(ANA_DATE)
   files = get_files(pathobs, ini, end)

   # Get slots to process
   slots = ctlg['slots']
   if not slots:
      slots = common_obs.get_slots(slots)

   # Get list of available radars
   RADARS = files['ID'].unique()

   # Process observations for each slot
   for slot in slots:
      slot_date, sini, send = common_obs.get_slot_dates(slot, ANA_DATE)

      print('')
      print('*** SLOT {}: {} ***'.format(int(slot), slot_date))

      sfiles = files.drop(files[(files.StartDate > send) | (files.EndDate < sini)].index, axis=0, inplace=False)
      sfiles.reset_index(drop=True, inplace=True)

      if sfiles.empty: 
         print('No files available for current slot')
         exit_code += 1
         continue


      arg_list = [(radar, ctlg, sfiles, sini, send, slot, slot_date, monit_file, column_write, pathout, MODEL) for radar in RADARS]
      with mp.Pool(min(ctlg['procs'], len(arg_list))) as pool:
         pool_out = pool.starmap(proc_radar, arg_list)
      #for radar in RADARS :
      #   proc_radar( radar , ctlg , sfiles , sini , send , slot , slot_date , monit_file , 
      #               column_write , pathout , MODEL  )


   return exit_code


### MAIN SCRIPT ###
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
