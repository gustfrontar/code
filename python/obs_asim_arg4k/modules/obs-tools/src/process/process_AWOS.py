# -*- coding: utf-8 -*
import sys, os
sys.path.append(f'{os.environ["UTILSDIR"]}/py-lib')
import common
ENVVARS = common.load_config_exp()

sys.path.append(ENVVARS['EXPDIR'])
import catalog_sources as src
import util as ut
from superobbing import so_th

from datetime import datetime, timedelta

from AWOS import get_files, get_data

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
      monit_file = f'{MONITDIR}/{ANA_DATE:{os.environ["DATEFOLDER_fmt"]}}/{EXPNAME_OBS}/'
      os.makedirs(monit_file, exist_ok=True)

      # Create files 
      monit_file = ut.monit_create_csv(source, monit_file, ANA_DATE)

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
      sfiles = files.drop(files[(files.StartDate > send) | (files.EndDate < sini)].index, axis = 0, inplace = False)
      sfiles.reset_index(drop = True, inplace = True)
      if sfiles.empty: continue
      #print(sfiles)

      # Get data
      data, nin, exit_code_slot = get_data(source, sini, send, sfiles, slot, monit_file)
      exit_code += exit_code_slot
      if data.empty: continue

      # Temporal superobbing
      data = data.drop('DateTime', axis = 1)
      data = data.groupby('ID').mean()

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

###############
# CALIBRACION #
###############

def main_calib(args, source):

   exit_code = 0

   EXPNAME_OBS = ENVVARS['EXPNAME_OBS']
   REPODIR = os.environ['REPODIR']
   OBSDIR = ENVVARS['H_OBSDIR']

   # Parse input parameters into date
   OBS_DATE = ut.parse_date(args)

   # Set variables
   #column_write = ['Lon', 'Lat', 'Lev'] + source['VARS']
   pathobs = f'{REPODIR}/{source["NAME"]}'
   pathout = f'{OBSDIR}/{OBS_DATE:%Y%m%d_%H%M%S}/{EXPNAME_OBS}'
   os.makedirs(pathout, exist_ok=True)

   # Get files in time window (considering slots) #FIXME Esta hardcodeado el intervalo de observaciones
   ini = OBS_DATE - timedelta(minutes = 5)
   end = OBS_DATE + timedelta(minutes = 5)
   files = get_files(pathobs, ini, end)
   if files.empty: return exit_code

   data, nin, exit_code = get_data(source, ini, end, files, slot = None, monit_file = None)
   if data.empty: return exit_code

   data = ut.filter_duplicates(data, ['Lon', 'Lat', 'ID'])

   # Standard data format
   data = ut.standard_data(data, data.columns)

   data['DateTime'] = OBS_DATE
   data['Tipo'] = 'AWOS'
   data = data.round(2)
   fileout = f'{pathout}/{source["NAME"]}_{OBS_DATE:%Y%m%d%H%M%S}.csv'
   data.to_csv(fileout, index = False)

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
   time = ut.measure_time(main, sys.argv[1:], src.AWOS)
   print('Execution Time: {} seconds'.format(time))


