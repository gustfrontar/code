from . import io_radar as io

import sys, os, glob
import numpy as np
from pandas import date_range
from datetime import datetime, timedelta
from pyart.io import read_cfradial

def get_radar_files(path, period, data_type='rma'):

   files = dict()

   if data_type == 'pawr':
      prefix ='PAWR'
      suffix ='dat'
   elif data_type == 'rma':
      prefix = 'cfrad'
      suffix = 'nc'
   else:
      print('Radar obs not coded yet'); sys.exit()

   # Get file list
   HOURS = list(date_range(start=period[0], end=period[1], freq='1H').hour.values)
   HOURS.insert(0, HOURS[0]-1)
   files['list'] = []
   for hour in HOURS:
      files['list'] += sorted(glob.glob('{}/{}.*_{}*.*.{}'.format(path, prefix, hour, suffix)))
 
   # Get file date in UTC
   files['times'] = [] 
   for path in files['list']:
      filename = os.path.basename(path)
 
      if data_type == 'pawr':
         time = filename[19:27] + filename[28:34]
         files['times'].append(datetime.strptime(time, '%Y%m%d%H%M%S') + timedelta(hours=-9.0))
      elif data_type == 'rma':
         radar = read_cfradial(path)
         it = datetime.strptime(radar.time['units'].split(' ')[-1], '%Y-%m-%dT%H:%M:%SZ')
         files['times'].append(it + timedelta(seconds=radar.time['data'][-1]/2))
         #time = filename.split('.')[1]
      else:
         print('Radar obs not coded yet'); sys.exit()

   return files

def get_radar_data(time, files, tdiff_thld=300, minref=0.0, data_type='rma'):

   # Compute time difference
   time_dist = [(file_time - time).total_seconds() for file_time in files['times']]

   # Select closest file
   radar = None
   if np.abs(time_dist).min() <= tdiff_thld:
      idx = np.abs(time_dist).argmin()
      print('Found radar data at ', files['times'][idx].strftime('%Y%m%d_%H:%M:%S')) #,' to be compared with model data valid at ', time.strftime('%Y%m%d_%H:%M:%S'))
      print(files['list'][idx])

      # Read data
      radar = io.read_radar(files['list'][idx], minref, data_type)

   return radar



