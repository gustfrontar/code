# ----------------------
# Common radar functions
#
# Author: P. Maldonado
# ----------------------
import glob, re, pyart
import numpy as np
import pandas as pd
from datetime import datetime, timedelta

def dbz_power(data, conversion):

   # dBZ = -10*log10(Z) 
   # Z = 10**(-dBZ/10)

   if conversion == 'to':
       res = np.power(10, data/10)
   elif conversion == 'from':
      res = 10 * np.log10(data)
   else:
      print('Conversion must be to or from')
      return

   return res

def add_power_field(radar, ref_name, ref_th, filter_interference=False, filter_th=None):

   # Get reflectivity data and mask below threshold
   ref = np.copy(radar.fields[ref_name]['data'])
   ref = np.ma.masked_less_equal(ref, ref_th)

   # Apply interference filter
   if filter_interference:
      range_km = radar.range['data'] * 1e-3
      pot = ref - 20 * np.log10(range_km) - 2 * 1e-2 * range_km - 0
      pot = np.ma.masked_where(pot < -200, pot)
      if filter_th is None:
         filter_th  = np.floor(np.nanmin(pot[np.isfinite(pot)])) + 12
      ref = np.ma.masked_where(pot < filter_th, ref)

   # Convert dbz to power and add to radar object
   power = dbz_power(ref, 'to')
   radar.add_field_like(ref_name, 'Zpower', power, True)

   # Remove gate transition and mask values 
   gatefilter = pyart.filters.GateFilter(radar)
   gatefilter.exclude_transition()
   gatefilter.exclude_masked('Zpower')

   return radar

def get_radar_files(path, period, data_type='qc_new'):

   if data_type in ('qc_new', 'qc_old'):
      prefix = 'cfrad'
      suffix = 'nc'
   else:
      print('Radar obs not coded yet'); sys.exit()

   # Load files in pd.DataFrame
   res = dict()
   files = sorted(glob.glob('{}/{}.*.{}'.format(path, prefix, suffix)))
   res = pd.DataFrame(files, columns=['Path'])

   # Get radar ID, start and end dates 
   ids, sdates, edates, mdates = [], [], [], []
   for filename in res['Path']:
      ids.append(re.search(r'(RMA\d+|PAR|ANG|PER)+', filename).group())

      if data_type == 'qc_new':
         # New radar format
         sdate = datetime.strptime(re.search(r's\d{8}_\d{6}', filename).group()[1:], '%Y%m%d_%H%M%S')
         edate = datetime.strptime(re.search(r'e\d{8}_\d{6}', filename).group()[1:], '%Y%m%d_%H%M%S')
         mdate = sdate + timedelta(seconds=(edate - sdate).total_seconds()/2)

      elif data_type == 'qc_old':
         # Old radar format
         radar = pyart.io.read_cfradial(filename)
         sdate = datetime.strptime(radar.time['units'].split(' ')[-1], '%Y-%m-%dT%H:%M:%SZ')
         edate = None
         mdate = sdate + timedelta(seconds=radar.time['data'][-1]/2)
      else:
         print('Radar obs not coded yet'); sys.exit()

      sdates.append(sdate)
      edates.append(edate)
      mdates.append(mdate)
   print(sdates)
   print(edates)
   res['ID'] = ids
   res['StartDate'] = sdates
   res['EndDate'] = edates
   res['MeanDate'] = mdates
   print(res)

   # Drop out of time interval
   res.drop(res[(res.StartDate > period[1]) | (res.EndDate < period[0])].index, axis=0, inplace=True)
   print(res)
   res.reset_index(drop=True, inplace=True)

   return res

def get_radar_data(date, files, tdiff_thld=300, data_type='qc_new', ref_name='cref'):

   # Compute time difference
   time_dist = [(file_date - date).total_seconds() for file_date in files['MeanDate']]

   # Select closest file
   radar = None
   if np.abs(time_dist).min() <= tdiff_thld:
      idx = np.abs(time_dist).argmin()
      #print('Found radar data at', files['MeanDate'][idx].strftime('%Y%m%d_%H:%M:%S'))

      # Read data
      if data_type.lower() in ('qc_new', 'qc_old'):
         radar = pyart.io.read(files['Path'][idx])

         # Check for reflectivity field
         if not ref_name in radar.fields:
            radar = None

      else:
         print('Radar obs not coded yet'); sys.exit()

   return radar

