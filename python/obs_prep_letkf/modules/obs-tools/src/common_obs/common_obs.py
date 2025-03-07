import sys, os
sys.path.append(f'{os.environ["UTILSDIR"]}/py-lib')
sys.path.append(f'{os.environ["CONFIGDIR"]}/')
print( os.environ["CONFIGDIR"] )
import common
import catalog_obs as ctlg_obs
ENVVARS = common.load_config_exp()

#sys.path.append(ENVVARS['RUNDIR'])

import re, requests
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
import xarray as xr
import cartopy.crs as ccrs
import time

################
### SETTINGS ###
################

EC_OK = int(os.environ['EC_OK'])
EC_ERROR = int(os.environ['EC_ERROR'])
EC_WARNING = int(os.environ['EC_WARNING'])

#################
### CONSTANTS ###
#################
K_C = 273.15
KT_MS = 0.514444
KMH_MS = 3.6
GKG_KGKG = 1e-3

##################
### CONVERSION ###
##################
def tai2utc(tai):
   import leapsec93 as ls93

   epoch = datetime(1993,1,1)
   fun = np.vectorize(lambda x,y: ls93.tai93_to_utc( x + timedelta(seconds=y)))

   if isinstance(tai, float):
      res = ls93.tai93_to_utc(epoch + timedelta(seconds=tai))
   else:
      res = fun(epoch, tai)

   return res

def utc2tai(utc):
   import leapsec93 as ls93

   epoch = datetime(1993,1,1)
   if isinstance(utc, datetime): 
      res = (ls93.utc_to_tai93(utc)-epoch).total_seconds()
   else:
      res = [(ls93.utc_to_tai93(i)-epoch).total_seconds() for i in tai]
   
   return res

def to_numeric(df):

    if np.issubdtype(df.dtype, np.datetime64): return df

    try:
        df = pd.to_numeric(df)
    except ValueError:
        df = df

    return df
        

#############
### LETKF ###
#############
def get_slots(slots):
   # 3D window
   if ENVVARS['WLENGTH'] == ENVVARS['SLENGTH']:
      nslots = 1
   # 4D window
   else:
      nslots = int(ENVVARS['WLENGTH'])/int(ENVVARS['SLENGTH']) + 1
   return np.arange(nslots)

def get_awin_dates(ana_date):
   # Get assimilation window parameters 
   wlength = int(ENVVARS['WLENGTH'])  # window length
   woffset = int(ENVVARS['WOFFSET'])  # window displacement (sec previous to analysis time)
   slength = int(ENVVARS['SLENGTH'])  # slot frequency

   # Assimilation window time interval 
   ini = ana_date - timedelta(seconds=woffset)
   end = ini + timedelta(seconds=wlength)

   # Consider full window with centered slots for 4D
   if wlength != slength:
      ini -= timedelta(seconds=slength/2.)
      end += timedelta(seconds=slength/2.)

   if ENVVARS['DEBUG']:
      print('Assimilation window:', ini, end)
   return ini, end

def get_slot_dates(slot, ana_date):

   # Get assimilation window parameters 
   wlength = int(ENVVARS['WLENGTH'])  # window length 
   slength = int(ENVVARS['SLENGTH'])  # slot length
   wini, wend = get_awin_dates(ana_date)

   # Observation slot time interval
   date = wend
   ini = wini
   end = wend

   ### 4D
   if wlength != slength: 
      date = ini + timedelta(seconds=(slength * int(slot) + slength/2))
      ini = date - timedelta(seconds=slength/2)
      end = date + timedelta(seconds=slength/2)

   if ENVVARS['DEBUG']:
      print('Slot {}:'.format(slot), date, ini , end)

   return date, ini, end

def standard_data(data, columns):
   ''' 
   data: pd.DataFrame
   columns: list
   '''
   # Remove unnecessary columns
   data.drop(data.columns.difference(columns), axis=1, inplace=True)
   data.reset_index(drop=True, inplace=True)
 
   # Convert to float
   data = data.apply(to_numeric)

   # Rearange columns
   #data = data[columns]

   return data

def get_obserr_from_dict(data, ctlg, var):
   errors = []
   keys = np.fromiter(ctlg['errors'][var].keys(), dtype=float)
   for row in range(data.shape[0]):
      errors.append(ctlg['errors'][var][keys[np.abs(data.Lev[row] - keys).argmin()]])
   return errors

def df2wk(ctlg, data, model):
   ''' 
   data: pd.DataFrame
   res: np.array
   '''
   # Set variables based on model
   model = model.upper()
   if model == 'WRF':
      nwk = 10
   elif model == 'SCALE':
      nwk = 11
   else:
      print('NWP model not coded yet')
      sys.exit()

   # Loop over variables
   res = np.full([1, nwk + 2], np.nan)
   variables = sorted(list(set(ctlg['vars']).intersection(data.columns)))

   data_size = np.int32(nwk * 4).view(np.float32)

   for var in variables:
      # Get observation error
      if isinstance(ctlg['errors'][var], dict):
         errors = get_obserr_from_dict(data, ctlg, var)
      elif isinstance(ctlg['errors'][var], str):
         errors = data['{}Err'.format(var)]
      else:
         errors = ctlg['errors'][var]

      # Create wk array
      arr = np.full([data.shape[0], nwk + 2], np.nan)
      arr[:,1] = ctlg['id_var'][var]
      arr[:,2] = data.Lon
      arr[:,3] = data.Lat
      arr[:,4] = data.Lev
      arr[:,5] = data[var]
      arr[:,6] = errors 
      arr[:,7] = ctlg['id_obs']
      arr[:,8] = data['tdif']
      arr[:,9] = data['ohx']
      arr[:,10] = data['oqc']
      # TODO: for 4D scale
      if model == 'SCALE':
         arr[:,11] = 0.
      arr[:, 0] = data_size
      arr[:, -1] = data_size
      res = np.append(res, arr, axis=0)

   # Remove NaN values
   res =  res[np.isfinite(res).all(axis=1)]
   
   # Convert to float32
   return np.float32(res)

def df2wk_radar(ctlg, data, model):
   ''' 
   data: pd.DataFrame
   res: np.array
   '''
   # Set variables based on model
   model = model.upper()
   if model == 'WRF':
      nwk = 10
      x, y, z = data.Azim, data.Elev, data.Rang
   elif model == 'SCALE':
      nwk = 11
      x, y, z = data.Lon, data.Lat, data.Lev
   else:
      print('NWP model not coded yet')
      sys.exit()

   data_size = np.int32(nwk * 4).view(np.float32)

   # Loop over variables
   res = np.full([1, nwk + 2], np.nan)
   common_vars = sorted(list(set(data.columns) & set(ctlg['vars'])))
   for var in common_vars:

      # Get observation error
      if isinstance(ctlg['errors'][var], dict):
         errors = get_obserr_from_dict(data, ctlg, var)
      elif isinstance(ctlg['errors'][var], str):
         errors = data['{}Err'.format(var)]
      else:
         errors = ctlg['errors'][var]

      # Create wk array
      arr = np.full([data.shape[0], nwk + 2], np.nan)
      arr[:,1] = ctlg['id_var'][var]
      arr[:,2] = x
      arr[:,3] = y
      arr[:,4] = z
      arr[:,5] = data[var]
      arr[:,6] = errors
      arr[:,7] = ctlg['id_obs']
      arr[:,8] = data['tdif']
      arr[:,9] = data['ohx']
      arr[:,10] = data['oqc']
      # TODO: for 4D scale
      if model == 'SCALE':
         arr[:,11] = 0.
      arr[:, 0] = data_size
      arr[:, -1] = data_size
      res = np.append(res, arr, axis=0)

   # Remove NaN values
   res =  res[np.isfinite(res).all(axis=1)]
   print('------------- OBS SUMMARY ---------')
   for my_var in common_vars :
       print('Number of ' + my_var + 'obs: ',str( np.sum( res[:,1] == ctlg['id_var'][my_var] ) ) )
   print('-----------------------------------')

   # Convert to float32
   return np.float32(res)


def get_radar_header(radar):
   '''
   Get array for write binary radar header
   '''

   data_size = np.int32(1 * 4).view(np.float32)
   radar_header = np.full([3, 3], data_size)
   radar_header[:, 1] = np.array(radar).squeeze()

   return radar_header

#############
### ATMOS ###
#############
def calc_wind_components(wspd, wdir, conv='atm'):
   '''
   wspd: pd.DataFrame Series of wind speed
   wdir: pd.DataFrame Series of wind direction (degree)
   conv: str. 'atm' or 'ocean'
   u, v: pd.DataFrame Series of wind components in same units as wspd
   '''
   # Compute wind components in atmospheric convention
   u = - wspd * np.sin(np.deg2rad(wdir))
   v = - wspd * np.cos(np.deg2rad(wdir)) 

   # Replace small values to zero
   try:
      u[np.abs(u) < 1e-6] = 0.
      v[np.abs(v) < 1e-6] = 0.
   except:
      print('using scalar version of u,v')
      if np.abs(u) < 1.e-6: u = 0.
      if np.abs(v) < 1.e-6: v = 0.

   # Use other type of convention
   if conv == 'ocean': u, v = -u, -v

   return u, v

def calc_relhum_from_dewpoint(Temp, DPTemp, unit='C'):
   import metpy.calc as mcalc
   from metpy.units import units
   if unit == 'C':
      unit = units.degC
   elif unit == 'K':
      unit = units.K
   else:
      return None 
   return mcalc.relative_humidity_from_dewpoint(Temp*unit, DPTemp*unit)

def calc_relhum_from_dewpoint_ifmissing(data):
   tmp_T = data.t2[data.rh2.isna()]
   tmp_Td = data.td2[data.rh2.isna()]
   if (tmp_Td < tmp_T).any():
      tmp = calc_relhum_from_dewpoint(tmp_T[tmp_Td < tmp_T].values, tmp_Td[tmp_Td < tmp_T].values)
      data.loc[(data.rh2.isna()) & (tmp_Td < tmp_T), 'rh2'] = tmp * 100                  
      #print('     Replacing RH', tmp *100)

   return data

###############
### FILTERS ###
###############
def filter_coordinates(data, columns):
   data.dropna(subset=columns, how='any', axis=0, inplace=True)
   return data

def filter_variables(ctlg, data):
   common_vars = list(set(data.columns) & set(ctlg['vars']))
   data.dropna(subset=common_vars, how='all', axis=0, inplace=True)
   return data

def filter_time(data, ini, end):
   nin = data.shape[0]
   data.drop(data[data.DateTime <= ini].index, axis=0,  inplace=True)
   data.drop(data[data.DateTime > end].index, axis=0, inplace=True)
   if ENVVARS['DEBUG']: print('   Filter Out of Time', nin - data.shape[0])
   return data

def filter_domain(data):

   #Read static file once for domain selection
   #JR This is a simplified version of the domain check using the geo_em.d?? 
   #This only works with WRF ... 
   static_file = f'{ENVVARS["DOMAIN_GEO_EM"]}'
   ds_static = xr.open_dataset(static_file)

   min_x = ds_static['XLONG_M'].min().item()
   max_x = ds_static['XLONG_M'].max().item()
   min_y = ds_static['XLAT_M'].min().item()
   max_y = ds_static['XLAT_M'].max().item()
   if min_x < 0 :
      min_x = min_x + 360.0
   if max_x < 0 :
      max_x = max_x + 360.0 

   #data_crs = ccrs.LambertConformal(central_longitude = ds_static['Lambert_Conformal'].attrs['longitude_of_central_meridian'],
   #                                 central_latitude = ds_static['Lambert_Conformal'].attrs['latitude_of_projection_origin'],
   #                                 standard_parallels = ds_static['Lambert_Conformal'].attrs['standard_parallel'])

   #x_model, y_model, _ = data_crs.transform_points(src_crs = ccrs.PlateCarree(), x = data['Lon'], y = data['Lat']).T
   #flag_domain = (x_model > min_x) * (x_model < max_x) * (y_model > min_y) * (y_model < max_y)
   flag_domain  = ( data['Lon'] > min_x ) * ( data['Lon'] < max_x ) * ( data['Lat'] > min_y ) * ( data['Lat'] < max_y )
   data.drop(data[~flag_domain].index, axis=0, inplace=True)


   return data

def filter_duplicates(data, columns):
   #print(data.loc[data.duplicated(subset=columns, keep=False)])
   data.drop_duplicates(subset=columns, keep='first', inplace=True)
   data.reset_index(drop=True, inplace=True)
   return data

def apply_filters(ctlg, data, coords_columns, time_interval=None):

   if ENVVARS['DEBUG']: print('  ******* Filters *******')

   # 1) Missing observation point
   obs_in = data.shape[0]
   data = filter_coordinates(data, coords_columns)
   if ENVVARS['DEBUG']: print('   Filter Missing Coords', obs_in - data.shape[0])
   if data.empty: return data

   # 2) Missing variables
   obs_in = data.shape[0]
   data = filter_variables(ctlg, data)
   if ENVVARS['DEBUG']: print('   Filter Missing Vars', obs_in - data.shape[0])
   if data.empty: return data

   # 3) Out of time interval
   if time_interval is not None:
      obs_in = data.shape[0]
      data = filter_time(data, time_interval[0], time_interval[1])
      if ENVVARS['DEBUG']: print('   Filter Out of Time', obs_in - data.shape[0])
      if data.empty: return data

   # 4) Out of domain
   obs_in = data.shape[0]
   data = filter_domain(data)
   if ENVVARS['DEBUG']: print('   Filter Out of Domain', obs_in - data.shape[0])
   if data.empty: return data

   # 5) Duplicates 
   obs_in = data.shape[0]
   data = filter_duplicates(data, coords_columns)
   if ENVVARS['DEBUG']: print('   Filter Duplicates', obs_in - data.shape[0])
   if data.empty: return data

   if ENVVARS['DEBUG']: print('  ***********************')

   return data

#############
### MONIT ###
#############
def do_monit(data, ctlg, slot, TYPE, monit_file, ID = None):

   if ID is None:
      ID = float(ctlg_obs.generic_id)

   records = monit_get_nobs(ctlg, data)
   common_line = '{},{},{},{}'.format(TYPE, ctlg['id_obs'], slot, ID)
   monit_append_csv(monit_file, records, common_line)

def monit_get_filename(ctlg, path, date):
   model = ENVVARS['MODEL']
   return f'{path}/{model}_{ctlg["name"]}_{date:%Y%m%d_%H0000}.csv'

def monit_get_nobs(ctlg, data): 
   varlist = ctlg_obs.monit_vars

   common_vars = list(set(ctlg['vars']).intersection(data.columns))
   data = data[common_vars].count()
   data = data.astype('Int64')
   if isinstance(data, pd.Series):
      data = data.to_frame().transpose()
  
   records = []
   for irow, row in data.iterrows():
      nobs = row.reindex(index=varlist)
      records.append((',').join(nobs.values.astype(str)))
   return records 

def monit_create_csv(ctlg, path, date):
   varlist = [(',').join(ctlg_obs.monit_vars)]
   prefix = 'TYPE,ID_OBS,SLOT,ID_INST'      
   monit_file = monit_get_filename(ctlg, path, date)

   # Remove file if exists
   try: 
      os.remove(monit_file)
   except:
      pass

   # Write file
   monit_append_csv(monit_file, varlist, prefix)

   return monit_file

def monit_append_csv(filename, records, prefix):
   with open(filename, 'a', encoding = 'latin-1') as f:
      for record in records:
         f.write('{},{}\n'.format(prefix, record))



###########
### I/O ###
###########

def write_letkf_dat(ctlg, slot_date, data, filename, model='WRF'):
   '''
   ctlg: dict from settings.py
   data: pd.DataFrame
   filename: str with absolute path
   '''
   # Remove NaN values
   data.dropna(how='all', inplace=True)
   data.reset_index(drop=True, inplace=True)

   # Add columns needed for EFSOI
   data['tdif'] = slot_date.minute - 60 if slot_date.minute != 0 else 0
   data['ohx'] = 0
   data['oqc'] = 0

   # Round all values to 4 decimals
   data = data.round(4)

   # Unpack data to LETKF array (wk)
   data = df2wk(ctlg, data, model)

   # Write binary file
   with open(filename, 'wb') as f:
      f.write(data.ravel())

def write_letkf_dat_radar(ctlg, slot_date, radar, data, filename, model='WRF'):
   '''
   ctlg: dict from settings.py
   data: pd.DataFrame
   filename: str with absolute path
   '''

   # Set variables
   if model == 'SCALE':
      radar_lon += 360.

   # Remove NaN values
   data.dropna(how='all', inplace=True)
   data.reset_index(drop=True, inplace=True)

   # Add columns needed for EFSOI
   data['tdif'] = slot_date.minute - 60 if slot_date.minute != 0 else 0
   data['ohx'] = 0
   data['oqc'] = 0

   # Round all values to 4 decimals
   data = data.round(4)

   radar_header = get_radar_header(radar)

   # Unpack data to LETKF array (wk)
   data = df2wk_radar(ctlg, data, model)

   # Write binary file one record at a time
   with open(filename, 'wb') as f:
      f.write(radar_header.ravel())
      f.write(data.ravel())


def read_letkf_dat(filename):
   '''
   Read LETKF binary file into a pd.DataFrame
   '''
   import pandas as pd
   import numpy as np

   # Create a dtype array with the binary data format and the desired column names
   dtype='float32'
   dt = np.dtype([('HEADER1', dtype), ('ID_VAR', dtype), ('Lon', dtype), ('Lat', dtype), ('Lev', dtype), ('Val', dtype), ('Err', dtype), ('ID_OBS', dtype), ('Tdif', dtype), ('Ohx', dtype), ('Oqc', dtype), ('HEADER2', dtype)])

   # Read binary file
   data = np.fromfile(filename, dtype=dt)

   # Create pandas dataframe, set column names and remove HEADER columns
   df = pd.DataFrame(data, columns=dt.names)
   df = df.drop(['HEADER1', 'HEADER2'], axis=1)

   return df

def read_letkf_dat_radar(filename):
   '''
   Read LETKF binary file into a pd.DataFrame
   '''
   import pandas as pd
   import numpy as np

   # Create a dtype array with the binary data format and the desired column names
   dtype='float32'
   dt = np.dtype([('HEADER1', dtype), ('ID_VAR', dtype), ('Azim', dtype), ('Elev', dtype), ('Rang', dtype), ('Val', dtype), ('Err', dtype), ('ID_OBS', dtype), ('Tdif', dtype), ('Ohx', dtype), ('Oqc', dtype), ('HEADER2', dtype)])

   # Radar header
   with open (filename, 'r') as f:
      header = np.fromfile(f, dtype=dtype, count=1)
      lon = np.fromfile(f, dtype=dtype, count=1)
      header = np.fromfile(f, dtype=dtype, count=1)

      header = np.fromfile(f, dtype=dtype, count=1)
      lat = np.fromfile(f, dtype=dtype, count=1)
      header = np.fromfile(f, dtype=dtype, count=1)

      header = np.fromfile(f, dtype=dtype, count=1)
      lev = np.fromfile(f, dtype=dtype, count=1)
      header = np.fromfile(f, dtype=dtype, count=1)

      # Read binary file
      data = np.fromfile(f, dtype=dt)
   
   # Create pandas dataframe, set column names and remove HEADER columns
   df = pd.DataFrame(data, columns=dt.names)
   df = df.drop(['HEADER1', 'HEADER2'], axis=1)

   return df, [lon, lat, lev]

def read_letkf_dat_2(filename):
   '''
   Read LETKF binary file into a pd.DataFrame
   '''
   import pandas as pd
   import numpy as np

   # Create a dtype array with the binary data format and the desired column names
   dtype='float32'
   dt = np.dtype([('HEADER1', dtype), ('ID_VAR', dtype), ('Lon', dtype), ('Lat', dtype), ('Lev', dtype), ('Val', dtype), ('Err', dtype), ('ID_OBS', dtype), ('STD', dtype), ('HEADER2', dtype)])

   # Read binary file
   data = np.fromfile(filename, dtype=dt)

   # Create pandas dataframe, set column names and remove HEADER columns
   df = pd.DataFrame(data, columns=dt.names)
   df = df.drop(['HEADER1', 'HEADER2'], axis=1)

   return df

###############
### PARSERS ###
###############
def parse_date(args, fmt = '%Y%m%d%H%M%S'):
   '''
   args: list with elements [yyyy, mm, dd, hh, nn, ss]. Minimum length: 3
   return: datetime object
   '''
   import sys
   from datetime import datetime

   # Check number of args and set to 00 if missing
   #nargs = len(args)
   #for i in range(nargs+1, 7):
   #   args.append('00')
   #
   # Get date and convert to datetime object 
   date = args[0]  #('').join(args)
   return datetime.strptime(date, fmt)

## funcion para parsear archivos de texto con lineas de ancho fijo
## cols debe contener una lista del ancho de cada columna
def parse_fileFW(filename,cols,colname=None,skipH=0):
   res=[]
   with open(filename,encoding="latin-1") as file:
      for _ in range(skipH):
        file.readline()
      for line in file.readlines():
        row=[]
        last=0
        for i in cols:
           row.append(line[last:last+i].strip())
           last+=i
        res.append(row)
   res=pd.DataFrame(res)
   if not colname is None:
       res.columns = colname
   return res

## funcion para parsear archivos de texto con  expresiones regulares
## cols debe contener una lista del ancho de cada columna
def parse_fileRE(filename,rex,colname=None,skipH=0):
   res=[]
   with open(filename,encoding="latin-1") as file:
      for _ in range(skipH):
        file.readline()
      for line in file.readlines():
        try:
                 row=re.match(rex,line).groups()
                 res.append(row)
        except:
                print(f"ERROR: linea mal formada: {line}", file = sys.stderr)
   res=pd.DataFrame(res)
   if not colname is None:
       res.columns = colname
   return res

## funcion para parsear archivos de texto con separdor de campo 
## cols debe contener una lista del ancho de cada columna
def parse_fileFS(filename, fs, colname=None, skipH=0):
   res=[]
   with open(filename,encoding="latin-1") as file:
      for _ in range(skipH):
        file.readline()
      for line in file.readlines():
        #row=re.findall(f'(\w+){fs}*', line).groups()
        row=re.split(f'[{fs}]+',line)
        if row[-1]=="":
           row=row[:-1]
        res.append(row)
   res = pd.DataFrame(res)
   if res.empty: return res

   if colname is not None:
       res.columns = colname

   return res

################
### DOWNLOAD ###
################

def download_from_podaac(date, collection, outdir, box = None, provider = None, extension = None, timeout = None):

   import subprocess, shlex

   podaac_cmd = f'podaac-data-subscriber -c {collection} -d {outdir} -sd {date:%Y-%m-%dT%H:%M:%SZ}'
   if box:
      lon_w = box['lon_w']
      lon_e = box['lon_e']
      lat_s = box['lat_s']
      lat_n = box['lat_n']
      podaac_cmd += f' -b="{lon_w},{lat_s},{lon_e},{lat_n}"'

   if provider:
      podaac_cmd += f' -p {provider}'

   if extension:
      podaac_cmd += f' -e .hdf'

   CP = subprocess.run(shlex.split(podaac_cmd), capture_output = True, text = True, timeout = timeout)
   EC = CP.returncode
   if EC != 0:
      print(CP.stderr, file = sys.stderr)

   return EC


##############
### OTHERS ###
##############
def measure_time(fn, *args, **kwargs):
   ''' Measure execution time '''
   import time
   tini = time.time()
   fn(*args, **kwargs)
   return float('{:.4f}'.format(time.time()-tini))


# Signal handler
def handler(signum, frame):
   raise TimeoutError('WALLTIME reached, killing process.')


def set_walltime(walltime):
    import signal
    signal.signal(signal.SIGALRM, handler)
    signal.alarm(walltime)  # Seconds before killing the process by walltime

