from src.python import io_scale as io
from src.python import common_scale_to_radar as cs2r
from src.python import calc

import conf_real as conf
import os, time, warnings
import matplotlib.pyplot as plt
import datetime as dt
import numpy as np
import pickle as pkl
from pandas import date_range
import matplotlib.pyplot as plt
warnings.filterwarnings("ignore")

# PARAMETERS
EXP = 'RMA1_d2_2km_scalebdy_radar_init18_4D_CTRL'
print(EXP)
TYPE = 'anal'
MEMBER = 'mean'

INIDATE = dt.datetime(2018,11,10,20,5,0)	# Initial time
ENDDATE = dt.datetime(2018,11,10,20,5,0)	# End time
FREQ = '300S'	# Model output frequency (seconds)
TDIFF = 120	# Maximum time tolerance (seconds). If scale time and time in nearest radar data are larger than this the interpolation is not performed.)

# Set variables
MODELDIR = '../../../DATA/EXPS/CORDOBA_20181110_OFP/{}'.format(EXP)
RADARDIR = '../../../DATA/OBS_RADAR/QC'
OUTPUTDIR =  '{}/scale2radar'.format(MODELDIR) 
os.makedirs(OUTPUTDIR, exist_ok=True)

MINREF = 0.0	# Ref values below this threshold will be assumed equal to the threshold

PROJ = {
'type': 'LC',
'basepoint_lon': 295.809,
'basepoint_lat': -31.441,
'basepoint_x': 250000.0,
'basepoint_y': 250000.0,
'LC_lat1': -31.6,
'LC_lat2': -31.4
}

# Get available radar files
print('Getting available radar files')
#files = cs2r.get_radar_files(RADARDIR, [INIDATE, ENDDATE], 'rma')
files = dict() 
files['list'] = ['../../../DATA/OBS_RADAR/QC/cfrad.20181110_200546.RMA1_0301_02.nc']
files['times'] = [dt.datetime(2018, 11, 10, 20, 6, 27, 500000)]
 
#  Read model topography data
sioh = io.ScaleIO('{}/const/topo/topo'.format(MODELDIR) , verbose=0)
topo = sioh.readvar('TOPO', bufsize=2)
io.scale_close(sioh.rootgrps)

# Loop over time
dates = date_range(start=INIDATE, end=ENDDATE, freq=FREQ)
for date in dates:

   print(date)

   # Get radar data 
   radar = cs2r.get_radar_data(date, files, TDIFF, MINREF, 'rma')
   if radar is None: continue
   radar['radar_lat'] = PROJ['basepoint_lat']
   radar['radar_lon'] = PROJ['basepoint_lon'] - 360.

   # Read model data
   sio = io.ScaleIO( '{}/{}/{}/{}/init'.format(MODELDIR, date.strftime('%Y%m%d%H%M%S'), TYPE, MEMBER), bufsize=2)

   # Interpolate model data to radar grid
   radar = calc.radar_int(sio, PROJ, topo, radar) 

   # Mask model data according to radar data
   #radar['model_rv'][radar['rv'].mask] = radar['undef'] 
   radar['model_rv'] = np.ma.masked_values(radar['model_rv'], radar['undef']) 

   #radar['model_ref'][radar['ref'].mask] = radar['undef'] 
   radar['model_ref'] = np.ma.masked_values(radar['model_ref'], radar['undef'])
   mask_ = np.logical_and(radar['model_ref'].mask == False, radar['model_ref'] < radar['minref'])
   radar['model_ref'][mask_] = radar['minref']

   # Save radar structure
   filename = '{}/s2r_{}_{}_{}.pkl'.format(OUTPUTDIR, TYPE, MEMBER, date.strftime('%Y%m%d%H%M%S'))
   filehandler = open(filename, 'wb')
   pkl.dump(radar, filehandler)

   # Close model data
   io.scale_close(sio.rootgrps)
   del sio

k=0
fig, axes = plt.subplots(2,2)

axes[0,0].set_title('MODEL REF')
pc = axes[0,0].pcolormesh(radar['lon_gate'][k,:,:], radar['lat_gate'][k,:,:], radar['model_ref'][k,:,:], vmin=conf.CBAR['dbz']['vmin'], vmax=conf.CBAR['dbz']['vmax'], cmap=conf.CBAR['dbz']['cmap'])
plt.colorbar(pc, ax=axes[0,0])
axes[0,1].set_title('OBS REF')
pc = axes[0,1].pcolormesh(radar['lon_gate'][k,:,:], radar['lat_gate'][k,:,:], radar['ref'][k,:,:], vmin=conf.CBAR['dbz']['vmin'], vmax=conf.CBAR['dbz']['vmax'], cmap=conf.CBAR['dbz']['cmap'])
plt.colorbar(pc, ax=axes[0,1])

axes[1,0].set_title('MODEL DV')
pc = axes[1,0].pcolormesh(radar['lon_gate'][k,:,:], radar['lat_gate'][k,:,:], radar['model_rv'][k,:,:], vmin=conf.CBAR['dv']['vmin'], vmax=conf.CBAR['dv']['vmax'], cmap=conf.CBAR['dv']['cmap'])
plt.colorbar(pc, ax=axes[1,0])
axes[1,1].set_title('OBS DV')
pc = axes[1,1].pcolormesh(radar['lon_gate'][k,:,:], radar['lat_gate'][k,:,:], radar['rv'][k,:,:], vmin=conf.CBAR['dv']['vmin'], vmax=conf.CBAR['dv']['vmax'], cmap=conf.CBAR['dv']['cmap'])
plt.colorbar(pc, ax=axes[1,1])

plt.show()

