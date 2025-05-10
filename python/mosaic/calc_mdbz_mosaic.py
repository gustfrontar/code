# ---------------------------------------------------
# Compute radar reflectivity mosaic in cartesian grid
# ---------------------------------------------------
import sys, os, re, pyart, glob, time
import numpy as np
from datetime import datetime, timedelta
from pandas import date_range
import utils as utrad

np.seterr(divide = 'ignore') 

tini = time.time()
print('--- Hello from {} ---'.format(os.path.basename(__file__)))

# Input parameters
INIDATE = datetime.strptime(sys.argv[1], '%Y%m%d_%H%M%S') # Initial date

# Main directories
RADARDIR = os.environ['DATADIR']
OUTPUTDIR = re.sub('radar_qc', 'mdbz_mosaic', RADARDIR)
os.makedirs(OUTPUTDIR, exist_ok=True)

# Set variables
TYPE = 'qc_new'		# Radar qc file format
REF_NAME = 'cref'	# Reflectivity variable name
FREQ = '300S'  # Model output frequency (seconds)
TDIFF = 300     # Maximum time tolerance (seconds)
GRID_ORI = (-33, -61.)		# SAP.SMN grid origin (LAT, LON)
GRID_RES = (500, 2e3, 2e3)	# SAP.SMN grid resolution (V, H, H)
GRID_DIM = (30, 850, 550)	# SAP.SMN grid dimension (V, H, H)
REF_MIN = -0.
ENDDATE = INIDATE + timedelta(hours=7)
print(ENDDATE)

# Compute grid limits
zlim = [(0., GRID_DIM[0] * GRID_RES[0])]  
yxlim = [(-(dim-1)/2 * res, (dim-1)/2 * res) for dim, res in zip(GRID_DIM[1:], GRID_RES[1:])]  
grid_lim = tuple(zlim + yxlim)

# Select available radar files
print('Getting available radar files')
files = utrad.get_radar_files(RADARDIR, [INIDATE, ENDDATE], TYPE)
if files.empty: sys.exit()
print(files)

# Loop over time
dates = date_range(start=INIDATE, end=ENDDATE, freq=FREQ)
for date in dates:

   print(date)

   # Loop over radars
   radars, gatefilters = [], []
   for radar_id in sorted(files.ID.unique()):
      print(radar_id) 
      # Get radar data 
      radar_files = files[files.ID == radar_id].reset_index(drop=True) 
      radar = utrad.get_radar_data(date, radar_files, TDIFF, TYPE, REF_NAME)
      if radar is None: continue
      print(radar)

      # Convert dbz to power for grid interpolation
      radar = utrad.add_power_field(radar, REF_NAME, REF_MIN)

      # Remove gate transition and mask values 
      gatefilter = pyart.filters.GateFilter(radar)
      gatefilter.exclude_transition()
      gatefilter.exclude_masked('Zpower')

      # Append to list
      radars.append(radar)
      gatefilters.append(gatefilter)

   if not radars: continue

   # Interpolate to cartesian grid
   grid = pyart.map.grid_from_radars(radars, fields=['Zpower'],
             grid_limits=grid_lim, grid_shape=GRID_DIM, 
             grid_origin=GRID_ORI, grid_origin_alt=0.,
             gatefilters=gatefilters,
             gridding_algo='map_gates_to_grid',
             map_roi=False,
             roi_func='dist', z_factor=0.005, xy_factor=0.010, min_radius=200.,
             weighting_function='Barnes2')

   # Compute colmax and convert to dbz
   colmax = np.max(grid.fields['Zpower']['data'], axis=0)
   colmax = utrad.dbz_power(colmax, 'from')
   ref = utrad.dbz_power(grid.fields['Zpower']['data'], 'from')

   # Set fill value
   colmax = np.ma.filled(colmax, REF_MIN)
   ref = np.ma.filled(ref, REF_MIN)

   # Get grid coordinates
   lons, lats = grid.get_point_longitude_latitude(edges=False)
   levs = np.arange(grid_lim[0][0], grid_lim[0][1], GRID_RES[0])

   # Save to npz file
   os.makedirs(OUTPUTDIR, exist_ok=True)
   with open('{}/mdbz_mosaic.{}.npz'.format(OUTPUTDIR, date.strftime('%Y%m%d_%H%M%S')), 'wb') as f:
      np.savez_compressed(f, lon=lons, lat=lats, lev=levs, DBZ=ref, MDBZ=colmax)

time = float('{:.4f}'.format(time.time()-tini))
print('Execution Time: {} seconds'.format(time))
