import numpy as np
import pandas as pd
import os

def so_th(ctlg, data, lats, lons, levs):
   '''
   data, dataout: pd.DataFrame
   '''
   # Get grid parameters
   dx = ctlg['so/th']['dx']
   dz = ctlg['so/th']['dz']
   grid_ini, grid_dim, grid_res = get_grid(dx, dx, dz, lats, lons, levs)

   # Compute box average
   method = ctlg['so/th']['method']
   if method == 'so':
      dataout = superobbing(grid_ini, grid_dim, grid_res, data)
   elif method == 'th':
      dataout = thinning(grid_ini, grid_dim, grid_res, data)
   else:
      print('Method not coded yet')

   # Remove empty boxes
   dataout = dataout.loc[(dataout != 0).any(axis=1)]
   dataout.reset_index(drop=True, inplace=True)

   if os.environ['DEBUG']: print(' SO In Out: {} {}'.format(data.shape[0], dataout.shape[0]))

   return dataout


def superobbing_radar(ctlg, data, radar_loc, limits):
   '''
   data, dataout: pd.DataFrame
   radar_loc: list with radar longitude, latitude, altitude
   limits: list with maximum range and height (km)
   '''
   # Set variables
   obs_in = data.shape[0]
   dx = ctlg['so/th']['dx']
   dz = ctlg['so/th']['dz'] * 1e3 # (meters)
   maxr, maxz = limits 
   maxz *= 1e3 #(meters)

   # Get grid parameters
   grid_ini, grid_dim, grid_res = get_grid_radar(dx, dz, radar_loc, maxr, maxz)
   #print(radar_loc)

   # Get (i,j,k) of each observation point
   data['k'] = ((data.Lev - grid_ini[2])/grid_res[2]).astype(int) + 1
   data['j'] = ((data.Lat - grid_ini[1])/grid_res[1]).astype(int) + 1
   dlon = grid_res[0]
   if isinstance(dlon, list):
      dlon = dlon[j]
   data['i'] = ((data.Lon - grid_ini[0])/dlon).astype(int) + 1
 
   # Remove points outside grid limits
   data.drop(data[data.k > grid_dim[2]].index, axis=0, inplace=True)
   data.drop(data[data.j > grid_dim[1]].index, axis=0, inplace=True)
   data.drop(data[data.i > grid_dim[0]].index, axis=0, inplace=True)
   data.reset_index(drop=True, inplace=True)
   #print('Antes SO', data.dbz.min(), data.dbz.max())

   # Compute box average (average over time too)

   #Define the sine and cosine of the Azimuth for a proper averaging of Azimuths.
   data.insert(1 , "sinaz" , np.sin( data.Azim.to_numpy()*np.pi/180.0 ) ) #Sin of the Azimuth
   data.insert(1 , "cosaz" , np.cos( data.Azim.to_numpy()*np.pi/180.0 ) ) #Cos of the Azimuth

   dataout = data.groupby(['i', 'j', 'k'], as_index=False).mean(numeric_only=True) 
   count = data.groupby(['i', 'j', 'k'], as_index=False).count()
   std   = data.groupby(['i', 'j', 'k'], as_index=False).std()

   #Recompute the supperobed Azimuth from the supperobed cosaz and sinaz.
   dataout.Azim = np.atan2( dataout.sinaz.to_numpy() , dataout.cosaz.to_numpy() ) * 180.0 / ( np.pi ) 
   dataout.Azim[ dataout.Azim < 0.0 ] = dataout.Azim + 360.0

   #Add the number of observations per grid-box and the intra box standard deviation to the 
   #dataout structure
   for var in ctlg['vars'] :
      if var in dataout.keys() :
         dataout['NObs_'+var] = count[var]
         dataout['STD_'+var]  = std[var]

   if os.environ['DEBUG']: print(' SO In Out: {} {}'.format(obs_in, dataout.shape[0]))
   
   return dataout

def superobbing(grid_ini, grid_dim, grid_res, data):
   '''
   data, dataout: pd.DataFrame
   '''
   nlon = grid_dim[0]
   nlat = grid_dim[1]
   nlev = grid_dim[2]

   dlon = grid_res[0]
   dlat = grid_res[1]
   dlev = grid_res[2]

   inilon = grid_ini[0]
   inilat = grid_ini[1]
   inilev = grid_ini[2]

   data['k'] = np.int32((data.loc[:, 'Lev'] - inilev)/dlev) + 1
   data['j'] = np.int32((data.loc[:, 'Lat'] - inilat)/dlat) + 1
   data['i'] = np.int32((data.loc[:, 'Lon'] - inilon)/dlon[data['j']]) + 1

   # Remove data outside of grid
   inside_of_grid = ((data['i'] < nlon.max()) * (data['i'] >= 0) * (data['j'] < nlat) * (data['j'] >= 0) * (data['k'] < nlev) * (data['k'] >= 0))
   data = data[inside_of_grid]

   # Mean by grid index
   mean_data = data.groupby(['i', 'j', 'k']).mean()

   return mean_data.reset_index(drop = True)

def thinning(grid_ini, grid_dim, grid_res, data):
   '''
   data, dataout: pd.DataFrame
   '''
   nlon = grid_dim[0]
   nlat = grid_dim[1]
   nlev = grid_dim[2]

   dlon = grid_res[0]
   dlat = grid_res[1]
   dlev = grid_res[2]

   inilon = grid_ini[0]
   inilat = grid_ini[1]
   inilev = grid_ini[2]

   data['k_int'] = np.int32((data.loc[:, 'Lev'] - inilev)/dlev) + 1
   data['j_int'] = np.int32((data.loc[:, 'Lat'] - inilat)/dlat) + 1
   data['i_int'] = np.int32((data.loc[:, 'Lon'] - inilon)/dlon[data['j_int']]) + 1

   # Remove data outside of grid
   inside_of_grid = ((data['i_int'] < nlon.max()) * (data['i_int'] >= 0) * (data['j_int'] < nlat) * (data['j_int'] >= 0) * (data['k_int'] < nlev) * (data['k_int'] >= 0))
   data = data[inside_of_grid]

   # Order data by distance to grid center
   data['k'] = ((data.loc[:, 'Lev'] - inilev)/dlev) + 1
   data['j'] = ((data.loc[:, 'Lat'] - inilat)/dlat) + 1
   data['i'] = ((data.loc[:, 'Lon'] - inilon)/dlon[data['j_int']]) + 1

   data[['i', 'j', 'k']] = data[['i', 'j', 'k']] % 1
   data['dist'] = np.sqrt(data['i']**2 + data['j']**2 + data['k']**2)
   data = data.sort_values('dist')

   # select one observation per grid
   thinned_data = data.drop_duplicates(subset = ['i_int', 'j_int', 'k_int'])


   return thinned_data.reset_index(drop = True)


def get_grid(dx, dy, dz, lats, lons, levs):

   # Earth radius (km)
   re = 6371. 

   # Pressure/Height
   dlev = dz
   if levs[0] > levs[1]: 
      lev = np.arange(levs[0], levs[1]-1e-6, -dlev)
      levini = lev[-1]
   if levs[0] < levs[1]:
      lev = np.arange(levs[0], levs[1]+dz-1e-6, dlev)
      levini = lev[0]
   nlev = lev.size

   # Latitude
   dlat = float(np.rad2deg(dx/re))
   lat = np.round(np.arange(lats[0], lats[1]+dlat-1e-6, dlat), 3)
   nlat = lat.size

   # Longitude
   dlon = np.rad2deg(dx/(re*np.cos(np.deg2rad(lat))))
   lon = []
   nlon = []
   for idlon in dlon:
      tmp = np.arange(lons[0], lons[1]+idlon-1e-6, idlon) + 360.
      lon.append(np.round(tmp, 3))
      nlon.append(tmp.size)
   nlon = np.asarray(nlon)

   return (lon[0][0], lat[0], levini), (nlon, nlat, nlev), (dlon, dlat, dlev)

def get_grid_radar(dx, dz, radar, maxr, maxz):

   # Radar position
   radar_lon = radar[0] + 360.
   radar_lat = radar[1]

   # Grid dimensions
   nlev = int(np.ceil(maxz/dz))
   nlat = int(np.ceil(2.*maxr/dx))
   nlon = nlat
   if np.mod(nlat, 2) == 0: 
      nlat += 1
      nlon += 1

   # Grid resolution
   dlev = dz
   dlat, dlon = dx2ddeg(dx, radar_lat)

   # Grid initial point 
   lev0 = 0.
   lat0 = float(radar_lat - dlat * (nlat-1)/2.)
   lon0 = float(radar_lon - dlon * (nlon-1)/2.)

   return (lon0, lat0, lev0), (nlon, nlat, nlev), (dlon, dlat, dlev)

def dx2ddeg(dx, lat_ref):
   ''' Distance diferential to lat/lon degree differential '''

   # Earth radius (km)
   R = 6371.

   dlat = float(np.rad2deg(dx/R))
   dlon = np.rad2deg(dx/(R*np.cos(np.deg2rad(lat_ref))))

   return dlat, dlon
