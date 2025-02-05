import glob
import scipy.io as sio
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import rvd_read as rr
import pyart

#========================================================================================#

DataPath = '/home/lucho/trabajo/tesis_pedro/data/radar/EZE/20120404/rvd/ar1.cz240/'
FilelistRVD = np.sort(glob.glob(DataPath+'*.z.rvd'))

nfile=121
fname = FilelistRVD[nfile][len(DataPath):]

radar_lat = -34.787778
radar_lon = -58.536667
radar_alt = 30          # m

radar = rr.rvd_read( fname , radar_lon , radar_lat , radar_alt )

#========================================================================================#

# Coordenadas del centro del area de interes
lat_cen = -34.85
lon_cen = -59.94

# Radio del cilindro en metros
radio_cilindro = 10000.0

# Buscamos los puntos que estan en el cilindro
dlon =  np.cos(radar.gate_latitude['data']*np.pi/180.0)*( radar.gate_longitude['data'] - lon_cen )
dlat =  ( radar.gate_latitude['data'] - lat_cen )
distancia = np.sqrt( ( dlon * 111000.0 )**2 + ( dlat * 111000.0 )**2 )

# Ploteamos la distancia para la primera elevacion y la posicion del centro del cilindro
# y del radar (+)
#plt.figure(figsize=[8,8])
#plt.pcolor(radar.gate_longitude['data'][0:359],radar.gate_latitude['data'][0:359],distancia[0:359,:],cmap='seismic')
#plt.plot(radar.longitude['data'],radar.latitude['data'],'+w')
#plt.plot(lon_cen,lat_cen,'ow')
#plt.grid()

#========================================================================================#

mascara = distancia <= radio_cilindro

#plt.figure(figsize=[8,8])
#plt.pcolor(radar.gate_longitude['data'][0:359],radar.gate_latitude['data'][0:359],mascara[0:359,:].astype(float),cmap='seismic')
#plt.plot(radar.longitude['data'],radar.latitude['data'],'+w')
#plt.plot(lon_cen,lat_cen,'ow')
#plt.grid()

#========================================================================================#

plt.figure(figsize=[10,8])
plt.pcolor(radar.gate_longitude['data'][0:359],
           radar.gate_latitude['data'][0:359],
           radar.fields['reflectivity']['data'][0:359,:],
           vmin=0,
           vmax=60,
           cmap='gist_ncar')
plt.colorbar()
plt.plot(radar.longitude['data'],radar.latitude['data'],'+w')
plt.plot(lon_cen,lat_cen,'+w')
plt.grid()
plt.show()

#========================================================================================#

# Obtenemos los puntos que estan dnetro del cilindro y obtenemos los valores de
# reflectividad, altura, elevacion  para dichos puntos.

#elevations= np.tile( radar.elevation['data'] , (radar.fields['reflectivity']['data'].shape[1] ,1 )).transpose()

ref_cilindro = radar.fields['reflectivity']['data'][ mascara ]
elev_cilindro = elevations[ mascara ]
alt_cilindro  = radar.gate_z['data'][ mascara ]
