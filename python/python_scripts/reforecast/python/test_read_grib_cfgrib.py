##############
## Cargo librerias

import cfgrib
import numpy as np
import os
import gc
import itertools
from datetime import datetime
from datetime import timedelta
from numpy import loadtxt
import matplotlib.pyplot as plt

lonmax = 360
lonmin = 320
latmax = -40
latmin = -60

lrfactor  = 8  #Cantidad de puntos de reticula que se toman para promediar (8 implica que pasamos de 0.25 de resolucion a 2 grado de resolucion)


filename = '../pres_msl/pres_msl_2005122400_p01.grib2'


print('Leyendo : ' + filename )
ds=cfgrib.open_datasets(filename)

pres_msl = ds[0].msl.data[0,:,:]

latitude  = ds[0].latitude.data
longitude = ds[0].longitude.data

[lon,lat] = np.meshgrid( longitude , latitude )

ind_lon = np.where( np.logical_and( longitude >= lonmin , longitude <= lonmax ) )[0]
ind_lat = np.where( np.logical_and( latitude  >= latmin , latitude  <= latmax ) )[0]

ilon = ind_lon.min()
elon = ind_lon.max()
ilat = ind_lat.min()
elat = ind_lat.max()

pres_msl = ds[0].msl.data[0,ilat:elat,ilon:elon]
lon = lon[ilat:elat,ilon:elon]
lat = lat[ilat:elat,ilon:elon]

[ny,nx]=pres_msl.shape
lrny = int( ny/lrfactor )
lrnx = int( nx/lrfactor )

pres_msl_lowres = np.asarray( [np.nanmean( pres_msl[i*lrfactor:(i+1)*(lrfactor-1)+i,j*lrfactor:(j+1)*(lrfactor-1)+j]) for i in range(lrny) for j in range(lrnx)] ).reshape(lrny,lrnx)
lon_lowres = np.asarray( [np.nanmean( lon[i*lrfactor:(i+1)*(lrfactor-1)+i,j*lrfactor:(j+1)*(lrfactor-1)+j]) for i in range(lrny) for j in range(lrnx)] ).reshape(lrny,lrnx)
lat_lowres = np.asarray( [np.nanmean( lat[i*lrfactor:(i+1)*(lrfactor-1)+i,j*lrfactor:(j+1)*(lrfactor-1)+j]) for i in range(lrny) for j in range(lrnx)] ).reshape(lrny,lrnx)

plt.figure()
plt.subplot(1,2,1)
plt.pcolor( lon , lat , pres_msl )

plt.subplot(1,2,2)
plt.pcolor( lon_lowres , lat_lowres , pres_msl_lowres )

plt.show()

