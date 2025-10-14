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

#Select area
area_name = 'atlantico_sur'
lonmax = 355
lonmin = 315
latmax = -40
latmin = -60

#Resolution reduction
lrfactor  = 8  #Cantidad de puntos de reticula que se toman para promediar (8 implica que pasamos de 0.25 de resolucion a 2 grado de resolucion)

#Max time (store all time up to max time)
maxtime = 25

#Perturbation
pert = 'p04'  #c00, p01, p02, p03, p04 

#Paths
outpath = '../npz/'
inppath = '../'

#Variable 
var_name = 'pres_msl'  #Notar que el script no es totalmente independiente del nombre de la variable. 

#Dates
idate_str = '2000010100'
edate_str = '2019123100'
init_frec = timedelta( days=1 )

#Outfile
outfile = outpath + '/' + var_name + '_' + area_name + '_' + pert + '.npz'


#filename = '../pres_msl/pres_msl_2005122400_p01.grib2'

idate = datetime.strptime( idate_str , '%Y%m%d%H' )
edate = datetime.strptime( edate_str , '%Y%m%d%H' )

cdate = idate

while( cdate <= edate ) : 

   itime = int( ( cdate - idate ).total_seconds() / init_frec.total_seconds() ) 

   filename = inppath + '/' + var_name + '/' + var_name + '_' + datetime.strftime( cdate , '%Y%m%d%H' ) + '_' + pert + '.grib2'
   print('Leyendo : ' + filename )
   #Abro el archivo grib.
   ds=cfgrib.open_datasets(filename)

   if cdate == idate : 
      print('This is the first iteration, get lat and lon info')
      latitude  = ds[0].latitude.data
      longitude = ds[0].longitude.data
      [lon,lat] = np.meshgrid( longitude , latitude )
      ind_lon = np.where( np.logical_and( longitude >= lonmin , longitude <= lonmax ) )[0]
      ind_lat = np.where( np.logical_and( latitude  >= latmin , latitude  <= latmax ) )[0]
      ilon = ind_lon.min()
      elon = ind_lon.max()
      ilat = ind_lat.min()
      elat = ind_lat.max()

      lon = lon[ilat:elat,ilon:elon]
      lat = lat[ilat:elat,ilon:elon]

      [ny,nx] = lon.shape
      lrny = int( ny/lrfactor )
      lrnx = int( nx/lrfactor )

      lon_lowres = np.asarray( [np.nanmean( lon[i*lrfactor:(i+1)*(lrfactor)+i,j*lrfactor:(j+1)*(lrfactor)+j]) for i in range(lrny) for j in range(lrnx)] ).reshape(lrny,lrnx)
      lat_lowres = np.asarray( [np.nanmean( lat[i*lrfactor:(i+1)*(lrfactor)+i,j*lrfactor:(j+1)*(lrfactor)+j]) for i in range(lrny) for j in range(lrnx)] ).reshape(lrny,lrnx)

      #Allocate memory for the forecast dataset
      nfor = int( ( edate - idate ).total_seconds() / init_frec.total_seconds() ) + 1
      data_lowres = np.ones( ( maxtime , lrny , lrnx , nfor ) )


   #Reduce the resolution of the original data
   data = ds[0].msl.data[0:maxtime,ilat:elat,ilon:elon]
   data_lowres[:,:,:,itime] = np.asarray( [np.nanmean( data[k,i*lrfactor:(i+1)*(lrfactor)+i,j*lrfactor:(j+1)*(lrfactor)+j]) for k in range(maxtime) for i in range(lrny) for j in range(lrnx)] ).reshape(maxtime,lrny,lrnx)

   cdate = cdate + init_frec 

   np.savez( outfile , data = data_lowres , lon = lon_lowres , lat = lat_lowres , idate_str = idate_str , edate_str = edate_str )

   #plt.figure()
   #plt.subplot(1,2,1)
   #plt.pcolor( lon , lat , data[20,:,:] )

   #plt.subplot(1,2,2)
   #plt.pcolor( lon_lowres , lat_lowres , data_lowres[20,:,:,0] )

   #plt.show()

