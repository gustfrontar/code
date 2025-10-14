import glob
import numpy as np
import os
from matplotlib import pyplot
from matplotlib.cm import get_cmap
from matplotlib.colors import from_levels_and_colors
import matplotlib.patches as mpatches
from cartopy import crs
from cartopy.feature import NaturalEarthFeature, COLORS
from netCDF4 import Dataset
from wrf import (getvar, to_np, get_cartopy, latlon_coords, vertcross,
                         cartopy_xlim, cartopy_ylim, interpline, CoordPair)

exp_path="../../2018121100_2K/" #path donde estan los archivos de salida

file_list=glob.glob( exp_path + '/WRF_S91/wrfout*')

file_list.sort()

print( exp_path )

for it,filename in enumerate( file_list ): 

    print(filename)
    wrf_file=Dataset(filename)
    wa = getvar(wrf_file, 'wa') 

    if filename == file_list[0]  :
       w_max = np.zeros( ( np.shape(wa)[1] , np.shape(wa)[2] , len(file_list) ) )
       dbz_max = np.zeros( ( np.shape(wa)[1] , np.shape(wa)[2] , len(file_list) ) )
       lats,lons= latlon_coords(wa)

    w_max[:,:,it] = np.max( to_np( wa ) , 0 )
    dbz_max[:,:,it] = to_np( getvar(wrf_file, 'mdbz') )

np.savez( exp_path + '/postproc/w_max.npz',w_max=w_max,dbz_max=dbz_max,lats=lats,lons=lons)

