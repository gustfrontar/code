import glob
import numpy as np
import os
from matplotlib import pyplot
from matplotlib.cm import get_cmap
from matplotlib.colors import from_levels_and_colors
import matplotlib.patches as mpatches
from netCDF4 import Dataset
from wrf import (getvar, to_np, get_cartopy, latlon_coords, vertcross,
                         cartopy_xlim, cartopy_ylim, interpline, CoordPair)


exp_path="/home/jruiz/WRF_ELEC_RELAMPAGO/2018120500/" #path donde estan los archivos de salida

file_list=glob.glob( exp_path + '/WRF/wrfout*')

#print(file_list)

file_list.sort()

#file_list=file_list[300 : 302]

#print(file_list)


for it,filename in enumerate( file_list ): 

    print(filename)
    wrf_file=Dataset(filename)
    w = getvar(wrf_file, 'wa')

    if filename == file_list[0]  :
       w_mean = np.zeros( ( np.shape(w)[1] , np.shape(w)[2] , len(file_list) ) )
       lats,lons= latlon_coords(w)
        

    w=to_np(w)
    w_mean[:,:,it] = np.mean(w,0)


np.savez(exp_path + '/postproc/w_mean.npz',w_mean=w_mean,lats=lats,lons=lons)



