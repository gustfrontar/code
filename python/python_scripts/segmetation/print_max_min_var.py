import glob
import numpy as np
import os
from netCDF4 import Dataset
from wrf import (getvar, to_np, get_cartopy, latlon_coords, vertcross,
                         cartopy_xlim, cartopy_ylim, interpline, CoordPair)


exp_path="/home/jruiz/share/EXPERIMENTS/EXPERIMENT_RELAMPAGO/2018120500/" #path donde estan los archivos de salida

file_list=glob.glob( exp_path + '/WRF/wrfout*')

file_list.sort()

var = 'wa'

file_list = file_list[180:]


for filename in file_list: 

    wrf_file=Dataset(filename)
    
    my_var = to_np( getvar(wrf_file, var , timeidx=None) )

    print( var ,'max ', my_var.max() , ' min ', my_var.min() , ' ' ,filename)
    
