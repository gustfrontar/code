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

exp_path="/home/jruiz/WRF_ELEC_RELAMPAGO/2018120500/" #path donde estan los archivos de salida

input_data = np.load( exp_path + '/postproc/w_mean.npz' )

w_mean=input_data['w_mean']
lats =input_data['lats']
lons =input_data['lons']


pyplot.figure()
pyplot.contourf( to_np(lons) , to_np(lats) , np.max( w_mean , 2) )
pyplot.colorbar()
pyplot.savefig( exp_path + '/figuras/w_mean.png', dpi=None, facecolor='w',
        edgecolor='w',orientation='portrait', papertype=None, format=None,
        transparent=False, bbox_inches=None, pad_inches=0.1,frameon=None, metadata=None)


pyplot.close()




