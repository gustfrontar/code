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

file_list=glob.glob( exp_path + '/WRF/wrfout*')

#print(file_list)

file_list.sort()

#file_list=file_list[300 : 302]

#print(file_list)


dbz_levels = np.arange(5., 75., 5.)

dbz_rgb = np.array([[4,233,231],[1,159,244], [3,0,244],
                        [2,253,2], [1,197,1],
                        [0,142,0], [253,248,2],
                        [229,188,0], [253,149,0],
                        [253,0,0], [212,0,0],
                        [188,0,0],[248,0,253],
                        [152,84,198]], np.float32) / 255.0
#

dbz_map, dbz_norm = from_levels_and_colors(dbz_levels, dbz_rgb, extend='max')


for filename in file_list: 

    print(filename)
    
    

    wrf_file=Dataset(filename)
    
    mdbz = getvar(wrf_file, "mdbz", timeidx=None)
    
    w = getvar(wrf_file, 'wa')
    
    w=to_np(w)

    mw = np.max(w,0)

    lats,lons= latlon_coords(mdbz)

    cart_proj=get_cartopy(mdbz)

    fig = pyplot.figure(figsize=(8,6))

    ax = pyplot.axes(projection=cart_proj) #creo los ejes

    # Download and add the states and coastlines
    states = NaturalEarthFeature(category="cultural", scale="10m",
                                         facecolor="none",name="admin_1_states_provinces_shp")
    ax.add_feature(states, linewidth=.5, edgecolor="black")
    ax.coastlines('50m', linewidth=0.8)
    
    pyplot.contourf(to_np(lons), to_np(lats), to_np(mdbz),
                         transform=crs.PlateCarree(),
                                      cmap=dbz_map, levels=dbz_levels, extend='both')

 # Add a color bar
    pyplot.colorbar(ax=ax, shrink=.98)

    pyplot.contour(to_np(lons), to_np(lats), mw,
                        transform=crs.PlateCarree(),levels=[5,10,20,30])


    # Set the map bounds
    ax.set_xlim(cartopy_xlim(mdbz))
    ax.set_ylim(cartopy_ylim(mdbz))

    # Add the gridlines
    ax.gridlines(color="black", linestyle="dotted")

    

    file_nombre=os.path.basename(filename)

    file_nombre=file_nombre[11:]

    file_nombre=file_nombre.replace(':','_')

    pyplot.title('Max Dbz ' + file_nombre)

    pyplot.savefig( exp_path +  '/figuras/mbdz' + file_nombre + '.png', dpi=None, facecolor='w', 
        edgecolor='w',
                 orientation='portrait', papertype=None, format=None,
      transparent=False, bbox_inches=None, pad_inches=0.1,
                                   frameon=None, metadata=None)



    #pyplot.show()

    pyplot.close()
