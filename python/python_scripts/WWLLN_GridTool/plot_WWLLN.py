import numpy as np
from datetime import datetime
from datetime import timedelta
from matplotlib import pyplot
from matplotlib.cm import get_cmap
from matplotlib.colors import from_levels_and_colors , ListedColormap , BoundaryNorm
import matplotlib.patches as mpatches
import cartopy
from cartopy import crs
from cartopy.feature import NaturalEarthFeature, COLORS

date = '201001010000'

#Load data
print('Reading the data')
path_data = '/home/jruiz/share/DATA/WWLLN/'

my_file = path_data + '/griddata/DataWWLLNGrid_' + date + '.npz'

mydata = np.load( my_file )

data = mydata['grid']
lon  = mydata['lon_grid']
lat  = mydata['lat_grid']

colors_fr = [  
             (255/255,255/255,255/255), #   0.0 -   2.5 mm
             (234/255,246/255,254/255), #   2.5 -   5.0
             (214/255,226/255,253/255), #   5.0 -  10.0
             (139/255,178/255,251/255), #  10.0 -  20.0
             (114/255,134/255,242/255), #  20.0 -  30.0
             (  0/255,158/255, 49/255), #  30.0 -  40.0
             (  0/255,188/255, 75/255), #  40.0 -  50.0
             (174/255,209/255,118/255), #  50.0 -  70.0
             (175/255,249/255,122/255), #  70.0 - 100.0
             (254/255,249/255, 72/255), # 100.0 - 150.0
             (255/255,162/255, 51/255), # 150.0 - 200.0
             (239/255,  0/255, 26/255), # 200.0 - 250.0
             (197/255,  0/255, 21/255), # 250.0 - 300.0
             (135/255,  0/255, 15/255), # 300.0 - 400.0
             (  0/255,  0/255,  0/255)  # 400.0 - 500.0
            ]
cmap_fr = ListedColormap(colors_fr)
bounds_fr = np.array([0,1.0,2.5,5,7.5,10,15.0,20,30,40,50,70,100,150,200,250]) # 15 niveles
norm_fr = BoundaryNorm(boundaries=bounds_fr, ncolors=15)

# Defino los limites de paises y provincias (descarga por unica vez)
costas = cartopy.feature.NaturalEarthFeature(
                                             category='physical',
                                             name='coastline',
                                             scale='10m',
                                             facecolor='none'
                                            )

paises = cartopy.feature.NaturalEarthFeature(
                                             category='cultural',
                                             name='admin_0_countries',
                                             scale='10m',
                                             facecolor='none'
                                            )

prov   = cartopy.feature.NaturalEarthFeature(
                                             category='cultural',
                                             name='admin_1_states_provinces_lines',
                                             scale='10m',
                                             facecolor='none'
                                            )

rivers = cartopy.feature.NaturalEarthFeature(
                                             category='physical',
                                             name='rivers_lake_centerlines',
                                             scale='10m',
                                             facecolor='none',
                                            )

lakes = cartopy.feature.NaturalEarthFeature(
                                             category='physical',
                                             name='lakes',
                                             scale='10m'
                                            )



# Download and add the states and coastlines
#states = NaturalEarthFeature(category="cultural", scale="10m",
#                             facecolor="none",name="admin_1_states_provinces_shp")

fig = pyplot.figure(figsize=(8,6))
ax = pyplot.axes(projection=crs.PlateCarree()) #creeo los ejes

img=ax.pcolormesh( lon,lat,np.sum(data[:,:,:,0],axis=2).T,vmin=bounds_fr.min(),vmax=bounds_fr.max(),cmap=cmap_fr,norm=norm_fr)
pyplot.colorbar(img, fraction=0.02, pad=0.01, aspect=20)


ax.add_feature(paises, linewidth=.5, edgecolor="black")

# AGREGAMOS LINES Y MARCAS LAT/LON
gl = ax.gridlines(
                  crs=crs.PlateCarree(),
                  color='gray',
                  linestyle='--',
                  linewidth=0.1,
                  xlocs=np.arange(-180, 180, 5),
                  ylocs=np.arange(-90, 90, 5),
                  draw_labels=True
                 )
gl.top_labels = False
gl.right_labels = False

ax.plot([-73.0,-34.0],[4.0,4.0],'-b')
ax.plot([-73.0,-34.0],[-34.0,-34.0],'-b')
ax.plot([-73.0,-73.0],[-34.0,4.0],'-b')
ax.plot([-34.0,-34.0],[-34.0,4.0],'-b')



pyplot.show()


