
import glob
import numpy as np
import numpy.ma as ma
import matplotlib
import matplotlib.pyplot as plt
import rvd_read as rr
import pyart

################################################################################
######                         LECTURA DE DATOS                           ######
################################################################################

#DataPath = '/media/lucho/Seagate Backup Plus Drive/data/radar/EZE/20170109/rvd/'
DataPath = './'
FilelistRVD = np.sort(glob.glob(DataPath+'ar1.cz240*.z.rvd'))

nfile=0
fname = FilelistRVD[nfile]
print(fname)

radar_lat = -34.787778
radar_lon = -58.536667
radar_alt = 0

radar = rr.rvd_read( fname , radar_lon , radar_lat , radar_alt )

################################################################################
######          Calculo de CAPPIs usando "grid_from_radars"               ######
################################################################################

gatefilter = pyart.filters.GateFilter(radar)

# Generamos una reticula con resolucion de 1 km en la horizontal y 0.5 km
# en la vertical
grid_zh = pyart.map.grid_from_radars(
             (radar, ),
             fields=['reflectivity'],
             grid_shape=(40, 480, 480),
             gatefilters=(gatefilter, ),
             grid_limits=((0,20000), (-240000., 240000.), (-240000., 240000.)),
             gridding_algo="map_gates_to_grid",
             map_roi=True,
             roi_func='dist', z_factor=0.015, xy_factor=0.010, min_radius=200.)

colmax = np.max(grid_zh.fields['reflectivity']['data'], axis=0)
colmax = ma.masked_where(colmax < 2, colmax)
ma.set_fill_value(colmax, -999)

################################################################################
######             Figura multipanel con cortes verticales                ######
################################################################################

# Donde queremos hacer los cortes verticales?
xvcut = 0
yvcut = 0

x_data = grid_zh.point_x['data'][0,:,:]
y_data = grid_zh.point_y['data'][0,:,:]

ixvcut = np.where(abs((x_data/1000)-xvcut) == abs((x_data/1000)-xvcut).min())[1][0]
iyvcut = np.where(abs((y_data/1000)-yvcut) == abs((y_data/1000)-yvcut).min())[0][0]

# creamos la figura
font = {'size': 12}
matplotlib.rc('font', **font)
fig = plt.figure(figsize=[12, 6])
plt.rcParams['axes.facecolor'] = '0.6'

# panel sizes
map_panel_axes   = [0.055, 0.055, 0.500, 0.900]
x_cut_panel_axes = [0.550, 0.055, 0.440, 0.400]
y_cut_panel_axes = [0.550, 0.550, 0.440, 0.400]
# map_panel_axes   = [0.10, 0.00, 0.5, 0.90]
# x_cut_panel_axes = [0.60, 0.00, 0.5, 0.40]
# y_cut_panel_axes = [0.60, 0.50, 0.5, 0.40]

# Parametros
zlevel = 2 # => Nivel del CAPPI a graficar
vmin   = -10
vmax   = 60

# Panel 1: COLMAX

ax1 = fig.add_axes(map_panel_axes)

plt.pcolormesh(grid_zh.point_x['data'][0,:,:]/1000,
               grid_zh.point_y['data'][0,:,:]/1000,
               colmax,
               #grid_zh.fields['reflectivity']['data'][2,:,:],
               cmap='nipy_spectral',
               vmin=vmin,
               vmax=vmax)
plt.colorbar(shrink=0.7,pad=0.01)

plt.xlim([-240,240])
plt.ylim([-240,240])

plt.plot(0,0,'+k',ms=10)

# Dibujo lineas donde estoy haciendo los cortes verticales ...
plt.plot([-240,240],[yvcut,yvcut],'--k',lw=1.5)
plt.plot([xvcut,xvcut],[-240,240],'--k',lw=1.5)

# Agregamos anillos radiales ...
for radio in [60,120,180,240]:
    theta = np.linspace(0, 2*np.pi, 100)
    x1 = radio*np.cos(theta)
    x2 = radio*np.sin(theta)
    plt.plot(x1,x2,'-k',lw=0.5)

plt.title('Producto COLMAX', fontsize=14)

# Panel 2: corte vertical zonal (Sur-Norte)
ax2 = fig.add_axes(y_cut_panel_axes)
plt.pcolormesh(grid_zh.point_x['data'][1,iyvcut,:]/1000,
               grid_zh.point_z['data'][:,iyvcut,0]/1000,
               grid_zh.fields['reflectivity']['data'][:,iyvcut,:],
               cmap='nipy_spectral',
               vmin=vmin,
               vmax=vmax)
#plt.plot([-240,240],[Hiso,Hiso],'--k',lw=2)   # Altura isoterma 0degC
plt.xlim([-240,240])
plt.ylim([0,20])
plt.title('Corte Oeste-Este', fontsize=14)

# Panel 3: corte vertical meridional (Oeste-Este)
ax3 = fig.add_axes(x_cut_panel_axes)
plt.pcolormesh(grid_zh.point_x['data'][0,ixvcut,:]/1000,
               grid_zh.point_altitude['data'][:,ixvcut,0]/1000,
               grid_zh.fields['reflectivity']['data'][:,:,ixvcut],
               cmap='nipy_spectral',
               vmin=vmin,
               vmax=vmax)
#plt.plot([-240,240],[Hiso,Hiso],'--k',lw=2)   # Altura isoterma 0degC
plt.xlim([-240,240])
plt.ylim([0,20])
plt.title('Corte Sur-Norte', fontsize=14)

# Guardamos la figura ...
#fig.savefig('radar_'+radar_name+'_'+date_file+'T'+time_file+'Z_ZH_VCUT.png', bbox_inches='tight', dpi=100)

plt.show()

################################################################################
