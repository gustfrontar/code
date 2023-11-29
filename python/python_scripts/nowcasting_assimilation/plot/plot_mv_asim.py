import numpy as np
from datetime import datetime
from datetime import timedelta
import scipy.io as sio
import matplotlib.pyplot as plt
import os
import glob
import pyart
from mpl_toolkits.basemap import Basemap

def pyplot_rings(lon_radar,lat_radar,radius, lat_correction=True):
    """
    Calculate lat-lon of the maximum range ring

    lat_radar : float32
       Radar latitude. Positive is north.
    lon_radar : float32
       Radar longitude. Positive is east.
    radius : float32
       Radar range in kilometers.
    lat_correction: bool
       To apply latitude correction. Default: False
    returns : numpy.array
       A 2d array containing the 'radius' range latitudes (lat) and longitudes (lon)
    """
    import numpy as np

    R = 12742./2.
    m = 2.*np.pi*R/360.
    alfa = np.arange(-np.pi, np.pi, 0.0001)

    lat_radius = lat_radar + (radius/m)*np.sin(alfa)
    if lat_correction:
        lon_radius = lon_radar + ((radius/m)*np.cos(alfa)/np.cos(lat_radius*np.pi/180))
    else:
        lon_radius = lon_radar + (radius/m)*np.cos(alfa)
    return lon_radius, lat_radius

def plot_u(lons, lats, u_motion, v_motion, motion_shaded, num_subplot, Titulo):
    plt.subplot(3, 2, num_subplot)
    levels = [-40, -36, -32, -28, -24, -20, -16, -12, -8, -4, 0, 4, 8, 12, 16, \
    20, 24, 28, 32, 36, 40]
    cs = plt.contourf(lons, lats, motion_shaded, levels, cmap=plt.cm.seismic)
    Q = plt.quiver(lons[::10,::10], lats[::10,::10], u_motion[::10, ::10], \
    v_motion[::10, ::10], pivot='tail', color='k', units='inches', scale=100.0)
#    qk = plt.quiverkey(Q, X=1.025, Y=1.01, U=20,label=r'$20 \frac{m}{s}$')
    lon_Rmax, lat_Rmax = pyplot_rings(-63.983334, -36.5, 240)
    plt.plot(lon_Rmax, lat_Rmax, '-k', lw=1)
    plt.plot(prov[:, 0], prov[:, 1], color='k', linewidth=0.3)
    plt.plot(mun[:, 1], mun[:, 0], color='k', linewidth=0.2)
    plt.xlim(xmin=-66.7458944139, xmax=-61.2207735861)
    plt.ylim(ymin=-38.6583723193, ymax=-34.3128525571)
    plt.title(Titulo)
    cbaxes = fig.add_axes([0.85, 0.2, 0.03, 0.6])
    cb = plt.colorbar(cs, cax=cbaxes)
    if num_subplot==2:
        qk = plt.quiverkey(Q, X=1.22, Y=0.7, U=20, label=r'$20 \frac{m}{s}$')

UNDEF = -999
path_shp_arg='/ms-36/mrugna/RMA/shp_arg'
#PARAMETROS
OBS = 'QC_ANGUIL'
CASE = '20100111'
DATA = '240'
DBZ = 'QC_DATA'
INPUT = 'asim_prueba'
CASE1 = 'fig_assimilation'
OUTPUT = 'dt10'
BASEDIR = '/home/aarruti'
FILEDIR = BASEDIR + '/' + OBS + '/' + CASE + '/' + DATA + '/' + INPUT 
BASEOUTDIR = '/home/aarruti'
FILEOUTDIR = BASEOUTDIR + '/' + OBS + '/' + CASE + '/' + DATA + '/' + INPUT + '/'+  CASE1
if not os.path.exists(FILEOUTDIR):
    os.makedirs(FILEOUTDIR)

lat = np.load(BASEDIR+ '/' + OBS + '/' + CASE + '/' + DATA + '/'+ DBZ + '/lat_lon/latitud_anguil.npy')
lon = np.load(BASEDIR+ '/' + OBS + '/' + CASE + '/' + DATA + '/'+ DBZ + '/lat_lon/longitud_anguil.npy')

nx = 240
ny = 240

mat = sio.loadmat('/export/home/aarruti/mapas.mat')
prov = mat['provincias']
mun = mat['municipios']

formatofecha = '%Y%m%d_%H%M'
FileList = np.sort(glob.glob(FILEDIR+'/*.npz'))
#print(FileList)
endlist = len(FileList)
mv_asim = np.load(FILEDIR + '/asimilacion.npy')

for i in range(1, endlist-1):
    fileread = FileList[i-1]
    fileread1 = FileList[i]
    name = fileread[56:69]
    name1 = fileread1[56:69]
    print(name)
    anio = name[0:4]
#    print(anio)
    mes = name[4:6]
#    print(mes)
    dia = name[6:8]
#    print(dia)
    hora = name[9:11]
#    print(hora)
    minuto = name[11:13]
#   print(minuto)
    
    fecha = dia+'-'+mes+'-'+anio+' ' +hora+':'+minuto

    inidate = datetime.strptime(name, formatofecha)
    inidate1 = datetime.strptime(name1, formatofecha)
    dt_entreimag = inidate1-inidate
    seg_entreimag = timedelta.total_seconds(dt_entreimag)
#    print(seg_entreimag)
    if seg_entreimag>0:
        mv_obs = np.load(FILEDIR + '/motion_'+ name +'_040_MSE_uniform_promaroundmax.npz')
        u_motion_obs = mv_obs['u_motion1']
        v_motion_obs = mv_obs['v_motion1']
        u_motion_obs[u_motion_obs<=UNDEF] = np.nan
        v_motion_obs[v_motion_obs<=UNDEF] = np.nan
        print(np.nanmax(u_motion_obs))
        print(np.nanmax(v_motion_obs))
        u_motion_asim = np.mean(mv_asim[:,:,0,i-1],1).reshape((nx,ny))
        v_motion_asim = np.mean(mv_asim[:,:,1,i-1],1).reshape((nx,ny))
        print(u_motion_asim.max())
        print(u_motion_asim.min())   
        print(u_motion_asim-v_motion_asim)  
        u_motion_dif = u_motion_asim - u_motion_obs
        v_motion_dif = v_motion_asim - v_motion_obs      
"""
        fig = plt.figure(1, figsize=(10,10))
        plt.subplots_adjust(bottom=0.1, hspace=0.3, right=0.8, top=0.9)

        Titulo = 'MV observado: ' + fecha
        plot_u(lon, lat, u_motion_obs, v_motion_obs, u_motion_obs, 1, Titulo)
        Titulo = 'MV observado: ' + fecha
        plot_u(lon, lat, u_motion_obs, v_motion_obs, v_motion_obs, 2, Titulo)
        Titulo = 'Media MV asim: ' + fecha
        plot_u(lon, lat, u_motion_asim, v_motion_asim, u_motion_asim, 3, Titulo)
        Titulo = 'Media MV asim: ' + fecha
        plot_u(lon, lat, u_motion_asim, v_motion_asim, v_motion_asim, 4, Titulo)
        Titulo = 'Diferencia: ' + fecha
        plot_u(lon, lat, u_motion_dif, v_motion_dif, u_motion_dif, 5, Titulo)
        Titulo = 'Diferencia: ' +fecha
        plot_u(lon, lat, u_motion_dif, v_motion_dif, v_motion_dif, 6, Titulo)
        plt.savefig(FILEOUTDIR+ '/VM_asim_'+ name + '.png', dpi=250, format='png', bbox_inches='tight')
#        plt.show()
        plt.close(fig)
"""


