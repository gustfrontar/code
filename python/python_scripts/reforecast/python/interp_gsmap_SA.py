import gsmap_tool as gsmap 
import numpy as np
import glob
import xesmf as xe
import matplotlib.pyplot as plt
from datetime import datetime
import os
#Nombre de la carpeta donde se almacenara los archivos
folder_output = "/home/fernando.huaranca/datosmunin/Gsmap_24hs"

#Creacion de carpeta si no existe 
os.makedirs(folder_output, exist_ok=True)

data_path ='/home/fernando.huaranca/datos/Datos_GSMAP/daily_G_v8'

#Gsmap subset
lon_min=200.0
lon_max=340.0
lat_min=-60.0
lat_max=20.0



file_list = glob.glob( data_path + '/gsmap_gauge.*.0.1d.daily.00Z-23Z.v8.0000.0.dat.gz' )

#Porcentaje
total = len(file_list)
print('Cantidad de archivos a procesar: ',total)

j = 0


for ifile , my_file in enumerate( file_list ) :
    j = j+1
    porcentaje = (j/total)*100
    print('El porcentaje es: ',porcentaje,' %')
    
    print(ifile,my_file)

    if ifile == 0 :
      #For the first file select the subset of the GSMap data to be used.
      #This will speed up the interpolation. 
      lon_gsmap_raw , lat_gsmap_raw = gsmap.get_gsmap_latlon( )
      index_lon = np.where( (lon_gsmap_raw >= lon_min) & (lon_gsmap_raw <= lon_max) )[0]
      index_lat = np.where( (lat_gsmap_raw >= lat_min) & (lat_gsmap_raw <= lat_max) )[0]
      lon_gsmap = lon_gsmap_raw[ index_lon.min():index_lon.max() ]
      lat_gsmap = lat_gsmap_raw[ index_lat.min():index_lat.max() ] 
      #Get the input lon , lat in the appropiate format
      lon_gsmap_b = np.append( lon_gsmap - 0.1/2.0 , lon_gsmap[-1] + 0.1/2.0 )
      lat_gsmap_b = np.append( lat_gsmap - 0.1/2.0 , lat_gsmap[-1] + 0.1/2.0 )
      grid_in ={"lon":lon_gsmap,"lat":lat_gsmap,"lon_b":lon_gsmap_b,"lat_b":lat_gsmap_b}
      #Get the output lon , lat in the appropiate format
      grid_gfs = np.load( './grid.npz' )
      lon_gfs = grid_gfs['longitudes_gfs']
      lat_gfs = grid_gfs['latitudes_gfs']
      lon_gfs_b = np.append( lon_gfs - 0.1/2.0 , lon_gfs[-1] + 0.1/2.0 )
      lat_gfs_b = np.append( lat_gfs - 0.1/2.0 , lat_gfs[-1] + 0.1/2.0 )
      grid_out ={"lon":lon_gfs,"lat":lat_gfs,"lon_b":lon_gfs_b,"lat_b":lat_gfs_b}
      #Get the interpolator
      interpolator = xe.Regridder( grid_in , grid_out , "conservative")

    qpe_gsmap_raw = gsmap.read_gsmap( my_file )

    #Matriz bidimensional de precipitaciones diarias lo multiplicamos
    #por 24 dado que teniamos mm/h
    qpe_gsmap = qpe_gsmap_raw[ index_lat.min():index_lat.max() , index_lon.min():index_lon.max() ] * 24 #horas
    print( qpe_gsmap.shape , lon_gsmap.shape , lat_gsmap.shape )

   
    qpe_gsmap_out = interpolator( qpe_gsmap ) 

    #Get the output filename
    nombre = my_file.split('.')[2]
    nombre = datetime.strptime(nombre, '%Y%m%d')

    # Formatear la fecha como YYYY-MM-DD
    nombre = nombre.strftime('%Y-%m-%d')
    outfile = f'{folder_output}/Gsmap_R0.25_24hs_{nombre}.npz'

    np.savez(outfile ,pp_daily= qpe_gsmap_out, 
             longitudes = lon_gfs,
             latitudes = lat_gfs ) 

print('Proceso completado!')