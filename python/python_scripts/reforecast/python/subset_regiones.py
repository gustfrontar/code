#Este es un codigo que realiza un subset de dos regiones.



from periodo_en_comun import devuelve_periodo_comun
import numpy as np
import os
import pandas as pd
import glob

#Paths de archivos de las carpetas
gfs_lista = glob.glob('/home/fernando.huaranca/datosmunin/GFS_24hs/*.npz')
gsmap_lista = glob.glob('/home/fernando.huaranca/datosmunin/Gsmap_24hs/*.npz')

#Archivos comunes entre los dos periodos. 
#En los testeos hay un archivo que no funciona por un error proveniente desde un comienzo, 
#un error de los datos de gfs de 1 dia. 
Files = devuelve_periodo_comun(gfs_lista,gsmap_lista)
Files.remove("2012-05-17") #Borro un archivo que esta erroneo

#----------------------------------------------------------------------------------------

#Variable booleana para que la primera CM 
inicio = True

#Lista donde se almacenan los archivos que fallaron
fallidos = []

#Listas donde se almacenan las matrices

lista_tropical_gfs = []
lista_tropical_gsmap = []
lista_medios_gfs = []
lista_medios_gsmap = []

#Carpetas donde se almacenan los datos de diferentes fuentes
folder_gfs = '/home/fernando.huaranca/datosmunin/GFS_24hs'
folder_gsmap = '/home/fernando.huaranca/datosmunin/Gsmap_24hs'

#-----Seleccionamos Areas------------------

#Area no tropical o de Clima de latitudes medias
lat_north_med = -20
lat_south_med =  -45
long_west_med = 285
long_east_med = 320

#Area Tropical o de Clima de latitudes tropicales
lat_north_trop = 7
lat_south_trop = -18
long_west_trop = 285
long_east_trop = 320


#------------------------------------------------------------------------------
#Extraemos las latitudes y longitudes de un archivo solo y luego usamos eso para
#todos los demas archivos

#Archivo unico
my_path_unico = '/home/fernando.huaranca/datosmunin/Gsmap_24hs/Gsmap_R0.25_24hs_2000-01-01.npz'
my_file_unico = np.load(my_path_unico)

#Extraemos las latitudes y longitudes de 1 archivo solo
latitudes = my_file_unico['latitudes']
longitudes = my_file_unico['longitudes']

#Definimos los indices y regiones 

#AREA DE CLIMA TROPICAL en LATITUDES ALTAS-------------------------
lat_index_trop = np.where((latitudes >= lat_south_trop) & (latitudes<=lat_north_trop))[0]
LAT_trop = latitudes[lat_index_trop] 

long_index_trop = np.where((longitudes >= long_west_trop) & (longitudes<=long_east_trop))[0]
LONG_trop = longitudes[long_index_trop]


#AREA DE CLIMA DE LATITUDES MEDIAS---------------
lat_index_med = np.where((latitudes >= lat_south_med) & (latitudes<=lat_north_med))[0]
LAT_med = latitudes[lat_index_med] 

long_index_med = np.where((longitudes >= long_west_med) & (longitudes<=long_east_med))[0]
LONG_med = longitudes[long_index_med]

#----------------------------------------------------------------------------------
#Para calcular porcentajes
k = 0 

#Cantidad de fechas 
total = len(Files)
print('Cantidad de archivos a leer: ',total)
#-------------------------------------------------------------------------

#Bucle aca le hace esto a cada archivo
for file in Files:

    #-----Porcentaje----------------
    k = k +1
    porcentaje = (k/total)*100
    print(file,'  ',porcentaje,' %')

    #----Paths------------------------
    #Cargamos los archivos 
    path_modelo = os.path.join(folder_gfs,f'GFS_R0.25_24hs_{file}.npz')
    path_observacion = os.path.join(folder_gsmap,f'Gsmap_R0.25_24hs_{file}.npz')

    #---Abrir con numpy los archivos-------------
    #Cargamos los archivos
    file_modelo = np.load(path_modelo)
    file_observacion = np.load(path_observacion)

    #---Cargamos las precipitaciones-----------------
    #Precipitaciones del GFS Modelo
    pp_modelo = file_modelo['pp_daily']
    
    #Precipitaciones del Gsmap Observaciones
    pp_observacion= file_observacion['pp_daily']

   

   #---------Realizamos el SUBSET-------------------------


    
    try:
        

        #----MODELO----------------------------------

        #Definimos la Matriz de Clima_Tropical con GFS
        CLIMA_TROP_gfs = pp_modelo[lat_index_trop.min():lat_index_trop.max()+1,long_index_trop.min():long_index_trop.max()+1]

        lista_tropical_gfs.append(CLIMA_TROP_gfs)

        #Definimos la Matriz de Clima Tropical con Gsmap
        CLIMA_TROP_gsmap = pp_observacion[lat_index_trop.min():lat_index_trop.max()+1,long_index_trop.min():long_index_trop.max()+1]

        lista_tropical_gsmap.append(CLIMA_TROP_gsmap)

        #----OBSERVACION----------------
            
        #Definimos la Matriz de Clima_Tropical con GFS
        CLIMA_MEDIOS_gfs = pp_modelo[lat_index_med.min():lat_index_med.max()+1,long_index_med.min():long_index_med.max()+1]

        lista_medios_gfs.append(CLIMA_MEDIOS_gfs)
        #Definimos la Matriz de Clima Tropical con Gsmap
        CLIMA_MEDIOS_gsmap = pp_observacion[lat_index_med.min():lat_index_med.max()+1,long_index_med.min():long_index_med.max()+1]

        lista_medios_gsmap.append(CLIMA_MEDIOS_gsmap)
        #Agregamos cada matriz
        
    except Exception as e:

        #Muestro mensaje por pantalla
        print(f'Error al procesar el archivo {file}: {str(e)}')

        #Almacena los archivos fallidos en una lista
        fallidos.append(file)

#---------------------------------------------------------------------


#Generamos un array tridimensional de la Region Tropical
gfs_tropical = np.stack(lista_tropical_gfs,axis=0)
gsmap_tropical = np.stack(lista_tropical_gsmap,axis=0)

#Generamos un array tridimensional de la Region de Clima Medios
gfs_medios = np.stack(lista_medios_gfs,axis=0)
gsmap_medios = np.stack(lista_medios_gsmap,axis=0)


#Guardamos los archivos
outpath_tropical = '/home/fernando.huaranca/datosmunin/Regions_R_025_with_grilla/tropical.npz'
outpath_medios = '/home/fernando.huaranca/datosmunin/Regions_R_025_with_grilla/medios.npz'


#NPZ region tropical
np.savez(outpath_tropical,
         pp_tropical_gfs = gfs_tropical,
         pp_tropical_gsmap = gsmap_tropical,
         files=Files,
         LAT=LAT_trop,
         LONG=LONG_trop)

#NPZ region latitudes medias
np.savez(outpath_medios,
         pp_medios_gfs = gfs_medios,
         pp_medios_gsmap = gsmap_medios,
         files=Files,
         LAT=LAT_med,
         LONG=LONG_med)



print('Archivos fallados ',fallidos)

print('Proceso Completado!')