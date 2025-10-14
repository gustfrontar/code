#Este es un codigo que tiene como input archivos .grib y como ouput
#archivos de tipo npz. que almacenan la precipitacion diaria.
#Esto se logra extrayendo de cada archivo grib los pronosticos
#correspondientes a 6hs 12hs 18hs y 24hs, y se los suma obteniendo
#el pronostico de pp acumulada de nuestro grib para las primeras
#24 hs. El nombre que recibira el archivo sera el de 
#la fecha de nuestro grib en formato DD-MM-YYYY

#----------------------------------------------------
#Los npz seran arrays de tipo:

#----Un array que almacena la pp acumulada de sudamerica en 24hs
#----Un array que contiene las latitudes de sudamerica
#----Un array que contiene las longitudes de sudamerica
#----Un array que contiene la fecha a la que se inicio el pronostico
#----Un array que contiene la fecha 24 hs posterior = nombre del archivo

#----------------------------------------------------------------

#Librerias

import os
import glob
import xarray as xr
import numpy as np
import pandas as pd
import os
from datetime import datetime

#-------------------------------------------------------------
#Input: Archivos grib con los que trabajaremos

folder = '/home/fernando.huaranca/datos/DATOS_REFORECAST/apcp_sfc'

#Nombre de la carpeta donde se almacenara los archivos
folder_output = "/home/fernando.huaranca/datosmunin/GFS_24hs"

#Creacion de carpeta si no existe 
os.makedirs(folder_output, exist_ok=True)

#Solo lee los archivos que terminan en extension .grib2
FileS = glob.glob(f'{folder}/*.grib2')
print('Cantidad de archivos a procesar: ',len(FileS))

#FileS = FileS[0:50] #esto es para probar despues borrar

#Lista para almacenar archivos erroneos

fallidos = []

#Porcentaje
i = 0
total = len(FileS)

print('Lista de archivos cargada')

#Variables booleana para ayudar al bucle
mi_inicio = True

#----Limites para el subset-------------------------------

#Latitudes y longitudes (box)
lat_north = 15
lat_south = -59
lon_east = 330
lon_west = 260

#---TIEMPOS---------------------------
#Steps de los pronosticos para formar 24 hs
StepS = [1,3,5,7]  #06 utc + 12utc + 18utc + 00utc
#suma = np.sum( var[Steps, lon_ini:lon_fin , lat_ini:lat_fin ] )
#----------

for file in FileS:

    path = os.path.join(folder,file)

    print('Leyendo archivo: ',file)

    #Iteraciones para visualizar el tiempo de carga del proceso
    i = i + 1 
    porcentaje = (i/total) * 100
    print('El porcentaje es: ',porcentaje,'%')

    try:
        
        #Abrimos el archivo grib como dataset
        ds = xr.open_dataset(path)

        #Extraigo array de precipitaciones
        var = ds.tp.values

        #Al leer el primer archivo mi_inicio es True.
        #A este archivo se le extraen las latitudes y longitudes  
        #que seran las mismas para todos los archivos
        if mi_inicio==True:
            
            print('Inicio extraccion de Latitudes y Longitudes del DS')

            #Extraigo latitudes y longitudes
            lat = ds.tp.latitude.values
            lon = ds.tp.longitude.values

            #Recorta la seccion de sudamerica o la que se desee
            #LATITUDES

            lat_index = np.where((lat <=lat_north) & (lat >= lat_south))[0]


            lat = lat[lat_index]

            #LONGITUDES
            lon_index = np.where((lon >= lon_west) & (lon <= lon_east))[0]

            lon = lon[lon_index]
            

            #Shape para la matriz de ceros
            row = lat.shape[0]
            col = lon.shape[0]

            mi_inicio = False

        #Extraccion de steps en formato de horas
        horas = ds.step.values/np.timedelta64(1, 'h')

        #Extracion fecha de inicio de corrida (nuestro tiempo de inicio)
        ref_time = ds.time.values

        #Cerramos dataset
        ds.close()

        print('Se extrajo correctamente las variables')

        ##########################################################

        #-----------NOMBRE DEL ARCHIVO-----------------------

        #Fecha de pronostico a 24hs, y por facilidad tambien la que ira de nombre
        #del archivo
        fecha_p_24 = ref_time# + np.timedelta64(1, 'D') #lo comente ACORDE A JAXA

        #Transformamos nuestra fecha a string pero nos dara en YYYY-MM-DD y en 
        #formato np.str_
        fecha_p_24 = np.datetime_as_string(fecha_p_24,unit='D')

        ######################################################        
        #Genero una matriz llena de ceros para completarla 

        print('Inicia creacion de matriz de ceros para: ',fecha_p_24)
        pp = np.zeros([row,col])

        print('Finaliza la creacion de matriz para',fecha_p_24)

        ############################################################

        #----ARRAY DE PRECIPITACION ACUMULADA DIARIA----------

        print('Inicio creacion de matriz de pp diaria para: ',fecha_p_24)

        pp = np.sum(var[StepS,
                        lat_index.min():lat_index.max()+1,
                        lon_index.min():lon_index.max()+1],
                        axis=0)
    
        print('Matriz creada correspondiente a',fecha_p_24)

        #----GUARDAR MATRIZ DE PRECIPITACION ACUMULADA-----

        print('Inicia almacenamiento de la matriz: ',fecha_p_24)

        # Ruta completa del archivo donde guardar los datos
        out_path = f'{folder_output}/GFS_R0.25_24hs_{fecha_p_24}.npz'

        # Guardar los arreglos en el archivo
        np.savez(out_path,pp_daily = pp, latitudes = lat,longitudes = lon,inicio_corrida=ref_time,forecast_24=fecha_p_24)

        print('Finaliza almacenamiento de matriz: ',fecha_p_24)

    except Exception as e:
        print(f'Error al procesar el archivo {file}: {str(e)}')
        fallidos.append(file)
        # Aquí puedes almacenar el nombre del archivo con errores en un registro si es necesario.

    #Muestra continuamente por pantalla cuantos errores tuvo
    print('Cantidad de archivos fallados: ',len(fallidos))

    #Muestra continuamente por pantalla la hora actual
    print('La hora actual es: ',datetime.now().time(),' hs')

print('Proceso finalizado')


print(fallidos)
print('Generando un csv con los archivos que fallaron en el proceso')

#Se genera un csv lista con los archivos fallidos
# Crea un DataFrame de Pandas con una sola columna
df = pd.DataFrame({'Archivos': fallidos})

# Guarda el DataFrame en un archivo CSV
nombre_archivo = f'{folder_output}/fallados_GFS.csv'
df.to_csv(nombre_archivo, index=False)

print('CSV generado')

print('PROCESO FINALIZADO!')





       