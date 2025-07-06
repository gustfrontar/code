import os
import time
from datetime import datetime , timedelta 
from netCDF4 import Dataset
import s3fs
import gc
import numpy as np
from pyproj import Proj

box_size = 8

def time_from_filename( filename , lat = None ) :

    #lat es un parametro optativo. Si esta presente, trato de buscar el tiempo de la imagen goes 
    #mas cercano a la latitud ingresada. Esto se hace asumiendo que 90N es el tiempo inicial del GOES 
    #y 90S el tiempo final y asumiendo una progresion lineal (algo sobresimplificado). 
    my_file = os.path.basename( filename )  #Si pasan un path completo esta linea se queda solo con el nombre del archivo.

    #Convierto a objeto fecha sacando los segundos.
    date_ini = datetime.strptime( my_file[27:38] , '%Y%j%H%M' )
    date_end = datetime.strptime( my_file[43:54] , '%Y%j%H%M' )
 
    #Calculo la fecha central de la imagen (corresponde al tiempo de adquisicion del ecuador)
    date_center = date_ini + 0.5*( date_end - date_ini )

    if lat is None : 
        date_lat = date_center
    else           :
        factor = 1.0 - ( lat + 90.0 ) / 180.0  #Hago un mapeo lineal muy grosero para aproximar el tiempo en el que el GOES captura la imagen a una dada latitud.
        date_lat =  date_ini + factor*( date_end - date_ini )


    return date_ini , date_end , date_center , date_lat


def busca_goes( datei , datee , product='ABI-L2-CMIPF' , band='C13' ) :
    fs = s3fs.S3FileSystem(anon=True)
    #datei , datee: la fecha en yyyymmddhhMMSS del inicio y fin sobre el cual queremos buscar las imagenes.
    #product: el producto que necesitamos (ej. ABI-L2-CMIPF)
    #band: ej la banda  C13

    #Definimos un delta de tiempo de 1 hora que es como estan agrupados los archivos en el servidor.
    dt = timedelta(hours=1)

    #Inicializamos la variable de salida.
    file_list = []  #Esta lista contiene los nombres de las imagenes goes dentro del periodo seleccionado

    #Convertimos la fecha inicial y final del periodo en objetos fecha.
    #Los objetos fecha permiten operar muy facilmente, sumando y restando fechas o incrementandolas en un delta.
    #Podemos pasar de fecha a string y de string a fecha usando las funciones del modulo datetime
    date_ini = datetime.strptime( datei , '%Y%m%d%H%M%S')
    date_end = datetime.strptime( datee , '%Y%m%d%H%M%S')

    current_date_loop = datetime.strptime(datei[0:10], '%Y%m%d%H') - dt  #Le agrego una hora al periodo definido por datei,datee
    final_date_loop   = datetime.strptime(datee[0:10], '%Y%m%d%H') + dt

    #Loop sobre las horas en el periodo de busqueda.
    while current_date_loop <= final_date_loop :
       #print( current_date_loop )

       datetuple = current_date_loop.timetuple()
       julday    = str( datetuple.tm_yday ).zfill(3)   #Obtengo el dia juliano en formato de 3 digitos.
       year      = str( datetuple.tm_year ).zfill(4)   #Obtengo el anio en formato de 4 digitos
       hour      = str( datetuple.tm_hour ).zfill(2)   #Obtengo la hora en formato de 2 digitos.

       #Busco el bloque de archivos correspondientes a esta hora.
       #Interrogamos al servidor de amazon para saber que archivos G16 hay para este anio, dia juliano y hora.
       try :
           #print('Pepe')
           files = np.array(fs.ls('noaa-goes16/'+product+'/'+year+'/'+julday+'/'+hour+'/'))
           #print(files)
           #Hago un for sobre todos los archivos encontrados y me voy a quedar solamente con los que correspondan a la banda
           #seleccionada.
           for my_file in files : 
               #print( my_file )
               #Me fijo si el nombre del archivo contiene al string que identifica la banda
               if band in my_file :
                   #print( band )
                   #Obtengo la fecha de inicio, fin de toma de la imagen y la fecha central.
                   [file_date_ini,file_date_end,file_date_center,file_date_lat] = time_from_filename( my_file )

                   #Me fijo si la fecha central esta dentro del periodo solicitado
                   if file_date_center > date_ini and file_date_center < date_end : 
                       #Si esta dentro del periodo solicitado agrego el nombre del archivo a la lista.
                       file_list.append( my_file )
       except :
           print('Warning: No files found for noaa-goes16/'+product+'/'+year+'/'+julday+'/'+hour+'/') 
   

       current_date_loop = current_date_loop + dt

    #Devuelvo la lista con todos los archivos GOES dentro del periodo solicitado.
    return file_list



def lonlat2xy( date_in , lon_est , lat_est ) :
    fs = s3fs.S3FileSystem(anon=True)
    #date_in: the goes file date (string yyyymmddhhmn ) 
    #lon_est: longitud of the stations (vector)
    #lat_est: latitude of the stations (vector)

    #==================================================================================================================#
    # DATA DOWNLOAD
    #==================================================================================================================#
    #Get date range to search for goes data   
    date_ini = ( datetime.strptime( date_in, '%Y%m%d%H%M') - timedelta(hours=1) ).strftime('%Y%m%d%H%M') 
    date_end = ( datetime.strptime( date_in, '%Y%m%d%H%M') + timedelta(hours=1) ).strftime('%Y%m%d%H%M')

    #Get all the files between date_ini and date_end 
    file_list = busca_goes( date_ini , date_end ) 

    file_name = file_list[0] #We don't care about the exact date. 

    print( file_name )
    my_file= fs.open( file_name )
    goesds = Dataset( file_name.split('/')[-1] , memory=my_file.read())

    #TBdata = goesds.variables['CMI']
    goes_data_shape = goesds.variables['CMI'].shape
    # Convert lat/lon to grid-coordinates (ideally this will be run only once )
    print('Computing x,y from lat,lon')

    H = goesds.variables['goes_imager_projection'].getncattr('perspective_point_height')
    lon_0 = goesds.variables['goes_imager_projection'].getncattr('longitude_of_projection_origin')
    sat_sweep = goesds.variables['goes_imager_projection'].getncattr('sweep_angle_axis')
    x = goesds.variables['x'][:] * H
    y = goesds.variables['y'][:] * H
    xv, yv = np.meshgrid( np.array(x) , np.array(y) )
    # Doc: https://proj.org/operations/projections/geos.html
    geo = Proj( proj='geos', h=H , lon_0=lon_0 , sweep=sat_sweep )
    # Lo que sigue son las matrices de latitud y longitud para graficar
    lon_goes , lat_goes = geo(xv, yv, inverse=True)
    x_goes = np.zeros( lat_est.shape ).astype(int)
    y_goes = np.zeros( lat_est.shape ).astype(int)
 
    for iest in range( lat_est.shape[0] ) :

       dist = ( lon_goes - lon_est[iest] ) ** 2 + ( lat_goes - lat_est[iest] ) ** 2
       min_index = np.where( dist == dist.min())
       x_goes[iest] = int( min_index[0][0] )
       y_goes[iest] = int( min_index[1][0] )
         
    print('Finish computing x,y from lat,lon') 
    goesds.close()  #Closing the dataset to free memory. 
    gc.collect()    #Garbage collector just in case.
    return x_goes , y_goes 


def goes2station( args ):
    fs1 = s3fs.S3FileSystem(anon=True)

    date_in = args[0]
    OutFile = args[1]
    x_goes = args[2]
    y_goes = args[3]

    #print( date_in ) 
    #print( OutFile )
    #print( x_goes )
    #print( y_goes )
    #date_in: the goes file date (string yyyymmddhhmn )
    #OutFile: Name of the output file.
    #x_goes: x position of each station in the goes grid
    #y_goes: y position of each station in the goes grid

    #==================================================================================================================#
    # DATA DOWNLOAD
    #==================================================================================================================#
    if os.path.isfile( OutFile )  :
      print('This date has been procesesd')
      return 

    #Define the output in case goes data is not available. 
    TBloc = np.nan + np.zeros( len( x_goes ) )
    TBreg  = np.nan + np.zeros( len( x_goes ) )

    #Get date range to search for goes data   
    date_ini = ( datetime.strptime( date_in, '%Y%m%d%H%M') - timedelta(hours=1) ).strftime('%Y%m%d%H%M') 
    date_end = ( datetime.strptime( date_in, '%Y%m%d%H%M') + timedelta(hours=1) ).strftime('%Y%m%d%H%M')
 
    #print( date_ini , date_end )
    #Get all the files between date_ini and date_end 
    file_list = busca_goes( date_ini , date_end ) 
    #print( file_list )
    if len( file_list ) > 0 : 

       #Get the file which is closest to the given date (yyyymmddhhmn). 
       diff_time = np.zeros( len( file_list ) ) 
       #For each file in the list, compute the time distance with the station data. 
       for ii , ifile in enumerate( file_list ) :
         sdate , edate , cdate , ldate = time_from_filename( ifile , lat = None )
         diff_time[ii] = np.abs( ( cdate -  datetime.strptime( date_in, '%Y%m%d%H%M') ).total_seconds() )
       #Get the index of the list corresponding to the minimum time-distance
       mini = np.argmin( diff_time )
       #Get the file corresponding to the minimum time-distance 
       file_name = file_list[mini]   

       print( file_name  )

       if ( diff_time[mini] < 600 ) :

          my_file= fs1.open( file_name )
          goesds = Dataset( file_name.split('/')[-1] , memory=my_file.read())

          #==================================================================================================================#
          # GET TB DATA
          #==================================================================================================================#

          ###  AHORA QUIERO CALCULAR UN ÁREA ALREDEDOR DE MI EMA PARA CONSIDERAR TORMENTAS QUE SE MOVIERON HACIA LA EMA EN ESE PERIODO DE 10 MIN.
          # un desplazamiento máximo de una tormenta convectiva fuerte con una velocidad de 100 km/h.
          # Aproximo 1 pixel a resolución de 2 km.
          # Extraer la región de datos
          data =  goesds.variables['CMI'][:,:]
          goes_data_shape = data.shape
          for ii in range( len( x_goes ) ) : 
             xmin = max( x_goes[ii] - box_size , 0 ) 
             xmax = min( x_goes[ii] + box_size , goes_data_shape[0] )
             ymin = max( y_goes[ii] - box_size , 0 )
             ymax = min( y_goes[ii] + box_size , goes_data_shape[1] )
                     
             TBreg[ii] = np.nanmin( data[xmin:xmax,ymin:ymax] ) #TB de una region.
             TBloc[ii] = ( ( data[ x_goes[ii] , y_goes[ii] ] ) ) # dato TB del punto mas cercano a la estacion
          goesds.close()  #Closing the dataset to free memory. 
          gc.collect()    #Garbage collector just in case.  

       else  :

          print('Could not find goes data for this time ' , date_in )

    np.savez_compressed( OutFile , TBreg=TBreg , TBloc=TBloc )
   
    return    


