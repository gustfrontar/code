import os
import time
from datetime import datetime , timedelta 
from netCDF4 import Dataset
import s3fs
import gc
import numpy as np

fs = s3fs.S3FileSystem(anon=True)

def descarga_goes( file_list , dest_dir ) :
    os.makedirs( dest_dir , exist_ok = True )
    for ii in range( len( file_list ) ) :
       filename = os.path.basename( file_list[ii] )
       print('Downloading ' + filename )
       fs.get( file_list[ii] , filename )
       os.rename('./'+filename,dest_dir+'/'+filename )


def time_from_filename( filename , lat = None ) :

    #lat es un parametro optativo. Si esta presente, trato de buscar el tiempo de la imagen goes 
    #mas cercano a la latitud ingresada. Esto se hace asumiendo que 90N es el tiempo inicial del GOES 
    #y 90S el tiempo final y asumiendo una progresion lineal (algo sobresimplificado). 
    my_file = os.path.basename( filename )  #Si pasan un path completo esta linea se queda solo con el nombre del archivo.
    print( my_file , my_file[23:34] )
    #Convierto a objeto fecha sacando los segundos.
    date_ini = datetime.strptime( my_file[23:34] , '%Y%j%H%M' )
    date_end = datetime.strptime( my_file[39:50] , '%Y%j%H%M' )
 
    #Calculo la fecha central de la imagen (corresponde al tiempo de adquisicion del ecuador)
    date_center = date_ini + 0.5*( date_end - date_ini )

    if lat is None : 
        date_lat = date_center
    else           :
        factor = 1.0 - ( lat + 90.0 ) / 180.0  #Hago un mapeo lineal muy grosero para aproximar el tiempo en el que el GOES captura la imagen a una dada latitud.
        date_lat =  date_ini + factor*( date_end - date_ini )

    return date_ini , date_end , date_center , date_lat


def busca_goes( datei , datee , product='ABI-L2-CMIPF' , band='C13' ) :

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
    end_date_loop   = datetime.strptime(datee[0:10], '%Y%m%d%H') + dt

    #Loop sobre las horas en el periodo de busqueda.
    while current_date_loop <= end_date_loop :

       datetuple = current_date_loop.timetuple()
       julday    = str( datetuple.tm_yday ).zfill(3)   #Obtengo el dia juliano en formato de 3 digitos.
       year      = str( datetuple.tm_year ).zfill(4)   #Obtengo el anio en formato de 4 digitos
       hour      = str( datetuple.tm_hour ).zfill(2)   #Obtengo la hora en formato de 2 digitos.

       #Busco el bloque de archivos correspondientes a esta hora.
       #Interrogamos al servidor de amazon para saber que archivos G16 hay para este anio, dia juliano y hora.
       try    : 
          files = np.array(fs.ls('noaa-goes16/'+product+'/'+year+'/'+julday+'/'+hour+'/'))
       except :
          print('Warning: we could not retrieve file list for noaa-goes16/'+product+'/'+year+'/'+julday+'/'+hour+'/' )
          files = np.array([])

       #Hago un for sobre todos los archivos encontrados y me voy a quedar solamente con los que correspondan a la banda
       #seleccionada.
       for my_file in files : 

           #Me fijo si el nombre del archivo contiene al string que identifica la banda
           if ( band in my_file ) or ( band == 'null' ) :

               #Obtengo la fecha de inicio, fin de toma de la imagen y la fecha central.
               [file_date_ini,file_date_end,file_date_center,file_date_lat] = time_from_filename( my_file )

               #Me fijo si la fecha central esta dentro del periodo solicitado
               if file_date_center > date_ini and file_date_center < date_end : 
                  #Si esta dentro del periodo solicitado agrego el nombre del archivo a la lista.
                  file_list.append( my_file )

       current_date_loop = current_date_loop + dt

    #Devuelvo la lista con todos los archivos GOES dentro del periodo solicitado.
    return file_list


def busca_goes_local( datei , datee , product='ABI-L2-CMIPF' , band='C13' , local_dir='/home/jruiz/datosmunin3/datos/Datos_GOES/SouthAmerica/All/IR/' ) :
    import glob
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

    ini_date_loop = datetime.strptime(datei[0:10], '%Y%m%d%H') - dt  #Le agrego una hora al periodo definido por datei,datee
    end_date_loop   = datetime.strptime(datee[0:10], '%Y%m%d%H') + dt
    current_date_loop = ini_date_loop

    #Loop sobre las horas en el periodo de busqueda.
    while current_date_loop <= end_date_loop :

       datetuple = current_date_loop.timetuple()
       julday    = str( datetuple.tm_yday ).zfill(3)   #Obtengo el dia juliano en formato de 3 digitos.
       year      = str( datetuple.tm_year ).zfill(4)   #Obtengo el anio en formato de 4 digitos
       hour      = str( datetuple.tm_hour ).zfill(2)   #Obtengo la hora en formato de 2 digitos.

       #Busco el bloque de archivos correspondientes a esta hora.
       #Interrogamos al servidor de amazon para saber que archivos G16 hay para este anio, dia juliano y hora.
       files = glob.glob(local_dir + '/'+product+'/'+year+'/'+julday+'/'+hour+'/*.nc')

       #Hago un for sobre todos los archivos encontrados y me voy a quedar solamente con los que correspondan a la banda
       #seleccionada.
       for my_file in files :

           #Me fijo si el nombre del archivo contiene al string que identifica la banda
           if ( band in my_file ) or ( band == 'null') :

               #Obtengo la fecha de inicio, fin de toma de la imagen y la fecha central.
               [file_date_ini,file_date_end,file_date_center,file_date_lat] = time_from_filename( my_file[:])
               #Me fijo si la fecha central esta dentro del periodo solicitado
               if file_date_center > ini_date_loop  and file_date_center < end_date_loop :
                  #Si esta dentro del periodo solicitado agrego el nombre del archivo a la lista.
                  file_list.append( my_file )

       current_date_loop = current_date_loop + dt
 
    #Devuelvo la lista con todos los archivos GOES dentro del periodo solicitado.
    return file_list



#-----------------------------------------------------------------------------------------------------------
def get_data_from_aws( yyyymmddhhmn , band='C13' , product_name='ABI-L2-CMIPF' , bucket_name = 'noaa-goes16' ):

  year = datetime.strptime(yyyymmddhhmn, '%Y%m%d%H%M').strftime('%Y')
  day_of_year = datetime.strptime(yyyymmddhhmn, '%Y%m%d%H%M').strftime('%j')
  hour = datetime.strptime(yyyymmddhhmn, '%Y%m%d%H%M').strftime('%H')
  min = datetime.strptime(yyyymmddhhmn, '%Y%m%d%H%M').strftime('%M')

  # AMAZON repository information
  # https://noaa-goes16.s3.amazonaws.com/index.html
  #-----------------------------------------------------------------------------------------------------------
  # File structure
  awspath = f'{bucket_name}/{product_name}/{year}/{day_of_year}/{hour}/'
  time  = f's{year}{day_of_year}{hour}{min}'
  #Get file list from server (in this case we expect only one file)
  files = fs.ls( awspath , prefix=prefix ) 
  print( files )
  files = [i for i in files if band in i]

  print( 'Files ' , files )


  #-----------------------------------------------------------------------------------------------------------
  # Check if there are files available
  if len(files) == 0 :
    # There are no files
    print(f'No files found for the date: {yyyymmddhhmn}, Band-{band}')
    return -1
  else:
    # There are files
    fname = files[0]  #Assuming there is only one filename in the list. 
    my_file=fs.open(fname) 
    goesds = Dataset(fname.split('/')[-1], memory=my_file.read())

  return goesds   #Return the dataset handle directly loaded in memory. 

#-----------------------------------------------------------------------------------------------------------
def goes_lon_lat( nc ) :
    #Given x , y and a netcdf file (to obtain the projection parameters) 
    #compute lat , lon grid. 

    from pyproj import Proj

    sat_h = nc['goes_imager_projection'].perspective_point_height
    #Extraigo la longitud del punto central del disco
    sat_lon = nc['goes_imager_projection'].longitude_of_projection_origin
    #Extraigo el eje de rotacion
    sat_sweep = nc['goes_imager_projection'].sweep_angle_axis
    # Generamos un objeto con la proyeccion del satelite
    p = Proj(proj = 'geos', h = sat_h, lon_0 = sat_lon, sweep = sat_sweep)
    x = nc.variables['x'] * sat_h 
    y = nc.variables['y'] * sat_h
    XX, YY = np.meshgrid(x, y) 
    # Aplicamos la transformacion para obtener las coordenadas en formato de latitud y longitud
    lon_goes, lat_goes = p(XX, YY, inverse=True)

    return lon_goes , lat_goes

# Functions to convert lat / lon extent to array indices
def geo2grid( lon , lat , lon_goes , lat_goes ):

    if isinstance( lon , np.ndarray ) :
       print(lon,lat)
       ndata = len( lat ) 
    else :
       ndata = 1
    x = np.zeros( ndata ).astype(int)
    y = np.zeros( ndata ).astype(int)

    if ndata == 1 : 
       dist = ( lat_goes - lat )**2 + ( lon_goes - lon )**2
       tmp = np.argwhere(dist == np.min(dist))[0]
       x=tmp[0]
       y=tmp[1]
    else : 
       for ii in range( ndata ) :
          dist = ( lat_goes - lat[ii] )**2 + ( lon_goes - lon[ii] )**2 
          tmp = np.argwhere(dist == np.min(dist))[0]
          x[ii]=tmp[0]
          y[ii]=tmp[1] 

    return x , y 

def get_corners( lon_min , lon_max , lat_min , lat_max , nc ) :
    #Given a box in lat lon , provide the circunscripted domain in x , y 

    lon_goes , lat_goes = goes_lon_lat( nc )
     
    x1 , y1 = geo2grid( lon_min , lat_min , lon_goes , lat_goes ) #SW corner
    x2 , y2 = geo2grid( lon_max , lat_min , lon_goes , lat_goes ) #SE corner
    x3 , y3 = geo2grid( lon_min , lat_max , lon_goes , lat_goes ) #NW corner
    x4 , y4 = geo2grid( lon_max , lat_max , lon_goes , lat_goes ) #NE corner

    x_min = min( x1 , x3 )
    x_max = max( x2 , x4 )
    y_min = min( y1 , y2 )
    y_max = max( y3 , y4 )

    return x_min , x_max , y_min , y_max 

# Functiona to convert lat / lon extent to array indices

def goes2station( date_in , lon_est , lat_est , box_size=8 , x_est=None , y_est=None ):
    #date_sat: the goes file date (string yyyymmddhhmn )
    #lon_est: longitudes of stations (numpy 1d array)
    #lat_est: as lon_est but for the latitude.
    #radious: size of the box that will be used to compute the local temperature minimum sourrionding each station.
    #x_est: nearest neigbhour row index in the goes data array corresponding to each station (numpy array)
    #y_est: as x_est but for the y coordinate
    #If x_est and y_est are None (default value) they are computed.

    #==================================================================================================================#
    # DATA DOWNLOAD
    #==================================================================================================================#

    #Define the output in case goes data is not available. 
    TB_station = np.nan + np.zeros( len( lat_est ) )
    TB_region  = np.nan + np.zeros( len( lat_est ) )

    #Get all the files between date_ini and date_end 
    file_list = busca_goes_local( date_in , date_in ) 

    if len( file_list ) > 0 : 

       #Get the file which is closest to the given date (yyyymmddhhmn). 
       diff_time = np.zeros( len( file_list ) ) 
       #For each file in the list, compute the time distance with the station data. 
       for ii , ifile in enumerate( file_list ) :
         sdate , edate , cdate , ldate = time_from_filename( ifile , lat = None )
         diff_time[ii] = np.abs( ( cdate -  datetime.strptime( date_in, '%Y%m%d%H%M%S') ).total_seconds() )
       #Get the index of the list corresponding to the minimum time-distance
       mini = np.argmin( diff_time )
       #Get the file corresponding to the minimum time-distance 
       file_name = file_list[mini]   

       print( file_name )

       if ( diff_time[mini] < 600 ) :

          #my_file= fs.open( file_name )
          goesds = Dataset( file_name )

          #==================================================================================================================#
          # GET TB DATA
          #==================================================================================================================#

          TBdata = goesds.variables['CMI'][:]
          goes_data_shape = TBdata.shape
          
          # Convert lat/lon to grid-coordinates (ideally this will be run only once )
          if x_est is None :
             print('Computing x,y from lat,lon')
             x_est , y_est = geo2grid( lat_est , lon_est )

          # Obtengo el valor de TB en el punto mas cercano a la estacion. 

          ###  AHORA QUIERO CALCULAR UN ÁREA ALREDEDOR DE MI EMA PARA CONSIDERAR TORMENTAS QUE SE MOVIERON HACIA LA EMA EN ESE PERIODO DE 10 MIN.
          # un desplazamiento máximo de una tormenta convectiva fuerte con una velocidad de 100 km/h.
          # Aproximo 1 pixel a resolución de 2 km.
          # Extraer la región de datos
          for ii in range( len( x_est ) ) : 
             xmin = max( x_est[ii] - box_size , 0 ) 
             xmax = min( x_est[ii] + box_size , goes_data_shape[0] )
             ymin = max( y_est[ii] - box_size , 0 )
             ymax = min( y_est[ii] + box_size , goes_data_shape[1] )
        
             TB_region[ii] = np.nanmin( TBdata[xmin:xmax,ymin:ymax] ) #TB de una region.
             TB_station[ii]= TBdata[x_est[ii],y_est[ii]]
          goesds.close()  #Closing the dataset to free memory. 
          gc.collect()    #Garbage collector just in case.  

       else  :

          print('Could not find goes data for this time ' , date_in )


    return TB_station , TB_region , x_est , y_est 


