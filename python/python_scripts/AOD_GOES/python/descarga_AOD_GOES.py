import numpy as np
import os
from netCDF4 import Dataset
from datetime import datetime, timedelta
import goes_module as goes
import gc

#Nombre del dominio a recortar (para generar los archivos correspondientes )
domain_name = 'SA'

#Path con los datos originales de GOES-QPE
goes_file_path = '/home/jruiz/share/AOD_GOES/data/'


date_ini = '2020010600'  #Fecha de inicio de la descarga de datos
date_fin = '2020010700'  #Fecha de fin de la descarga de datos

#Definicion del producto a descargar
goes_product = 'ABI-L2-AODF'
goes_band    = 'null'

#Definicion del dominio a recortar.
lat_min , lat_max , lon_min , lon_max = -60, 10,-80,-30



current_date = datetime.strptime( date_ini , '%Y%m%d%H' )
date_inc = timedelta( days = 1 )

while ( current_date < datetime.strptime( date_fin , '%Y%m%d%H' ) ) :
   #######################################################################
   #Busco los archivos de la base de amazon que tengan la menor distancia al recorte del GPM-DPR

   goes_file_list = goes.busca_goes( datetime.strftime( current_date , '%Y%m%d' ) + '0000' , datetime.strftime( current_date , '%Y%m%d' ) + '2359', goes_product , goes_band )
        
   print('Para la fecha ' + datetime.strftime( current_date , '%Y%m%d' ) + ' Hay ' + str(len(goes_file_list)) + ' archivos.')

   #Hago un loop sobre los archivos que encontre en este dia para descargarlos.
   for ifile , my_file in enumerate( goes_file_list ) : 
       goes.descarga_goes( [ my_file ] , goes_file_path )

#       local_file_name = goes_file_path + '/' + os.path.basename( my_file )
#     
#       nc_goes = Dataset( local_file_name )
#
#       if ifile == 0 :
#          #Determino los indices que definen la porcion de la matriz de datos que contiene
#          #al dominio definido por lon_min , lon_max , lat_min , lat_max
#          x_min , x_max , y_min , y_max = goes.get_corners( lon_min , lon_max , lat_min , lat_max , nc_goes )
#          x = nc_goes.variables['x'][x_min:x_max]
#          y = nc_goes.variables['y'][y_min:y_max]
#          #Obtengo las matrices de longitud y latitud 
#          lon_goes , lat_goes = goes.goes_lon_lat( nc_goes )
#          lon_goes = lon_goes[y_min:y_max,x_min:x_max]
#          lat_goes = lat_goes[y_min:y_max,x_min:x_max]
#
#       print( x_min , x_max , y_min , y_max )
#       # Extraemos el netcdf la banda y la region de interes
#       AOD  = nc_goes.variables['AOD'][y_min:y_max,x_min:x_max]
#       DQF  = nc_goes.variables['DQF'][y_min:y_max,x_min:x_max]
#       AE1  = nc_goes.variables['AE1'][y_min:y_max,x_min:x_max]
#       AE2  = nc_goes.variables['AE2'][y_min:y_max,x_min:x_max]
#       AE_DQF=nc_goes.variables['AE_DQF'][y_min:y_max,x_min:x_max]

        
#       #Cierro el netcdf
#       nc_goes.close()
#       os.remove( local_file_name ) #Eliminamos el archivo netcdf completo.
#        
#       final_goes_file = local_file_name[:-3]+'_'+domain_name+'.nc'
#
#       nc_goes_final = Dataset( final_goes_file , 'w' , format='NETCDF4' )
#
#       #Le doy todos los permisos al archivo
#       #os.system(["chmod", "777", final_goes_file])
#
#       #Creo el netCDF con los datos interpolados
#       nc_goes_final.createDimension('X',lon_goes.shape[0])
#       nc_goes_final.createDimension('Y',lon_goes.shape[1])
#       #nc_goes_final.createDimension('Z',3) #por (X,Y,Z)
#
#       lat=nc_goes_final.createVariable("Latitude",np.float32,('X','Y'),zlib=True)
#       lon=nc_goes_final.createVariable("Longitude",np.float32,('X','Y'),zlib=True)
#       aod=nc_goes_final.createVariable("AOD",np.float32,('X','Y'),zlib=True)
#       dqf=nc_goes_final.createVariable("DQF",np.uint8,('X','Y'),zlib=True) 
#       ae1=nc_goes_final.createVariable("AE1",np.float32,('X','Y'),zlib=True)
#       ae2=nc_goes_final.createVariable("AE2",np.float32,('X','Y'),zlib=True)
#       aedqf=nc_goes_final.createVariable("AE_DQF",np.uint8,('X','Y'),zlib=True)

#       lat[:,:] = lat_goes
#       lon[:,:] = lon_goes
#       aod[:,:] = AOD
#       dqf[:,:] = DQF
#       ae1[:,:] = AE1
#       ae2[:,:] = AE2
#       aedqf[:,:] = AE_DQF
#
#       nc_goes_final.close()
#
   collected = gc.collect()

   current_date = current_date + date_inc 
 
   print("Garbage collector: collected %d objects." % (collected))
