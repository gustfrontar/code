
import numpy as np
import pygrib 
import datetime as dt



lat_estacion=-38.5
lon_estacion=298.0
nom_estacion='BAHIA'

variables=['ugrd_80m','vgrd_80m']

fechaini=dt.datetime.strptime( '2008052000' , '%Y%m%d%H')
fechafin=dt.datetime.strptime( '2010043000' , '%Y%m%d%H')

dtini=dt.timedelta( seconds=86400 )  #Time delta entre 2 inicializaciones del pronostico.

dtout=dt.timedelta( seconds= 10800 ) #Time delta entre 2 outputs del forecast.

datapath='/home/jruiz/share/DATOS_REFORECAST/'

#Numero de puntos de grilla que vamos a tomar.
#La caja quedaria de box_size*2 + 1 , box_size*2 + 1
box_size=5
nt=17

ninit=int( ( fechafin - fechaini ).total_seconds() / dtini.total_seconds()  )


local_dat = np.zeros( ( box_size*2 + 1 , box_size*2 + 1 , ninit , nt ) )


#Loop en tiempo.


for variable in variables :

   fechacurr=fechaini

   asciiout = open(nom_estacion + '_' + variable + '.txt', 'w')

   #Loop en tiempo
   it = 0

   while ( fechacurr <= fechafin )  :

     fecha_grib = dt.datetime.strftime( fechacurr , '%Y%m%d%H')

     print( fecha_grib )

     if fechacurr == fechaini  :

        #Obtengo las lats y lons
        grib_file = datapath + '/' + variable + '/' + variable + '_' + fecha_grib + '_mean.grib2'

        print( grib_file )

        data_handler = pygrib.open( grib_file )

        data_handler.seek(0) 

        my_var = data_handler[1]

        print( my_var )

        tmp_var = data_handler.message(1)

        lat , lon = tmp_var.latlons()

        #Obtengo el punto de reticula mas cercano a la estacion.

        d= np.power( lat - lat_estacion , 2 ) + np.power( lon - lon_estacion , 2 )

        center_i , center_j = np.where( d == np.min(d) ) 

        #Obtengo los indices 

        max_i = int( center_i[0] + box_size )
        min_i = int( center_i[0] - box_size )
        max_j = int( center_j[0] + box_size )
        min_j = int( center_j[0] - box_size )

        local_lon = lon[min_i:max_i+1,min_j:max_j+1]
        local_lat = lat[min_i:max_i+1,min_j:max_j+1]

        #Write the header of the output file

        ndata = np.size( local_lon )

        tmp_lon = np.reshape( local_lon , (np.shape( local_lon )[0] * np.shape( local_lon )[1] ) )
        tmp_lat = np.reshape( local_lat , (np.shape( local_lon )[0] * np.shape( local_lon )[1] ) )

        asciiout.write( 'lon ' )
        for ii in range( np.size(tmp_lon) )  :
            asciiout.write( '%4.1f ' %tmp_lon[ii] )
        asciiout.write( '\n' )
        asciiout.write( 'lat ' )
        for ii in range( np.size(tmp_lat) )  :
            asciiout.write( '%4.1f ' %tmp_lat[ii] )
        asciiout.write( '\n' )


     #Ahora leo y escribo los pronosticos

     grib_file = datapath + '/' + variable + '/' + variable + '_' + fecha_grib + '_mean.grib2'

     data_handler = pygrib.open( grib_file )

     data_handler.seek(0)

     fechafor = fechacurr


     for ivar in range( nt )  :

        tmp_var = data_handler.message(ivar+1).values

        #tmp_var = data_handler.select( name = my_var )[ivar]

        local_var = tmp_var[min_i:max_i+1,min_j:max_j+1]

        tmp = np.round( 10.0*np.reshape( local_var , (np.shape( local_var )[0] * np.shape( local_var )[1] ) ) )/10.0

        asciiout.write( fecha_grib + ' ' + dt.datetime.strftime( fechafor , '%Y%m%d%H') + ' ')

        for ii in range( np.size(tmp_lon) )  :
           #asciiout.write( str( tmp[ii] ) + ' ' )
           asciiout.write( '%4.1f ' %tmp[ii] )

        asciiout.write( '\n' )

        fechafor = fechafor + dtout


     fechacurr = fechacurr + dtini 

     it = it + 1

   asciiout.close()

print('Finish')







