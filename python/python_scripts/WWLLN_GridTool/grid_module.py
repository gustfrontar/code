import numpy as np
from datetime  import datetime , timedelta 
import gc

import matplotlib.pyplot as plt

def grid_data(lon_grid,lat_grid,time_res,file_time_res,data,fig_path='./figs/',out_path='./griddata/' , plot_data = False , file_name_prefix='DataWWLLNGrid') :
    #data dict[lon,lat,time,data]
    #Assuming that data in data is sorted in time from the oldest time to the newest.
    
    nitems = data['lon'].size
    nvars  = data['data'].shape[1]

    if ( (file_time_res % time_res) == 0 ) :
       ntimes = int( file_time_res / time_res )
    else  :
       print('Error: file_time_res must be divisible by time_res')
       return

    #Assuming a regular grid
    dx = lon_grid[1] - lon_grid[0]
    dy = lat_grid[1] - lat_grid[0] 

    max_lon = np.max( lon_grid ) 
    min_lon = np.min( lon_grid )
    max_lat = np.max( lat_grid )
    min_lat = np.min( lat_grid )

    #Define the initial file time 
    my_file_time = roundTime( data['date'][0] , roundTo = file_time_res , mode='floor' )
    
    #Memory allocation
    grid = np.zeros((lon_grid.shape[0],lat_grid.shape[0],ntimes,1+nvars))
    ii = 0 
    while ii < nitems   :

        min_file_time = my_file_time
        max_file_time = my_file_time + timedelta( seconds = file_time_res )


        if( (data['lon'][ii] >= min_lon) & (data['lon'][ii] <= max_lon) 
          & (data['lat'][ii] >= min_lat) & (data['lat'][ii] <= max_lat) ) :
        
           xidx = int( np.round( (data['lon'][ii] - lon_grid[0])/dx ) )
           yidx = int( np.round( (data['lat'][ii] - lat_grid[0])/dy ) )

           if ( (data['date'][ii] >= min_file_time) & (data['date'][ii] < max_file_time) ) :

              tidx = int( np.floor( (data['date'][ii] - min_file_time ).total_seconds() / time_res ))
              #We add this event to the current file 
              grid[xidx,yidx,tidx,0] = grid[xidx,yidx,tidx,0] + 1.0 
              grid[xidx,yidx,tidx,:] = grid[xidx,yidx,tidx,:] + data['data'][ii,:] 

              if ii == 0 & tidx > 0 : #This is the first time
                 grid[:,:,0:tidx,:] = np.nan #Previous times are missing.
           
              ii = ii + 1 #The event has been registered go to the next event.

           elif ( data['date'][ii] >= max_file_time ) :
              #Save the data in the current file.
              my_date = min_file_time + timedelta( seconds = time_res )
              date_list = list()
              while my_date <= max_file_time :
                    date_list.append( my_date )
                    my_date = my_date + timedelta( seconds = time_res )
              print('Saving data for '+datetime.strftime( my_file_time , '%Y%m%d%H%M' ) )
              print('A total number of '+ str( np.sum( grid ) ) + ' events have been detected')
              np.savez_compressed( out_path + '/' + file_name_prefix + '_' + datetime.strftime( my_file_time , '%Y%m%d%H%M' ) + '.npz' , grid=grid , date_list = date_list , lon_grid = lon_grid , lat_grid = lat_grid )
              #If requested we plot the total number of events per grid in the file
              if plot_data :
                 tot_data = np.mean( grid[:,:,:,0] ,axis=2 )
                 plt.pcolor( lat_grid , lon_grid , tot_data )
                 plt.savefig( fig_path + '/' + file_name_prefix + '_' + datetime.strftime( my_file_time , '%Y%m%d%H%M' ) + '.png' )

              my_file_time = max_file_time  #Go to the next file time.
              grid[...]= 0.0 #Reset grid
              gc.collect()
              #Do not increment ii since we need to check if it can be added on the next file.

           elif ( data['date'][ii] < min_file_time ) :
              print('Error: time is not order as expected')


        else : 
          ii = ii + 1  #The event is outside the domain. Go to the next event.


    return grid             
        
def roundTime(datein , roundTo=3600 , mode='round') :
   """Round a datetime object to any time lapse in seconds
   dt : datetime.datetime object, default now.
   roundTo : Closest number of seconds to round to, default 1 minute.
   Author: Thierry Husson 2012 - Use it as you want but don't blame me.
   Sligthly modified by Juan Ruiz (don't blame me either)
   """
   seconds = (datein.replace(tzinfo=None) - datein.min).total_seconds()
   if mode=='round' :
      roundseconds = roundTo * np.round( seconds / roundTo )
   elif mode=='floor' :
      roundseconds = roundTo * np.floor( seconds / roundTo )
   elif mode=='ceil'  :
      roundseconds = roundTo * np.ceil( seconds / roundTo )
   
   return datein.min + timedelta(seconds=roundseconds) 
        
