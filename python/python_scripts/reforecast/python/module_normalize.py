from datetime import datetime as dt
import numpy as np

def normalize_data_seasonal( my_data , normalize_window , ini_date , data_frec ) :

    #Normalize data tacking into account the seasonal cycle.
    #my_data a numpy array with the following dimensions [ny,nx,times]
    #normalize_window the number of days to consider for computing local mean and standard deviation. 
    #ini_date , end_date initial date and final date

    #WARNING: The diurnal cycle is not removed from the data and the climatology is computed only as a function of the day of the year.

    year_length = 366

    

    #Compute the seasonal mean and standard deviation.
    my_data['seasonal_mean'] = np.zeros( ( ny , nx , year_length  ) )
    my_data['seasonal_std']  = np.zeros( ( ny , nx , year_length  ) )

    my_data['data_count'] = np.zeros( ( year_length  ) )
     
    for ii , date in enumerate( my_data['dates'] ) :

       doy = date.timetuple().tm_yday  #Get day of year.

       max_index = int( doy + normalize_window - 1 )
       min_index = int( doy - normalize_window - 1 )

       if max_index > year_length - 1 :
          max_index = int( max_index - year_length ) 
       if min_index < 0 :
          min_index = int( year_length + min_index )  

       if max_index < min_index :

          my_data['seasonal_mean'][0:max_index+1] = my_data['seasonal_mean'][0:max_index+1] + my_data['data'][ii]
          my_data['seasonal_std'][0:max_index+1] = my_data['seasonal_std'][0:max_index+1] + my_data['data'][ii] ** 2
          my_data['data_count'][0:max_index+1] = my_data['data_count'][0:max_index+1] + 1

          my_data['seasonal_mean'][min_index:] = my_data['seasonal_mean'][min_index:] + my_data['data'][ii]
          my_data['seasonal_std'][min_index:] = my_data['seasonal_std'][min_index:] + my_data['data'][ii] ** 2
          my_data['data_count'][min_index:] = my_data['data_count'][min_index:] + 1

       else                     :

          my_data['seasonal_mean'][min_index:max_index+1] = my_data['seasonal_mean'][min_index:max_index+1] + my_data['data'][ii]
          my_data['seasonal_std'][min_index:max_index+1] = my_data['seasonal_std'][min_index:max_index+1] + my_data['data'][ii] ** 2
          my_data['data_count'][min_index:max_index+1] = my_data['data_count'][min_index:max_index+1] + 1

    my_data['seasonal_mean'] = np.where( my_data['data_count'] > 0 , my_data['seasonal_mean'] / my_data['data_count'] , np.nan + np.zeros( ( year_length  ) ) )   

    my_data['seasonal_std']  = np.where( my_data['data_count'] > 0 , ( my_data['seasonal_std'] / my_data['data_count'] - ( my_data['seasonal_mean'] ** 2 ) ) ** 0.5 , np.nan + np.zeros( ( year_length  ) ) )

   
    #Standarize the data using the seasonal mean and standard deviation.

    for ii , date in enumerate( my_data['dates'] ) :

        doy = date.timetuple().tm_yday  #Get day of year.

        #print( ii , my_data['data'][ii] , my_data['seasonal_mean'][doy-1] , my_data['seasonal_std'][doy-1] )

        my_data['data'][ii] = ( my_data['data'][ii] - my_data['seasonal_mean'][doy-1] ) / my_data['seasonal_std'][doy-1]

    return my_data 
