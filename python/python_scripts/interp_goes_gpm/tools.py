import numpy as np
import netCDF4 as nc
import goes_module as goes




def load_gpm( filename ) :


    data_gpm = dict()

    gpmds = nc.Dataset( filename )

    data_gpm['RainRate'] = gpmds['FS_SLV_precipRateESurface'][:,:]
    data_gpm['Latitude'] = gpmds['FS_Latitude'][:,:]
    data_gpm['Longitude']= gpmds['FS_Longitude'][:,:]
    data_gpm['HailMask'] = gpmds['FS_Experimental_flagGraupelHail'][:,:]
    data_gpm['Year']     = gpmds['FS_ScanTime_Year'][:]
    data_gpm['Month']    = gpmds['FS_ScanTime_Month'][:]
    data_gpm['Day']      = gpmds['FS_ScanTime_DayOfMonth'][:]
    data_gpm['Hour']     = gpmds['FS_ScanTime_Hour'][:]
    data_gpm['Minute']   = gpmds['FS_ScanTime_Minute'][:]
    data_gpm['Second']   = gpmds['FS_ScanTime_Second'][:]
    data_gpm['FileName'] = filename
    data_gpm['Nx']       = data_gpm['Latitude'].shape[0]
    data_gpm['Ny']       = data_gpm['Latitude'].shape[1]
  


    return data_gpm


def interp_goes_to_gpm( filename , conf ) :


    #1) Open GPM data file.
    #2) Split file into sectors.
    #3) Select the sectors within the area. 
    #4) Loop over the sectors. 
    #5) For each sector search for the closest GOES image.
    #6) Download corresponding GOES data
    #7) Interp GOES data to the selected sector. 

    minlat = conf['MinLat']
    minlon = conf['MinLon']
    maxlat = conf['MaxLat']
    maxlon = conf['MaxLon']

    imsize = conf['ImSize']

    overlap = conf['Overlap']

    halo = conf['Halo']

    #Get GPM data
    data_gpm = load_gpm( filename ) 

    #Start the subsampling loop. 
    s_index = 0
    e_index = imsize
    rainrate=list()
    hailmask=list()
    goes=list()
    latitude=list()
    longitud=list()
    year=list()
    month=list()
    day=list()
    hour=list()
    minute=list()
    second=list()
    
    while ( e_index < data_gpm['Ny'] ) : 

      #Get the center lon and lat of the subdomain & check if its within the selected region.
      clon = np.mean( data_gpm['Longitude'][s_index:e_index,:] )
      clat = np.mean( data_gpm['Latitude'][s_index:e_index,:] )

      if ( clat > minlat & clat < maxlat & clon > minlon & clon < maxlon ) :

         c_index = round( 0.5 * ( s_index + e_index ) )
         c_date  = str(data_gpm['Year'][c_index])  + str(data_gpm['Month'][c_index]).zfill(2) 
                 + str(data_gpm['Day'][c_index]).zfill(2) + str(data_gpm['Hour'][c_index]).zfill(2)
                 + str(data_gpm['Minute'][c_index]).zfill(2) + str(round(data_gpm['Second'])[c_index]).zfill(2)
          


         

       





    





