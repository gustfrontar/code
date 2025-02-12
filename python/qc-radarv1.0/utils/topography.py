"""
Get topography
"""
import numpy as np
#from scipy.interpolate import interp2d
from scipy.interpolate import RegularGridInterpolator as interp2d
from utils.common_function import common_function as cf
import os
import osgeo.gdal as gdal
import elevation

def get_topography(opt, radar):
    """
    Get topography
    """
    coord_topo_file = get_topography_filename(opt, radar, 'dat')
    raster_topo_file = get_topography_filename(opt, radar, 'tif')
    try:
        topography = read_topography(coord_topo_file)
    except:
        print('An adequate binary file was not found. Preparing to get the data...')
        rlon = radar.longitude['data'][0]
        rlat = radar.latitude['data'][0]
        rrange =  radar.range['data']
        razimuth = radar.get_order_azimuth()
        raster_path = opt.topo_raw_datapath
        topo_file = coord_topo_file 
        topography = generate_topo_file(rlon, rlat, rrange, razimuth, raster_path, topo_file)
    interpolator = get_topography_interpolator(topography)
    
    distance = radar.get_distance()
    az = radar.get_order_azimuth()
    ne = radar.get_ne()
    na = radar.get_na()
    nr = radar.get_nr()
    
    topography_data = np.zeros((na, nr, ne))
    
    for ie in range(0, ne):
        ppi_r = distance[:,ie]
        ppi_a = az
        ppi_rg, ppi_ag = np.meshgrid(ppi_r,ppi_a,indexing='ij')
        topography_data[:,:,ie] = interpolator( ( ppi_rg , ppi_ag ) ).T
    
    return topography_data
      
    
def get_topography_filename(opt, radar, ext):
    topography_path = opt.topo_radar_datapath
    return ('%s%s_%.1f_%.1f.%s')%(topography_path, radar.metadata['instrument_name'], 
                   radar.range['meters_between_gates'], 
                   np.size(radar.range['data'])*radar.range['meters_between_gates'] / 1000, ext)


def read_topography(file):
    with open(file,'r') as f:
        #First read the header containing nr and na
        tmpdata = np.fromfile(f,dtype='f4',count=2)
        na = int(tmpdata[0])
        nr = int(tmpdata[1])

        topography = dict()
        topography['mean'] = np.reshape(np.fromfile(f, dtype='f4', count=nr*na), (na,nr))
        topography['max'] = np.reshape(np.fromfile(f, dtype='f4', count=nr*na), (na,nr))
        topography['min'] = np.reshape(np.fromfile(f,dtype='f4', count=nr*na), (na,nr))
        topography['number'] = np.reshape(np.fromfile(f,dtype='f4', count=nr*na), (na,nr))
        topography['range'] = np.reshape(np.fromfile(f,dtype='f4', count=nr*na), (na,nr))
        topography['azimuth'] = np.reshape(np.fromfile(f,dtype='f4', count=nr*na), (na,nr))
        topography['latitude'] = np.reshape(np.fromfile(f,dtype='f4', count=nr*na), (na,nr))
        topography['longitude'] = np.reshape(np.fromfile(f,dtype='f4', count=nr*na), (na,nr))

    return topography


#Download topography data and generate binary topography file. LO DEJO COMO ESTA, no debería correr siempre.
def generate_topo_file(rlon, rlat, rrange, razimuth, raster_path, topo_file):
    """ Generate topography file"""
    product = 'SRTM1'  #SRTM1 - 30 m res , SRTM3 - 90 m res.

    my_topo = dict()

    [my_topo['range'], my_topo['azimuth']] = np.meshgrid(rrange, razimuth)
    nr = np.size(rrange)
    na = np.size(razimuth)

    #Get lat and lon corresponding to the grid points in the polar coordinate.

    [my_topo['longitude'], my_topo['latitude']] = cf.com_ra_to_ll(cen_lon=rlon, cen_lat=rlat,
                                                               r=my_topo['range'], a=my_topo['azimuth'],
                                                               nr=nr, na=na)

    #Define grid limits
    maxlon = int(np.ceil(np.max(my_topo['longitude'])))
    minlon = int(np.floor(np.min(my_topo['longitude'])))
    maxlat = int(np.ceil(np.max(my_topo['latitude'])))
    minlat = int(np.floor(np.min(my_topo['latitude'])))

    array_size = np.shape(my_topo['range'])

    #Download the data to a raster file.
    #west, south, east, north 

    my_topo['mean'] = np.zeros(array_size, order='F', dtype=np.float32)  
    my_topo['min'] = np.zeros(array_size, order='F', dtype=np.float32)
    my_topo['max'] = np.zeros(array_size, order='F', dtype=np.float32)
    my_topo['number'] = np.zeros(array_size, order='F', dtype=np.int32) 

    #Data is downloaded and processed into 1 deg patches.
    for ilon in range(minlon, maxlon):
        for ilat in range(minlat, maxlat): 

            print('Downloading data for tile Lon=', ilon, ' Lat=', ilat) 

            #If raster file is not present, then download it from the internet.
            raster_file = raster_path + '/' + product + str(ilon) + '_' + str(ilat) + '.tif'
            print('Downloading ' + raster_file)

            if not os.path.isfile(raster_file):
                elevation.clip(bounds=(ilon,ilat,ilon+1,ilat+1), output=raster_file, product=product)

             #Read data from raster file.
            print('Reading ' + raster_file) 
            [raster_lon, raster_lat, raster_data] = read_raster_data(raster_file)
            raster_nx = raster_data.shape[0]
            raster_ny = raster_data.shape[1]

             #Convert raster lat lon to range and azimuth.
            [raster_r, raster_a] = cf.com_ll_to_ra(cen_lon=rlon, cen_lat=rlat,
                                            lon=raster_lon, lat=raster_lat,
                                            nx=raster_nx, ny=raster_ny)

            #Interpolate raster data to polar coordinates surrounding the radar.
            rmin = np.min(rrange)
            dr = rrange[1] - rrange[0]
            amin = np.min(razimuth)
            da = razimuth[1]  - razimuth[0]

            print('Interpolating patch Lon=',ilon,' Lat=',ilat) 
            cf.com_interp_boxavereg(xini=amin, dx=da, nx=na,
                                    yini=rmin, dy=dr, ny=nr,
                                xin=raster_a.reshape( raster_nx * raster_ny ),
                                yin=raster_r.reshape( raster_nx * raster_ny ),
                                datain=raster_data.reshape(raster_nx * raster_ny),
                                nin=raster_nx*raster_ny,
                                data_sum=my_topo['mean'],
                                data_max=my_topo['max'],
                                data_min=my_topo['min'],
                                data_n=my_topo['number'], undef=-999)


    #Compute the mean topography.
    mask = my_topo['number'] > 0
    my_topo['mean'][mask] = my_topo['mean'][mask] / my_topo['number'][mask]

    #Complete missing values using neighborhood values.
    mask = my_topo['number'] == 0  #Identify missing data points
    my_topo['mean'] = cf.com_complete_missing_2d(field=my_topo['mean'],missing_mask=mask,
                                               nx=na,ny=nr,npass=4)
    my_topo['max'] = cf.com_complete_missing_2d(field=my_topo['max'],missing_mask=mask,
                                               nx=na,ny=nr,npass=4)
    my_topo['min'] = cf.com_complete_missing_2d(field=my_topo['min'],missing_mask=mask,
                                               nx=na,ny=nr,npass=4)


    #Write topo file in polar coordintes into binary format.
    #This data will be used for fast interpolation to the corresponding volume elevation angles.
    write_topo(my_topo, topo_file)

    return my_topo

def get_topography_interpolator(topography):
    range = topography['range'][0,:]
    azimuth = topography['azimuth'][:,0]
    mean = topography['mean']
    #return interp2d(range, azimuth, mean, kind='linear')
    return interp2d( (range,azimuth),mean.T,method='linear', bounds_error=False)

def set_topography(distance, azimuth, levels, interpolator):
    return np.array([interpolator(distance[lev,:],azimuth) for lev in levels])


def get_agl_height(altitude, topography):
    return altitude-topography


def get_lat_lon(file):

    raster = gdal.Open(file)

    gt = raster.GetGeoTransform()
    proj = raster.GetProjection()

    xres = gt[1]
    yres = gt[5]

    # get the edge coordinates and add half the resolution 
    # to go to center coordinates
    xmin = gt[0] + xres*0.5
    xmax = gt[0] + (xres*raster.RasterXSize) - xres*0.5
    ymin = gt[3] + (yres*raster.RasterYSize) + yres*0.5
    ymax = gt[3] - yres*0.5

    lon=np.zeros(raster.RasterXSize)
    lat=np.zeros(raster.RasterYSize)

    for ii in range(0, raster.RasterXSize):
       lon[ii] = xmin + ii*xres
    for ii in range(0, raster.RasterYSize):
       lat[ii] = ymax + ii*yres

    raster = None

    # create a grid of xy coordinates in the original projection
    [lon, lat] = np.meshgrid(lon, lat)

    return lon, lat


def read_raster_data(file):
    import osgeo.gdal as gdal
    import numpy as np

    raster = gdal.Open(file)

    nx = raster.RasterXSize
    ny = raster.RasterYSize
    nb = raster.RasterCount

    raster_data = np.zeros((nx, ny, nb))

    #Read the data and store it into a numpy array

    for ib in range(0, nb):

        raster_data[:,:,ib] = raster.ReadAsArray(ib)
    #Get the lat and lons.
    [lon,lat]=get_lat_lon(file)

    return lon , lat , raster_data


def write_topo( my_topo , my_file )           :
    

    f=open(my_file,'w')

    na=my_topo['mean'].shape[0]
    nr=my_topo['mean'].shape[1]

    #First write nr and na
    np.array(na).astype('f4').tofile(f)
    np.array(nr).astype('f4').tofile(f)

    #Write the components of the my_topo dictionary.
    tmp=my_topo['range'].data
    np.reshape( my_topo['mean'].astype('f4') , (nr*na) ).tofile(f)
    np.reshape( my_topo['max'].astype('f4') , (nr*na) ).tofile(f)
    np.reshape( my_topo['min'].astype('f4') , (nr*na) ).tofile(f)
    np.reshape( my_topo['number'].astype('f4') , (nr*na) ).tofile(f)
    try     : 
       tmp=my_topo['range'].data
       np.reshape( tmp.astype('f4') , (nr*na)  ).tofile(f)
    except  :
       tmp=my_topo['range']
       np.reshape( tmp.astype('f4') , (nr*na)  ).tofile(f)
       

    #np.reshape( (my_topo['range'].data).astype('f4') , (nr*na) ).tofile(f)
    np.reshape( my_topo['azimuth'].astype('f4') , (nr*na) ).tofile(f)
    np.reshape( my_topo['latitude'].astype('f4') , (nr*na) ).tofile(f)
    np.reshape( my_topo['longitude'].astype('f4') , (nr*na) ).tofile(f)

    return my_topo




