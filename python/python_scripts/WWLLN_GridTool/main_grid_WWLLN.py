import numpy as np
from datetime import datetime
from datetime import timedelta
import grid_module as gm
from scipy.io import loadmat
import glob

lon_max = -30.0
lon_min = -77.0
lat_max = 8.0
lat_min = -38.8

dt = 1800        #Time resolution [seconds]
file_dt = 86400  #Time resolution for files [seconds]

#Get the grid (lon,lat,time)

lon_grid = 0.0181891675 + 0.0363783345*( np.arange(9869) + 1 )
lon_grid[ lon_grid > 180.0 ] = lon_grid[ lon_grid > 180.0 ] - 360.0
lat_grid = -59.98181083 + 0.0363856885*( np.arange(3298) + 1 )

lon_grid = lon_grid[ ( lon_grid >= lon_min ) & ( lon_grid <= lon_max ) ]
lat_grid = lat_grid[ ( lat_grid >= lat_min ) & ( lat_grid <= lat_max ) ]

#Load data
print('Reading the data')
path_data = '/home/jruiz/share/DATA/WWLLN/'
FileList = np.sort(glob.glob(path_data+'*.mat'))

for ifile , my_file in enumerate( FileList ) :
    if ifile == 0 :
       data_set = np.array( loadmat(my_file)['datos1_w'][:,:] )
    else          :
       data_set = np.concatenate( ( data_set , np.array( loadmat( my_file )['datos1_w'][:,:] ) ) , axis=0)

print('Creating the data dict')
data = dict()
data['lon'] = data_set[:,7]
data['lat'] = data_set[:,6]
data['data'] = np.zeros( ( data_set.shape[0],1 ) )
data['data'][:,0] = data_set[:,9]
data['time'] = np.zeros( data_set.shape[0] )
data['date'] = list()

for ii in range( data_set.shape[0] ) :
    
    data['date'].append ( datetime(int(data_set[ii,0]),int(data_set[ii,1]),int(data_set[ii,2]),int(data_set[ii,3]),int(data_set[ii,4]),int(data_set[ii,5]) ) )
    
#Grid the data
print('Griding the data')
grid_data = gm.grid_data( lon_grid , lat_grid , dt , file_dt , data , fig_path=path_data+'/figs/' , out_path=path_data+'/griddata/')


