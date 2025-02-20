# %%
import numpy as np
from utils.radar import genero_nc , get_time_from_filename
from datetime import datetime, timedelta
import matplotlib.pyplot as plt
import os

radar_file_0 = '/home/ra000007/a04037/data/DATOS_RADAR//RMA1/cfrad.20231216_013503.0000_to_20231216_013724.0000_RMA1_0303_02.nc'
radar_file_1 = '/home/ra000007/a04037/data/DATOS_RADAR//RMA1/QC/cfrad.s20231216_013503.e20231216_013725.RMA1_0303_02.nc'
radar_file_2 = '/home/ra000007/a04037/data/DATOS_RADAR//RMA1/2NDTQC//cfrad.s20231216_013503.e20231216_013725.RMA1_0303_02.nc'

radar_0 = genero_nc( radar_file_0 , 'RMA1' , 'nc' , '20231216_013503' )
radar_1 = genero_nc( radar_file_1 , 'RMA1' , 'nc' , '20231216_013503' )
radar_2 = genero_nc( radar_file_2 , 'RMA1' , 'nc' , '20231216_013503' )


lat , lon , alt = radar_0.get_gate_lat_lon_alt(0)
print( lat.shape )
plt.figure()

plt.subplot(1,3,1)
plt.pcolor( lon , lat , radar_0.fields['reflectivity']['data'][0:360,:] )

plt.subplot(1,3,2)
plt.pcolor( lon , lat , radar_1.fields['cref']['data'][0:360,:] )

plt.subplot(1,3,3)
plt.pcolor( lon , lat , radar_2.fields['cref']['data'][0:360,:] - radar_1.fields['cref']['data'][0:360,:] )

plt.show()






