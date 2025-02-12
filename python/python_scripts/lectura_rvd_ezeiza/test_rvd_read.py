import rvd_read as rr 
import pyart
import matplotlib.pyplot as plt
import numpy as np

radar_lat=-58.0
radar_lon=-35.0
radar_alt=0.0
input_file='./ar1.cz240p0.20061201.1436.z.rvd'


radar = rr.rvd_read( input_file , radar_lon , radar_lat , radar_alt )


# Now what does that data look like plotted with Py-ART, also confirm if it works.
#plt.figure(figsize=[12,10])
#display = pyart.graph.RadarMapDisplay(radar)
#display.plot_ppi('reflectivity_horizontal',sweep=0,vmin=2,vmax=62)
#plt.show()


gatefilter = pyart.filters.GateFilter(radar)

#print(radar.gate_x)

grid_zh = pyart.map.grid_from_radars(
             (radar, ),
             fields=['reflectivity'],
             grid_shape=(20, 481, 481),
             gatefilters=(gatefilter, ),
             grid_limits=((0,20000), (-240000., 240000.), (-240000., 240000.)) ) #,

colmax = np.max(grid_zh.fields['reflectivity']['data'], axis=0)

plt.pcolor( grid_zh.fields['reflectivity']['data'][0,:,:] )
plt.pcolor( colmax )
plt.colorbar()
plt.show()

#colmax = np.max(grid_zh.fields['reflectivity_horizontal']['data'], axis=0)



