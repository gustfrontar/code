import rvd_read as rr 
import pyart
import matplotlib.pyplot as plt
import numpy as np

radar_lat=-58.0
radar_lon=-35.0
radar_alt=0.0
input_file='./ar1.cz240p1.20120404.2020.z.rvd'


radar = rr.rvd_read( input_file , radar_lon , radar_lat , radar_alt )


# Now what does that data look like plotted with Py-ART, also confirm if it works.
#plt.figure(figsize=[12,10])
#display = pyart.graph.RadarMapDisplay(radar)
#display.plot_ppi('reflectivity_horizontal',sweep=0,vmin=2,vmax=62)
#plt.show()


gatefilter = pyart.filters.GateFilter(radar)

print(radar.gate_x)



cappi_levels = np.arange(1000,21000,500)

Z_width=1000
R_width=1000
#AZ_width=0   # An smooth in azimuth can also be included by settin this value to an integer above 0.

n_cappi=len(cappi_levels)
cappi_data = np.empty( (na,nr,n_cappi) )
cappi_data[:]=np.NaN

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#  COMPUTE LUT FOR FAST INTERPOLATION.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# if not lut:

#     print('No lut available will generate the lut')
#     # We have to compute the ranges and elevations that falls within the boxes for each requested cappi height.

#     for ic in range(n_cappi):

#         for ir in range(int(radar.nr[0])):

#             R_max = radar.Rh[ir,0] + R_width
#             R_min = radar.Rh[ir,0] - R_width
#             Z_max = cappi_levels[ic] + Z_width
#             Z_min = cappi_levels[ic] - Z_width

#             #lut.index{ic,ir} = find( radar.Rh < R_max & radar.Rh > R_min & radar.height < Z_max & radar.height > Z_min );
#             #lut.number(ic,ir)=length(lut.index{ic,ir});

#             tmp = np.where( (radar.Rh>R_min) & (radar.Rh<R_max) & (radar.height>Z_min) & (radar.height<Z_max) )
#             lut[ic,ir]=len(tmp)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%   COMPUTE CAPPIS USING PRE COMPUTED LUT
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

for ic in range(n_cappi):

    print('Estoy calculando el CAPPI a ' + str(cappi_levels[ic]) + 'm')


    for ir in range(int(radar.nr[0])):

        R_max = radar.Rh[ir,0] + R_width
        R_min = radar.Rh[ir,0] - R_width
        Z_max = cappi_levels[ic] + Z_width
        Z_min = cappi_levels[ic] - Z_width

        lut_idx = np.where( (radar.Rh>R_min) & (radar.Rh<R_max) & (radar.height>Z_min) & (radar.height<Z_max) )
        lut_number = len(lut_idx)

        for iaz in rang( int(radar.na[0])) :

            tmp_data = np.squeeze(data[iaz,:,:])
            if (lut_number>0):

                cappi_data[iaz,ir,ic] = np.sum(tmp_data[lut_idx])/len(tmp_data[lut_idx])

            else:

                cappi_data[iaz,ir,ic] = np.NaN

    print('===========================================================================')



colmax = np.max(grid_zh.fields['reflectivity_horizontal']['data'], axis=0)

#plt.pcolor( grid_zh.fields['reflectivity_horizontal']['data'][0,:,:] )
plt.pcolor( colmax )
plt.colorbar()
plt.show()

colmax = np.max(grid_zh.fields['reflectivity_horizontal']['data'], axis=0)



