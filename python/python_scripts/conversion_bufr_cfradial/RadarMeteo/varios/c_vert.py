import pyart
import numpy as np
import matplotlib.pyplot as plt
import numpy.ma as ma
### Corte vertical
def corregir_azimuth(radar):
    azimuth_correctos=[[None] * 361 for i in range(radar.nsweeps)]
    for elev in range(0,radar.nsweeps):
        i1 = radar.sweep_start_ray_index['data'][elev]
        i2 = radar.sweep_end_ray_index['data'][elev]
        azimuth=radar.azimuth['data'][i1:i2]

        for azi in range(361):
            desaz=azi
            azdiff=abs(azimuth-desaz)    
            azimuth_correctos[elev][azi] = azdiff.argmin()+i1
    return azimuth_correctos     

def c_vert(radar,sazimuth):
    AzC=corregir_azimuth(radar)
    radar_lat = radar.latitude['data'][0]
    radar_lon = radar.longitude['data'][0]
    radar_alt = radar.altitude['data'][0]
    d2r=np.pi/180 

    #constant necessary 
    Re=6370000;  #radius of the earth
    ke=(4/3);
    data=[]                 #dBZ data
    S=[]                    #distance on the earth's surface
    H=[]                    #height

    for i in range (radar.nsweeps):
        sweep=i
        ang=radar.get_elevation(sweep)[0]
        data.append(np.ones([3,480]))
        azi=AzC[i][sazimuth]
        data[i][1,:] = radar.fields['dBZ']['data'].data[azi,:]
        rango=radar.range['data']

        H.append(np.ones([3,480])) 
        H[i][0,:]=radar_alt+np.sqrt((ke*Re)**2+2*rango*np.sin((ang-0.5)*d2r)*ke*Re+rango**2)-ke*Re
        H[i][1,:]=radar_alt+np.sqrt((ke*Re)**2+2*rango*np.sin(ang*d2r)*ke*Re+rango**2)-ke*Re
        H[i][2,:]=radar_alt+np.sqrt((ke*Re)**2+2*rango*np.sin((ang+0.5)*d2r)*ke*Re+rango**2)-ke*Re

        S.append(np.ones([3,480]))
        S[i][0,:]= ke*Re*np.arcsin((rango*np.cos((ang-0.5)*d2r))/(ke*Re))
        S[i][1,:]= ke*Re*np.arcsin((rango*np.cos(ang*d2r))/(ke*Re))
        S[i][2,:]=ke*Re*np.arcsin((rango*np.cos((ang+0.5)*d2r))/(ke*Re))
    ### Grafico
    
    
    mx = ma.masked_equal (data, 1)


    plt.figure(figsize=(10,10))
    for ii in range(radar.nsweeps):

        plt.pcolor(np.array(S[ii])/1e3,np.array(H[ii])/1e3,mx[ii],shading='flat')
        plt.axis([0, 120, 0, 20])
    plt.xlabel('Distance radar [Km]')
    plt.ylabel('Heigh [Km]')
    plt.colorbar()
    plt.show()
    plt.close()
