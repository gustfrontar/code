def corregir_azimuth(radar):
    azimuth_correctos=[[None] * 360 for i in range(radar.nsweeps)]
    for elev in range(0,radar.nsweeps):
        i1 = radar.sweep_start_ray_index['data'][elev]
        i2 = radar.sweep_end_ray_index['data'][elev]
        azimuth=radar.azimuth['data'][i1:i2]

        for azi in range(0,360):
            desaz=azi
            azdiff=abs(azimuth-desaz)    
            azimuth_correctos[elev][azi] = azdiff.argmin()+i1
    return azimuth_correctos     
