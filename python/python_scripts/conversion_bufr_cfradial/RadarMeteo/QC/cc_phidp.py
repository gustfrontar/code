import numpy as np
import pyart
#import matplotlib.pyplot as plt
from funciones import promediomovil
import os
import sistema as sis
import numpy.ma as ma

def c_phidp(filepath):

    radar = pyart.io.read(filepath)
    cphidp_ele=None

    for sweep in radar.sweep_number['data']:
    
        radar_ele=radar.extract_sweeps([sweep])
    
        #*************************************************************************
        # Calculo el phi del sistema y se lo resta
        system_zero = sis.det_sys_phase(radar_ele,ncp_lev=0.6,rhohv_lev=0.6,
                                ncp_field='RhoHV', rhv_field='RhoHV', phidp_field='uPhiDP')

        uphi=radar_ele.fields['uPhiDP']['data']
        rho=radar_ele.fields['RhoHV']['data']   
        
        nb=radar_ele.ngates
        nr=radar_ele.nrays
    
        # YO saque el fitro del rho, porque YO quiero todo el dato
        #rho_th=0.8 ###Filtro para el rho_hv
    
        #uphi[rho<rho_th]=np.nan
    
        phi_cor=np.zeros((nr,nb)) #Asigno cero a la nueva variable phidp corregida
    
        v1=np.zeros(nb)  #Vector v1
        v2=np.zeros(nb)  #Vector v2
    
        #****************************************************************************
        ## Unfolding
    
        diferencia=280 # Valor que toma la diferencia entre uno y otro pixel dentro de un mismo azimuth
        for j in range(0,nr):
            v1=uphi[j,:]
            for i in range(0,nb):
                a=v2[i-1]-v1[i]
                if a>diferencia:
                    v2[i]=v1[i]+360
                else:
                    v2[i]=v1[i]
            phi_cor[j,:]=v2
        
        phi_cor[phi_cor==0.0]=np.nan
    
        #************************************************************************
        # Suavizado con una media movil de x puntos (metodo convolucion)

        phi_suave=np.zeros(nb)

        s1=np.zeros(nb)
        s2=np.zeros(nb)
        phi_suave=np.zeros((nr,nb)) 

        for j in range(0,nr):
            s1=phi_cor[j,:]
            for i in range(0,nb):
                s2=promediomovil(s1,5)
            phi_suave[j,0:1]=np.nan
            phi_suave[j,479]=np.nan
            phi_suave[j,2:478]=s2  
        
        #**************************************************************        
        # Aca resto el phi del sistema al phi corregido y suavizado
              
        phi_final=np.zeros((nr,nb))
        
        phi_err=np.zeros((nr,nb))
        
        for j in range(0,nr):
            for i in range(0,nb):
                phi_err[j,i]=np.nan
        
        try:
            phi_final=phi_suave-system_zero
        except:
            phi_final=phi_err
      
        try:
            cphidp_ele = np.concatenate((cphidp_ele, phi_final))
        except:
            cphidp_ele = phi_final
            
        
           
    #******* Agrego la nueva variable al cfradial
    
    #radar.add_field_like('uPhiDP','cPhiDP',cphidp_ele)
    
    #anio = radar.time['units'][14:18]
    #mes = radar.time['units'][19:21]
    #dia = radar.time['units'][22:24]
    #hora = radar.time['units'][25:27] + radar.time['units'][28:30]
    #fecha=anio+mes+dia+hora
    
    #***** Guardo el nuevo cfradial 
    
    #pyart.io.write_cfradial(fecha+'.nc',radar, format='NETCDF3_64BIT')
    return(cphidp_ele)
    
    
 


def desvio_phidp(radar,pphidp):
    sd_phi=None
    for sweep in radar.sweep_number['data']:
        radar_ele=radar.extract_sweeps([sweep])
        pego=np.zeros([361,480])	
        pego[pego==0]=np.nan	
        for azi in range(1,360):
            for ran in range(1,479):
                pego[azi,ran]=radar_ele.fields[pphidp]['data'][azi-1:azi+1,ran-1:ran+1].std()
       
                
    #enmascaramos los bordes
        pego[0,:]=ma.masked
        pego[360,:]=ma.masked

        pego[:,0]=ma.masked
        pego[:,479]=ma.masked
        
        
        try:
            
            sd_phi = np.concatenate((sd_phi, pego))
            
        except:
            sd_phi = pego
           
    return sd_phi
    

    
    
    