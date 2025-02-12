# -*- coding: utf-8 -*-
"""
Created on Wed Apr  6 15:00:00 2016

@author: romina
"""

import numpy as np
import pyart
import matplotlib.pyplot as plt

file='/home/romina/Radar/PHIDP/ejemplo_parana/cfradial/20100118/cfrad.20100118_145002.000_to_20100118_145342.999_PAR_v1_SUR.nc'
radar = pyart.io.read(file)

#Extraigo la elevación que quiero utilizar 
#En este caso la primera elevacion

radar_ele=radar.extract_sweeps([0])

uphi=radar_ele.fields['uPhiDP']['data']
rho=radar_ele.fields['RhoHV']['data']

nb=radar_ele.ngates
nr=radar_ele.nrays

rho_th=0.8 ###Filtro para el rho_hv 

uphi[rho<rho_th]=np.nan

phi_cor=np.zeros((nr,nb)) #Asigno cero a la nueva variable phidp corregida

v1=np.zeros(nb)  #Vector v1
v2=np.zeros(nb)  #Vector v2

# Aca se hace el unfolding

diferencia=280 # Valor que toma la diferencia entre uno y otro pixel dentro de un mismo
               # azimuth

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
    
#Restar el phi del sistema

phi_cor_sys=np.zeros((nr,nb))

phisys=np.nanmean(phi_cor)

phi_cor_sys=phi_cor-phisys

phi_cor_sys[phi_cor_sys<0.0]=np.nan

# Agregamos la nueva variable PHIDP corregida en un nuevo campo de radar

radar_ele.add_field_like('uPhiDP','cPhiDP',phi_cor_sys)

# Grafico

display = pyart.graph.RadarDisplay(radar_ele)

fig = plt.figure(figsize=(10, 10))
plt.subplot(2,2,1)
display.plot_ppi('dBZ',vmin=-25,vmax=70)
plt.subplot(2,2,2)
display.plot_ppi('uPhiDP',vmin=0,vmax=360)
plt.subplot(2,2,3)
display.plot_ppi('cPhiDP',vmin=0,vmax=360)



## Prueba para un solo azimuth 
#
#fp=np.zeros(nb)
#ffp=np.zeros(nb)
#phi_corp=np.zeros((1,nb)) 
#
#ffp=uphi[291,:]   #aca elijo el haz que quiero probar
#
#for k in range(nb):
#    a=fp[k-1]-ffp[k]
#    if a>280:
#        fp[k]=ffp[k]+360
#    else:
#        fp[k]=ffp[k]
#
#
#phi=np.zeros(nb)
#
## Suavizado con una convolucion
#
#from numpy import convolve
# 
#def movingaverage (values, window):
#    weights = np.repeat(1.0, window)/window
#    sma = np.convolve(values, weights, 'valid')
#    return sma
# 
#for k in range(nb):
#	   phi=movingaverage(fp,5)
#    
#    
## Suavizado con una media movil de x puntos
#phi2=np.zeros(nb)
#
#
#def promedio_movil5(X):
#    n=len(X)
#    Y=X.copy()
#    for i in range(2,n-2):
#        Y[i]=X[i-2:i+3].mean()
#    return Y
#
#for l in range(nb):
#	   phi2=promedio_movil5(fp)
    