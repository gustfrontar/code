# -*- coding: utf-8 -*-
"""
Created on Wed Aug  1 14:07:34 2018

@author: Fran
"""

#########################################################################################################################################\n",
#En este programa lo que se quiere hacer es graficar imágenes PPI y Doppler en un carpeta con mucha cantidad de archivos con formato .nc\n",
#los cuales cada uno de ellos representa un tiempo distinto de la evolución del sistema convectivo.\n",
#########################################################################################################################################\n",

# Este comando \\\\\\\"limpia\\\\\\\" todas las variables definidas (equivalente a clear all de Matlab)\n",
#%reset_selective -f b\n",
#reset -f

# Este comando le indica a Python que las figuras se deben generar dentro de la misma Notebook, no en una ventana\n",
#matplotlib inline

# importo las galerias necesarias
from matplotlib import pyplot as plt
from sys import getsizeof
import numpy as np
import glob
import pyart
import glob, os
import datetime
import gc
from netCDF4 import Dataset as ncopen

direc='/home/franco.piscitelli/datosmate/datos/todos/'
os.chdir(direc)
salida='/home/franco.piscitelli/datosmate/datos/fig_todos/'


##########################################################################
##### Cambiar aca abajo el anio/mes, tanto en archivo1 como en archivo2
##########################################################################
archivos1='??????2009???????0*.nc'
archivos2='??????2009???????3*.nc'
#         'cfrad.20150501_000432.000_to_20150501_000728.000_INTA_Parana_SUR'

for file in glob.glob(archivos1):
    #"    # Defino las variables anio mes dia hora de manera de poder guardar las imágenes en orden cronológico. \n",
    anio = file[6:10]
    mes = file[10:12]
    dia = file[12:14]
    hora_inicio = file[15:19]
    hora_fin = file[38:42]
    
    #Defino la variable 'fecha'\n",
    fecha=anio+'.'+mes+'.'+dia+'.'
    hora=hora_inicio+'_to_'+hora_fin
    
    print(fecha + hora)
    print(datetime.datetime.time(datetime.datetime.now()))
    
    # lee el archivo 'file'\n",
    radar = pyart.io.read(file)
    sweeps=radar.sweep_number
    
    #############################################################################################################################################    \n",
    
    #lo que quiero hacer es graficar los PPI y los doppler de cada archivo de radar. Pero como el archivo tiene diferentes elevaciones hago un loop \n",
    #de manera que grafique una imagen PPI y un Doppler por cada elevacion y cada tiempo\n",
    
    #############################################################################################################################################\n",
    
    i=2
    try:
        levels=np.unique(radar.elevation['data'])
        ele=str(levels[i]) #vector que corresponde al vector levels en cada elevacion i\n",
        datos=radar.extract_sweeps([i])
        dBZ=datos.fields['dBZ']['data']
        plt.figure(figsize=[13,5])
        ro=datos.fields['V']['data']
        display = pyart.graph.RadarDisplay(radar)
    
        xlabel = 'Distancia en X (km)'
        ylabel = 'Distancia en Y (km)'
        
         # 1) CAMPO DE REFLECTIVIDAD Reflectividad equivalente \n",
        #como me interesa ver ecos meteorologicos le impongo la condicion de que la variable dBz supere un umbral determinado (60dBz)\n",
        
        #maxdBZ=np.max(dBZ)\n",
        #if(maxdBZ>=50):\n",
       
        plt.subplot(121,aspect=1.15)
        
        display.plot_ppi('dBZ', i,#sweep=0, \n",
                         axislabels=(xlabel,ylabel),
                         cmap='pyart_NWSRef',
                         vmin=-10, vmax=70,
                         colorbar_label='dBZ')
        
        display.plot_range_rings([240],lw=1.0,col='k',ls='-')
        display.plot_range_rings([60,120,180],lw=0.5,col='k',ls='-')
      
        display.plot_cross_hair(1.)
        
        #con esta linea de codigo tomo el tamaño del dominio que quiero graficar \n",
        display.set_limits(xlim=(-120, 120), ylim=(-120, 120))
        
        ####################################################################################################################################################################################\n",
        
        ####################################################################################################################################################################################        \n",
     
        # 2) campo de Vrad\n",
    
        plt.subplot(122,aspect=1.15)
    
        xlabel = 'Distancia en X (km)'
        ylabel = 'Distancia en Y (km)'
        
        display.plot_ppi('V', i,#sweep=0, \n",
                         axislabels=(xlabel,ylabel),
                         cmap='pyart_NWSRef',
                         vmin=-30, vmax=30,
                         colorbar_label='dBZ')
        
        display.plot_range_rings([240],lw=1.0,col='k',ls='-')
        display.plot_range_rings([60,120,180],lw=0.5,col='k',ls='-')
       
        display.plot_cross_hair(1.)
        
        display.set_limits(xlim=(-120, 120), ylim=(-120, 120))
       
        plt.savefig(salida + '/' + fecha + '_' + hora + '_' + ele + 'elevacion' + ".png",dpi=150)
        #file.close()\n",
        plt.close()
        gc.collect()
    
    except:
        gc.collect()
        pass
        plt.close()
    #del datos \n",
    #del dBZ \n",
    
    gc.collect()
    #%reset_selective -f datos ele hora fecha dBZ ro\n",

#    reset -f
   
gc.collect()




for file in glob.glob(archivos2):
    #"    # Defino las variables anio mes dia hora de manera de poder guardar las imágenes en orden cronológico. \n",
    anio = file[6:10]
    mes = file[10:12]
    dia = file[12:14]
    hora_inicio = file[15:19]
    hora_fin = file[38:42]
    
    #Defino la variable 'fecha'\n",
    fecha=anio+'.'+mes+'.'+dia+'.'
    hora=hora_inicio+'_to_'+hora_fin
    
    print(fecha + hora)
    print(datetime.datetime.time(datetime.datetime.now()))
    
    # lee el archivo 'file'\n",
    radar = pyart.io.read(file)
    sweeps=radar.sweep_number
    
    #############################################################################################################################################    \n",
    
    #lo que quiero hacer es graficar los PPI y los doppler de cada archivo de radar. Pero como el archivo tiene diferentes elevaciones hago un loop \n",
    #de manera que grafique una imagen PPI y un Doppler por cada elevacion y cada tiempo\n",
    
    #############################################################################################################################################\n",
    
    i=2
    try:
        levels=np.unique(radar.elevation['data'])
        ele=str(levels[i]) #vector que corresponde al vector levels en cada elevacion i\n",
        datos=radar.extract_sweeps([i])
        dBZ=datos.fields['dBZ']['data']
        plt.figure(figsize=[13,5])
        ro=datos.fields['RhoHV']['data']
        display = pyart.graph.RadarDisplay(radar)
    
        xlabel = 'Distancia en X (km)'
        ylabel = 'Distancia en Y (km)'
        
         # 1) CAMPO DE REFLECTIVIDAD Reflectividad equivalente \n",
        #como me interesa ver ecos meteorologicos le impongo la condicion de que la variable TH supere un umbral determinado (60dBz)\n",
        
        #maxdBZ=np.max(dBZ)\n",
        #if(maxdBZ>=50):\n",
       
        plt.subplot(121,aspect=1.15)
        
        display.plot_ppi('dBZ', i,#sweep=0, \n",
                         axislabels=(xlabel,ylabel),
                         cmap='pyart_NWSRef',
                         vmin=-10, vmax=70,
                         colorbar_label='dBZ')
        
        display.plot_range_rings([240],lw=1.0,col='k',ls='-')
        display.plot_range_rings([60,120,180],lw=0.5,col='k',ls='-')
      
        display.plot_cross_hair(1.)
        
        #con esta linea de codigo tomo el tamaño del dominio que quiero graficar \n",
        display.set_limits(xlim=(-120, 120), ylim=(-120, 120))
        
        ####################################################################################################################################################################################\n",
        
        ####################################################################################################################################################################################        \n",
     
        # 2) campo de Vrad\n",
    
        plt.subplot(122,aspect=1.15)
    
        xlabel = 'Distancia en X (km)'
        ylabel = 'Distancia en Y (km)'
        
        display.plot_ppi('V', i,#sweep=0, \n",
                         axislabels=(xlabel,ylabel),
                         cmap='pyart_NWSRef',
                         vmin=-30, vmax=30,
                         colorbar_label='dBZ')
        
        display.plot_range_rings([240],lw=1.0,col='k',ls='-')
        display.plot_range_rings([60,120,180],lw=0.5,col='k',ls='-')
       
        display.plot_cross_hair(1.)
        
        display.set_limits(xlim=(-120, 120), ylim=(-120, 120))
       
        plt.savefig(salida + '/' + fecha + '_' + hora + '_' + ele + 'elevacion' + ".png",dpi=150)
        #file.close()\n",
        plt.close()
        gc.collect()
    
    except:
        gc.collect()
        pass
        plt.close()
    #del datos \n",
    #del dBZ \n",
    
    gc.collect()
    #%reset_selective -f datos ele hora fecha dBZ ro\n",

#    reset -f
   
gc.collect()



print('finish')
#reset -f
