#Grafico los contenidos de las diferentes especies en un corte vertical S-N siguiendo a la celda.
#El grafico está centrado en la ascendente (que es lo que usamos para seguir el objeto).

import numpy as np
from matplotlib import pyplot as plt
#from matplotlib import colors
from netCDF4 import Dataset
from wrf import ( to_np , getvar , interplevel )
#from skimage import measure
import pickle as pkl
import glob
#from wrf import ( get_cartopy, latlon_coords, vertcross,
#                         cartopy_xlim, cartopy_ylim, interpline, CoordPair)


umbral_segmentacion = 10.0  #Umbral a partir del cual identificamos celdas de tormenta en m/s
pixel_area=4.0              #Area del pixel en km**2
delta_t=120.0               #Tiempo entre dos archivos.
selected_object=856         #ID del objeto seleccionado para plotear.
cross_section_width=15      #Ancho del corte vertical (en puntos de retítula)


labelsize_colorbar = 12    #Tamaño de la letra del colorbar
skipv              = 1     #Espaciamiento de los vectores de viento.
arrow_scale_factor = 1.0   #Factor de escala para los vectores de viento

exp_path="/home/melina.luque/datosmunin/20181211_2K/" #path donde estan los archivos de salida

#Obtenemos la lista de archivos original.
file_list=glob.glob( exp_path + '/WRF/wrfout*')
file_list.sort()

#Cargamos el pickle que contiene el diccionario con los objetos
filename = exp_path + '/PP/objetos_wmax_u' + str(umbral_segmentacion) + '.pkl'
objetos=pkl.load(open(filename,"rb"))

#################################################################################################
# DEFINO ESTA FUNCION QUE NECESITO PARA EL PLOTEO
#################################################################################################
def cmap_discretize(cmap, N):
    """Return a discrete colormap from the continuous colormap cmap.
    
        cmap: colormap instance, eg. cm.jet. 
        N: number of colors.
    
    Example
        x = resize(arange(100), (5,100))
        djet = cmap_discretize(cm.jet, 5)
        imshow(x, cmap=djet)
    """

    import matplotlib
    import matplotlib.cm     as cm
    import matplotlib.pyplot as plt
    import matplotlib.ticker as mticker

    if type(cmap) == str:
        cmap = plt.get_cmap(cmap)
    colors_i = np.concatenate((np.linspace(0, 1., N), (0.,0.,0.,0.)))
    colors_rgba = cmap(colors_i)
    indices = np.linspace(0, 1., N+1)
    cdict = {}
    for ki,key in enumerate(('red','green','blue')):
        cdict[key] = [ (indices[i], colors_rgba[i-1,ki], colors_rgba[i,ki]) for i in range(N+1) ]
    # Return colormap object.
    return matplotlib.colors.LinearSegmentedColormap(cmap.name + "_%d"%N, cdict, 1024)
###################################################################################################


obj_lat = np.copy(objetos['lat_cen'][selected_object])
obj_lon = np.copy(objetos['lon_cen'][selected_object])

ini_time = objetos['tiempos'][selected_object][0]

for it in objetos['tiempos'][selected_object] :
   fig_name = './refxy_object' + str(selected_object) + '_t' + str(it) + '.png'
   #Busco el punto x,y de la reticula del wrf mas cercano a la trayectoria.
   wrf_file=Dataset(file_list[it])
   lon = to_np( getvar(wrf_file,'lon')      )
   lat = to_np( getvar(wrf_file,'lat')      )
   [nx,ny]=np.shape(lon)

   dist = (obj_lat[it-ini_time] - lat )**2 + (obj_lon[it-ini_time] - lon )**2
   ind = np.unravel_index(np.argmin(dist, axis=None), dist.shape)

   x=ind[1] #Columna - longitudes
   y=ind[0] #Fila - latitudes

   ymin = y - cross_section_width
   ymax = y + cross_section_width

   xmin = x - cross_section_width
   xmax = x + cross_section_width

   #print( obj_lon[it-ini_time],obj_lat[it-ini_time],lon[y,x],lat[y,x])

   if ymin < 0 :
       ymin=0
   if ymax > ny-1 :
       ymax= ny-1

   if xmin < 0 :
       xmin=0
   if xmax > nx-1 :
       xmax= nx-1

   #Leo el netcdf del wrf para este tiempo para poder hacer un corte vertical
   #centrado en el objeto.
   qc  = to_np( getvar(wrf_file,'QCLOUD')   )[:,ymin:ymax,xmin:xmax] * 1.0e3   #Paso la concentracion de cada clase de hidrometeoro a g/kg
   qr  = to_np( getvar(wrf_file,'QRAIN')    )[:,ymin:ymax,xmin:xmax] * 1.0e3
   qi  = to_np( getvar(wrf_file,'QICE')     )[:,ymin:ymax,xmin:xmax] * 1.0e3 
   qs  = to_np( getvar(wrf_file,'QSNOW')    )[:,ymin:ymax,xmin:xmax] * 1.0e3
   qg  = to_np( getvar(wrf_file,'QGRAUP')   )[:,ymin:ymax,xmin:xmax] * 1.0e3
   qh  = to_np( getvar(wrf_file,'QHAIL')    )[:,ymin:ymax,xmin:xmax] * 1.0e3
   t   = to_np( getvar(wrf_file,'tc')       )[:,ymin:ymax,xmin:xmax]
   z   = to_np( getvar(wrf_file,'z')        )[:,ymin:ymax,xmin:xmax]
   ref = to_np( getvar(wrf_file,'dbz')      )[:,ymin:ymax,xmin:xmax]
   w   = to_np( getvar(wrf_file,'wa')       )[:,ymin:ymax,xmin:xmax]

   #Obtengo U y V interpolados al nivel de 500 hPa.
   [u,v] = to_np( getvar(wrf_file,'uvmet' ) )
   zint  = to_np( getvar(wrf_file,'z' )     )
   u500  = interplevel( u , zint , 500.0 )[ymin:ymax,xmin:xmax]
   v500  = interplevel( v , zint , 500.0 )[ymin:ymax,xmin:xmax]

   qt  = qc + qr + qi + qs + qg + qh
   nz=np.shape(z)[0]

   #Tomo el maximo en la vertical de la concentración de cada clase de hidrometeoros (g/Kg)
   #de la reflectividad y de W.
   ref = np.max( ref , axis=0 )
   wmax   = np.max( w   , axis=0 )
   wmin   = np.min( w   , axis=0 )
   qc   = np.max( qc   , axis=0 )
   qr   = np.max( qr   , axis=0 )
   qi   = np.max( qi   , axis=0 )
   qs   = np.max( qs   , axis=0 )
   qg   = np.max( qg   , axis=0 )
   qh   = np.max( qh   , axis=0 )

   ########################################################################
   # COMIENZA EL GRAFICADO
   ######################################################################## 
   x_plot=lon[ymin:ymax,xmin:xmax]
   y_plot=lat[ymin:ymax,xmin:xmax]

   ybound=[np.nanmin(y_plot),np.nanmax(y_plot)]
   xbound=[np.nanmin(x_plot),np.nanmax(x_plot)]


   ncols=3
   nrows=2
   scale_factor = 1.0
   clevs1=np.arange(0.1,20,0.1) * scale_factor
   my_map = cmap_discretize('YlOrRd',clevs1.size)

   fig, axs = plt.subplots( nrows,ncols , figsize=[15,9] , sharex=True , sharey=True )
   fig.subplots_adjust(wspace=0.15,hspace=0.1,bottom=0.095,left=0.045,right=0.98,top=0.96)

   #Ploteo la reflectividad y la velocidad vertical.
   ax = axs[0,0]
   clevs1=np.arange(0,70,1) * scale_factor
   my_map = cmap_discretize('gist_ncar',clevs1.size)
   p1=ax.contourf( x_plot , y_plot , ref , clevs1 , cmap=my_map)
   clevs2=np.array([10.0,20.0,30.0,40.0,50.0,60.0,70.0]) * scale_factor
   p2=ax.contour( x_plot , y_plot , wmax , clevs2 , colors='k',linestyles='solid' )
   clevs2=np.array([-25.0,-20.0,-15.0,-10.0]) * scale_factor
   p2=ax.contour( x_plot , y_plot , wmin , clevs2 , colors='b',linestyles='dashed' )
   
   ax.quiver(x_plot[0::skipv,0::skipv],y_plot[0::skipv,0::skipv],u500[0::skipv,0::skipv],v500[0::skipv,0::skipv],scale=100.0*arrow_scale_factor)

   ax.plot( obj_lon , obj_lat , 'ko' )
   ax.plot( obj_lon[it-ini_time] , obj_lat[it-ini_time] , 'ro' , markersize=12 )

   ax.set_ybound( ybound )
   ax.set_xbound( xbound )
   ax.grid()
   ax.set_title('Reflectividad (sh, dBZ) y W (cont., $ms^{-1}$)')
   cbar_ax = fig.add_axes([0.06, 0.03, 0.4, 0.02])
   m = plt.cm.ScalarMappable(cmap=my_map )
   m.set_array(ref)
   m.set_clim(np.min(clevs1),np.max(clevs1))
   delta= ( np.max(clevs1)-np.min(clevs1) )/ (clevs1.size-1)
   cb=plt.colorbar(m,cax=cbar_ax,boundaries=np.arange(np.min(clevs1),np.max(clevs1)+delta,delta),orientation='horizontal')
   cb.ax.tick_params(labelsize=labelsize_colorbar)

   #Grafico las gotas de nube.
   clevs1=np.arange(0.1,20,0.1) * scale_factor
   my_map = cmap_discretize('YlOrRd',clevs1.size)

   ax = axs[0,1]
   p1=ax.contourf( x_plot , y_plot , qc , clevs1 , cmap=my_map )
   if np.max( ref ) > 30.0 :
       p3=ax.contour( x_plot , y_plot , ref , [30.0,60.0] , colors='c',linestyles='solid' , linewidths=2.5 )
   ax.set_ybound( ybound )
   ax.set_xbound( xbound )
   ax.set_title(r'$q_c$ (cont., $gKg^{-1}$)')
   ax.grid()

   #Grafico las gotas de lluvia
   ax = axs[0,2]
   p1=ax.contourf( x_plot , y_plot , qr , clevs1 , cmap=my_map )
   if np.max( ref ) > 30.0 :
       p3=ax.contour( x_plot , y_plot , ref , [30.0,60.0] , colors='c',linestyles='solid' , linewidths=2.5 )
   ax.set_ybound( ybound )
   ax.set_xbound( xbound )
   ax.set_title(r'$q_r$ (cont., $gKg^{-1}$)')
   ax.grid()

   #Grafico los cristales de hielo
   ax = axs[1,0]
   p1=ax.contourf( x_plot , y_plot , qi , clevs1 , cmap=my_map )
   if np.max( ref ) > 30.0 :
        p3=ax.contour( x_plot , y_plot , ref , [30.0,60.0] , colors='c',linestyles='solid' , linewidths=2.5 )
   ax.set_ybound( ybound )
   ax.set_xbound( xbound )
   ax.set_title(r'$q_i$ (cont., $gKg^{-1}$)')
   ax.grid()

   #Grafico la nieve
   ax = axs[1,1]
   p1=ax.contourf( x_plot , y_plot , qs , clevs1 , cmap=my_map )
   if np.max( ref ) > 30.0 :
        p3=ax.contour( x_plot , y_plot , ref , [30.0,60.0] , colors='c',linestyles='solid' , linewidths=2.5 )
   ax.set_ybound( ybound )
   ax.set_xbound( xbound )
   ax.set_title(r'$q_s$ (cont., $gKg^{-1}$)')
   #ax.set_yticks([])
   ax.grid()

   #Grafico el graupel
   ax = axs[1,2]
   p1=ax.contourf( x_plot , y_plot , qg+qh , clevs1 , cmap=my_map )
   if np.max( ref ) > 30.0 :
        p3=ax.contour( x_plot , y_plot , ref , [30.0,60.0] , colors='c',linestyles='solid' , linewidths=2.5 )
   ax.set_ybound( ybound )
   ax.set_xbound( xbound )
   ax.set_title(r'$q_g$ (cont., $gKg^{-1}$)')
   #ax.set_yticks([])
   ax.grid()
   #if show :
   #   plt.show()

   plt.savefig(fig_name,dpi=150)
   plt.close()


    
