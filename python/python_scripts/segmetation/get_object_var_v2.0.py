import numpy as np
from matplotlib import pyplot as plt
from netCDF4 import Dataset
from wrf import ( to_np , getvar )
from skimage import measure
import pickle as pkl
import glob

umbral_segmentacion = 17.5  #Umbral a partir del cual identificamos celdas de tormenta en m/s
pixel_area=4.0             #Area del pixel en km**2
delta_t=120.0              #Tiempo entre dos archivos.

#Variables 3D y 2D cuyos perfiles siguiendo la tormenta vamos a obtener.
variables3d=['wa','dbz','SCR','SCW','SCI','SCS','SCH','SCHL','SCTOT','ELECMAG','ELECZ','z','QVAPOR','QCLOUD','QRAIN','QICE','QGRAUP','QSNOW','QHAIL']
variables2d=['mdbz','LIGHTDENS', 'LIGHT','LIGHTDIS']


exp_path="../../2018121100_2K/" #path donde estan los archivos de salida

#Obtenemos la lista de archivos original.
file_list=glob.glob( exp_path + '/WRF_S91/wrfout*')
file_list.sort()

#Cargamos el pickle que contiene el diccionario con los objetos
filename = exp_path + '/postproc/objetos_wmax_u' + str(umbral_segmentacion) + '.pkl'

print(filename)

objetos=pkl.load(open(filename,"rb"))


#Agregamos las variables adicionales al diccionario
for var in variables3d :
   objetos[var + '_mean']=list()
   objetos[var + '_min']=list()
   objetos[var + '_max']=list()
for var in variables2d :
   objetos[var + '_mean']=list()
   objetos[var + '_min']=list()
   objetos[var + '_max']=list()

#Agregamos las variables sobre la mascara extendida.
for var in variables3d :
   objetos[var + '_mean_ext']=list()
   objetos[var + '_min_ext']=list()
   objetos[var + '_max_ext']=list()
for var in variables2d :
   objetos[var + '_mean_ext']=list()
   objetos[var + '_min_ext']=list()
   objetos[var + '_max_ext']=list()




#Seleccionar los objetos de interes.
umbral_duracion = 20
umbral_w_max    = 30
objetos['selected']=np.zeros(objetos['nobj']).astype(bool)
for iobj in range( objetos['nobj'] )  :
    if np.max( objetos['w_max'][iobj] ) > umbral_w_max and  objetos['ntiempos'][iobj] > umbral_duracion :
        objetos['selected'][iobj]=True

#DEBUG#################################################################a
#Esta seccion reemplaza al criterio de las lineas precedentes y es para probar de graficar un objeto particular.
#761 14.0
#765 68.0
#objeto_n=12
#objetos['selected']=np.zeros(objetos['nobj']).astype(bool)
#objetos['selected'][objeto_n]=True

#########################################################################


#Leemos los datos

for iobj in range( objetos['nobj'] )  :

   for var in variables3d :
      objetos[var + '_mean'].append(None)
      objetos[var + '_min'].append(None)
      objetos[var + '_max'].append(None)
      objetos[var + '_mean_ext'].append(None)
      objetos[var + '_min_ext'].append(None)
      objetos[var + '_max_ext'].append(None)
   for var in variables2d :
      objetos[var + '_mean'].append(None)
      objetos[var + '_min'].append(None)
      objetos[var + '_max'].append(None)
      objetos[var + '_mean_ext'].append(None)
      objetos[var + '_min_ext'].append(None)
      objetos[var + '_max_ext'].append(None)


   if objetos['selected'][iobj] == True :
      print('Procesando objeto ',iobj)

      t_ini = objetos['tiempos'][iobj][0]

      for it in objetos['tiempos'][iobj] :
         
         mascara_t = objetos['t'][iobj] == it

         mascara_t_ext = objetos['t_ext'][iobj] == it

         my_x=objetos['x'][iobj][mascara_t]
         my_y=objetos['y'][iobj][mascara_t]

         my_x_ext=objetos['x_ext'][iobj][mascara_t_ext]
         my_y_ext=objetos['y_ext'][iobj][mascara_t_ext]


         #Read files
         wrf_file=Dataset(file_list[it])

         for var in variables3d  :

             my_var = to_np( getvar(wrf_file,var) )
             nz = my_var.shape[0]

             if it == t_ini :
   
                objetos[var + '_mean'][iobj]=np.zeros((nz,int(objetos['ntiempos'][iobj])))
                objetos[var + '_min'][iobj]=np.zeros((nz,int(objetos['ntiempos'][iobj])))
                objetos[var + '_max'][iobj]=np.zeros((nz,int(objetos['ntiempos'][iobj])))

                objetos[var + '_mean_ext'][iobj]=np.zeros((nz,int(objetos['ntiempos'][iobj])))
                objetos[var + '_min_ext'][iobj]=np.zeros((nz,int(objetos['ntiempos'][iobj])))
                objetos[var + '_max_ext'][iobj]=np.zeros((nz,int(objetos['ntiempos'][iobj])))

             objetos[var + '_mean'][iobj][:,it-t_ini] = np.mean( my_var[:,my_x,my_y] , 1 )
             objetos[var + '_max'][iobj][:,it-t_ini] = np.max( my_var[:,my_x,my_y] , 1 ) 
             objetos[var + '_min'][iobj][:,it-t_ini] = np.min( my_var[:,my_x,my_y] , 1 )

             print( np.size( my_x_ext ) , np.size( my_x ) )
             objetos[var + '_mean_ext'][iobj][:,it-t_ini] = np.mean( my_var[:,my_x_ext,my_y_ext] , 1 ) 
             objetos[var + '_max_ext'][iobj][:,it-t_ini] = np.max( my_var[:,my_x_ext,my_y_ext] , 1 ) 
             objetos[var + '_min_ext'][iobj][:,it-t_ini] = np.min( my_var[:,my_x_ext,my_y_ext] , 1 ) 

         for var in variables2d   :

             my_var = to_np( getvar(wrf_file,var) )

             if it == t_ini :

                objetos[var + '_mean'][iobj]=np.zeros((int(objetos['ntiempos'][iobj])))
                objetos[var + '_min'][iobj]=np.zeros((int(objetos['ntiempos'][iobj])))
                objetos[var + '_max'][iobj]=np.zeros((int(objetos['ntiempos'][iobj])))

                objetos[var + '_mean_ext'][iobj]=np.zeros((int(objetos['ntiempos'][iobj])))
                objetos[var + '_min_ext'][iobj]=np.zeros((int(objetos['ntiempos'][iobj])))
                objetos[var + '_max_ext'][iobj]=np.zeros((int(objetos['ntiempos'][iobj])))

             objetos[var + '_mean'][iobj][it-t_ini] = np.mean( my_var[my_x,my_y] , 0 )
             objetos[var + '_max'][iobj][it-t_ini] = np.max( my_var[my_x,my_y] , 0 )   
             objetos[var + '_min'][iobj][it-t_ini] = np.min( my_var[my_x,my_y] , 0 )

             objetos[var + '_mean_ext'][iobj][it-t_ini] = np.mean( my_var[my_x_ext,my_y_ext] , 0 )
             objetos[var + '_max_ext'][iobj][it-t_ini] = np.max( my_var[my_x_ext,my_y_ext] , 0 )              
             objetos[var + '_min_ext'][iobj][it-t_ini] = np.min( my_var[my_x_ext,my_y_ext] , 0 ) 

pkl.dump(objetos,open(filename,"wb"))


#plt.figure()
#plt.pcolor( objetos['tiempos'][objeto_n], objetos['z_mean'][objeto_n],objetos['dbz_mean'][objeto_n] )
#plt.colorbar()
#plt.savefig('dbz_mean.png')
#plt.close()

#plt.figure()
#plt.pcolor( objetos['tiempos'][objeto_n], objetos['z_mean'][objeto_n], objetos['wa_mean'][objeto_n] )
#plt.colorbar()
#plt.savefig('wa_mean.png')
#plt.close()


#plt.figure()
#plt.plot( objetos['mdbz_mean'][objeto_n] )
#plt.savefig('mdbz_mean.png')
#plt.close()


#plt.figure()
#plt.plot( objetos['lon_cen'][objeto_n] , objetos['lat_cen'][objeto_n] )
#plt.savefig('trayectoria.png')
#plt.close()

