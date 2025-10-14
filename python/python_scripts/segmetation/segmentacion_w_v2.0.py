import numpy as np
from matplotlib import pyplot
from wrf import to_np
from skimage import measure
import pickle as pkl

umbral_segmentacion = 17.5  #Umbral a partir del cual identificamos celdas de tormenta en m/s
pixel_area=4.0              #Area del pixel en km**2
delta_t=120.0               #Tiempo entre dos archivos.
time_threshold = 20         #Tiempo minimo que tiene que durar el objeto para entrar en la muestra.
ref_threshold = 35.0        #Reflectivity threshold para construir la mascara extendida.
dist_threshold= 15.0        #Distance threshold para construir la mascara extendida.

exp_path="../../2018121100_2K/postproc/" #path donde estan los archivos de salida

input_data = np.load( exp_path + '/w_max.npz' )

w_max=input_data['w_max']             #Maxima velocidad vertical en la columna.
dbz_max=input_data['dbz_max']         #Maxima reflectividad en la columna.
lats =to_np(input_data['lats'])
lons =to_np(input_data['lons'])

mascara_w = ( w_max > umbral_segmentacion ).astype(int)

#Identificacion de cluster a partir de la imagen binaria
mascara_obj =measure.label( mascara_w )

np.savez( exp_path + '/mascara_obj_w.npz',mascara_obj=mascara_obj,lats=lats,lons=lons,umbral_segmentacion=umbral_segmentacion)

nobj = np.max( mascara_obj )

print('Cantidad de objetos encontrados=',nobj)

# i_max_size=0
# max_size = 0
# for iobj in range( nobj ) :
#    print(iobj)

#    obj_size = np.sum( mascara_obj == iobj+1 )
#    if obj_size >  max_size :
#       max_size = obj_size
#       i_max_size = iobj + 1

# print(i_max_size,max_size)


objetos = dict()
#Cantidad de objetos identificados
objetos['nobj']=0  #np.max(mascara_obj)
#Objeto
objetos['obj']=mascara_obj
#Identificacion de cluster
objetos['id']=list() #np.arange(objetos['nobj']) + 1
#Tamaño de cada objeto:
objetos['size']=list() #np.zeros(objetos['nobj'])
objetos['size_ext']=list()
#Duración del objeto
objetos['ntiempos']=list() #=np.zeros(objetos['nobj'])
#Definición de los píxeles en espacio y tiempo:
objetos['x']=list()
objetos['y']=list()
objetos['t']=list()
objetos['x_ext']=list()
objetos['y_ext']=list()
objetos['t_ext']=list()
#Valor de W_MAX de cada pixel
objetos['w_max']=list()
objetos['lat']=list()
objetos['lon']=list()
objetos['w_max_ext']=list()
objetos['lat_ext']=list()
objetos['lon_ext']=list()
#Definir el tiempo total de la simulacion
objetos['tiempos']=list()
#Usado para definir el centroide
objetos['lat_cen']=list()
objetos['lon_cen']=list()
#Propiedades del W_max de cada objeto
objetos['area']=list()
objetos['area_ext']=list()
objetos['velocidad']=list()

#Recorro cada objeto para guardar las propiedades definidas anteriormente:
obj_id = 0
for iobj in range(int(np.max(mascara_obj))):

    [tmpx , tmpy , tmpt] = np.where( mascara_obj == iobj+1 )
    #Construyo la mascara extendida.
    objtimes = np.unique( tmpt )
    objlons  = lons[tmpx,tmpy]
    objlats  = lats[tmpx,tmpy]
    for count , it in enumerate(objtimes) :
        tmpindex = ( tmpt == it )
        lon_cen = np.mean( objlons[tmpindex] )
        lat_cen = np.mean( objlats[tmpindex] )

        

        #En la mascara extendida tomamos todos los puntos que estan a una distancia menor a dist_threshold (respecto del centro del objeto)
        #y que ademas tienen una reflectividad mayor a ref_threshold
       
        mask = np.sqrt( ( ( lats - lat_cen ) * 111.0 ) ** 2 + ( ( lons - lon_cen ) * 111.0 * np.cos(lat_cen * np.pi/180.0 ) ) ** 2 ) < dist_threshold  
        mask = np.logical_and( mask , dbz_max[:,:,it] > ref_threshold)
        #mask =  dbz_max[:,:,it] > ref_threshold 
        #Garantizamos que los elementos de la mascara original esten en la mascara extendida (sino puede suceder que si la reflectividad
        #es aun muy baja, la mascara extendida puede ser mas pequenia que la mascara original).
        mask[tmpx[tmpindex],tmpy[tmpindex]] = True

        [ tmpx_ext , tmpy_ext ] = np.where( mask )
        tmpt_ext = it * np.ones( tmpx_ext.shape ).astype(int)

        if count == 0 :
           x_ext = tmpx_ext 
           y_ext = tmpy_ext
           t_ext = tmpt_ext
        else :
           x_ext = np.concatenate( (x_ext , tmpx_ext) )
           y_ext = np.concatenate( (y_ext , tmpy_ext) )
           t_ext = np.concatenate( (t_ext , tmpt_ext) )


    if np.max(tmpt) - np.min(tmpt) > time_threshold :
        objetos['nobj']=objetos['nobj']+1
        objetos['id'].append(obj_id)
        objetos['x'].append(tmpx)
        objetos['y'].append(tmpy)
        objetos['t'].append(tmpt)
        objetos['x_ext'].append(x_ext)
        objetos['y_ext'].append(y_ext)
        objetos['t_ext'].append(t_ext)
        objetos['lat'].append(lats[tmpx,tmpy])      
        objetos['lon'].append(lons[tmpx,tmpy])
        objetos['lat_ext'].append(lats[x_ext,y_ext])
        objetos['lon_ext'].append(lons[x_ext,y_ext])
        objetos['w_max'].append(w_max[tmpx,tmpy,tmpt])
        objetos['w_max_ext'].append(w_max[x_ext,y_ext,t_ext])
        objetos['tiempos'].append( np.sort( np.unique( tmpt ) ) ) 
        objetos['ntiempos'].append( np.max( objetos['t'][obj_id]) - np.min( objetos['t'][obj_id] )+1 )
        objetos['size'].append( np.size(  objetos['t'][obj_id] ) )
        objetos['size_ext'].append( np.size(  objetos['t_ext'][obj_id] ) )
        objetos['area'].append( np.zeros( int(objetos['ntiempos'][obj_id] )) )
        objetos['area_ext'].append( np.zeros( int(objetos['ntiempos'][obj_id] )) )
        objetos['lat_cen'].append( np.zeros( int(objetos['ntiempos'][obj_id] ) ) )
        objetos['lon_cen'].append( np.zeros( int (objetos['ntiempos'][obj_id] )) )
        objetos['velocidad'].append( np.zeros( int (objetos['ntiempos'][obj_id] )) )
        print(obj_id,objetos['ntiempos'][obj_id])
    
        for count,it in enumerate (objetos['tiempos'][obj_id]) :
            tmpindex= objetos['t'][obj_id] == it
            tmpindex_ext = objetos['t_ext'][obj_id] == it
            objetos['lon_cen'][obj_id][count] = np.mean( objetos['lon'][obj_id][tmpindex] )
            objetos['lat_cen'][obj_id][count] = np.mean( objetos['lat'][obj_id][tmpindex] )
            #Area en km2        
            objetos['area'][obj_id][count] = np.sum( tmpindex.astype(int) ) *pixel_area
            objetos['area_ext'][obj_id][count] = np.sum( tmpindex_ext.astype(int) ) *pixel_area

    
            if  it > np.min( objetos['tiempos'][obj_id] )  :
                delta_lat = objetos['lat_cen'][obj_id][count] - objetos['lat_cen'][obj_id][count-1]
                delta_lon = objetos['lon_cen'][obj_id][count] - objetos['lon_cen'][obj_id][count-1]
                delta_y = delta_lat * 111.0e3
                delta_x = delta_lon  * 111.0e3 * np.cos( objetos['lat_cen'][obj_id][count]*np.pi/180. )
                objetos['velocidad'][obj_id][count] = np.sqrt( np.power(delta_y,2) + np.power(delta_x,2) ) / delta_t
    
                if it == np.min( objetos['tiempos'][obj_id] ) + 1 :
                    objetos['velocidad'][obj_id][count-1]=objetos['velocidad'][obj_id][count]
                    
        obj_id = obj_id + 1
    
filename = exp_path + '/objetos_wmax_u' + str(umbral_segmentacion) + '.pkl'    
pkl.dump(objetos,open(filename,"wb"))





import matplotlib.pyplot as plt

plt.figure()
for ii in range( objetos['nobj'] ) :
    plt.plot(objetos['lon_cen'][ii],objetos['lat_cen'][ii])
    plt.text( objetos['lon_cen'][ii][-1],objetos['lat_cen'][ii][-1],str(ii) )
    
    
#TODO agregar el id de objeto al grafico con todos los objetos para poder identificarlos facilmente.  
    

















