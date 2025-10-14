import numpy as np
from matplotlib import pyplot
from wrf import to_np
from skimage import measure
import pickle as pkl

umbral_segmentacion = 20.0  #Umbral a partir del cual identificamos celdas de tormenta en m/s
pixel_area=4.0             #Area del pixel en km**2
delta_t=120.0              #Tiempo entre dos archivos.


exp_path="/home/jruiz/WRF_ELEC_RELAMPAGO/2018121100_4K/" #path donde estan los archivos de salida

input_data = np.load( exp_path + '/postproc/w_max.npz' )

w_max=input_data['w_max']
lats =to_np(input_data['lats'])
lons =to_np(input_data['lons'])

mascara_w = ( w_max > umbral_segmentacion ).astype(int)

#Identificacion de cluster a partir de la imagen binaria
mascara_obj =measure.label( mascara_w )

np.savez( exp_path + '/postproc/mascara_obj_w.npz',mascara_obj=mascara_obj,lats=lats,lons=lons)

nobj = np.max( mascara_obj )

print('Cantidad de objetos encontrados=',nobj)

#i_max_size=0
#max_size = 0
#for iobj in range( nobj ) :
#   print(iobj)

#   obj_size = np.sum( mascara_obj == iobj+1 )
#   if obj_size >  max_size :
#      max_size = obj_size
#      i_max_size = iobj + 1

print(i_max_size,max_size)

#mascara_max_size=np.max( (mascara_obj == i_max_size).astype(int) , 2 )

#Graficamos el objeto con mayor tamanio espacio-temporal
#pyplot.figure()
#pyplot.pcolor(lons,lats,mascara_max_size)
#pyplot.show()


objetos = dict()
#Cantidad de objetos identificados
objetos['nobj']=np.max(mascara_obj)
#Objeto
objetos['obj']=mascara_obj
#Identificacion de cluster
objetos['id']=np.arange(objetos['nobj']) + 1
#Tamaño de cada objeto:
objetos['size']=np.zeros(objetos['nobj'])
#Duración del objeto
objetos['ntiempos']=np.zeros(objetos['nobj'])
#Definición de los píxeles en espacio y tiempo:
objetos['x']=list()
objetos['y']=list()
objetos['t']=list()
#Valor de W_MAX de cada pixel
objetos['w_max']=list()
objetos['lat']=list()
objetos['lon']=list()
#Definir el tiempo total de la simulacion
objetos['tiempos']=list()
#Usado para definir el centroide
objetos['lat_cen']=list()
objetos['lon_cen']=list()
#Propiedades del W_max de cada objeto
objetos['area']=list()
objetos['velocidad']=list()

#Recorro cada objeto para guardar las propiedades definidas anteriormente:
for iobj in range(int(np.max(mascara_obj))):

    [tmpx , tmpy , tmpt] = np.where( mascara_obj == iobj+1 )
    objetos['x'].append(tmpx)
    objetos['y'].append(tmpy)
    objetos['t'].append(tmpt)
    objetos['lat'].append(lats[tmpx,tmpy])      
    objetos['lon'].append(lons[tmpx,tmpy])
    objetos['w_max'].append(w_max[tmpx,tmpy,tmpt])
    objetos['tiempos'].append( np.sort( np.unique( tmpt ) ) ) 
    objetos['ntiempos'][iobj] = np.max( objetos['t'][iobj]) - np.min( objetos['t'][iobj] )+1
    objetos['size'][iobj] = np.size(  objetos['t'][iobj] )
    objetos['area'].append( np.zeros( int(objetos['ntiempos'][iobj] )) )
    objetos['lat_cen'].append( np.zeros( int(objetos['ntiempos'][iobj] ) ) )
    objetos['lon_cen'].append( np.zeros( int (objetos['ntiempos'][iobj] )) )
    objetos['velocidad'].append( np.zeros( int (objetos['ntiempos'][iobj] )) )
    print(iobj,objetos['ntiempos'][iobj])


    for count,it in enumerate (objetos['tiempos'][iobj]) :
        tmpindex= objetos['t'][iobj] == it
        objetos['lon_cen'][iobj][count] = np.mean( objetos['lon'][iobj][tmpindex] )
        objetos['lat_cen'][iobj][count] = np.mean( objetos['lat'][iobj][tmpindex] )
        #Area en km2        
        objetos['area'][iobj][count] = np.sum( tmpindex.astype(int) ) *pixel_area

        if  it > np.min( objetos['tiempos'][iobj] )  :
            delta_lat = objetos['lat_cen'][iobj][count] - objetos['lat_cen'][iobj][count-1]
            delta_lon = objetos['lon_cen'][iobj][count] - objetos['lon_cen'][iobj][count-1]
            delta_y = delta_lat * 111.0e3
            delta_x = delta_lon  * 111.0e3 * np.cos( objetos['lat_cen'][iobj][count]*np.pi/180. )
            objetos['velocidad'][iobj][count] = np.sqrt( np.power(delta_y,2) + np.power(delta_x,2) ) / delta_t

            if it == np.min( objetos['tiempos'][iobj] ) + 1 :
                objetos['velocidad'][iobj][count-1]=objetos['velocidad'][iobj][count]
    


filename = exp_path + '/postproc/objetos_wmax_u' + str(umbral_segmentacion) + '.pkl'    
pkl.dump(objetos,open(filename,"wb"))















