# -*- coding: utf-8 -*-

import subprocess
import numpy as np
import scipy.io as sio


def calculo(radar, fecha):
    '''
    Esta función porta en parte un script de bash y parte del paso previo a
    graficar.


    Atributos
    #########

    radar: str
        Siglas del radar a calcular. Son 3 letras en mayúscula.
    fecha: str
        Fecha para realizar el cálculo en formato YYYYMMDD.

    Devuelve
    ########

    matlab: dict
        Diccionario conteniendo los datos guardados por Matlab.
    '''


    if radar == 'EZE':
        PATH_ORIGEN_HOY='/yanina-sol-ms1/radar/eze/'+fecha+'/rvd/'
        PATH_DESTINO_HOY='/ms-36/mrugna/tmp'

        # Copio los datos a una carpeta temporal
        comando_copia=['scp', 'mrugna@10.10.23.168:'+PATH_ORIGEN_HOY+'ar1.cz240*.z.rvd', PATH_DESTINO_HOY]
        subprocess.call(comando_copia)

        # Para EZE, convierto los rvd a header y volscan con el script de Juan
        subprocess.call('/ms-36/mrugna/calibracion/python/ref_EZE.bash',
                        shell=True)

        # Ejecuto Matlab para generar las distintas matrices.
        comando_matlab='\'/home/apps/Matlab/bin/matlab -nodesktop -r \
            "run /ms-36/mrugna/calibracion/scripts/campo_medio_240_EZE.m;quit"\''
        comando_ejecuto_matlab='ssh mrugna@10.1.7.161 '+comando_matlab
        subprocess.call(comando_ejecuto_matlab, shell=True)

        # Elimino archivos temporales
        subprocess.call(['rm '+PATH_DESTINO_HOY+'/*.r'], shell=True)

        # Levanto la matriz con Scipy y la convierto en un diccionario más ameno
        matlab = sio.loadmat('/ms-36/mrugna/salidas/{radar}_{fecha}.mat'.format(
                             radar=radar, fecha=fecha))

        datos = {}

        datos['lat']=np.asarray(matlab['radar2'][0][0][0])
        datos['lon']=np.asarray(matlab['radar2'][0][0][1])

        elev1=np.asarray(matlab['elev1'])
        datos['elevacion']=np.ma.array(elev1,mask=np.isnan(matlab['elev1']))

        datos['municipios']=np.asarray(matlab['municipios'])
        datos['provincias']=np.asarray(matlab['provincias'])

        datos['radar_lat']=matlab['radar2'][0][0][-2][0][0]
        datos['radar_lon']=matlab['radar2'][0][0][-1][0][0]

        datos['anio'] = str(matlab['anio'][0])
        datos['mes'] = str(matlab['mes'][0])
        datos['dia'] = str(matlab['dia'][0])
        datos['cant']=matlab['cantidad'][0][0]
        datos['elev']=matlab['elev'][0][0]

        # Elimino la matriz
        subprocess.call('rm /ms-36/mrugna/salidas/{radar}_{fecha}.mat'.format(
                        radar=radar, fecha=fecha), shell=True)

        return datos

    if radar == 'PAR':
        PATH_ORIGEN_HOY='/yanina-sol-ms1/radar/par/'+fecha+'/vol'
        comando_matlab='\'/home/apps/Matlab/bin/matlab -nodesktop -r "run /ms-36/mrugna/calibracion/scripts/campo_medio_240_PAR.m;quit"\''

    elif radar == 'PER':
        PATH_ORIGEN_HOY='/yanina-sol-ms1/radar/per/'+fecha+'/vol'
        comando_matlab='\'/home/apps/Matlab/bin/matlab -nodesktop -r "run /ms-36/mrugna/calibracion/scripts/campo_medio_240_INTA.m;quit"\''


    elif radar == 'ANG':
        PATH_ORIGEN_HOY='/yanina-sol-ms1/radar/ang/'+fecha+'/vol'
        comando_matlab='\'/home/apps/Matlab/bin/matlab -nodesktop -r "run /ms-36/mrugna/calibracion/scripts/campo_medio_240_INTA.m;quit"\''


    if (radar == 'ANG') | (radar == 'PAR') | (radar == 'PER'):
        PATH_DESTINO_HOY='/ms-36/mrugna/tmp'

        comando_copia=['scp', 'mrugna@10.10.23.168:/'+PATH_ORIGEN_HOY+'/'+fecha+'???0??00dBZ.vol', PATH_DESTINO_HOY]
        subprocess.call(comando_copia)

        # Corro Matlab para calcular el campo medio

        comando_ejecuto_matlab='ssh mrugna@10.1.7.161 '+comando_matlab

        subprocess.call(comando_ejecuto_matlab, shell=True)

        # Elimino los archivos de radar
        subprocess.call(['rm '+PATH_DESTINO_HOY+'/*.vol'], shell=True)

        # Extraigo variables
        matlab = sio.loadmat('/ms-36/mrugna/salidas/{radar}_{fecha}.mat'.format(
                             radar=radar, fecha=fecha))

        datos = {}

        datos['lat']=np.asarray(matlab['radar2'][0][0][0])
        datos['lon']=np.asarray(matlab['radar2'][0][0][1])

        elev1=np.asarray(matlab['elev1'])
        datos['elevacion']=np.ma.array(elev1,mask=np.isnan(matlab['elev1']))

        datos['municipios']=np.asarray(matlab['municipios'])
        datos['provincias']=np.asarray(matlab['provincias'])

        datos['radar_lat']=matlab['radar2'][0][0][-2][0][0]
        datos['radar_lon']=matlab['radar2'][0][0][-1][0][0]

        datos['anio'] = str(matlab['anio'][0])
        datos['mes'] = str(matlab['mes'][0])
        datos['dia'] = str(matlab['dia'][0])
        datos['cant']=matlab['cantidad'][0][0]
        datos['elev']=matlab['elev'][0][0]
        datos['nombre']=matlab['radar2'][0][0][-3][0]

        # Elimino la matriz
        subprocess.call('rm /ms-36/mrugna/salidas/{radar}_{fecha}.mat'.format(
                        radar=radar, fecha=fecha), shell=True)

        return datos
