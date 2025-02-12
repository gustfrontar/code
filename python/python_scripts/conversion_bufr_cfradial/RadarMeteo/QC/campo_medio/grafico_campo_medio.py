# -*- coding: utf-8 -*-

import numpy as np
import matplotlib.pyplot as plt

def grafico(radar, fecha, datos):


    # Grafico el campo medio
    fig = plt.figure(figsize=(10,8))
    plt.pcolormesh(datos['lat'], datos['lon'], datos['elevacion'],
                   vmin=-32, vmax=70)

    cbar=plt.colorbar()
    cbar.set_label('Reflectividad (dBZ)')
    cbar.set_ticks(np.arange(-30,75,10))

    # Grafico municipios, provincias y el anillo de 240 km
    plt.plot(datos['municipios'][:,1], datos['municipios'][:,0], color='k',
             linewidth=0.5)
    plt.plot(datos['provincias'][:,0], datos['provincias'][:,1], color='k',
             linewidth=1)
    plt.plot(datos['lat'][:,-1],datos['lon'][:,-1], color='k', linewidth=0.5)

    path_guardo='/ms-36/mrugna/calibracion/python'

    if radar == 'PAR':
        lat_min, lat_max = -35, -29
        lon_min, lon_max = -64, -57

        # Dadas las lat y lon de arriba, grafico una cruz con centro en el radar
        plt.plot([lon_min, lon_max], [datos['radar_lat'], datos['radar_lat']],
                 '-k', [datos['radar_lon'], datos['radar_lon']],
                 [lat_min, lat_max], '-k', linewidth=1)

        plt.xticks(np.arange(lon_min, lon_max, .5))
        plt.yticks(np.arange(lat_min, lat_max, .5))

        plt.xlim(-63.1,-58)
        plt.ylim(-34,-29.7)

        plt.title(u'Radar Paraná - Reflectividad media del {dia}/{mes}/{anio} \n Elevación: {elev}° n={cant}'.format(
            dia=datos['dia'], mes=datos['mes'], anio=datos['anio'], elev=datos['elev'], cant=datos['cant']))

        plt.savefig(path_guardo+'/meanref_{radar}_{fecha}.png'.format(
                    radar=radar, fecha=fecha), dpi=120, transparent=False,
                    bbox_inches='tight')

    elif radar == 'PER':
        lat_min, lat_max = -37, -31
        lon_min, lon_max = -64, -57

        plt.plot([lon_min, lon_max], [datos['radar_lat'], datos['radar_lat']],
                 '-k', [datos['radar_lon'], datos['radar_lon']],
                 [lat_min, lat_max], '-k', linewidth=1)

        plt.xticks(np.arange(lon_min, lon_max, .5))
        plt.yticks(np.arange(lat_min, lat_max, .5))

        plt.xlim(-63.18,-57.95)
        plt.ylim(-36.1,-31.78)

        plt.title(u'Radar Pergamino - Reflectividad media del {dia}/{mes}/{anio} \n Elevación: {elev}° n={cant}'.format(
            dia=datos['dia'], mes=datos['mes'], anio=datos['anio'], elev=datos['elev'], cant=datos['cant']))

        plt.savefig(path_guardo+'/meanref_{radar}_{fecha}.png'.format(
                    radar=radar, fecha=fecha), dpi=120, transparent=False,
                    bbox_inches='tight')

    elif radar == 'ANG':
        lat_min, lat_max = -39, -34
        lon_min, lon_max = -67, -61

        plt.plot([lon_min, lon_max], [datos['radar_lat'], datos['radar_lat']],
                 '-k', [datos['radar_lon'], datos['radar_lon']],
                 [lat_min, lat_max], '-k', linewidth=1)

        plt.xticks(np.arange(lon_min, lon_max, .5))
        plt.yticks(np.arange(lat_min, lat_max, .5))

        plt.xlim(-66.7,-61.3)
        plt.ylim(-38.7,-34.37)

        plt.title(u'Radar Anguil - Reflectividad media del {dia}/{mes}/{anio} \n Elevación: {elev}° n={cant}'.format(
            dia=datos['dia'], mes=datos['mes'], anio=datos['anio'], elev=datos['elev'], cant=datos['cant']))

        plt.savefig(path_guardo+'/meanref_{radar}_{fecha}.png'.format(
                    radar=radar, fecha=fecha), dpi=120, transparent=False,
                    bbox_inches='tight')

    elif radar == 'EZE':
        lat_min, lat_max = -37, -32
        lon_min, lon_max = -62, -55

        plt.plot([lon_min, lon_max], [datos['radar_lat'], datos['radar_lat']],
                 '-k', [datos['radar_lon'], datos['radar_lon']],
                 [lat_min, lat_max], '-k', linewidth=10)

        plt.xticks(np.arange(lon_min, lon_max, .5))
        plt.yticks(np.arange(lat_min, lat_max, .5))

        plt.xlim(-61.17,-55.9)
        plt.ylim(-36.93,-32.63)

        plt.title(u'Radar Ezeiza - Reflectividad media del {dia}/{mes}/{anio} \n Elevación: {elev}° n={cant}'.format(
            dia=datos['dia'], mes=datos['mes'], anio=datos['anio'], elev=datos['elev'], cant=datos['cant']))

        plt.savefig(path_guardo+'/meanref_{fecha}.png'.format(
                    fecha=fecha), dpi=120, transparent=False,
                    bbox_inches='tight')
