from __future__ import print_function

import os
import numpy as np

try:
    import wradlib
except ImportError:
    print('Error importando wradlib')
except:
    print('No está instalado wradlib.')

import pyart
from datetime import datetime
from netcdftime import utime


def hdf5_to_cfrad(filename, save=False, disdro=False):

    path = str('')
    barra = '/'
    nombre = filename.split('/')[-1]
    for elemento in filename.split('/')[1:-1]:
        path = path + barra + str(elemento)

    os.chdir(path)
    os.system('RadxConvert -f '+nombre+' -outname archivoNc -outdir '+path)
    Nc = path + '/archivoNc'

    radar_VAD = pyart.io.read(Nc)
    radar = radar_VAD.extract_sweeps(range(radar_VAD.nsweeps)[:-3])
    radar.range['data'] = radar.range['data'][1]
    radar.fields.pop('range', None)

    raw = wradlib.io.read_OPERA_hdf5(filename)

    if disdro:

        az_data = np.array([])
        TH_data = np.zeros([1, 480])
        DBZH_data = np.zeros([1, 480])

        n = 0

        r = radar.extract_sweeps([n])

        az_data = np.append(az_data,
                            raw['dataset%d/data1/how' % (n+1)]['startazA'])

        TH_data = np.append(TH_data,
                            raw['dataset{elev}/data1/what'.format(
                                elev=n+1)]['offset']
                            + raw['dataset{elev}/data1/what'.format(
                                elev=n+1)]['gain']
                            * raw['dataset{elev}/data1/data'.format(elev=n+1)],
                            0)

        DBZH_data = np.append(DBZH_data,
                              raw['dataset{elev}/data2/what'.format(
                                  elev=n+1)]['offset']
                              + raw['dataset{elev}/data2/what'.format(
                                  elev=n+1)]['gain']
                              * raw['dataset{elev}/data2/data'.format(
                                  elev=n+1)],
                              0)

        radar.azimuth['data'] = az_data

        TH_data = TH_data[1:]
        TH_data = np.ma.masked_less_equal(TH_data, -32)
        radar.fields['TH']['data'] = TH_data

        os.remove(path+'/archivoNc')

    else:

        az_data = np.array([])
        TH_data = np.zeros([1, 480])
        DBZH_data = np.zeros([1, 480])
        VRAD_data = np.zeros([1, 480])
        WRAD_data = np.zeros([1, 480])

        # completamos los datos azimuth
        N = radar.nsweeps
        start = 0
        for n in range(0, N):
            r = radar.extract_sweeps([n])
            j = n + 1

            az_data = np.append(az_data,
                                raw['dataset%d/data1/how' % (n+1)]['startazA'])

            TH_data = np.append(TH_data,
                                raw['dataset{elev}/data1/what'.format(
                                    elev=n+1)]['offset']
                                + raw['dataset{elev}/data1/what'.format(
                                    elev=n+1)]['gain']
                                * raw['dataset{elev}/data1/data'.format(
                                    elev=n+1)],
                                0)

            DBZH_data = np.append(DBZH_data,
                                  raw['dataset{elev}/data2/what'.format(
                                      elev=n+1)]['offset']
                                  + raw['dataset{elev}/data2/what'.format(
                                      elev=n+1)]['gain']
                                  * raw['dataset{elev}/data2/data'.format(
                                      elev=n+1)],
                                  0)

            VRAD_data = np.append(VRAD_data,
                                  raw['dataset{elev}/data3/what'.format(
                                      elev=n+1)]['offset']
                                  + raw['dataset{elev}/data3/what'.format(
                                      elev=n+1)]['gain']
                                  * raw['dataset{elev}/data3/data'.format(
                                      elev=n+1)],
                                  0)

            WRAD_data = np.append(WRAD_data,
                                  raw['dataset{elev}/data4/what'.format(
                                      elev=n+1)]['offset']
                                  + raw['dataset{elev}/data4/what'.format(
                                      elev=n+1)]['gain']
                                  * raw['dataset{elev}/data4/data'.format(
                                      elev=n+1)],
                                  0)

        radar.azimuth['data'] = az_data

        TH_data = TH_data[1:]
        TH_data = np.ma.masked_less_equal(TH_data, -32)
        radar.fields['TH']['data'] = TH_data

        DBZH_data = DBZH_data[1:]
        DBZH_data = np.ma.masked_less_equal(DBZH_data, -32)
        radar.fields['DBZH']['data'] = DBZH_data

        VRAD_data = VRAD_data[1:]
        VRAD_data = np.ma.masked_less_equal(VRAD_data, -6.775)
        radar.fields['VRAD']['data'] = VRAD_data

        WRAD_data = WRAD_data[1:]
        WRAD_data = np.ma.masked_less_equal(WRAD_data, 0)
        radar.fields['WRAD']['data'] = WRAD_data

        os.remove(path+'/archivoNc')

    if save:
        '''
        Cambiar el nombre para que sea igual que el RMA1

        cfrad.YYYYMMDD_HHMMSS.0000_to_YYYYMMDD_HHMMSS.0000_NOMBRE_ESTRATEGIA.nc

        Para RMA1 fue *_RMA1_SUR.nc
        Para PAR fue *_ANG120_v126_SUR.nc
        Ver la cantidad de decimales que se van a usar para los segundos.
        '''
        cdftime = utime(radar.time['units'])
        time1 = cdftime.num2date(
            radar.time['data'][0]).strftime('%Y%m%d_%H%M%S')
        name = 'cfrad.{time1}.000_to_{time1}.000_EZE_SUR.nc'.format(
            time1=time1)

        pyart.io.cfradial.write_cfradial(path+barra+name,
                                         radar,
                                         format='NETCDF4_CLASSIC',
                                         time_reference=None,
                                         arm_time_variables=False)

    return radar

if __name__ == '__main__':
    hdf5_to_cfrad()
