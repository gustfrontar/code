from __future__ import print_function

import glob
import os
import sys
from datetime import datetime
import pickle
try:
    import gzip
except ImportError:
    print('No esta instalado el modulo gzip.',
          'No se podra guardar en pklz')

from netcdftime import utime
import numpy as np
import pyart

try:
    from pyart.aux_io.sinarame_h5 import read_sinarame_h5
except ImportError:
    from .sinarame_h5 import read_sinarame_h5
    # raise ImportError('Falta el modulo sinarame_h5')


def genero_nc_RMA(path, save=None):
    """
    Esta funcion toma archivos H5 (donde cada archivo es una variable)
    dentro de una carpeta y tiene dos formas de uso dependiendo del
    argumento save.

    Si se usa dentro de un script de python, no hace falta asignar el
    valor de save (es None por defecto) y devuelve el objeto radar
    generado con todas las variables incluidas. En caso de que algun
    archivo este corrupto o no se pueda agregar, mostrara por pantalla
    cual es.

    Explicitando el formato en el que se quiere guardar el archivo se
    puede usar dentro de python o en una terminal (explicacion mas
    abajo) para generar un nuevo archivo con todas las variables
    incluidas. Se puede guardar en formato NetCDF, pkl o pklz, donde
    NetCDF presenta algunos inconvenientes, pkl es la convencion al usar
    el modulo pickle y pklz es el archivo pkl comprimido. El problema de
    pkl es que no usa compresion y los archivos ocupan 10 veces mas
    espacio que el NetCDF o los H5 juntos. Se soluciona usando gzip
    aunque el proceso de compresion hace que se ralentice el guardado
    del archivo.

    Parametros
    ----------
    path : str
        Path donde estan los datos. Usando el script para acomodar los
        archivos se puede senialar la carpeta de la estrategia.

    save : str (opcional)
        Puede ser None (por defecto), 'nc', 'pkl' o 'pklz'

    Devuelve
    --------
    radar : Radar
        Objeto Radar de PyART.


    Ejemplo de uso dentro de python:

    path='~/SMN/RMA/RMA1/'
    radar=genero_nc_RMA(path)


    Usando la consola:
    ------------------

    Este archivo se puede ejecutar tambien en una terminal (es requisito
    que el archivo sinarame_h5_martin se encuentre en la misma carpeta)
    para guardar el objeto radar desde las variables en formato H5.

    Parametros
    ----------

    --nc : Permite utilizar PyART para guardar el objeto radar en un
        archivo NETCDF4_CLASSIC. Es la opcion por defecto para la
        terminal.

    --pkl : Permite utilizar el modulo pickle para guardar el objeto
        radar en un archivo binario con extension pkl. Esta opcion fue
        agregada debido a algunos inconvenientes con la escritura de
        archivos netcdf.

    --pklz : Idem pkl pero usa la libreria gzip para comprimir. Es
        mas lento.

    # Para guardar en nc, se puede omitir el --nc
    python genero_nc_RMA1.py /path/to/datos/

    # Para guardar en pkl
    python genero_nc_RMA1.py /path/to/datos/ --pkl

    """

    path_user = os.path.expanduser(path)

    files_TH = glob.glob(path_user + '/*_TH_*.H5')

    fecha = [i.split('_')[-1][9:-4] for i in files_TH]
    fecha.sort()

    for i in fecha:
        files = glob.glob(path_user + '/*' + i + 'Z.H5')

        # Ordeno la lista para que siempre arranque con T
        files.sort(reverse=True)
        files.sort(key=lambda x: len(x.split('_')[-2]))

        print('\tEl primer archivo es:', str(files[0]), end='\n\n')

        for j in np.arange(len(files)):
            basename = os.path.basename(files[j])
            bs = basename.split('_')
            base1 = '{b1}_{b2}_{b3}_{fn}_{b4}'.format(
                b1=bs[0], b2=bs[1], b3=bs[2], fn=bs[3], b4=bs[4])
            file = '{path}/{base1}'.format(path=path_user, base1=base1)

            if j == 0:
                try:
                    radar = read_sinarame_h5(file, file_field_names=True)
                    azi_todos, rango = radar.fields['TV']['data'].shape
                    print('Creo objeto radar de', base1, sep='\t')

                # Si no puede crear radar
                # Seria recomendable agarrar las excepciones de mejor manera
                except ValueError:
                    print('x - No se pudo crear el objeto Radar de',
                          base1, sep='\t')

                # Puede venir mal convertido el archivo H5 desde el BUFR
                except KeyError:
                    print('x - Se convirtio mal de BUFR', base1, sep='\t')

            else:
                try:
                    radar_prov = read_sinarame_h5(file, file_field_names=True)
                    print('Creo objeto radar de', base1, sep='\t')

                    campo = radar_prov.fields.keys()[0]

                    # Si el archivo tiene menos gates que TH hay que agregarlos
                    if radar_prov.fields[campo]['data'].shape[1] != rango:
                        falta = (rango
                                 - radar_prov.fields[campo]['data'].shape[1])
                        resto = np.ma.masked_all((azi_todos, falta))
                        datos = np.ma.concatenate(
                            [radar_prov.fields[campo]['data'], resto], 1)
                        radar_prov.fields[campo]['data'] = datos

                    radar.fields.update(radar_prov.fields)

                # Puede venir mal convertido el archivo H5 desde el BUFR
                except KeyError:
                    print('x - Se convirtio mal de BUFR', base1, sep='\t')

                # Si no puede crear radar_prov
                except ValueError:
                    print('x - No se puede crear el objeto radar_prov para',
                          base1, sep='\t')

                # Si no encuentra radar
                except NameError:
                    print('No existia radar, lo creo')
                    radar = read_sinarame_h5(file, file_field_names=True)

        if save in ['nc', 'pkl', 'pklz']:
            cal_temps = u"gregorian"
            cdftime = utime(radar.time['units'])

            time1 = cdftime.num2date(radar.time['data'][0]).strftime(
                '%Y%m%d_%H%M%S')
            time2 = cdftime.num2date(radar.time['data'][-1]).strftime(
                '%Y%m%d_%H%M%S')

            radar._DeflateLevel = 5

            cffile = 'cfrad.{time1}.0000_to_{time2}.0000'\
                     '_{b1}_{est}_{ran}'.format(time1=time1, time2=time2,
                                                b1=bs[0], est=bs[1], ran=bs[2])

            if save == 'nc':
                pyart.io.write_cfradial(path_user + '/' + cffile + '.nc',
                                        radar, format='NETCDF4_CLASSIC')
                print('\n' + cffile + '\n\n')

            elif save == 'pkl':
                with open(path_user + '/' + cffile + '.pkl', 'wb') as outfile:
                    pickle.dump(radar, outfile, pickle.HIGHEST_PROTOCOL)
                print('\n' + cffile + '\n\n')

            elif save == 'pklz':
                # Es mas lento porque comprime
                with gzip.open(path_user + cffile + '.pklz', 'wb') as outfile:
                    pickle.dump(radar, outfile, pickle.HIGHEST_PROTOCOL)
                print('\n' + cffile + '\n\n')

    return radar


if __name__ == "__main__":
    path = sys.argv[1]
    try:
        save = str(sys.argv[2])
    except:  # Es recomendable agarrar la excepcion
        save = 'nc'

    if save in ['--pkl', '-pkl', 'pkl']:
        genero_nc_RMA(path, save='pkl')
    if save in ['--pklz', '-pklz', 'pklz']:
        genero_nc_RMA(path, save='pklz')
    elif save in ['--nc', '-nc', 'nc']:
        genero_nc_RMA(path, save='nc')
