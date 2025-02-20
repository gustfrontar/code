import os
import re
import gc
import sys
import glob
import pickle
from datetime import datetime , timedelta
import numpy as np
import numpy.ma as ma
import pyart
from pyart.aux_io.sinarame_h5 import read_sinarame_h5
from pyart.io import read
from pyart.aux_io.rainbow_wrl import read_rainbow_wrl
from configparser import ConfigParser, ExtendedInterpolation
#from utils.config import DICT_FIELD_NAMES

# Constantes varias

ray_angle_res = 1.0
radar_beam_width_h = 1.0
radar_beam_width_v = 1.0
local_fill_value = 1e20

# Funciones


def read_radar(path, file_ext):
    """
    Leer los datos de radar dado un formato y devuelve un objeto radar.

    Parametros
    ----------
    path : str
        Camino al archivo de radar a leer

    file_ext : str
        Tipo de archivo. Soporta H5, nc y vol

    Devuelve
    -------
    radar : Radar
        Devuelve un objeto radar y si no puede leer el archivo devuelve un None

     """
    radar = None

    """
    Este diccionario contiene los nombres que van a tener las variables
    dentro del objeto radar. Deben coincidir con el nombre que se pasa en
    experimento.qcr pero además deberían ser únicas para todos los radares.
    Se pueden agregar a requerimiento de variables.
    """
    config = ConfigParser(interpolation=ExtendedInterpolation())
    config.optionxform = str
    config.read("config.ini")
    DICT_FIELD_NAMES=config['DICT_FIELD_NAMES']
    if file_ext == 'H5':
        try:
            radar = read_sinarame_h5(path, field_names=DICT_FIELD_NAMES)
        except Exception as e:
            print(e)
            print('No se puede leer radar con read_sinarame_h5')

    elif file_ext == 'nc':
        try:
            radar = read(path, field_names=DICT_FIELD_NAMES)
        except:
            print('No se puede leer radar con read_cfradial')

    elif file_ext == 'vol':
        #radar = read_rainbow_wrl(path, field_names=DICT_FIELD_NAMES)
        try:
            radar = read_rainbow_wrl(path, field_names=DICT_FIELD_NAMES)
            if 'PER' in path:
                set_per_time(radar)
        except:
            print('No se puede leer radar con read_rainbow_wrl')

    else:
        raise Exception('No se reconoce el formato del radar')

    return radar


def genero_nc(files, radar_id, ext, date):
    """
    Esta funcion toma archivos H5 (donde cada archivo es una variable)
    dentro de una carpeta y los convierte a nc

    Parametros
    ----------
    path : str
        Path donde estan los datos. Usando el script para acomodar los
        archivos se puede senialar la carpeta de la estrategia.

    Devuelve
    --------
    radar : Radar
        Objeto Radar de PyART.

    """

    # La lista de entrada puede venir desordenada, ya sea un str, una lista
    # desde glob. Aca se ordena de forma descendente por lo que el primer
    # archivo puede ser CM o DBZH. Esto es valido para RMA.
    # TODO: Revisar que esta funcionalidad sirva para los archivos separados
    # que vienen de INTA.
    if isinstance(files, str):
        files = [files]
    files.sort()
    for j in np.arange(len(files)):
        basename = os.path.basename(files[j])
        bs = basename.split('_')
        file = files[j]
        #radar = read_radar(file, ext) #Deibug
        #print(radar) 
        #quit()
        if j == 0:
            try:
                radar = read_radar(file, ext)
                campo = [x for x in radar.fields.keys()][0]
                # Estos datos de azimuth y rango los guardo para completar
                # de ser necesario con datos vacios las variables que tienen
                # menos datos
                azi_todos, rango = radar.fields[campo]['data'].shape
                # Si no puede crear el radar, se pasa un Error
            except ValueError:
                print('VALUE ERROR - No se pudo crear el objeto Radar de',
                      basename, sep='\t')
            except AttributeError:
                print('ATT ERROR - No se pudo crear el objeto Radar de',
                      basename, sep='\t')
            # Puede venir mal convertido el archivo H5 desde el BUFR
            except KeyError as e:
                print(e)
                print('KEY ERROR - Se convirtio mal de BUFR',
                      basename, sep='\t')
        else:
            try:
                radar_prov = read_radar(file, ext)
                campo = [x for x in radar_prov.fields.keys()][0]
                # Si el archivo tiene menos gates que la primera variable
                # hay que agregarlos como pixeles vacios
                if radar_prov.fields[campo]['data'].shape[1] != rango:
                    print('Trying to solve inconsistent range dimensions for file ',file)
                    falta = (rango - radar_prov.fields[campo]['data'].shape[1])
                    resto = np.ma.masked_all((azi_todos, falta))
                    datos = np.ma.concatenate(
                        [radar_prov.fields[campo]['data'], resto], 1)
                    radar_prov.fields[campo]['data'] = datos
                if radar_prov.fields[campo]['data'].shape[0] != azi_todos :
                   print('Trying to solvo inconsistent azimuth dimensions for file ',file)
                   datos = np.ma.masked_all( ( azi_todos , rango ) )
                   datos[:] = radar_prov.fields[campo]['_FillValue']
                   for ii in range( len( radar.azimuth['data'] ) ) :
                       azi = radar.azimuth['data'][ii]
                       ele = radar.elevation['data'][ii] 
                       loc_index = np.where( ( azi == radar_prov.azimuth['data'] ) & ( ele == radar_prov.elevation['data'] ) )
                       #print( loc_index[0].size )
                       if loc_index[0].size == 1  :
                          #print( ii , loc_index[0] )
                          datos[ii,:] = radar_prov.fields[campo]['data'][loc_index[0]]        
                   radar_prov.fields[campo]['data'] = datos


                radar.fields.update(radar_prov.fields)
            except KeyError as e:
                # Puede venir mal convertido el archivo H5 desde el BUFR
                print("KeyError {}".format(e))
                print('x - Se convirtio mal de BUFR', basename, sep='\t')
            # Si no puede crear radar_prov
            except ValueError:
                print('x - No se puede crear el objeto radar_prov para',
                      basename, sep='\t')
                if len(files) == j+1:
                    return

            # Si no encuentra radar
            except NameError as e:
                print(e)
                print('No existia radar, lo creo')
                radar = read_radar(file, ext)
            except AttributeError:
                print('Radar vacío')

    try:
        levels = np.unique(radar.elevation['data'])
        add_missing_structures(radar, radar_id)
        radar.metadata['instrument_name'] = radar_id
        if isinstance(date, str):
            radar.metadata['date'] = date
        else:
            radar.metadata['date'] = date.strftime('%Y%m%d_%H%M%S')
        set_instrument_parameters(file, radar_id, levels, radar)
        #Add the filename to the radar object. So the second trip filter can identify the path where the radar data is.
        radar.metadata['radarfile'] = file 
    except AttributeError as e:
        print(e, 'x - No se pudo crear el objeto Radar de',
              basename, sep='\t')

    return radar


def set_radar_metadata(filename, radar_id, radar):
    """
    Agregar la metadata a un objeto radar

    Parametros
    ----------
    filename : str
        Nombre del archivo
    radar_id : str
        Nombre del radar RMA##, ANG, PAR, PER, MER
    radar : objeto Radar
        Radar a actualizar

    """

    if radar.metadata is None:
        radar.metadata = dict()

    radar.metadata['instrument_name'] = radar_id


def set_instrument_parameters(filename, radar_id, levels, radar):
    """
    Agregar el campo instrument_parameters a un objeto radar

    Parametros
    ----------
    filename : str
        Nombre del archivo
    radar_id : str
        Nombre del radar RMA##, ANG, PAR, PER, MER
    levels : list
        lista con las elevaciones del radar
    radar : objeto Radar
        Radar a actualizar

    """

    if radar.instrument_parameters is None:
        radar.instrument_parameters = dict()
    strategy = get_strat(filename, radar_id, levels, radar)

    radar.instrument_parameters['strategy'] = strategy  # {'long_name': "strategy",}
    nyquist_velocity = get_nyquist_velocity(radar, levels, strategy)
    set_nyquist_velocity(radar, nyquist_velocity)

    set_radar_beam_width(radar)

    set_ray_angle_res(radar, levels)
    set_range(radar)


def add_missing_structures(radar, radar_id):
    """
    Agregar estructuras faltantes al objeto radar. Agrega la altitud agl

    Parametros
    ----------
    radar : objeto Radar
        Radar a actualizar
    radar_id : str
        Nombre del radar RMA# o ANG,PAR, PER

    """
    if radar.altitude_agl is None:
        radar.altitude_agl = dict()

    radar.altitude_agl['data'] = 0.0

    correct_radar_altitude(radar_id, radar)


def get_strat(filename, radar_id, levels, radar):
    """
    Definir la estrategia del radar

    Parametros
    ----------
    filename : str
        Nombre del archivo
    radar_id : str
        Nombre del radar RMA# o ANG,PAR, PER
    levels : list
        lista con las elevaciones del radar
    radar : objeto Radar
        Radar a actualizar

    Devuelve
    --------
    strategy : str
        Estrategia del radar

    """

    strategy = 'Unknown'

    if 'RMA' in radar_id:
        if '9005_01' in filename:  #9005-1 STRATEGY
            nyquist_velocity = 6.63 * np.ones(levels.shape)
            strategy = '9005_01'

        if '9005_02' in filename:  #9005-2 STRATEGY
            nyquist_velocity = 33.04 * np.ones(levels.shape)
            strategy = '9005_02'

        if '9005_03' in filename:  #9005-3 STRATEGY
            nyquist_velocity = 3.98 * np.ones(levels.shape)
            strategy = '9005_03'

        if '0117_01' in filename:  #122-1 STRATEGY
            nyquist_velocity = 6.63 * np.ones(levels.shape)
            strategy = '0117_01'

        if '0117_02' in filename:  #122-2 STRATEGY
            nyquist_velocity = 13.25 * np.ones(levels.shape)
            strategy = '0117_02'

        if '0121_01' in filename:  #122-1 STRATEGY
            nyquist_velocity = 6.63 * np.ones(levels.shape)
            strategy = '0121_01'

        if '0121_02' in filename:  #122-2 STRATEGY
            nyquist_velocity = 13.25 * np.ones(levels.shape)
            strategy = '0121_02'

        if '0122_01' in filename:  #122-1 STRATEGY
            nyquist_velocity = 8.28 * np.ones(levels.shape)
            strategy = '0122_01'

        if '0122_02' in filename:  #122-2 STRATEGY
            nyquist_velocity = 39.79 * np.ones(levels.shape)
            strategy = '0122_02'

        if '0122_03' in filename:  #122-3 STRATEGY
            nyquist_velocity = 13.35 * np.ones(levels.shape)
            strategy = '0122_03'

        if '0123_01' in filename:  #123-1 STRATEGY
            nyquist_velocity = 8.28 * np.ones(levels.shape)
            strategy = '0123_01'

        if '0123_02' in filename:  #123-2 STRATEGY
            nyquist_velocity = 39.79 * np.ones(levels.shape)
            strategy = '0123_02'

        if '0123_03' in filename:  #123-3 STRATEGY
            nyquist_velocity = 13.25 * np.ones(levels.shape)
            strategy = '0123_03'

        if '0123_04' in filename:  #123-4 STRATEGY
            nyquist_velocity = 8.28 * np.ones(levels.shape)
            strategy = '0123_04'

        if '0200_01' in filename:  #200-1 STRATEGY
            nyquist_velocity = 4.42 * np.ones(levels.shape)
            strategy = '0200_01'

        if '0200_02' in filename:  #200-2 STRATEGY
            nyquist_velocity = 13.25 * np.ones(levels.shape)
            strategy = '0200_02'

        if '0300_01' in filename:  #300-1 STRATEGY
            nyquist_velocity = 4.42 * np.ones(levels.shape)
            strategy = '0300_01'

        if '0300_02' in filename:  #300-2 STRATEGY
            nyquist_velocity = 16.56 * np.ones(levels.shape)
            strategy = '0300_02'

        if '0301_01' in filename:  #301-1 STRATEGY
            # Esta estrategia tiene una velocidad nyquist que varia con el
            # angulo de elevacion.
            nyquist_velocity = np.array([4.42, 4.42, 4.42,
                                         6.63, 6.63, 6.63, 6.63,
                                         8.28, 8.28, 8.28, 8.28,
                                         8.28, 8.28, 8.28, 8.28])
            strategy = '0301_01'

        if '0301_02' in filename:  #301-2 STRATEGY
            nyquist_velocity = 13.25 * np.ones(levels.shape)
            strategy = '0301_02'

        if '0201_01' in filename:  #201-1 STRATEGY
            nyquist_velocity = 4.42 * np.ones(levels.shape)
            strategy = '0201_01'

        if '0201_02' in filename:  #201-2 STRATEGY
            nyquist_velocity = 13.25 * np.ones(levels.shape)
            strategy = '0201_02'

        if '0201_03' in filename:  #201-3 STRATEGY
            nyquist_velocity = 8.28 * np.ones(levels.shape)
            strategy = '0201_03'

        if '0202_01' in filename:  #200-1 STRATEGY
            nyquist_velocity = 4.42 * np.ones(levels.shape)
            strategy = '0202_01'

        if '0202_02' in filename:  #200-2 STRATEGY
            nyquist_velocity = 13.25 * np.ones(levels.shape)
            strategy = '0202_02'

        if '0210_01' in filename:  #210-1 STRATEGY
            nyquist_velocity = 6.63 * np.ones(levels.shape)
            strategy = '0210_01'

        if '0211_01' in filename:  #211-1 STRATEGY
            nyquist_velocity = 5.3 * np.ones(levels.shape)
            strategy = '0211_01'

        if '0211_02' in filename:  #211-1 STRATEGY
            nyquist_velocity = 10.6 * np.ones(levels.shape)
            strategy = '0211_02'

        if '0216_01' in filename:  #216-1 STRATEGY
            nyquist_velocity = 6.63 * np.ones(levels.shape)
            strategy = '0216_01'

        if '0216_02' in filename:  #216-2 STRATEGY
            nyquist_velocity = 13.25 * np.ones(levels.shape)
            strategy = '0216_02'

        if '0216_03' in filename:  #216-3 STRATEGY
            nyquist_velocity = 39.79 * np.ones(levels.shape)
            strategy = '0216_03'

        if '0217_01' in filename:  #217-1 STRATEGY
            nyquist_velocity = 6.63 * np.ones(levels.shape)
            strategy = '0217_01'

        if '0217_02' in filename:  #217-2 STRATEGY
            nyquist_velocity = 26.5 * np.ones(levels.shape)
            strategy = '0217_02'

        if '0217_03' in filename:  #217-3 STRATEGY
            nyquist_velocity = 39.79 * np.ones(levels.shape)
            strategy = '0217_03'

        if '0302_01' in filename:  #302-1 STRATEGY
            nyquist_velocity = np.array([4.42, 4.42, 4.42,
                                         6.63, 6.63, 6.63, 6.63,
                                         8.28, 8.28, 8.28, 8.28,
                                         8.28, 8.28, 8.28, 8.28])
            strategy = '0302_01'

        if '0302_02' in filename:  #302-2 STRATEGY
            nyquist_velocity = 16.56 * np.ones(levels.shape)
            strategy = '0302_02'

        if '0302_03' in filename:  #302-3 STRATEGY
            nyquist_velocity = 26.5 * np.ones(levels.shape)
            strategy = '0302_03'

        if '0303_01' in filename:  #303-1 STRATEGY
            nyquist_velocity = np.array([4.42, 4.42, 4.42,
                                         6.63, 6.63, 6.63, 6.63,
                                         8.28, 8.28, 8.28, 8.28,
                                         8.28, 8.28, 8.28, 8.28])
            strategy = '0303_01'

        if '0303_02' in filename:  #303-2 STRATEGY
            nyquist_velocity = 26.5 * np.ones(levels.shape)
            strategy = '0303_02'

        if '0303_03' in filename:  #303-3 STRATEGY
            nyquist_velocity = 39.79 * np.ones(levels.shape)
            strategy = '0303_03'

        if '0309_01' in filename:  #303-1 STRATEGY
            nyquist_velocity = np.array([5.3, 5.3, 5.3,
                                         8.83, 8.83, 8.83, 8.83,
                                         8.83, 8.83, 8.83, 8.83,
                                         8.38, 8.83, 8.83, 8.83])
            strategy = '0309_01'

        if '0309_02' in filename:  #303-2 STRATEGY
            nyquist_velocity = 33.13 * np.ones(levels.shape)
            strategy = '0309_02'


    else:

        if np.max(radar.range['data']) == 119875.0:
            nyquist_velocity = 39.8 * np.ones(levels.shape)  #120
            strategy = '0120_IN'

        if np.max(radar.range['data']) == 239750.0:
            nyquist_velocity = 6.63 * np.ones(levels.shape)  #240
            strategy = '0240_IN'

    return strategy


def get_nyquist_velocity(radar, levels, strategy):
    """
    Obtener la velocidad de Nyquist

    Parametros
    ----------
    radar : objeto Radar
        Radar a actualizar
    levels : list
        lista con las elevaciones del radar
    strategy : str
        Estrategia del radar

    Devuelve
    --------
    nyquist_velocity : array
        velocidad de nyquist en cada nivel

    """
    nyquist_velocity = None
    if strategy == '9005_01':  #9005-1 STRATEGY
        nyquist_velocity = 6.63 * np.ones(levels.shape)

    if strategy == '9005_02':  #9005-2 STRATEGY
        nyquist_velocity = 33.04 * np.ones(levels.shape)

    if strategy == '9005_03':  #9005-3 STRATEGY
        nyquist_velocity = 3.98 * np.ones(levels.shape)

    if strategy == '0117_01':  #122-1 STRATEGY
        nyquist_velocity = 6.63 * np.ones(levels.shape)

    if strategy == '0117_02':  #122-2 STRATEGY
        nyquist_velocity = 13.25 * np.ones(levels.shape)

    if strategy == '0121_01':  #122-1 STRATEGY
        nyquist_velocity = 6.63 * np.ones(levels.shape)

    if strategy == '0121_02':  #122-2 STRATEGY
        nyquist_velocity = 13.25 * np.ones(levels.shape)

    if strategy == '0122_01':  #122-1 STRATEGY
        nyquist_velocity = 8.28 * np.ones(levels.shape)

    if strategy == '0122_02':  #122-2 STRATEGY
        nyquist_velocity = 39.79 * np.ones(levels.shape)

    if strategy == '0122_03':  #122-3 STRATEGY
        nyquist_velocity = 13.35 * np.ones(levels.shape)

    if strategy == '0123_01':  #123-1 STRATEGY
        nyquist_velocity = 8.28 * np.ones(levels.shape)

    if strategy == '0123_02':  #123-2 STRATEGY
        nyquist_velocity = 39.79 * np.ones(levels.shape)

    if strategy == '0123_03':  #123-3 STRATEGY
        nyquist_velocity = 13.25 * np.ones(levels.shape)

    if strategy == '0123_04':  #123-4 STRATEGY
        nyquist_velocity = 8.28 * np.ones(levels.shape)

    if strategy == '0200_01':  #200-1 STRATEGY
        nyquist_velocity = 4.42 * np.ones(levels.shape)

    if strategy == '0200_02':  #200-2 STRATEGY
        nyquist_velocity = 13.25 * np.ones(levels.shape)

    if strategy == '0300_01':  #300-1 STRATEGY
        nyquist_velocity = 4.42 * np.ones(levels.shape)

    if strategy == '0300_02':  #300-2 STRATEGY
        nyquist_velocity = 16.56 * np.ones(levels.shape)

    if strategy == '0301_01':  #301-1 STRATEGY
        # Esta estrategia tiene una velocidad nyquist que varia con el angulo
        # de elevacion.
        nyquist_velocity = np.array([4.42, 4.42, 4.42,
                                     6.63, 6.63, 6.63, 6.63,
                                     8.28, 8.28, 8.28, 8.28,
                                     8.28, 8.28, 8.28, 8.28])

    if strategy == '0301_02':  #301-2 STRATEGY
        nyquist_velocity = 13.25 * np.ones(levels.shape)

    if strategy == '0201_01': #201-1 STRATEGY
        nyquist_velocity = 4.42 * np.ones(levels.shape)

    if strategy == '0201_02':  #201-2 STRATEGY
        nyquist_velocity = 13.25 * np.ones(levels.shape)

    if strategy == '0201_03':  #201-3 STRATEGY
        nyquist_velocity = 8.28 * np.ones(levels.shape)

    if strategy == '0202_01':  #200-1 STRATEGY
        nyquist_velocity = 4.42 * np.ones(levels.shape)

    if strategy == '0202_02':  #200-2 STRATEGY
        nyquist_velocity = 13.25 * np.ones(levels.shape)

    if strategy == '0210_01':  #210-1 STRATEGY
        nyquist_velocity = 6.63 * np.ones(levels.shape)

    if strategy == '0211_01':  #211-1 STRATEGY
        nyquist_velocity = 5.3 * np.ones(levels.shape)

    if strategy == '0211_02':  #211-1 STRATEGY
        nyquist_velocity = 10.6 * np.ones(levels.shape)

    if strategy == '0216_01':  #216-1 STRATEGY
        nyquist_velocity = 6.63 * np.ones(levels.shape)

    if strategy == '0216_02':  #216-2 STRATEGY
        nyquist_velocity = 13.25 * np.ones(levels.shape)

    if strategy == '0216_03':  #216-3 STRATEGY
        nyquist_velocity = 39.79 * np.ones(levels.shape)

    if strategy == '0217_01':  #216-1 STRATEGY
        nyquist_velocity = 6.63 * np.ones(levels.shape)

    if strategy == '0217_02':  #216-2 STRATEGY
        nyquist_velocity = 26.5 * np.ones(levels.shape)

    if strategy == '0217_03':  #216-3 STRATEGY
        nyquist_velocity = 39.79 * np.ones(levels.shape)

    if strategy == '0302_01':  #302-1 STRATEGY
        # Esta estrategia tiene una velocidad nyquist que varia con el angulo
        # de elevacion.
        nyquist_velocity = np.array([4.42, 4.42, 4.42,
                                     6.63, 6.63, 6.63, 6.63,
                                     8.28, 8.28, 8.28, 8.28,
                                     8.28, 8.28, 8.28, 8.28])

    if strategy == '0302_02':  #302-2 STRATEGY
        nyquist_velocity = 16.56 * np.ones(levels.shape)

    if strategy == '0302_03':  #302-3 STRATEGY
        nyquist_velocity = 26.5 * np.ones(levels.shape)

    if strategy == '0303_01':  #302-1 STRATEGY
        # Esta estrategia tiene una velocidad nyquist que varia con el angulo
        # de elevacion.
        nyquist_velocity = np.array([4.42, 4.42, 4.42,
                                     6.63, 6.63, 6.63, 6.63,
                                     8.28, 8.28, 8.28, 8.28,
                                     8.28, 8.28, 8.28, 8.28])

    if strategy == '0303_02':  #302-2 STRATEGY
        nyquist_velocity = 26.5 * np.ones(levels.shape)

    if strategy == '0303_03':  #302-3 STRATEGY
        nyquist_velocity = 39.79 * np.ones(levels.shape)

    if strategy == '0309_01':  #302-1 STRATEGY
        # Esta estrategia tiene una velocidad nyquist que varia con el angulo
        # de elevacion.
        nyquist_velocity = np.array([5.3, 5.3, 5.3,
                                     8.83, 8.83, 8.83, 8.83,
                                     8.83, 8.83, 8.83, 8.83,
                                     8.38, 8.83, 8.83, 8.83])

    if strategy == '0309_02':  #302-2 STRATEGY
        nyquist_velocity = 33.13 * np.ones(levels.shape)



    if strategy == '0120_IN':
        nyquist_velocity = 39.8 * np.ones(levels.shape)  #120

    if strategy == '0240_IN':
        nyquist_velocity = 6.63 * np.ones(levels.shape)  #240

    return nyquist_velocity


def correct_radar_altitude(radar_id, radar):
    """
    Corregir la altitud del radar

    Parametros
    ----------
    radar_id : str
        Nombre del radar RMA# o ANG,PAR, PER
    radar : objeto Radar
        Radar a actualizar

"""

    if radar_id == 'PAR':
        radar.altitude_agl['data'] = np.array(30.0)
        radar.altitude['data'] = np.array(122.0)

    if radar_id == 'ANG':
        radar.altitude_agl['data'] = np.array(30.0)
        radar.altitude['data'] = np.array(190.0)

    if radar_id == 'PER':
        radar.altitude_agl['data'] = np.array(30.0)
        radar.altitude['data'] = np.array(100.0)

    if radar_id == 'RMA1':
        radar.altitude_agl['data'] = np.array(35.0)
        radar.altitude['data'] = np.array(476.0)

    if radar_id == 'RMA2':
        radar.altitude_agl['data'] = np.array(35.0)
        radar.altitude['data'] = np.array(47.0)

    if radar_id == 'RMA3':
        radar.altitude_agl['data'] = np.array(35.0)
        radar.altitude['data'] = np.array(197.0)

    if radar_id == 'RMA4':
        radar.altitude_agl['data'] = np.array(35.0)
        radar.altitude['data'] = np.array(119.0)

    if radar_id == 'RMA5':
        radar.altitude_agl['data'] = np.array(35.0)
        radar.altitude['data'] = np.array(841.0)

    if radar_id == 'RMA6':
        radar.altitude_agl['data'] = np.array(35.0)
        radar.altitude['data'] = np.array(80.0)

    if radar_id == 'RMA7':
        radar.altitude_agl['data'] = np.array(35.0)
        radar.altitude['data'] = np.array(387.0)

    if radar_id == 'RMA8':
        radar.altitude_agl['data'] = np.array(35.0)
        radar.altitude['data'] = np.array(130.0)

    if radar_id == 'RMA9':
        radar.altitude_agl['data'] = np.array(35.0)
        radar.altitude['data'] = np.array(53.0)

    if radar_id == 'RMA10':
        radar.altitude_agl['data'] = np.array(35.0)
        radar.altitude['data'] = np.array(103.0)

    if radar_id == 'RMA11':
        radar.altitude_agl['data'] = np.array(35.0)
        radar.altitude['data'] = np.array(327.0)

    if radar_id == 'RMA12':
        radar.altitude_agl['data'] = np.array(35.0)
        radar.altitude['data'] = np.array(86.0)



def set_nyquist_velocity(radar, nyquist_velocity):
    """
    Agregar la velocidad de nyquist a los parametros del instrumento en
    el radar

    Parametros
    ----------
    radar : objeto Radar
        Radar a actualizar
    nyquist_velocity : array
        velocidad de nyquist en cada nivel

    """
    if (not 'nyquist_velocity' in radar.instrument_parameters) or (radar.instrument_parameters['nyquist_velocity'] == None):
        radar.instrument_parameters['nyquist_velocity'] = dict()
        radar.instrument_parameters['nyquist_velocity']['long_name'] = 'unambiguous_doppler_velocity'
        radar.instrument_parameters['nyquist_velocity']['units'] = 'meters per second'
        radar.instrument_parameters['nyquist_velocity']['meta_group'] = 'instrument_parameters'

        tmp_nyquist = np.ones(np.shape(radar.azimuth['data']))

        #print( np.unique(radar.elevation['data']) , radar.elevation['data'] )
        print( nyquist_velocity )

        for ielev, elev in enumerate(np.unique(radar.elevation['data'])):
            tmp_nyquist[radar.elevation['data']==elev] = nyquist_velocity[ielev]

        radar.instrument_parameters['nyquist_velocity']['data'] = ma.array(tmp_nyquist,
                                                                           mask=np.zeros(np.shape(tmp_nyquist), dtype=bool),)


def set_radar_beam_width(radar):
    """
    Agrega el ancho del haz del radar a los parametros del instrumento

    Parametros
    ----------
    radar : objeto Radar
        Radar a actualizar

     """

    if (not 'radar_beam_width_v' in radar.instrument_parameters) or (radar.instrument_parameters['radar_beam_width_v'] == None):
        radar.instrument_parameters['radar_beam_width_v'] = dict()
        radar.instrument_parameters['radar_beam_width_v']['long_name'] = 'half_power_radar_beam_width_v_channel'
        radar.instrument_parameters['radar_beam_width_v']['units'] = 'degrees'
        radar.instrument_parameters['radar_beam_width_v']['meta_group'] ='instrument_parameters'
        radar.instrument_parameters['radar_beam_width_v']['data'] = ma.array(radar_beam_width_v,
                                                                             mask=False,)

    if (not 'radar_beam_width_h' in radar.instrument_parameters) or (radar.instrument_parameters['radar_beam_width_h'] == None):
        radar.instrument_parameters['radar_beam_width_h'] = dict()
        radar.instrument_parameters['radar_beam_width_h']['long_name'] = 'half_power_radar_beam_width_h_channel'
        radar.instrument_parameters['radar_beam_width_h']['units'] = 'degrees'
        radar.instrument_parameters['radar_beam_width_h']['meta_group'] = 'instrument_parameters'

        radar.instrument_parameters['radar_beam_width_h']['data'] = ma.array(radar_beam_width_h,
                                                                             mask=False,)


def set_ray_angle_res(radar, levels):
    """
    Agrega la resolución angular de los rayos del radar como un atributo
    del objeto radar

    Parametros
    ----------
    radar : objeto Radar
        Radar a actualizar
    levels : list
        lista con las elevaciones del radar

    """
    if radar.ray_angle_res == None:
        radar.ray_angle_res = dict()
        radar.ray_angle_res['long_name'] = 'angular_resolution_between_rays'
        radar.ray_angle_res['units'] = 'degrees'
        radar.ray_angle_res['data'] = ma.array(np.ones(levels.shape)*ray_angle_res,
                                               mask=np.zeros(levels.shape, dtype=bool),)


def set_range(radar):
    """
    Agregar al radar el atributo range con datos de distancias entre gates

    Parametros
    ----------
    radar : objeto Radar
        Radar a actualizar

    """
    if radar.range is None:
        radar.range = dict()

    meters_between_gates = radar.range['data'][1] - radar.range['data'][0]

    if not 'meters_between_gates' in radar.range:
        radar.range['meters_between_gates'] = meters_between_gates

    if not 'meters_to_center_of_first_gate' in radar.range:
        radar.range['meters_to_center_of_first_gate'] = radar.range['data'][0]  #meters_between_gates / 2.0


def set_per_time(radar):
    """
    El radar de Pergamino no guarda la fecha de cada rayo en la key 'time'
    del objeto radar. Para solucionarlo de alguna manera agregamos esta
    función que calcula el tiempo de cada rayo. Esta no es la mejor solución
    ya que no tiene en cuenta el tiempo de elevación y estabilización de la
    antena, pero es la mejor que tenemos hasta el momento.

    """
    ray_i = 0
    ray_e = 360
    time = radar.time['data']
    new_time = np.zeros_like(radar.time['data'])
    n_levs = len(np.unique(radar.elevation['data']))
    for lev in range(n_levs):
        new_time[ray_i:ray_e] = time[ray_i:ray_e] + lev * time[359]
        ray_i+=360
        ray_e+=360

    radar.time['data'] = new_time


def get_file_list( datapath , init_time , end_time , time_search_type = None , file_type_list = None , instrument_type_list = None )     :
   #datapath : base path of radar data
   #init time: datetime object beginning of the time window
   #end time : datetune object end of the time window
   #time_search_type : [filename] or [timestamp]
   #file_types_list  : a list with file extensions that will be included in the file_list

   if time_search_type == None :
      time_search_type = 'timestamp'

   #date_min = datetime.strptime( init_time , '%Y%m%d%H%M%S')
   #date_max = datetime.strptime( end_time  , '%Y%m%d%H%M%S')
   file_list=[]

   for (dirpath, dirnames, filenames) in os.walk( datapath ):
      for filename in filenames            :
         current_filename = '/'.join([dirpath,filename])
         if time_search_type == 'filename'   :
            date_c = get_time_from_filename( current_filename )
         if time_search_type == 'timestamp'  :
            date_c = fromtimestamp( os.stat(current_filename).st_ctime )
         if date_c is not None  :
            if date_c >= init_time and date_c <= end_time  :
               file_list.append( current_filename )
   #Keep only some file names and some paths.
   tmp_file_list = []
   if file_type_list is not None :
      for my_file in file_list  :
         filename = os.path.basename( my_file )
         if any(ft in filename for ft in file_type_list ):
            tmp_file_list.append( my_file )
      file_list = tmp_file_list[:]
   tmp_file_list = []

   if instrument_type_list != None :
      for my_file in file_list :
         if any(it in my_file for it in instrument_type_list ):
            tmp_file_list.append( my_file )
      file_list = tmp_file_list[:]

   return file_list

def find_prev_post_files( datapath , radar_ext , radar_id , radar_time , time_threshold = 600 ) :

    ini_time = radar_time - timedelta( seconds = 2 * time_threshold )
    end_time = radar_time + timedelta( seconds = 2 * time_threshold )
    #Get the file list corresponding to the current instrument.
    file_list = get_file_list( datapath , ini_time , end_time , time_search_type='filename' , file_type_list = [radar_ext], instrument_type_list=[radar_id])
    #Find the previous and/or next volumes 301_01 or _0240
    file_before = None
    file_after  = None
    seconds_before = time_threshold
    seconds_after  = time_threshold

    #Get the closest previous and posterior files 
    #with a 240km strategy.
    for my_file in file_list :
       if 'nc.OK' in my_file :
           continue
       strat = get_strategy_from_filename( my_file )
       if strat == '240' :
           my_time = get_time_from_filename( my_file )
           time_delta = ( my_time - radar_time ).total_seconds()
           if np.abs( time_delta ) <= seconds_after  :
              seconds_after = time_delta 
              file_after = my_file
              time_after = my_time
           if time_delta < 0 and np.abs( time_delta ) <= seconds_before  :
              seconds_before = time_delta 
              file_before = my_file
              time_before = my_time

    print('==========================================')
    print('File after :', os.path.basename( file_after ))
    print('File before :', os.path.basename( file_before ))
    return file_before , file_after 




def get_time_from_filename( file_complete_path )    :
   #import datetime as dt
   #import os 
   filename = os.path.basename( file_complete_path )
   file_time = None

   format = get_format_from_filename( file_complete_path )
   if format == 'h5'    :
      file_time  = datetime.strptime(filename.split('_')[-1][:15], '%Y%m%dT%H%M%S')
   if format == 'vol'   :
      file_time  = datetime.strptime(filename[:14], '%Y%m%d%H%M%S')
   if format == 'nc'   :
      file_time  = datetime.strptime( filename.split('.')[1] , 's%Y%m%d_%H%M%S')
   if format == 'letkf'   :
      file_time  = datetime.strptime(filename.split('_')[-1][:14], '%Y%m%d%H%M%S')
   if format == 'pickle'  :
      file_time  = datetime.strptime(filename.split('_')[-1][:14], '%Y%m%d%H%M%S')
   if format == 'letkf'   :
      file_time  = datetime.strptime(filename.split('_')[-1][:14], '%Y%m%d%H%M%S')
   if format == 'tgz'     :
      file_time  = datetime.strptime(filename[:10], '%Y%m%d_%H')


   return file_time


def get_format_from_filename( filename )            :
   file_format = None
   if ('.h5' in filename ) or ( '.H5' in filename )    :
      file_format = 'h5'
   if ( '.vol' in filename ) or ( '.VOL' in filename ) :
      file_format = 'vol'
   if ( '.nc'  in filename ) or ( '.NC' in filename )  or ( 'cfrad' in filename ) :
      file_format = 'nc'
   if ( '.dat' in filename ) or ( '.DAT' in filename)  :
      file_format = 'letkf'
   if ( '.pkl' in filename ) or ( '.PKL' in filename)  :
      file_format = 'pickle'
   if ( '.tar.gz' in filename ) or ( '.TAR.GZ' in filename ) :
      file_format = 'tgz'

   return file_format

def get_strategy_from_filename(filename):

   s = re.search(r'_\w{4}_\w{2}\.',filename).group()
   if '_01.' in s or '0240' in s:
       strategy = '240'
   elif '_02.' in s or '0120' in s:
      strategy = '120'
   else:
      #print('Strategy not coded', s)
      strategy = None

   return strategy



    
