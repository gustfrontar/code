import os
import subprocess
import numpy as np
import pandas as pd
import datetime
from datetime import datetime, timedelta

import xml.etree.ElementTree as ET


def get_radar_data(root=None, opt=None):
    """
    Esta funcion lee archivos desde un directorio `root` y parsea toda la
    informacion posible, dependiendo del modelo del radar, hacia un DataFrame
    de Pandas.

    Los archivos en forma operativa se encuentran con la siguiente estructura:

    - Para radares Gematronik - INTA
        /dir/dir/dir/radar_id/strat/*.vol
    - Para radares RMA
        /dir/dir/dir/radar_id/yyyy/mm/dd/HH/MMSS/filename.ext

    Aunque en esta version no es necesario que se pase toda la ruta sino
    que la ruta base donde estan todos los archivos (hasta radar_id).

    Parametros
    ----------
    root : str
        Nombre de la carpeta que contiene a los archivos de radar sin importar
        el modelo.

    opt : clase BaseOptions
        Opciones que vienen de las variables de entorno.

    Devuelve
    --------
    df : DataFrame de pandas indexado por fecha.
        Las columnas del DataFrame son:
        'file_path': Path completo del archivo.
        'ext': Extension del archivo (nc, vol, H5).
        'radar_id': Nombre del radar (RMA1, RMA2,..., ANG, PAR, PER).
        'names': Nombre del archivo.

    Notas
    -----
    Este DataFrame debe ser utilizado en conjunto con `utils.filter_data` para
    poder elegir efectivamente cuales de los archivos seran procesados.
    Alli se parsean desde el entorno los instrumentos, extensiones, fechas
    iniciales y finales y se devuelve el DataFrame filtrado. Mas detalles en
    el archivo referido.

    Esta funcion asume que van a ingresar datos de RMA o INTA. No esta probado
    con otros radares.

    """

    # Genero una lista con todos los archivos y sus path
    lista_radar = [os.path.join(path, name)
                   for path, subdirs, files in os.walk(root)
                   for name in files if 'BUFR' not in name
                   if 'tmp' not in path]

    if opt is not None:

        init = datetime.strptime(opt.start_date, "%Y%m%d_%H%M%S")
        endt = datetime.strptime(opt.end_date, "%Y%m%d_%H%M%S")

        for i in lista_radar[:]:
            if ('ANG' in i) or ('PAR' in i) or ('PER' in i):
                dt = datetime.strptime(i.split('/')[-1][:14], '%Y%m%d%H%M%S')
                if dt > endt or dt < init:
                    lista_radar.remove(i)
            elif 'RMA' in i:
                ext = i.split('.')[-1]
                if ext == 'nc':
                    dt = datetime.strptime(i.split('.')[1],
                                     '%Y%m%d_%H%M%S')
                else:
                    dt = datetime.strptime(i.split('_')[-1], '%Y%m%dT%H%M%SZ.H5')
                if dt > endt or dt < init:
                    lista_radar.remove(i)

    # Genero el DataFrame
    df = pd.DataFrame(lista_radar, columns=['file_path'])

    # Extraigo la extension usando el ultimo punto
    ext = [file.split('.')[-1] for file in df['file_path']]
    df['ext'] = ext

    # Extraigo el nombre del archivo usando la ultima barra del path
    names = [df['file_path'][row].split('/')[-1] for row in range(len(df))]
    df['names'] = names

    radar_ids = [get_radar_id(df, row) for row in range(len(df))]
    df['radar_id'] = radar_ids

    ranges = [get_rango(df, row) for row in range(len(df))]
    df['range'] = ranges

    dates = [get_fechas(df, row) for row in range(len(df))]
    df['dates'] = dates
    df = df.set_index('dates')

    df.sort_index(inplace=True)

    if opt is not None:
        instruments = opt.instrument_list
        file_ext = opt.file_ext

        df = df[df['radar_id'].isin(instruments)]
        df = df[df['ext'].isin(file_ext)]

    return df

# Funciones auxiliares


def get_radar_id(df, row):
    """ Obtengo el nombre del radar en cada fila. """
    if is_RMA(df, row):
        radar_id = get_radar_id_RMA(df, row)
    else:
        # TODO: Generar un getter para saber si el radar es Gematronik
        radar_id = get_radar_id_INTA(df, row)
    return radar_id


def get_radar_id_RMA(df, row):
    """ Obtengo el nombre del radar RMA. """
    if df['ext'][row] == 'H5':
        radar_id = df['names'][row].split('_')[0]
    elif df['ext'][row] == 'nc':
        radar_id = df['names'][row].split('_')[-3]
    return radar_id


def get_radar_id_INTA(df, row):
    """ Obtengo el nombre del radar Gematronik. """
    with open(df['file_path'][row], 'rb') as f:
        it = ET.iterparse(f, ['start', 'end'])
        for event, elem in it:
            if 'radarinfo' in elem.tag:
                radar_id = elem.attrib['id']
                break
    return radar_id


def get_fechas(df, row):
    """ Obtengo la fecha del radar. """
    if is_RMA(df, row):
        date = get_date_RMA(df, row)
    else:
        date = get_date_INTA(df, row)
    return date


def get_date_RMA(df, row):
    """ Obtengo la fecha de radares RMA. """
    file_name = df['names'][row]
    if df['ext'][row] == 'H5':
        date_RMA = datetime.strptime(file_name.split('.')[0].split('_')[-1],
                                     '%Y%m%dT%H%M%SZ')
    elif df['ext'][row] == 'nc':
        date_RMA = datetime.strptime(file_name.split('.')[1],
                                     '%Y%m%d_%H%M%S')

    return date_RMA


def get_date_INTA(df, row):
    """ Obtengo la fecha de radares INTA. """
    file_name = df['names'][row]
    return datetime.strptime(file_name[:14], "%Y%m%d%H%M%S")


def get_rango(df, row):
    """ Obtengo el rango de alcance del radar. """
    if is_RMA(df, row):
        rango = get_range_RMA(df, row)
    else:
        rango = get_range_INTA(df, row)
    return rango


def get_range_RMA(df, row):
    """ Obtengo el rango de alcance de radares RMA. """
    file_name = df['names'][row]
    if df['ext'][row] == 'H5':
        estrategia = file_name.split('_')[1]  # no se usa por ahora
        volumen = file_name.split('_')[2]
    elif df['ext'][row] == 'nc':
        volumen = file_name.split('_')[-1].split('.')[0]
    if volumen == '01':
        rango = '240'
    elif volumen == '02':
        rango = '120'
    return rango


def get_range_INTA(df, row):
    """ Obtengo el rango de alcance de radares INTA. """
    with open(df['file_path'][row], 'rb') as f:
        it = ET.iterparse(f, ['start', 'end'])
        for event, elem in it:
            if 'scan' in elem.tag and 'name' in elem.keys():
                volumen = elem.attrib['name']
                break
    if '120' in volumen:
        rango = '120'
    elif '240' in volumen:
        rango = '240'
    if '480' in volumen or '400' in volumen:
        rango = '480'
    return rango


def is_RMA(df, row):
    """ Me fijo si el radar es un RMA mirando su nombre. """
    return 'RMA' in df.iloc[row]['names']


def get_so_radar_data(root=None, opt=None):
    """
    Parametros
    ----------
    root : str
        Nombre de la carpeta que contiene a los archivos de radar.

    opt : clase BaseOptions
        Opciones que vienen de las variables de entorno.

    Devuelve
    --------
    df : DataFrame de pandas indexado por fecha.
        Las columnas del DataFrame son:
        'file_path': Path completo del archivo.
        'ext': Extension del archivo (nc, vol, H5).
        'radar_id': Nombre del radar (RMA1, RMA2,..., ANG, PAR, PER).

    """

    # Genero una lista con todos los archivos y sus path
    lista_radar = [os.path.join(path, name)
                   for path, subdirs, files in os.walk(root)
                   for name in files]

    # Compute slot inital and final time according to window type
    if isinstance(opt.reference_date, str):
       reference_date = datetime.strptime(opt.reference_date, '%Y%m%d_%H%M%S')

    if opt.window_type == 'centered':
        start_date = reference_date - timedelta(minutes=opt.window_length/2.0)
        end_date = reference_date + timedelta(minutes=opt.window_length/2.0)
    elif opt.window_type == 'backward':
        start_date = reference_date - timedelta(minutes=opt.window_length)
        end_date = reference_date
    elif opt.window_type == 'forward':
        start_date = reference_date
        end_date = reference_date + timedelta(minutes=opt.window_length)

    # Genero el DataFrame
    df = pd.DataFrame(lista_radar)
    df['file_path'] = df[0]  # genero columna file_path con todo el path

    # Get the dates
    dates = [datetime.strptime(file.split('.')[-3], '%Y%m%d_%H%M%S')
             for file in df['file_path']]
    df['dates'] = dates
    df = df.set_index('dates') #set dates as index
    mask = (df.index > start_date) & (df.index <= end_date)
    df = df.loc[mask]

    #Split the file by the '.' to get the extention
    ext = [file.split('.')[-1] for file in df['file_path']]
    df['ext'] = ext
    file_ext = opt.file_ext
    df = df[df['ext'].isin(file_ext)]

    # Get the radar name
    radar_id = [file.split('.')[-2].split('_')[0] for file in df['file_path']]
    df['radar_id'] = radar_id

    instruments = opt.instrument_list
    df = df[df['radar_id'].isin(instruments)]

    return df
