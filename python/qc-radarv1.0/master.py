import threads_init
import os
import subprocess
import numpy as np
import pandas as pd
import datetime
from datetime import datetime, timedelta
from configparser import ConfigParser, ExtendedInterpolation
import xml.etree.ElementTree as ET
import netCDF4
import pyart

from despachar import Despachar, DespacharLocal, DespacharSlurm




"""
Buscar Volumenes:
1) Se recorre el directorio en busca de NUEVOS archivos de radar
(radarID, tiempo, variable, estrategia, volumen)
2) crea un registro del DF para cada caso encontrado.

Despachar Workers:
1) Genera una lista de variables disponibles para cada
(RadarID, fechaini, config) en el DF
2) Para cada elemento de la lista de 1) se verifica que las variables disponibles
son al menos las que están en config:varlistmin, entonces lanza un Worker
(Radar ID,Fecha, config)
3) Cambiar estado a “Procesando” para todos los registros asociados a cada worker
creado en 2) ---> Escribir el DF

Escribir DF
1) Identificar todos los volúmenes de cada config que estén completos y
procesados (se procesaron todas las variables que figuran en config:varlist)
y eliminarlos del DF
2) Eliminar del DF todos los registros más viejos que :
config:ventana_espera_variable
3) Actualizar el archivo físico del DF
"""

# Funciones auxiliares


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
    if df['file_ext'][row] == 'H5':
        radar_id = df['names'][row].split('_')[0]
    elif df['file_ext'][row] == 'nc':
        radar_id = df['names'][row].split('_')[-3]
    return radar_id


def get_radar_id_INTA(df, row):
    """ Obtengo el nombre del radar Gematronik. """
    with open(df['file_input'][row], 'rb') as f:
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
    if df['file_ext'][row] == 'H5':
        date_RMA = datetime.strptime(file_name.split('.')[0].split('_')[-1],
                                     '%Y%m%dT%H%M%SZ')
    elif df['file_ext'][row] == 'nc':
        date_RMA = datetime.strptime(file_name.split('.')[1],
                                     '%Y%m%d_%H%M%S')
    return date_RMA


def get_date_INTA(df, row):
    """ Obtengo la fecha de radares INTA. """
    file_name = df['names'][row]
    return datetime.strptime(file_name[:14], "%Y%m%d%H%M%S")


def get_fecha_fin(df, row):
    """ Obtengo la fecha del radar. """
    if is_RMA(df, row):
        date = get_date_fin_RMA(df, row)
    else:
        date = get_date_fin_INTA(df, row)
    return date


def get_date_fin_RMA(df, row):
    """ Obtengo la fecha de fin de escaneo de radares RMA. """
    if df['file_ext'][row] == 'H5':
        with netCDF4.Dataset(df['file_input'][row]) as nc:
            ### TODO: Aca puede tirar un OSError al abrir el H5
            # Es conveniente sacar un raise para que un try lo agarre y elimine la fila del df
            dset = [i for i in nc.groups.keys() if 'dataset' in i]
            enddate = nc.groups[f'dataset{len(dset)}']['what'].enddate
            endtime = nc.groups[f'dataset{len(dset)}']['what'].endtime
            date_RMA = datetime.strptime(enddate + endtime,'%Y%m%d%H%M%S')
    elif df['file_ext'][row] == 'nc':
        with netCDF4.Dataset(df['file_input'][row]) as nc:
            end_time = str(netCDF4.chartostring(nc['time_coverage_end'][:]))
            date_RMA = datetime.strptime(end_time, '%Y-%m-%dT%H:%M:%SZ')
    return date_RMA


def get_date_fin_INTA(df, row):
    """ Obtengo la fecha de fin de escaneo de radares INTA. """
    radar = pyart.aux_io.read_rainbow_wrl(df['file_input'][row])
    end_dt = netCDF4.num2date(radar.time['data'][-1], radar.time['units'])
    end_dt += (timedelta(seconds=1) - timedelta(microseconds=end_dt.microsecond))
    # Martin: no se si esto es necesario hacerlo ya que esta en la funcion
    radar = []
    return end_dt


def get_rango(df, row):
    """ Obtengo el rango de alcance del radar. """
    if is_RMA(df, row):
        rango, volumen, estrategia = get_range_RMA(df, row)
    else:
        rango, volumen, estrategia = get_range_INTA(df, row)
    return rango, volumen, estrategia


def get_range_RMA(df, row):
    """ Obtengo el rango de alcance de radares RMA. """
    file_name = df['names'][row]
    if df['file_ext'][row] == 'H5':
        estrategia = file_name.split('_')[1]
        volumen = file_name.split('_')[2]
    elif df['file_ext'][row] == 'nc':
        estrategia = file_name.split('_')[-2]
        volumen = file_name.split('_')[-1].split('.')[0]
    if volumen == '01':
        rango = '240'
    elif volumen == '02':
        rango = '120'
    return rango, volumen, estrategia


def get_range_INTA(df, row):
    """ Obtengo el rango de alcance de radares INTA. """
    with open(df['file_input'][row], 'rb') as f:
        it = ET.iterparse(f, ['start', 'end'])
        for event, elem in it:
            if 'scan' in elem.tag and 'name' in elem.keys():
                volumen = elem.attrib['name']
                break
    if '120' in volumen:
        rango = '120'
        estrategia = '0120'
        volumen = 'IN'
    elif '240' in volumen:
        rango = '240'
        estrategia = '0240'
        volumen = 'IN'
    if '480' in volumen or '400' in volumen:
        rango = '480'
        estrategia = '0480'
        volumen = 'IN'
    return rango, volumen, estrategia


def get_variables(df, row):
    """ Obtengo la fecha del radar. """
    if is_RMA(df, row):
        var = get_variables_RMA(df, row)
    else:
        var = get_variables_INTA(df, row)
    var_estandar = DICT_FIELD_NAMES[var]
    return var_estandar


def get_variables_RMA(df, row):
    """ Obtengo la variable de radares RMA. """
    if df['file_ext'][row] == 'H5':
        var = df['names'][row].split('_')[3]
    elif df['file_ext'][row] == 'nc':
        var = 'reflectivity'  # no corresponde aca
    return var


def get_variables_INTA(df, row):
    """ Obtengo la fecha de radares INTA. """
    var = df['names'][row].split('.')[0][16:]
    return var


def is_RMA(df, row):
    """ Me fijo si el radar es un RMA mirando su nombre. """
    return 'RMA' in df.iloc[row]['names']


def armo_output(df, row, output_path):
    """ Armo el path completo con el nombre de archivo final. """
    fecha_ini = df['fecha_ini'][row].strftime('s%Y%m%d_%H%M%S')
    fecha_fin = df['fecha_fin'][row].strftime('e%Y%m%d_%H%M%S')
    radar_id = df['radar_id'][row]
    estrategia = df['estrategia'][row]
    volumen = df['volumen'][row]

    # cfrad.20200317_225925.RMA8_0200_01.nc
    output = f'{output_path}/cfrad.{fecha_ini}.{fecha_fin}.{radar_id}_{estrategia}_{volumen}.nc'
    return output

# Arranca script
config = ConfigParser(interpolation=ExtendedInterpolation())
config.optionxform = str
config.read("config.ini")
DICT_FIELD_NAMES=config['DICT_FIELD_NAMES']
root = config['System']["DATAROOT"]
BASEDIR=config['System']["BASEDIR"]
win=int(config['System']["WINDOWS"])
lista_radar = [os.path.join(path, name)
               for path, subdirs, files in os.walk(root)
               for name in files if 'BUFR' not in name if 'azi' not in name
               if 'tmp' not in path]

# Borro despues de ventana_espera_variable
fecha_para_borrar = datetime.utcnow() - timedelta(minutes=win)
print(datetime.utcnow())

# Si tengo muchos datos viejos los saco antes de armar el nuevo dataframe
for i in lista_radar[:]:
    if ('ANG' in i) or ('PAR' in i) or ('PER' in i):
        try:
            dt = datetime.strptime(i.split('/')[-1][:14], '%Y%m%d%H%M%S')
        except ValueError:
            lista_radar.remove(i)
            continue
        if dt < fecha_para_borrar:
            lista_radar.remove(i)
    elif 'RMA' in i:
        try:
            dt = datetime.strptime(i.split('_')[-1], '%Y%m%dT%H%M%SZ.H5')
        except ValueError: # esto es cuando rsync no termino de sincronizar
            lista_radar.remove(i)
            continue
        if dt < fecha_para_borrar:
            lista_radar.remove(i)
        if not i.endswith('H5'):
            lista_radar.remove(i)

df_filename = BASEDIR + '/df_qc-radar.csv'
# abro el DataFrame
df = pd.read_csv(df_filename)
# Puede ser que alguna de las lineas del csv esté corrompida. Tengo que eliminarla
for ind in df.index:
    try:
        pd.to_datetime(df.loc[ind]['fecha_ini'], infer_datetime_format=True)
    except ValueError as e:
        print(e)
        df.drop([ind], inplace=True)


df['fecha_ini'] = pd.to_datetime(df['fecha_ini'], infer_datetime_format=True)
df['fecha_fin'] = pd.to_datetime(df['fecha_fin'], infer_datetime_format=True)

# busco en el df las columas de file_input y si coinciden con
# lista_radar las saco de la lista
for i in df['file_input']:
    if i in lista_radar[:]:
        lista_radar.remove(i)

# ACA PUEDE IR LO DE REVISAR QUE EL ARCHIVO ESTE COMPLETO Y PASARLO DE P A T

# Tengo que revisar primero si los archivos de lista_radar se pueden leer
# En ocasiones da un OSError cuando lee con netCDF4
for i in lista_radar[:]:
    if not 'RMA' in i:
        continue

    try:
        with netCDF4.Dataset(i) as nc:
            dset = [i for i in nc.groups.keys() if 'dataset' in i]
            # dummy para que lea algo
    except OSError as e:
        print(f'Hubo un OSError en {i}, lo saco de la lista. ', e)
        lista_radar.remove(i)

# A partir de aca se arma el nuevo dataframe con los datos que mas recientes
df_files = pd.DataFrame(lista_radar, columns=['file_input'])

# Extraigo la extension usando el ultimo punto
file_ext = [file.split('.')[-1] for file in df_files['file_input']]
df_files['file_ext'] = file_ext

# Extraigo el nombre del archivo usando la ultima barra del path
names = [df_files['file_input'][row].split('/')[-1] for row in range(len(df_files))]
df_files['names'] = names

radar_ids = [get_radar_id(df_files, row) for row in range(len(df_files))]
df_files['radar_id'] = radar_ids

# saco rango, volumen y estrategia de los archivos
ranges = [get_rango(df_files, row) for row in range(len(df_files))]
rangos = [i[0] for i in ranges]
volumenes = [i[1] for i in ranges]
estrategias = [i[2] for i in ranges]
df_files['range'] = rangos
df_files['volumen'] = volumenes
df_files['estrategia'] = estrategias

# extraigo cual es la variable de cada archivo
variables = [get_variables(df_files, row) for row in range(len(df_files))]
df_files['variable'] = variables

# extraigo las fechas de inicio del volumen
dates = [get_fechas(df_files, row) for row in range(len(df_files))]
df_files['fecha_ini'] = dates

# extraigo las fechas de fin del volumen
dates_fin = [get_fecha_fin(df_files, row) for row in range(len(df_files))]
df_files['fecha_fin'] = dates_fin

#### hasta aca trabajo con los nombres de los archivos
#### desde aca trabajo con los config

# config es un dict de dict, primero config (tiemporeal asimilacion), luego
# los valores de la configuracion (varlist varlistmin) y el path de salida
dict_config = {}
for modo in config['System']["USERCONF"].split(','):
    tmpconf = ConfigParser(interpolation=ExtendedInterpolation())
    tmpconf.optionxform = str
    tmpconf.read(modo+".qcr")
    dict_config[modo]={
        'instrument_list':tmpconf['Base']['INSTRUMENT_LIST'],
        'varlist':tmpconf['Base']['VARLIST'],
        'varlistmin': tmpconf['Base']['VARLISTMIN'],
        'out_path': tmpconf['Base']['NETCDF_OUTPATH']}

# Agrego estas columnas con NaN para despues llenarlas con lo que
# este en el diccionario de config en df_files, las filas que tengan
# coincidencia con el diccionario se agregan al df principal
df_files['config'] = np.nan
df_files['state'] = np.nan
df_files['file_output'] = np.nan

for nombre_config in dict_config.keys():
    varlist = dict_config[nombre_config]['varlist']
    out_path = dict_config[nombre_config]['out_path']
    instrument_list = dict_config[nombre_config]['instrument_list']
    for row in range(len(df_files)):
        if df_files['variable'][row] in varlist and df_files['radar_id'][row] in instrument_list:
            # TODO: Aca hay un bug donde siempre levanta el RMA1 si existe un RMA1X en instrument_list
            df_files['config'][row] = nombre_config
            df_files['state'][row] = 'D'
            df_files['file_output'][row] = armo_output(df_files, row, out_path)
            df = df.append(df_files.iloc[row])

# TODO: Agregar varlistmin aca.
# En el for de arriba se agregaron al df principal los archivos que tienen las
# variables elegidas en varlist. Pero no puedo comprobar que esten las de
# varlistmin porque voy iterando a medida que aparecen. Como esto cambia por
# cada configuracion entonces tengo que volver a recorrer el dataframe y
# buscar los conjuntos de config/file_output y revisar que este lo necesario de
# varlistmin, sino lo saco del df principal
"""IDEA:
for i in np.unique(df['file_output']).tolist():
    for nombre_config in dict_config.keys():
        varlistmin = dict_config[nombre_config]['varlistmin']
        df_prov = df.loc[df['file_output'] == i]
        if (df_prov['state'] == 'D').any():
            if df_prov['variable'] in varlistmin: (?)
                df.remove(...)
"""

# a esta altura ya tengo mergeado el df original con el nuevo y unicamente con
# las variables que esten en varlist para cada config.

# preparo la lista de tuplas para mandar al dispatcher/Workers
# (radar_id, fecha_ini, config)

lista_workers = []
# como tengo separados los output_path, busco los nombres de archivo final
for i in np.unique(df['file_output']).tolist():
    df_prov = df.loc[df['file_output'] == i]
    # si alguno de los archivos esta en D, lo agrego a la lista de workers
    if (df_prov['state'] == 'D').any():
        radar_id = df_prov.iloc[0]['radar_id']
        fecha_ini = df_prov.iloc[0]['fecha_ini'].strftime('%Y-%m-%d %H:%M:%S')
        fecha_fin = df_prov.iloc[0]['fecha_fin'].strftime('%Y-%m-%d %H:%M:%S')
        config_tupla = df_prov.iloc[0]['config']

        lista_workers.append((radar_id, fecha_ini, fecha_fin, config_tupla))
        # cambio los estados de todos los que mando a procesar a P
        df.loc[df['file_output'] == i, 'state'] = 'P'

print(lista_workers)
df.to_csv(df_filename, index=False)
maxRadar = config["System"]["MAXRADAR"]
maxTHR = config["System"]["THREADS"]

if lista_workers:
    if config['System']['MODO'] == 'local' :
        despachados = DespacharLocal(int(maxRadar), int(maxTHR))
    else:
        despachados = DespacharSlurm(maxRadar, maxTHR)
    despachados.despachar(lista_workers)

# Borro los datos anteriores a fecha_para_borrar
df = df[df['fecha_ini'] > fecha_para_borrar]
df.to_csv(df_filename, index=False)
