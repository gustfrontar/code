from __future__ import print_function

import os
import glob
import numpy as np

try:
    import wradlib
except ImportError:
    print('Error importando wradlib')
except:
    print('No está instalado wradlib.')


import pyart
from datetime import datetime, timedelta
from netcdftime import utime
import gc

from ..conversores.hdf5_to_cfrad_EZE import hdf5_to_cfrad


def leo_radar(datetime_final=None, n_horas_atras=None,
              disdro=None, nombre=None, disdro_lat=None, disdro_lon=None,
              importo_datos=False, path_out_hdf_ezeiza=None,
              archivos_radar=None):
    """
    Esta funcion utiliza la clase RadarReaderForDSD para generar un
    diccionario con la reflectividad del punto y la hora de inicio del
    escaneo.

    Parametros
    ----------
    datetime_final : datetime obj
        Fecha (anio, mes, dia, hora) del ultimo dato a buscar.

    n_horas_atras : int
        Cantidad de horas hacia atras desde el ultimo dato.

    disdro : str o None
        Sigla del sitio del disdrometro. Por ejemplo SADL para La Plata
        y CUBA para Ciudad Universitaria.

    nombre : str
        Sigla del radar a analizar, por ejemplo EZE o RMA1.

    disdro_lat : float o None
        Coordenada decimal. En caso de no especificar sitio de
        disdrometro, se puede usar para conseguir la reflectividad
        promedio y el pixel mas cercano del radar a ese sitio.
        Preferentemente con precision de 6 decimales para comparar con
        el objeto radar creado por PyART.

    disdro_lon : float o None
        Idem disdro_lat.

    path_out_hdf_ezeiza : str
        Path donde guardar los datos hdf de manera local.

    Devuelve
    --------
    RAD : dict
        Diccionario con campos data y time, que contienen la
        reflectividad puntual y el tiempo del inicio del escaneo donde
        fue obtenido ese valor.

    """

    if importo_datos:
        if nombre == 'EZE':
            _importo_datos_EZE(path_out_hdf_ezeiza, datetime_final,
                               n_horas_atras)

            archivos_radar = glob.glob(
                os.path.expanduser(path_out_hdf_ezeiza)+'*.hdf')
            archivos_radar.sort()

    RAD = {}
    RAD['data'] = []
    RAD['time'] = []

    for archivo in archivos_radar:
        radar = RadarReaderForDSD(archivo, disdro=disdro, nombre=nombre,
                                  disdro_lat=disdro_lat, disdro_lon=disdro_lon)

        RAD['time'].append(radar.time)

        '''
        # Aca abajo es para agregar la ref promedio de una caja
        if type(radar.mean_ref)==np.float64:
            RAD['data'].append(float(radar.mean_ref))
        else:
            RAD['data'].append(float(radar.mean_ref.data))
        '''

        if radar.dbz.data == 0.:
            radar.dbz = np.ma.masked_values(radar.dbz, 0.)
            RAD['data'] = np.ma.append(RAD['data'], radar.dbz)
        else:
            RAD['data'] = np.ma.append(RAD['data'], radar.dbz)

        # Esto no es lo mas eficiente pero si no fuerzo al garbage collector
        # el programa se vuelve inmanejable.
        gc.collect()
        if importo_datos:
            os.remove(archivo)

    return RAD


class RadarReaderForDSD(object):
    """
    Esta clase contiene el metodo no-publico que convierte los archivos
    HDF5 de Ezeiza en objetos radar de PyART.

    Parametros
    ----------
    filename : str
        Path completo del archivo de radar. Esta optimizado actualmente
        para usarse con los H5 del radar de Ezeiza.

    disdro : str o None
        Sigla del sitio del disdrometro. Por ejemplo SADL para La Plata
        y CUBA para Ciudad Universitaria.

    nombre : str
        Sigla del radar a analizar, por ejemplo EZE o RMA1.

    disdro_lat : float o None
        Coordenada decimal. En caso de no especificar sitio de
        disdrometro, se puede usar para conseguir la reflectividad
        promedio y el pixel mas cercano del radar a ese sitio.
        Preferentemente con precision de 6 decimales para comparar con
        el objeto radar creado por PyART.

    disdro_lon : float o None
        Idem disdro_lat.

    Atributos
    ---------
    nombre : str
        Nombre del radar.

    min_azi : float
        Para un sweep es el azimuth mas cercano al punto seleccionado.

    min_rango : float
        Para un sweep es el gate mas cercano al punto seleccionado.

    dbz : float
        Factor de reflectividad en el punto mas cercano.

    mean_ref : float
        Factor de reflectividad promediado en una caja de, por defecto,
        2x2.

    time : datetime obj
        Tiempo de inicio del escaneo.

    """

    def __init__(self, filename, disdro=None, nombre=None, disdro_lat=None,
                 disdro_lon=None):
        self.filename = filename

        self.__radar = []
        self.nombre = nombre

        self.min_azi = []
        self.min_rango = []
        self.time = []

        self.dbz = []
        self.mean_ref = []

        if disdro == 'SADL':
            self.disdro_lat = -34.966184
            self.disdro_lon = -57.896028
        elif disdro == 'CUBA':
            self.disdro_lat = -34.541936
            self.disdro_lon = -58.442274
        elif disdro == 'RMA1':
            self.disdro_lat = -31.521422
            self.disdro_lon = -64.465536
        elif disdro is None:
            # Recordar usar 6 decimales
            self.disdro_lat = disdro_lat
            self.disdro_lon = disdro_lon

        if self.nombre == 'EZE':
            radar_prov = EZE_hdf5_to_cfrad(self.filename, disdro=True)
            self.__radar = radar_prov.extract_sweeps([0])
        elif self.nombre == 'RMA1':
            radar_prov = pyart.io.read(filename)
            self.__radar = radar_prov.extract_sweeps([0])

        self._tiempo()

        self.min_azi, self.min_rango = gate_mas_cercano(self.__radar,
                                                        self.disdro_lat,
                                                        self.disdro_lon)

        self.dbz = _reflectividad_punto(self.__radar, self.min_azi,
                                        self.min_rango)

        self.mean_ref = _reflectividad_promedio(self.__radar, self.min_azi,
                                                self.min_rango)

    def _tiempo(self):
        if self.nombre == 'EZE':
            fecha = (self.filename).split('/')[-1][11:-6]
            self.time = datetime.strptime(fecha, '%Y%m%d-%H%M')
        if self.nombre == 'RMA1':
            self.time = datetime.strptime(self.__radar.time['units'][-20:-4],
                                          '%Y-%m-%dT%H:%M')


def gate_mas_cercano(radar, disdro_lat, disdro_lon):
    """
    Este metodo usa la latitud y longitud del punto dados en el init
    de la clase para buscar el gate mas cercano a esa ubicacion.
    Estos valores son opcionales en la clase, aunque designando un
    disdrometro en el init se asignan los valores.

    Parametros
    ----------
    radar : obj
        Objeto radar de PyART
    disdro_lat : float
        Latitud del punto deseado
    disdro_lon : float
        longitud del punto deseado

    Devuelve
    --------
    min_azi: int
        Indice del azimuth más cercano al punto.
    min_rango: int
        Indice del rango más cercano al punto.

    """

    # Redondeo los arrays de lat y lon a la misma cantidad de decimales
    # de la ubicación
    lat_round = np.around(radar.gate_latitude['data'], 6)
    lon_round = np.around(radar.gate_longitude['data'], 6)

    lon = lon_round - disdro_lon
    lat = lat_round - disdro_lat

    # Para encontrar el gate más cercano
    suma_lat_lon = np.add(np.absolute(lon), np.absolute(lat))

    min_azi, min_rango = np.where(suma_lat_lon == suma_lat_lon.min())

    min_azi, min_rango = min_azi[0], min_rango[0]

    return min_azi, min_rango


def _reflectividad_punto(radar, min_azi, min_rango):
    '''
    Tomo la reflectividad del gate más cercano
    '''
    dbz = np.ma.asarray(radar.fields['TH']['data'][min_azi][min_rango])

    return dbz


def _reflectividad_promedio(radar, min_azi, min_rango):
    '''
    Extraigo del objeto radar la reflectividad promedio para el
    punto mas cercano de la primera elevacion al disdrometro.
    '''
    fila = []
    inf_caja = 2
    sup_caja = 3
    minazi = min_azi - inf_caja
    maxazi = min_azi + sup_caja
    minran = min_rango - inf_caja
    maxran = min_rango + sup_caja

    for i in range(inf_caja + sup_caja):
        fila.append(
            np.ma.mean(radar.fields['TH']['data'][minazi:maxazi][i]
                       [minran:maxran]))

    mean_ref = np.ma.mean(fila)

    return mean_ref


def _importo_datos_EZE(path_out_hdf_ezeiza, datetime_final, n_horas_atras):
    """
    Esta funcion copia los datos H5 del radar (cada 10 minutos) en un
    directorio temporal.

    Parametros
    ----------
    path_out_hdf_ezeiza : str
        Path donde guardar los datos hdf de manera local.

    datetime_final : datetime obj
        Fecha (anio, mes, dia, hora) del ultimo dato a buscar.

    n_horas_atras : int
        Cantidad de horas hacia atras desde el ultimo dato.

    """

    # Ingreso el datetime final y lo convierto a la cadena de caracteres que
    # necesito.
    datetime_final_str = datetime_final.strftime('%Y%m%d%H')

    path_datos = os.path.expanduser(path_out_hdf_ezeiza)

    # Si este modulo llega a crecer el usuario se puede pasar como
    # parametro de la funcion.
    usuario = 'mrugna'

    # Ahora hago un for sobre la cantidad de horas totales
    for i in range(n_horas_atras, 0, -1):
        delta = timedelta(hours=i)

        fecha_atras = datetime_final - delta
        fecha_atras_str = fecha_atras.strftime('%Y%m%d%H')

        os.system('scp {usuario}@10.10.23.168:/yanina-sol-ms1/radar/eze/'
                  '{ANIO1}{MES1}{DIA1}/hdf5/'
                  'EZE-PPIVol-{ANIO2}{MES2}{DIA2}-{HORA2}?0??.hdf '
                  '{path}'.format(
                      usuario=usuario, ANIO1=fecha_atras_str[0:4],
                      MES1=fecha_atras_str[4:6], DIA1=fecha_atras_str[6:8],
                      ANIO2=fecha_atras_str[0:4], MES2=fecha_atras_str[4:6],
                      DIA2=fecha_atras_str[6:8], HORA2=fecha_atras_str[8:10],
                      path=path_datos))

    # Copio el ultimo dato
    os.system('scp {usuario}@10.10.23.168:/yanina-sol-ms1/radar/eze/'
              '{ANIO1}{MES1}{DIA1}/hdf5/'
              'EZE-PPIVol-{ANIO2}{MES2}{DIA2}-{HORA2}00??.hdf {path}'.format(
                  usuario=usuario, ANIO1=datetime_final_str[0:4],
                  MES1=datetime_final_str[4:6], DIA1=datetime_final_str[6:8],
                  ANIO2=datetime_final_str[0:4], MES2=datetime_final_str[4:6],
                  DIA2=datetime_final_str[6:8], HORA2=datetime_final_str[8:10],
                  path=path_datos))
