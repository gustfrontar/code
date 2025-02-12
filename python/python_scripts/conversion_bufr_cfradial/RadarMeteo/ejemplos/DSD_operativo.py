# -*- coding: utf-8 -*-

import datetime

from RadarMeteo.disdrometro.disdro_parsivel import leo_parsivel
from RadarMeteo.disdrometro.lector_ezeiza import leo_radar
from RadarMeteo.disdrometro.grafico_disdro import grafico

# Doy la fecha de final del grafico
anio = 2016
mes = 3
dia = 12
hora_final = 0  # UTC
n_horas_atras = 24

# Disdrometro que uso
'''
En caso de no querer especificar el aparato se debe definir un latitud
y longitud de referencia y hacer el mismo calculo en la funcion leo_radar
'''

disdro = 'CUBA'
# nombre del Radar
nombre = 'EZE'

path_datos_disdro = '/ms-36/mrugna/data/CUBA'
path_out_hdf_ezeiza = '/ms-36/mrugna/tmp'
path_out = '/ms-36/mrugna/imagenes'

datetime_final = datetime.datetime(anio, mes, dia, hora_final)

# Genero un objeto dsd con los datos del disdrometro
if disdro is not None:
    dsd = leo_parsivel(path_datos_disdro, datetime_final,
                       n_horas_atras, disdro=disdro)
elif disdro is None:
    dsd = None

# Genero los datos de radar
if nombre is not None:
    RAD = leo_radar(path_out_hdf_ezeiza, datetime_final, n_horas_atras,
                    disdro=disdro, nombre=nombre)
elif nombre is None:
    RAD = None

# Grafico llamando a la funcion
grafico(path_out, datetime_final, n_horas_atras, disdro=disdro, dsd=dsd,
        nombre=nombre, RAD=RAD)
