# -*- coding: utf-8 -*-

from __future__ import print_function

import subprocess
import shlex
import os
from datetime import datetime, timedelta

import numpy as np

# import ClaseTMatrix

'''
ingreso ese diccionario a DropSizeDistribution
    calculo datos con la TMatrix
    puedo sacar datos extra como RR, Z-R, KDP-R...

'''


def leo_parsivel(path_datos_disdro, datetime_final, n_horas_atras, disdro):
    '''
    Esta funcion va a tener relevancia cuando se realicen analisis con
    la T-Matrix dado que se crea otra clase que lea las listas
    devueltas por ParsivelReader y les agregue las variables calculadas
    por el modelo.

    Parametros
    ----------
    path_datos_disdro : str
        Path donde se encuentran los archivos de texto del disdrometro.

    datetime_final: datetime obj
        Fecha (anio, mes, dia, hora) del ultimo dato a buscar. En hora
        UTC.

    n_horas_atras : int
        Cantidad de horas atras que voy a buscar datos.

    disdro : str
        Sigla del disdrometro a levantar.

    '''
    dsd = ParsivelReader(path_datos_disdro, datetime_final,
                         n_horas_atras, disdro)

    # Falta ACA un constructor que use la T-Matrix

    return dsd


class ParsivelReader(object):
    '''
    Dado un archivo de texto crudo del disdrometro (previo filtro de
    horas), genera un objeto con las reflectividades calculadas por el
    disdrometro y a mano, ademas de las horas.

    Parametros
    ----------
    path_datos_disdro : str
        Path donde se encuentran los archivos de texto del disdrometro.

    datetime_final: datetime obj
        Fecha (anio, mes, dia, hora) del ultimo dato a buscar. En hora
        UTC.

    n_horas_atras : int
        Cantidad de horas atras que voy a buscar datos.

    disdro : str
        Sigla del disdrometro a levantar.

    Atributos
    ---------
    time : lista
        Lista conteniendo el tiempo de cada dato del disdrometro.

    fields : dict
        Diccionario con los campos de reflectividad calculada.

    dg : lista
        Lista con la densidad de gotas. Sirve como parametro de entrada
        para T-Matrix.

    '''
    def __init__(self, path_datos_disdro, datetime_final,
                 n_horas_atras, disdro):
        self.time = []

        # Creo aca el diccionario de campos, despues lo puedo actualizar en
        # la clase que usa la T-Matrix
        self.fields = {}
        self.fields['ZH_par'] = {'data': []}
        self.fields['ZH_ray'] = {'data': []}
        self.dg = []

        self._texto = []
        self._importo_datos(path_datos_disdro, datetime_final,
                            n_horas_atras, disdro)

        self._creo_array_ref()

    def _importo_datos(self, path_datos_disdro, datetime_final,
                       n_horas_atras, disdro):
        """
        Crea una lista con cada linea que grep encuentra. Con esto evito
        crear un archivo temporal cada vez que uso la funcion.

        Parametros
        ----------
        path_datos_disdro : str
            Path donde se encuentran los archivos de texto del
            disdrometro.

        datetime_final: datetime obj
            Fecha (anio, mes, dia, hora) del ultimo dato a buscar. En
            hora UTC.

        n_horas_atras : int
            Cantidad de horas hacia atras que se va a buscar datos.

        disdro : str
            Sigla del disdrometro a consultar.

        Devuelve
        --------
        self._texto: lista
            Lista conteniendo los datos crudos del disdrometro.

        """

        fecha_final = datetime_final
        fecha_final_str = fecha_final.strftime('%Y%m%d%H')

        utc = timedelta(hours=3)

        hora_local_final = fecha_final - utc
        hora_local_final_str = hora_local_final.strftime('%Y%m%d%H')

        path_datos = os.path.expanduser(path_datos_disdro)

        # Por las dudas hago la busqueda en orden cronologico.
        # Por eso el range(...,-1)
        for i in range(n_horas_atras, 0, -1):
            delta = timedelta(hours=i)

            fecha_atras = fecha_final - delta
            hora_local = fecha_atras - utc
            hora_local_str = hora_local.strftime('%Y%m%d%H')
            # PRINT
            print(fecha_atras)

            for minu in range(60):
                delta_minuto = timedelta(minutes=minu)
                minuto = (hora_local+delta_minuto).strftime('%M')

                # grep -F "DIA.MES.ANIO;HORA" path/to/SADL_DSD_ANIOMES.txt
                '''Uso try porque hay casos donde no hay dato cada minuto del
                disdrometro, subprocess da error y frena el proceso. No estoy
                interesado en ese error y lo paso.'''
                try:
                    comando = 'grep -F "{DIA1}.{MES1}.{ANIO1};{HORA1}:{MIN1}"'\
                              ' {path}{dis}_DSD_{ANIO2}{MES2}.txt'.format(
                                  DIA1=hora_local_str[6:8],
                                  MES1=hora_local_str[4:6],
                                  ANIO1=hora_local_str[0:4],
                                  HORA1=hora_local_str[8:10],
                                  MIN1=minuto,
                                  path=path_datos,
                                  dis=disdro,
                                  ANIO2=hora_local_str[0:4],
                                  MES2=hora_local_str[4:6])
                    args = shlex.split(comando)
                    self._texto.append(subprocess.check_output(args))
                except:
                    pass

        try:
            comando = 'grep -F "{DIA1}.{MES1}.{ANIO1};{HORA1}:00" '\
                '{path}{dis}_DSD_{ANIO2}{MES2}.txt'.format(
                    DIA1=hora_local_final_str[6:8],
                    MES1=hora_local_final_str[4:6],
                    ANIO1=hora_local_final_str[0:4],
                    HORA1=hora_local_final_str[8:10],
                    path=path_datos,
                    dis=disdro,
                    ANIO2=hora_local_final_str[0:4],
                    MES2=hora_local_final_str[4:6])
            args = shlex.split(comando)
            self._texto.append(subprocess.check_output(args))
        except:
            pass

        return self._texto

    def _creo_array_ref(self):
        '''
        Este programa procesa el archivo de texto generado por el
        disdrometro (o en su defecto, el archivo filtrado por fechas).

        Parametros
        ----------
        self._texto : lista
            Lista conteniendo los datos crudos del disdrometro

        Devuelve
        --------
        self.time: lista
            Lista con datetime ordenados. Este orden se mantiene en las
            siguientes listas.

        self.fields['ZH_par']['data']:
            Lista con reflectividad calculada por el disdrometro.

        self.fields['ZH_ray']['data']:
            Lista con reflectividad calculada por la funcion. Se les
            aplica un control de calidad.

        self.dg:
            Lista con las distribuciones de gotas calculadas a partir
            de los datos crudos. Se usan como entrada en la T-Matrix.

        '''

        utc = timedelta(hours=3)

        # Variables para calcular reflectividad
        bins = np.array([0.062, 0.187, 0.312, 0.437, 0.562, 0.687, 0.812,
                         0.937, 1.062, 1.187, 1.375, 1.625, 1.875, 2.125,
                         2.375, 2.75, 3.25, 3.75, 4.25, 4.75, 5.5, 6.5, 7.5,
                         8.5, 9.5, 11., 13., 15., 17., 19., 21.5, 24.5])

        bins_diff = [0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125,
                     0.125, 0.125, 0.25, 0.25, 0.25, 0.25, 0.25, 0.5, 0.5, 0.5,
                     0.5, 0.5, 1., 1., 1., 1., 1., 2., 2., 2., 2., 2., 3., 3.]

        drop_vel = np.array([0.05, 0.15, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75,
                             0.85, 0.95, 1.1, 1.3, 1.5, 1.7, 1.9, 2.2, 2.6, 3,
                             3.4, 3.8, 4.4, 5.2, 6, 6.8, 7.6, 8.8, 10.4, 12,
                             13.6, 15.2, 17.6, 20.8])

        # Velocidad del paper de Campos 2006
        vf = -0.193 + (4.96 * bins) - (0.904 * np.power(bins, 2)) + \
            (0.0566 * np.power(bins, 3))

        mascara = np.zeros((32, 32), dtype=np.int)

        # Esta mascara viene del paper de Campos 2006
        # 60% de la velocidad calculada mas arriba
        matriz_velocidad = np.reshape(np.repeat(drop_vel, 32), (32, 32))
        for i in range(vf.size):
            for j in range(vf.size):
                if not(vf[j]*0.4 < matriz_velocidad[i][j] < vf[j]*1.6):
                    mascara[i][j] = 1

        fecha = []

        dg = []
        dbz_rayleigh = []
        dbz_parsivel = []

        # Leo los datos y los guardo en el diccionario
        for line in self._texto:
            mitad1 = line.split('>')
            mitad1_split = mitad1[0].split(';')

            date = mitad1_split[0]
            time = mitad1_split[1]
            fecha_local = datetime.strptime(date+' '+time, '%d.%m.%Y %H:%M:%S')
            # PRINT
            print(fecha_local, utc)
            fecha.append(fecha_local+utc)

            # Mantengo el string por las dudas. Cuando hago la comparación para
            # saber si agrego NaN o no puede haber algun error al convertir a
            # float y que compare mal.
            ref_parsivel = mitad1_split[7]

            mitad2 = line.split('>')[1]
            espectro_raw = mitad2.split('<')[0]

            if ref_parsivel == '-9.999':
                dbz_parsivel.append(float('nan'))
                # break
            elif ref_parsivel == '-9,999':
                # En CUBA aparece con coma en vez de punto.
                dbz_parsivel.append(float('nan'))
            elif (ref_parsivel[-4] == ',' and ref_parsivel != '-9,999'):
                refle = ref_parsivel[:-4]+'.'+ref_parsivel[-3:]
                dbz_parsivel.append(float(refle))
            else:
                dbz_parsivel.append(float(ref_parsivel))

            if espectro_raw == 'ZERO':
                dbz_rayleigh.append(float('nan'))
            else:
                # Creo la lista y la modifico para un array de 32x32
                raw = [spec for spec in espectro_raw.split(';')]

                # Dependo fuertemente de cual es el primer valor que toma el
                # espectro. Hay 1025 elementos en raw, se va el primero o el
                # ultimo. En el script de matlab se va el ultimo.
                lista = raw[:-1]

                for i in range(len(lista)):
                    if lista[i] == '':
                        lista[i] = 0

                array_1024 = np.asarray(lista).astype(np.uint16)
                matriz = np.reshape(array_1024, (32, 32))

                # Control de calidad, aplico la máscara del paper
                matriz_mask = np.ma.array(matriz, mask=mascara)

                NDS = np.sum(matriz_mask, axis=0)  # emulo a leo_parsivel.m

                # Mas variables para generar la DSD desde la cantidad de gotas
                T = 60
                SUP = 180. * 30. * np.power(10., -6.)

                DG = NDS / (vf * SUP * T * bins_diff)
                DG = np.around(DG, 4)

                ray_Z = np.sum(np.multiply(DG*bins_diff, np.power(bins, 6.)))
                DBZ = 10. * np.log10(ray_Z)

                dbz_rayleigh.append(DBZ)
                dg.append(DG)

        # Lo borro de la memoria porque ya tengo los datos
        self._texto = []

        self.time = fecha
        self.fields['ZH_par']['data'] = dbz_parsivel
        self.fields['ZH_ray']['data'] = dbz_rayleigh
        self.dg = dg
