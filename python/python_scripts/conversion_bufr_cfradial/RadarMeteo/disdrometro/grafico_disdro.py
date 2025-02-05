# -*- coding: utf-8 -*-

import numpy as np
import datetime
import matplotlib.pyplot as plt
import matplotlib.dates as mdates


def grafico(path_out, datetime_final, n_horas_atras, disdro=None, dsd=None,
            nombre=None, RAD=None):
    """
    Esta funcion genera un grafico entre la reflectividad obtenida del
    radar y del disdrometro. Puede usarse por separado para cualquiera
    de las dos variables.

    Parametros
    ----------
    path_out : str
        Directorio donde se guardan las imagenes.

    datetime_final: datetime obj
        Fecha (anio, mes, dia, hora) del ultimo dato a buscar. En hora
        UTC.

    n_horas_atras : int
        Cantidad de horas atras que voy a buscar datos.

    disdro : str
        Sigla del disdrometro a levantar.

    dsd : dict
        Salida de leo_parsivel.

    nombre : str
        Sigla del radar a analizar, por ejemplo EZE o RMA1.

    RAD : dict
        Salida de leo_radar.
    """

    # Estas son fechas para usar en el nombre de la figura
    horaini = datetime.timedelta(hours=n_horas_atras)

    fechaini = datetime_final - horaini

    fechaini_str = fechaini.strftime('%Y%m%d%H')

    if nombre is 'EZE':

        if disdro is not None:
            # Creo arrays de las distintas reflectividades
            dbz_ray = np.asarray(dsd.fields['ZH_ray']['data'])
            dbz_ott = np.asarray(dsd.fields['ZH_par']['data'])

        dbz = np.asarray(RAD['data'])

        # Edito el formato de la hora (va a ir al eje x)
        horas_mdates = mdates.HourLocator()
        horasfmt = mdates.DateFormatter('%d/%m/%Y %HUTC')

        fig, ax = plt.subplots(figsize=(16, 12))

        ax.grid(True)

        if disdro is 'SADL':
            ax.set_title(u'Comparación de reflectividad entre \n'
                         u'disdrómetro La Plata (SADL) y radar Ezeiza',
                         size=18)
        elif disdro is 'CUBA':
            ax.set_title(u'Comparación de reflectividad entre \n'
                         u'disdrómetro Ciudad Universitaria y radar Ezeiza',
                         size=18)

        ax.set_ylabel('Factor de Reflectividad (dBZ)', size=13)

        # Le digo en que formato poner la hora en el eje x
        ax.xaxis.set_major_locator(horas_mdates)
        ax.xaxis.set_major_formatter(horasfmt)

        if disdro is not None:
            ax.plot(dsd.time, dbz_ray, 'b', label='Rayleigh', linewidth=2)

        ax.plot(RAD['time'], dbz, 'or-', label='Radar EZE', linewidth=2)

        ax.tick_params(axis='both', labelsize=13)

        ax.set_ylim([-10, 60])
        ax.set_xlim(datetime_final-horaini, datetime_final)

        fig.autofmt_xdate()
        plt.legend(loc='best')

        if disdro is 'SADL':
            # Guardo la figura
            fig.savefig(path_out + '/disdro_' + fechaini_str[0:4]
                        + fechaini_str[4:6] + fechaini_str[6:8] + '.png',
                        dpi=65, bbox_inches='tight')
        elif disdro is 'CUBA':
            fig.savefig(path_out + '/disdro_CUBA_' + fechaini_str[0:4]
                        + fechaini_str[4:6] + fechaini_str[6:8] + '.png',
                        dpi=65, bbox_inches='tight')

    elif nombre is None:

        # Creo arrays de las distintas reflectividades
        dbz_ray = np.asarray(dsd.fields['ZH_ray']['data'])
        dbz_ott = np.asarray(dsd.fields['ZH_par']['data'])

        # Edito el formato de la hora (va a ir al eje x)
        horas_mdates = mdates.HourLocator()
        horasfmt = mdates.DateFormatter('%d/%m/%Y %HUTC')

        fig, ax = plt.subplots(figsize=(16, 12))

        ax.grid(True)

        if disdro is 'SADL':
            ax.set_title(u'Factor de reflectividad de disdrómetro '
                         'La Plata (SADL)', size=18)
        elif disdro is 'CUBA':
            ax.set_title(u'Factor de reflectividad de disdrómetro '
                         'Ciudad Universitaria', size=18)

        ax.set_ylabel('Factor de Reflectividad (dBZ)', size=13)

        # Le digo en que formato poner la hora en el eje x
        ax.xaxis.set_major_locator(horas_mdates)
        ax.xaxis.set_major_formatter(horasfmt)

        ax.plot(dsd.time, dbz_ray, 'b', label='Rayleigh', linewidth=2)

        ax.tick_params(axis='both', labelsize=13)

        ax.set_ylim([-10, 60])
        ax.set_xlim(datetime_final-horaini, datetime_final)

        fig.autofmt_xdate()
        plt.legend(loc='best')

        if disdro is 'SADL':
            # Guardo la figura
            fig.savefig(path_out + '/disdro_' + fechaini_str[0:4]
                        + fechaini_str[4:6] + fechaini_str[6:8] + '.png',
                        dpi=65, bbox_inches='tight')
        elif disdro is 'CUBA':
            fig.savefig(path_out + '/disdro_CUBA_' + fechaini_str[0:4]
                        + fechaini_str[4:6] + fechaini_str[6:8] + '.png',
                        dpi=65, bbox_inches='tight')
