from datetime import datetime
from utils.radar import genero_nc
import pandas as pd
import numpy as np
from filters import run_all_filters
from options.base_options import BaseOptions
from configparser import ConfigParser, ExtendedInterpolation


def worker(vol_id=None, vol_date_ini=None, vol_date_fin=None, vol_conf=None):
    '''
    Esta funcion corre el qc para un volumen determinado de radar.
    Input:
        - vol_id: Nombre del radar
        - vol_date_ini: Fecha de inicio del escaneo del radar
        - vol_date_fin: Fecha de fin del escaneo del radar
        - vol_conf: Configuracion que se utiliza para los filtros de qc
    '''
    config = ConfigParser(interpolation=ExtendedInterpolation())
    config.optionxform = str
    config.read("config.ini")  # Martin: habria que agregar aca el path?
    df = pd.read_csv(config["System"]["BASEDIR"]+'/df_qc-radar.csv')

    # Obtengo la lista de variables para el volumen de radar determinado por vol_id, vol_date, conf
    file_list, file_ext = input_create_object(vol_id, vol_date_ini, vol_conf, df)
    # Creo el objeto radar
    #vol_date_ini = datetime.strptime(vol_date_ini, '%Y-%m-%d %H:%M:%S')
    radar = genero_nc(file_list, vol_id, file_ext, vol_date_ini)
    # Inicializo las opciones con las que correra el qc
    opt = BaseOptions(vol_conf+".qcr")
    #Corro al qc
    cradar, output = run_all_filters(radar, opt,
                                     vol_date=(vol_date_ini, vol_date_fin))

def input_create_object(rad_id, date, conf, df):
    '''
    Input:
        - radar_id: Nombre del radar
        - date: Fecha de inicio del escaneo del radar
        - conf: Configuracion que se utiliza para los filtros de qc
    Output:
        - file_list: Lista de variables que identifican a un volumen con rad_id, date y conf
        - file_ext: Extension del archivo (Hs para los RMA o vol para los INTA)
    '''
    df = df[df.radar_id.isin([rad_id])]
    df = df[df.fecha_ini.isin([date])]
    df = df[df.config.isin([conf])]
    print(df)
    file_list = list(df['file_input'])
    file_ext = df['file_ext'].iloc[0]
    return file_list, file_ext
