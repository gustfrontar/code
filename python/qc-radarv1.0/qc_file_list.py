from options.base_options_list_file import BaseOptions
from utils.files_to_dataframe import get_radar_data
from utils.filter_data import filter_data
from utils.radar import genero_nc
from filters import run_all_filters
import os
import pickle
import numpy as np
from configparser import ConfigParser, ExtendedInterpolation


if __name__ == '__main__':
    print('Empieza QC')
    #Levantamos la configuracion del experimento.
    opt = BaseOptions('file_list.qcr')
    # Leemos los archivos de radar disponibles y los listamos en una tabla
    print('Leo los archivos')
    raw_df = get_radar_data(opt.dataroot,opt)
    # Filtramos los datos segun las opciones 
    df = filter_data(raw_df, opt)
    raw_df = None # Esto borra el dataframe con todos los datos de radar
    dates = df.index.unique()
    for date in dates:
        dataT = df[df.index==date]
        radarT = dataT['radar_id'].unique()
        for radar_id in radarT:
           data=dataT[dataT['radar_id']==radar_id]
           file_list = list(data['file_path'])
           ext=data['ext'][0]
           fecha=data.index[0]
           print("file_list:{}".format(file_list))
           print("radar_id:{}".format(radar_id))
           print("ext:{}".format(ext))
           print("fecha:{}".format(fecha))
           radar = genero_nc(file_list, radar_id, ext, fecha)
           print(date)
           if radar is not None:
              cradar, output  = run_all_filters(radar, opt)
 



