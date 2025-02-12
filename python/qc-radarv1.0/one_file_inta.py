from options.base_options import BaseOptions
from utils.files_to_dataframe import get_radar_data
from utils import filter_data, save_cfradial
from utils.radar import genero_nc
from filters import run_all_filters
from datetime import datetime, timedelta
import os
import pickle
import numpy as np
import argparse
import glob

if __name__ == '__main__':
    # Cargo las opciones de los filtros
    parser = argparse.ArgumentParser()

    parser.add_argument('--config', required = True, help='Archivo de configuracion')   
    parser.add_argument('--radar_file', required = True, help='Ahivo de radar que se quiere corregir')
    parser.add_argument('--radar_id', type=str, required=True, help='Nombre del radar: RMA#, PAR, PER, ANG')
    a = parser.parse_args()

    vol_conf   = a.config
    radar_file = a.radar_file
    radar_id   = a.radar_id
    opt        = BaseOptions(vol_conf) 

    radar_path = os.path.dirname( radar_file ) 
    radar_filename = os.path.basename( radar_file )

    ext= radar_file.split('.')[-1]
    if 'RMA' in radar_id :
       if ext == 'H5':
          date = datetime.strptime( radar_filename.split('.')[0].split('_')[-1],
                                     '%Y%m%dT%H%M%SZ')
       elif ext == 'nc':
          date = datetime.strptime( radar_filename.split('.')[1],
                                     '%Y%m%d_%H%M%S')
    else :
       date = datetime.strptime( radar_filename[:14], "%Y%m%d%H%M%S")

    tmp_list = file_list=glob.glob( os.path.dirname( radar_file ) + '/' + radar_filename[:14] + '*.vol' )
    print( tmp_list )
    radar = genero_nc( tmp_list , radar_id , ext , date )
    
    # Corro los filtros
    c_radar, output = run_all_filters(radar, opt)


