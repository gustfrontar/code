from options.base_options import BaseOptions
from utils.files_to_dataframe import get_radar_data
from utils import filter_data, save_cfradial
from utils.radar import genero_nc
from filters import run_all_filters
import os
import pickle
import numpy as np
import argparse

if __name__ == '__main__':
    # Cargo las opciones de los filtros
    parser = argparse.ArgumentParser()

    parser.add_argument('--config', required = True, help='Archivo de configuracion')   
    parser.add_argument('--radar_path', required = True, help='Ahivo de radar que se quiere corregir')
    parser.add_argument('--radar_id', type=str, required=True, help='Nombre del radar: RMA#, PAR, PER, ANG')
    parser.add_argument('--radar_date', type=str, required=True, help='Fecha del escaneo. Debe estar en formato %Y%m%d_%H%M%S y debe ser una str')
    a = parser.parse_args()

    vol_conf = a.config
    radar_path = a.radar_path
    radar_id = a.radar_id
    radar_date = a.radar_date
    opt = BaseOptions(vol_conf) 
    
    # Leo el archivo 
    ext = radar_path.split('.')[-1]
    radar = genero_nc(radar_path, radar_id, ext, radar_date)
    
    # Corro los filtros
    c_radar, output = run_all_filters(radar, opt)


