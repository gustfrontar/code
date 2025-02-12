from options.base_options import BaseOptions
from utils import filter_data, save_cfradial
from utils.radar import genero_nc
from filters import run_all_filters
from multiprocessing import Pool
from datetime import datetime, timedelta
import os
import gc
import pickle
import numpy as np
import argparse
import copy
import glob

ncores=20
qc_dict=dict()
radars=['PER']  #['PAR','PER','ANG'] 
qc_dict['root_data_path']='/home/jruiz/datosmunin3/datos/DATOS_RADAR/'
qc_dict['vol_conf']='asimilacion_yaka.qcr'
qc_dict['opt']=BaseOptions(qc_dict['vol_conf']) 

for my_radar in radars :
   qc_dict['radar'] = my_radar
   qc_dict['radar_path']=qc_dict['root_data_path'] + '/' + qc_dict['radar'] + '/'
   qc_dict['qc_radar_path']=qc_dict['root_data_path'] + '/' + qc_dict['radar'] + '/QC/'
   os.makedirs( qc_dict['qc_radar_path'] , exist_ok=True )
   qc_dict['opt'].netcdf_output_path=qc_dict['qc_radar_path']
   file_list=glob.glob( qc_dict['radar_path'] + '*dBZ.vol' )
   

   input_list = []
   for my_file in file_list :
      qc_dict['radar_file'] = my_file
      input_list.append( copy.deepcopy( qc_dict ) )

   def radar_qc( qc_dict ) :

      opt=qc_dict['opt']
      ext= qc_dict['radar_file'].split('.')[-1]
      print('Processing radar file' + qc_dict['radar_file'] )
      out_file_name = os.path.basename( qc_dict['radar_file'] )
      if os.path.isfile( qc_dict['radar_file'] + '.OK' ) :
         print('This file has been processed, contnue to the next file')
         return



      if 'RMA' in qc_dict['radar'] :
         if ext == 'H5':
            date = datetime.strptime(qc_dict['radar_file'].split('.')[0].split('_')[-1],
                                     '%Y%m%dT%H%M%SZ')
         elif ext == 'nc':
            date = datetime.strptime(qc_dict['radar_file'].split('.')[1],
                                     '%Y%m%d_%H%M%S')
      else :
         date = datetime.strptime( os.path.basename( qc_dict['radar_file'] )[:14], "%Y%m%d%H%M%S")

      tmp_list = file_list=glob.glob( qc_dict['radar_path'] + '/' + os.path.basename( qc_dict['radar_file'])[:14] + '*.vol' )
      print( qc_dict['radar_path'] + '/' + os.path.basename( qc_dict['radar_file'][:14] ) + '*.vol' )
      radar = genero_nc( tmp_list , qc_dict['radar'], ext , date )
      c_radar, output = run_all_filters(radar, opt)
      os.system('touch ' + qc_dict['radar_file'] + '.OK')
      gc.collect()

   p = Pool(ncores)
   p.map( radar_qc, input_list )

