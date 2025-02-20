from options.base_options import BaseOptions
from utils import filter_data, save_cfradial
from utils.radar import genero_nc , get_time_from_filename
from filters import run_2nd_trip
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
radars=['RMA1']
qc_dict['root_data_path']='/home/ra000007/a04037/data/DATOS_RADAR/'
qc_dict['vol_conf']='asimilacion_yaka.qcr'
qc_dict['opt']=BaseOptions(qc_dict['vol_conf']) 

for my_radar in radars :
   qc_dict['radar'] = my_radar
   qc_dict['radar_path']=qc_dict['root_data_path'] + '/' + qc_dict['radar'] + '/QC/'
   qc_dict['qc_radar_path']=qc_dict['root_data_path'] + '/' + qc_dict['radar'] + '/2NDTQC/'
   os.makedirs( qc_dict['qc_radar_path'] , exist_ok=True )
   qc_dict['opt'].netcdf_output_path=qc_dict['qc_radar_path']
   file_list=glob.glob( qc_dict['radar_path'] + '*.nc' )
   
   input_list = []
   for my_file in file_list :
      qc_dict['radar_file'] = my_file
      input_list.append( copy.deepcopy( qc_dict ) )

def radar_2ndt_qc( qc_dict ) :

   opt=qc_dict['opt']
   ext= qc_dict['radar_file'].split('.')[-1]
   print('Processing radar file' + qc_dict['radar_file'] )
   if os.path.isfile( qc_dict['radar_file'] + '.OK' ) :
      print('This file has been processed, continue to the next file')
      return

   date = get_time_from_filename( qc_dict['radar_file'] )

   radar = genero_nc( qc_dict['radar_file'] , qc_dict['radar'] , ext , date )
   
   radar_strat= radar.instrument_parameters['strategy']
   if not '.nc.OK' in qc_dict['radar_file'] :
      if not ( '_02' in radar_strat or '0120_IN' in radar_strat ) :
         #If the file is not a 120 km range strategy, then I copy the file to the final dir.
         #no second trip qc is applied on this file. 
         print('This is not a 120km strategy ' + qc_dict['radar_file'] )
         os.system('cp ' + qc_dict['radar_file'] + ' ' + qc_dict['qc_radar_path'] )
         os.system('touch ' + qc_dict['radar_file'] + '.OK')
      else :
         c_radar = run_2nd_trip( radar, opt )
         os.system('touch ' + qc_dict['radar_file'] + '.OK')
   
   gc.collect()

#radar_2ndt_qc( input_list[0] )
p = Pool(ncores)
p.map( radar_2ndt_qc , input_list )




