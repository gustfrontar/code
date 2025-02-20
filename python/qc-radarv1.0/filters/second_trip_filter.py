import os, gc
from scipy.interpolate import RegularGridInterpolator as interp2d
import numpy as np                  
from utils.qc_utils import qc    #Fortran routines
import utils.radar as rt
from utils.super_radar import SuperRadar 
from datetime import datetime , timedelta


def SecondTripFilter( sradar , radar , opt , boxx=2 , boxy=2 , boxz= 0 ) :

   datapath = os.path.dirname( radar.metadata['radarfile'] )
   filename = os.path.basename( radar.metadata['radarfile'] )
   radar_time = datetime.strptime( radar.metadata['date'] , '%Y%m%d_%H%M%S') 
   radar_ext  = filename.split('.')[-1] 
   radar_id   = radar.metadata['instrument_name']
   radar_strat= radar.instrument_parameters['strategy']
   
   #1)For each file in the list identify those with the strategy 120 (RMA:_02 or INTA: _0120)
   #2)Get the volume after and before this one with the strategy 240 (RMA: _01 or INTA: _0240)
   #3)Average these two and compare it to the volume 120.

   #This filter is applied only to volumes in 120.
   if not ( '_02' in radar_strat or '0120_IN' in radar_strat ) :
      print('This is not a 120km strategy ' + filename )
   else :

      file_before , file_after = rt.find_prev_post_files( datapath , radar_ext , radar_id , radar_time , time_threshold = 600 )

      #Load and read files found
      radar_range = radar.range['data'][:]

      undef_ref = radar.fields[opt.name_ref]['_FillValue']
      ray_angle_res = np.unique( radar.ray_angle_res['data'] )
      order_azimuth = sradar.get_order_azimuth()
      order_ran     = radar.range['data'].data
      order_ref     = sradar.order_variable( radar.fields[ opt.name_ref ]['data'] , undef_ref )
      if opt.name_v in radar.fields:
         undef_v       = radar.fields[opt.name_v]['_FillValue']
         order_v       = sradar.order_variable( radar.fields[ opt.name_v ]['data'] , undef_v )

      if file_before != None :
         time_before = rt.get_time_from_filename( file_before )
         radar_bf = rt.genero_nc( file_before , radar_id ,  radar_ext , time_before )
         common_elev_bf = get_common_elev_idx( radar_bf , radar )
         #print('Common elevation angles in bf file are: ', common_elev_bf)

         if opt.name_ref in radar_bf.fields:
            sradar_bf     = SuperRadar( radar_bf , opt )
            order_ref_bf  = sradar_bf.order_variable( radar_bf.fields[ opt.name_ref ]['data'] , undef_ref )
            order_ref_bf  = order_ref_bf[:,:,common_elev_bf]

      if file_after != None :
         time_after = rt.get_time_from_filename( file_after )
         radar_af = rt.genero_nc( file_after , radar_id ,  radar_ext , time_after )
         common_elev_af = get_common_elev_idx( radar_af , radar )
         #print('Common elevation angles in af file are: ',common_elev_af)

         if opt.name_ref in radar_af.fields :
            sradar_af     = SuperRadar( radar_af , opt )
            order_ref_af  = sradar_af.order_variable( radar_af.fields[ opt.name_ref ]['data'] , undef_ref )
            order_ref_af  = order_ref_af[:,:,common_elev_af]

      #Compute reference relfectivity from files found
      proceed = False
      if common_elev_af == common_elev_bf :
         if file_after is not None and file_before is not None and np.array_equal( order_ref_af.shape , order_ref_bf.shape ) :
            order_ref_ctrl = 0.5*( order_ref_bf + order_ref_af )   
            order_ran_ctrl = radar_af.range['data'].data
            proceed = True   
         elif file_after is None and file_before is not None :
            order_ref_ctrl = order_ref_af
            order_ran_ctrl = radar_af.range['data'].data
            print( 'Warning : no file before ' )
            proceed = True
         elif file_after is not None and file_before is None :
            order_ref_ctrl = order_ref_bf
            order_ran_ctrl = radar_bf.range['data'].data
            print( 'Warning : no file after ' )
            proceed = True
         elif file_after is None and file_before is None :
            print( 'Warning : no file after or before' )
      else :
         print('Warning: common elevations do not conform, skiping this file')
         print('Common elevation before' , common_elev_bf )
         print('Common elevation after ' , common_elev_af )
         #Apply mask to relfectivity 
      if proceed:
         #Interpolate ctrl_ref to the reference    
         order_ref_ctrl_120 = np.zeros( np.shape( order_ref ) )
         for ilev in range(len(common_elev_bf)):  
         
            interpolator = interp2d( ( order_ran_ctrl , order_azimuth ) , order_ref_ctrl[:,:,ilev].T 
                                    , method='linear', bounds_error=False )  
            order_ran_gr , order_azimuth_gr = np.meshgrid( order_ran , order_azimuth , indexing='ij' )
            order_ref_ctrl_120[:,:,ilev] = interpolator( ( order_ran_gr , order_azimuth_gr ) ).T
            
         #Take the local maximum of order_ref_120 to take account cell movement between different times.
         na , nr , ne = order_ref_ctrl_120.shape
         tmp_ref = qc.box_functions_2d( datain=order_ref_ctrl_120 ,na=na ,nr=nr ,ne=ne ,undef=undef_ref
                                       ,boxx=boxx,boxy=boxy,boxz=boxz,operation='MAXN',threshold=0.0)
         diff_ref = order_ref - tmp_ref

         #Where reference reflectivity is very small 
         #and difference between both ref is large
         #and order_ref does not have a min_ref value (for QC)
         #then eliminate the echos in the radar volume
         ref_mask =  ( diff_ref > 10.0 ) & ( tmp_ref < 10.0 ) & ( order_ref != -0.1 ) 
         #Filter current reflectivity based on order_ref_ctrl_120 data.
         order_ref[ ref_mask ] = undef_ref 
         if opt.name_v in radar.fields:
            order_v[ ref_mask ]  = undef_v
         
         #Replace the original field by the corrected field.             
         tmp=sradar.order_variable_inv( order_ref , undef_ref )
         radar.fields[opt.name_ref]['data']=np.ma.masked_array(tmp , mask = (tmp==undef_ref) )
         if opt.name_v in radar.fields:
            tmp=sradar.order_variable_inv( order_v , undef_v )
            radar.fields[opt.name_v]['data']=np.ma.masked_array(tmp , mask = (tmp==undef_v) )

   gc.collect()
   return radar

def get_common_elev_idx(origin, target):
   #origin and target are two pyart radar objects
   #idxs is a list with the common elevation indices
   idxs = []
   for i in np.unique(target.elevation['data']):
      diff = np.abs(i - np.unique(origin.elevation['data']))
      idxs.append(np.where(diff == diff.min())[0][0])

   return idxs