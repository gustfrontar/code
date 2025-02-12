import os
GeneralConf=dict()

GeneralConf['exp_sim_name'] = '../exp_test/'
GeneralConf['DataBase'] = '../casos_estudio/'
GeneralConf['data'] = ''
GeneralConf['rad'] = os.environ['radar']
GeneralConf['path_ref'] = 'cappis'
GeneralConf['date'] = os.environ['date']
GeneralConf['date_start_mv'] = os.environ['date_start']
GeneralConf['date_end_mv']   = os.environ['date_end']
GeneralConf['dt_imag'] = '/dt10'
GeneralConf['filedir_mv'] = '/motion_vectors'+ GeneralConf['dt_imag']+GeneralConf['exp_sim_name'] 

ModelConf=dict()
ModelConf['nx'] = 241
ModelConf['ny'] = 241
ModelConf['ref_umb'] = 15
ModelConf['ref_min'] = 0
ModelConf['sectors'] = ((70, 35, 10, 4), (70,35, 10, 4)) 
ModelConf['smooth'] = 50000.0
ModelConf['first_guess'] = None
ModelConf['inst_steps'] = False
ModelConf['verb'] = True
ModelConf['yx'] = 'yx'
ModelConf['padding'] = 20
ModelConf['options'] = None

if GeneralConf['dt_imag']=='/dt10':
    ModelConf['mint_scan'] = 0
    ModelConf['maxt_scan'] = 660
    ModelConf['dt'] = 1 
elif GeneralConf['dt_imag']=='/dt20':
    ModelConf['mint_scan'] = 660
    ModelConf['maxt_scan'] = 1200
    ModelConf['dt'] = 2
elif GeneralConf['dt_imag']=='/dt30':
    ModelConf['mint_scan'] = 1200
    ModelConf['maxt_scan'] = 1800
    ModelConf['dt'] = 3



