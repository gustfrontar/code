import numpy as np
import pysteps
from datetime import timedelta
import pickle

'''
Parametros que se estan usando para SEDRA actualmente
lead_fcst = 125
ddt_sec = 60
ddt_min = 1
'''

def adv_mv(cappi, mv_fields, config,str_date_ini, filename=None):
    nx, ny = np.shape(cappi['data'])
    cappi['data'][cappi['data']<=-0.1]=0
    motion = np.zeros(((2,nx,ny)))
    motion[0,:,:] = mv_fields['u_motions']*float(config['ACR']['ddt_sec'])/mv_fields['dt']
    motion[1,:,:] = mv_fields['v_motions']*float(config['ACR']['ddt_sec'])/mv_fields['dt']
    fcst = pysteps.extrapolation.semilagrangian.extrapolate(cappi['data'], motion,int(config['ACR']['lead_fcst']))

    if filename:
        for i in range(1,int(config['ACR']['lead_fcst'])+1):
            with open(filename+'/fcst.'+str_date_ini+'_'+str(i).zfill(3)+'.pckl', 'wb') as handle:
                pickle.dump(fcst[i-1,:,:],handle)

    return fcst

