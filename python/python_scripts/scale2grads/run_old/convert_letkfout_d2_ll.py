import os
import numpy as np
import datetime as dt
from scale.letkf import letkfout_grads
from mpi4py import MPI


EXP = 'RMA1_d2_2km_scalebdy_radar_init18_4D_CTRL'
stime = dt.datetime(2018, 11, 10, 18, 5,  0)
etime = dt.datetime(2018, 11, 10, 18, 5,  0)
tint = dt.timedelta(seconds=300)
outtype =['anal']
member = ['mean'] 

#---
BASEDIR = '/home/paula.maldonado/datosalertar1/TESIS_DOC/DATA/EXPS/CORDOBA_20181110_OFP'
letkfoutdir = '{}/{}'.format(BASEDIR, EXP)
topofile = '{}/const/topo/topo'.format(letkfoutdir)

sim_read = 1
comm = MPI.COMM_WORLD # or None
nprocs = comm.Get_size()
myrank = comm.Get_rank()
if nprocs > sim_read:
    raise ValueError('The maximum number of simultaneous I/O threads is set to ' + str(sim_read) + ', please use nprocs <= ' + str(sim_read))

hcoor = 'o'
vcoor = 'z'
plevels = []
zlevels = [500., 1000., 1500., 2000., 2500., 3000., 3500., 4000., 4500., 5000., 5500., 6000., 6500., 7000., 7500., 8000., 8500., 9000., 9500., 10000]
varout_3d = []
varout_2d = ['glon', 'glat', 'topo']

proj = {
'type': 'LC',
'basepoint_lon': 295.809,
'basepoint_lat': -31.441,
'basepoint_x': 250000.0,
'basepoint_y': 250000.0,
'LC_lat1': -31.6,
'LC_lat2': -31.4
}
extrap = False 
dlon = 0.1
dlat = 0.1

letkfout_grads(letkfoutdir, topofile=topofile, proj=proj, stime=stime, etime=etime, tint=tint,
               outtype=outtype, member=member,
               vcoor=vcoor, hcoor=hcoor, plevels=plevels, zlevels=zlevels, dlon=dlon, dlat=dlat,
               varout_3d=varout_3d, varout_2d=varout_2d, extrap=extrap,
               comm=comm, sim_read=sim_read, pnetcdf=False)

print('==========')
print('NORMAL END')
print('==========')
