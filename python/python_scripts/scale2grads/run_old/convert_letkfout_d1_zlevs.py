import os, sys
import numpy as np
import datetime as dt
from scale.letkf import letkfout_grads
from mpi4py import MPI

member = [int(sys.argv[1])]
EXP = 'RMA1_d1_10km_wrfbdy'
outtype = ['anal', 'gues']
stime = dt.datetime(2018,  11, 9, 19,  0,  0)
etime = dt.datetime(2018,  11, 10, 22,  0,  0)
tint = dt.timedelta(seconds=3600)

#---
BASEDIR = '/home/paula.maldonado/datosalertar1/TESIS_DOC/DATA/EXPS/CORDOBA_20181110_OFP'
letkfoutdir = '{}/{}'.format(BASEDIR, EXP)
topofile = '{}/const/topo/topo'.format(letkfoutdir)

sim_read = 10 
comm = MPI.COMM_WORLD # or None
nprocs = comm.Get_size()
myrank = comm.Get_rank()
if nprocs > sim_read:
    raise ValueError('The maximum number of simultaneous I/O threads is set to ' + str(sim_read) + ', please use nprocs <= ' + str(sim_read))

hcoor = 'o'
vcoor = 'z'
plevels = []
#zlevels = [2,10, 100] + list(np.arange(500,3000,500)) + list(np.arange(3000,11000,1000))

varout_3d = ['dbz']
varout_2d = ['max_dbz']

proj = {
'type': 'LC',
'basepoint_lon': 295.809,
'basepoint_lat': -31.441,
'basepoint_x': 1000000.0,
'basepoint_y': 1000000.0,
'LC_lat1': -31.6,
'LC_lat2': -31.4
}
extrap = False
dlon = 0.1
dlat = 0.1

# Run postprocessing
letkfout_grads(letkfoutdir, topofile=topofile, proj=proj, stime=stime, etime=etime, tint=tint,
               outtype=outtype, member=member,
               vcoor=vcoor, hcoor=hcoor, plevels=plevels, dlon=dlon, dlat=dlat,
               varout_3d=varout_3d, varout_2d=varout_2d, extrap=extrap,
               comm=comm, sim_read=sim_read, pnetcdf=False)

print('==========')
print('NORMAL END')
print('==========')
