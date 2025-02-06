# -*- coding: utf-8 -*-
"""
This is a python driver for the simple_letkf_wloc fortran routine.
This code provides a way to run simple assimilation experiments with
realistic priors. 

@author:
"""
import sys
sys.path.append('../../common_python/common_functions/')
sys.path.append('../../common_python/common_modules/')
sys.path.append('../../common_python/common_letkf/')

import numpy as np
#import matplotlib.pyplot as plt
import datetime as dt
import ctl_reader as ctlr
import binary_io  as bio
import os
import matplotlib.pyplot as plt

from common_functions import common_functions as comm
from cletkf_wloc      import common_da        as cda


root_data_path=''


truth_ens_member = 0 #Which ensemble member will be considered as the true state.
qr_index = 0         #Variable index corresponding to QR
qs_index = 0         #Variable index corresponding to QS
qg_index = 0         #Variable index corresponding to QG
tt_index = 1         #Variable index corresponding to TEMP
pp_index = 2         #Variable index corresponding to PRES

obs_loc_x = [50]     #List of x coordinate of observations
obs_loc_y = [0]      #List of y coordinate of observations
obs_loc_z = [50]     #List of z coordinate of observations

obs_error = 5.0      #Standard deviation of the observation error.

#=========================================================
#  READ DATA
#=========================================================
#TODO here we should load the realistic prior.
#For the moment we will generate a random prior. 

input_ens = np.ones(( 100 , 1 , 100 , 41 , 3 ))
                     # nx    ny   nz  nbv  nvar

#TODO This random ensemble is for testing only.
for ii in range( input_ens.shape[3] ) :
    input_ens[:,:,:,ii,:]  = 1.0e-4 * np.abs( np.random.randn(1) )

input_ens[:,:,:,:,1] = input_ens[:,:,:,:,1] + 273.0   #This variable will represent the temperature
input_ens[:,:,:,:,2] = input_ens[:,:,:,:,2] + 1000.0  #This variable will represent the pressure

#=========================================================
#  SEPARATE THE TRUE STATE AND THE FORECAST ENSEMBLE
#=========================================================

tmp_mask = np.false( nbv ) 
tmp_mask[truth_ens_member] = True

true_state = input_ens[:,:,:,tmp_mask,:]
xf = input_ens[:,:,:,~tmp_mask,:]

#Get the size of the forecast ensemble (as will be used in the DA)
[nx , ny , nz , nbv , nvar] = xf.shape

xf = xf.astype('float32')   #Transform data type 

#=========================================================
#  GET THE OBSERVATIONS
#=========================================================

nobs = len(obs_loc_x)
yo = np.zeros( nobs )
hxf = np.zeros( ( nobs , nbv ) )

for ii in range( nobs )  : 
 
    ox = obs_loc_x[ii]
    oy = obs_loc_y[ii]
    oz = obs_loc_z[ii]
    qr = true_state[ox,oy,oz,qr_index]
    qg = true_state[ox,oy,oz,qg_index]
    qs = true_state[ox,oy,oz,qs_index]
    tt = true_state[ox,oy,oz,tt_index]
    pp = true_state[ox,oy,oz,pp_index]

    yo[ii] = cda.calc_ref( qr , qs , qg , tt , pp )

    obs_error = obs_error * np.ones( nobs )

#=========================================================
#  APPLY OBSERVATION OPERATOR
#=========================================================

for ii in range( nobs ) :
    #Loop over the ensemble members
    for jj in range( nbv ) :

       qr = xf[ox,oy,oz,jj,qr_index]
       qg = xf[ox,oy,oz,jj,qg_index]
       qs = xf[ox,oy,oz,jj,qs_index]
       tt = xf[ox,oy,oz,jj,tt_index]
       pp = xf[ox,oy,oz,jj,pp_index]
 
       hxf[ii,jj] = cda.calc_ref( qr , qs , qg , tt , pp )

#Get the ensemble mean observation departure.

dep = yo - np.mean( hxf , 1 ) 

#=========================================================
#  COMPUTE THE ANALYSIS UPDATE
#=========================================================

#Compute the simple analysis update
print('Computing the letkf update')

xa=cda.simple_letkf_wloc(nx=nx,ny=ny,nz=nz,nbv=nbv,nvar=nvar,hxf=hxf,xf=xf,dep=dep,error=obs_error).astype('float32')

#Write the analysis for the update variables
print('Writing data')

np.savez_compressed(root_data_path + '/output.npz',xa=xa,xf=xf,yo=yo,hxf=hxf,obs_error=obs_error,obs_loc_x,obs_loc_y,obs_loc_z)

print ( "We are done" )



