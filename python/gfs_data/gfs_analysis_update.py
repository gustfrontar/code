# -*- coding: utf-8 -*-
"""
This is a python driver for the simple_letkf_wloc fortran routine.
This code provides a way to run simple assimilation experiments with
realistic priors. 

@author:
"""
import sys
sys.path.append('./common_letkf_py/')

import numpy as np
import datetime as dt
import os
import matplotlib.pyplot as plt
import common_da     as cda


root_data_path='./'

#Variable indices (do not modify)
cda.var_index = {'t':0,'u':1,'v':2,'q':3,'gh':4}

#Observations (python list)
obs_list = [ 
           [ 't' , 50 , 0 , 5 , 1.0 , 1.0 ] , #Each tuple in the list is an observation (type , x_location , y_location , z_location , innovation ,  obs error) 
            
           [ 'q' , 10 , 0 , 5 , 1.0e-3 , 1.0e-3 ] 
            
           ] 


loc_scales = np.array([2.5,2.5,2.5])  #Localization scales in x,y and z. 

#=========================================================
#  READ DATA
#=========================================================
#TODO here we should load the realistic prior.
#For the moment we will generate a random prior. 

xf = np.ones(( 100 , 1 , 100 , 41 , 5 ))
                     # nx    ny   nz  nbv  nvar

#TODO This random ensemble is for testing only.
for ii in range( xf.shape[3] ) :
    xf[:,:,:,ii,:]  = 1.0e-3 * np.abs( np.random.randn(1) )

xf[:,:,:,:,1] = xf[:,:,:,:,1] + 273.0   #This variable will represent the temperature
xf[:,:,:,:,2] = xf[:,:,:,:,2] + 1000.0  #This variable will represent the pressure

xf_mean = np.mean( xf , axis=3)
#Get the size of the forecast ensemble (as will be used in the DA)
[nx , ny , nz , nbv , nvar] = xf.shape

lon = np.arange(0,nx)
lat = np.arange(0,ny)
lev = np.arange(0,nz)

model_grid = ( lon , lat , lev )

#=========================================================
#  GET THE OBSERVATIONS
#=========================================================
print('Obtaining the observations')
#Call the observation operator to obtain the observable quantities from the ensemble mean.
yo , obs_error = cda.model_to_obs( obs_list , xf_mean , model_grid ) 

nobs = yo.size  #Number of observations to be assimilated.
#Add observation departure to the forecast ensemble mean.     
for iobs in range( nobs ) :
    yo[iobs] = yo[iobs] + obs_list[iobs][4]

#=========================================================
#  APPLY OBSERVATION OPERATOR TO THE ENSEMBLE MEMBERS
#=========================================================
print('Applying the observation operator to the ensemble')
Y = np.zeros( nobs , nbv )  #Array that will contain the obaservable quantities as derived from each ensemble member.
for imem in range( nbv ) :  #Loop over the ensemble members.
    Y[:,imem] , _ = cda.model_to_obs( obs_list , xf[:,:,:,imem,:] , model_grid )

#Get the ensemble mean observation departure.
dep = yo - np.mean( Y , axis = 1 ) 

#=========================================================
#  COMPUTE THE ANALYSIS UPDATE
#=========================================================
#Compute the simple analysis update
print('Computing the letkf update')
xa=cda.simple_letkf_wloc(xf=xf,Y=Y,yo=yo,model_grid=model_grid,obs_list=obs_list,
                         locs=loc_scales,
                         oerr=obs_error
                         )


#Write the analysis for the update variables
print('Writing data')

np.savez_compressed(root_data_path + '/output.npz',xa=xa,xf=xf,yo=yo,
                    hxf=hxf,obs_error=obs_error,
                    obs_loc_x=obs_loc_x,
                    obs_loc_y=obs_loc_y,
                    obs_loc_z=obs_loc_z)

print ( "We are done" )


plt.pcolor( np.mean( xa - xf , 3 )[:,0,:,0] )


