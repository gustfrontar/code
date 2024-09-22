#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jun 18 19:51:00 2021

@author: jruiz
"""
import read_ensemble as re 
import covariance    as cov
import numpy as np
import matplotlib.pyplot as plt
from glob import glob



wrf_data_path = '../data/'
ens_size      = 9*6

path_list = glob( wrf_data_path + '/*/' )

#T2 td2


for ip,my_path in enumerate(path_list) :
    
    
    #Read the ensemble
    T2_ens = re.read_ensemble( my_path , ens_size , 'T2' , 0 )
    Td2_ens = re.read_ensemble( my_path , ens_size , 'td2' , 0 )
    
    if ip == 0 :
        full_cov = np.zeros( T2_ens.shape[0:2] )
        ic_cov = np.zeros( T2_ens.shape[0:2] )
        par_cov = np.zeros( T2_ens.shape[0:2] )

    #Compute the "full covariance" using all the members.
    ngroups = 54
    groups_size = 1
    groups = np.zeros(( ngroups , groups_size )).astype(int)
    groups[:,0] = np.arange( ngroups ) 
    full_cov = full_cov + cov.cov2( T2_ens , Td2_ens , groups ) 
    
    
    #Compute the "parametrization covariance". 
    ngroups = 9
    group_size = 6
    groups = np.zeros((ngroups,group_size)).astype(int)
    for ig in range( ngroups ) :
        groups[ig,:] = np.arange(group_size) * ngroups  + ig
    par_cov = par_cov + cov.cov2( T2_ens , Td2_ens , groups ) 
    
    
    #Compute the "initial conditions covariance" 
    #groups2 = groups.T
    ic_cov = ic_cov + cov.cov( T2_ens , Td2_ens , groups ) 
    
full_cov = full_cov / len( path_list ) 
ic_cov   = ic_cov   / len( path_list )
par_cov  = par_cov  / len( path_list )
    
plt.figure()
plt.pcolor( full_cov , vmin = -5 , vmax = 5 , cmap ='bwr'); plt.colorbar()
plt.title('FULL COV')

plt.figure()
plt.pcolor( par_cov , vmin = -5 , vmax = 5 , cmap ='bwr'); plt.colorbar()
plt.title('PAR COV')

plt.figure()
plt.pcolor( ic_cov , vmin = -5 , vmax = 5 , cmap ='bwr'); plt.colorbar()
plt.title('IC COV')
