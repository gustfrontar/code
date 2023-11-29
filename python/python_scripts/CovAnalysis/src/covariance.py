#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jun 18 19:57:45 2021

@author: jruiz

"""
import numpy as np

def cov( var1 , var2 , groups ) :
    
    [ny,nx,nens] = np.shape( var1 )
    
    #Compute pointwise covariance between var1 and var2 taking into account the groups defined by groups.
    
    if len(groups.shape) == 1 :
        ngroups = 1
        group_size = groups.size
        groups = np.repeat( groups[np.newaxis,:] , 1 , 0 )
    else  :
        ngroups = groups.shape[0]
        group_size = groups.shape[1]
    
    cov_tmp = np.zeros( ( ny , nx , ngroups ) ) 
    cov_output = np.zeros( ( ny , nx ) )
    
    #Compute the covariance for each group.
    
    for ig in range( ngroups ) :  
        #Compute the covariance for each group.
        v1_mean = np.mean( var1[:,:,groups[ig,:]] , 2 )
        v2_mean = np.mean( var2[:,:,groups[ig,:]] , 2 )
        
        
        for ie in range( group_size )  :
            cov_tmp[:,:,ig] = cov_tmp[:,:,ig] + ( var1[:,:,groups[ig,ie]] - v1_mean )*( var2[:,:,groups[ig,ie]] - v2_mean )
        cov_tmp[:,:,ig] = cov_tmp[:,:,ig]/(group_size-1)
        
    cov_output = np.mean( cov_tmp , 2 )
    
    return cov_output 

def cov2( var1 , var2 , groups ) :
    
    [ny,nx,nens] = np.shape( var1 )
    
    #Compute pointwise covariance between var1 and var2 taking into account the groups defined by groups.

    if len(groups.shape) == 1 :
        ngroups = 1
        group_size = groups.size
        groups = np.repeat( groups[np.newaxis,:] , 1 , 0 )
    else  :
        ngroups = groups.shape[0]
        group_size = groups.shape[1]
    
    cov_output = np.zeros( ( ny , nx ) )
    
    #Compute the covariance for each group.
    var1_tmp = np.zeros(( ny , nx , ngroups ))
    var2_tmp = np.zeros(( ny , nx , ngroups ))
    for ig in range( ngroups ) :
        var1_tmp[:,:,ig] = np.mean( var1[:,:,groups[ig,:]] , 2 )
        var2_tmp[:,:,ig] = np.mean( var2[:,:,groups[ig,:]] , 2 )
   
    #Compute the covariance for each group.
    v1_mean = np.mean( var1_tmp[:,:,:] , 2 )
    v2_mean = np.mean( var1_tmp[:,:,:] , 2 )
  
    for ig in range( ngroups )  :
       cov_output = cov_output + ( var1_tmp[:,:,ig] - v1_mean )*( var2_tmp[:,:,ig] - v2_mean )
    if ngroups > 1 :
       cov_output = cov_output/(ngroups-1)
        
    
    return cov_output 
