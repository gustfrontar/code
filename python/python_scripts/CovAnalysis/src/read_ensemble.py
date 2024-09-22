#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jun 18 19:29:02 2021

@author: jruiz
"""
from netCDF4 import Dataset
import wrf 
import numpy as np



def read_ensemble( path , nens , var , lev ) :
    
    var3d = False
    var2d = False
    
    for ii in range( nens ) :
        
        ncfile = Dataset( path + 'analysis.mem' + str(ii+1).zfill(3)  )
        
        tmp_data = wrf.getvar( ncfile , var )
        
        if ii == 0 :
            if len(tmp_data.shape) == 2 :
                #Two d variable
                [ny,nx]=tmp_data.shape
                var2d = True
            if len(tmp_data.shape) == 3 :
                #Three d variable
                [nz,ny,nx]=tmp_data.shape
                var3d = True
            out_data = np.zeros((ny,nx,nens))
         
        if var3d :
           out_data[:,:,ii] = tmp_data[lev,:,:]  
        if var2d :
           out_data[:,:,ii] = tmp_data[:,:]
           
    return out_data
            
         
        
        
        