#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""

Created on Tue Feb 26 23:30:13 2019

@author: jruiz

"""
import numpy as np
import matplotlib.pyplot as plt
import common_function as cf

#PARAMETERS ---------

ens_size = 100                       #Size of the ensemble     
qr_mean = 0.5e-4                     #Mean of graupel concentration
qr_std  = 5.0e-5                     #Standard deviation of graupel concentration.
dbz_tr = -20.0                       #Minimum dBZ value.
qr_model_bias = 0.0e-4               #When generating the true qr we can add a model bias.                           
R=np.power( 1.0 , 2 )                #Observation error for reflectivity

alpha = np.array([90 , 5,  1])      #Tempering coefficients are computed normalizing the inverse of the components of this vector.
                                     #the larger the value, the less observations information that is assimilated in each iteration.
                                     #Usually values are greater for the first iteration an smaller for the rest of them. 

alpha=1 / ( ( 1/alpha ) /  np.sum( 1/alpha ) )

#--------------------
#Define qr ensemble
    
    
qr_ens = qr_mean+qr_std*np.random.randn( ens_size )

qr_ens[ qr_ens < 0.0 ] = 0.0

#Define the true and the observations

qr_true = qr_mean+qr_std*np.random.randn( 1 ) + qr_model_bias
if qr_true < 0.0 :
    qr_true = 0.0

dbz_true = cf.calc_ref( qr_true )
dbz_obs = dbz_true + (R**0.5) * np.random.randn( 1 )
if dbz_obs < dbz_tr :
    dbz_obs = dbz_tr

#First compute the ensemble in terms of graupel concentration.
dbz_ens = np.zeros( np.shape( qr_ens ) )   
#For each graupel concentration compute the corresponding reflectivity using 
#the non-linear observation operator.
for ii in range( ens_size ):
    dbz_ens[ii] = cf.calc_ref( qr_ens[ii] )

#Truncate the computed reflectivity values so there are no value below 
#the selected threshold.
dbz_ens[ dbz_ens < dbz_tr ] = dbz_tr

dbz_a_ens_temp = dbz_ens 
qr_a_ens_temp  = qr_ens
for it in range( alpha.size ) :
    dbz_a_ens_temp , qr_a_ens_temp = cf.calc_etkf_filter_update( dbz_a_ens_temp , qr_a_ens_temp , dbz_obs , R * alpha[it] )

dbz_a_ens , qr_a_ens = cf.calc_etkf_filter_update( dbz_ens , qr_ens , dbz_obs , R )



dbz_a_ens , qr_a_ens = cf.calc_etkf_filter_update( dbz_ens , qr_ens , dbz_obs , R )
dbz_a_ens[ dbz_a_ens < dbz_tr ] = dbz_tr
    
#Fit linear regression between truncated reflectivity and graupel concentration.
fit=np.polyfit(qr_ens,dbz_ens, 1)   


#Plot the background ensemble (reflectivity vs graupel concentration)    
plt.figure()
plt.plot( qr_ens , dbz_ens , 'ob' ,markersize=1.0)
plt.plot( qr_a_ens , dbz_a_ens , 'or',markersize=1.0 )

plt.plot(np.mean(qr_ens),np.mean(dbz_ens),'ob',markersize=5.0)
plt.plot(np.mean(qr_a_ens),np.mean(dbz_a_ens),'or',markersize=5.0)
plt.plot(np.mean(qr_a_ens_temp),np.mean(dbz_a_ens_temp),'om',markersize=5.0)


#x=np.array([-1e-4,np.max(qr_ens)])
#plt.plot(x,x*fit[0]+fit[1],'--b')
#plt.grid()

# #plt.plot(qr_a,dbz_a_lin,'or',markersize=10.0)
# #plt.plot(qr_a_pf,dbz_a_pf,'om',markersize=10.0)
plt.plot(qr_true,dbz_true,'ok',markersize=5.0)
plt.plot(qr_true,dbz_obs,'og',markersize=5.0)

