#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Oct  2 12:15:35 2023

@author: jruiz
"""
import numpy as np
import matplotlib.pyplot as plt
import common_function as cf


ens_size = 30                        #Size of the ensemble     
qr_mean = 2.0e-4                     #Mean of graupel concentration
qr_std_min  = 0.05*qr_mean           #Minimum Standard deviation of graupel concentration.
qr_std_max  = 3.0*qr_mean            #Maximum Standard deviation of graupel concentration.
dbz_tr = -20.0                       #Minimum dBZ value.
qr_model_bias = 2.0e-4              #When generating the true qr we can add a model bias.                           
R=np.power( 5.0 , 2 )                #Observation error for reflectivity

sample_size = 1000                   #Size of the sample of DA updates that will be performed with 
                                     #each parameter set. 
n_parameter = 10                     #Number of parameters that will be tested within the reange for each sensitivity.

alpha = np.array([90 , 5,  1])       #Tempering coefficients are computed normalizing the inverse of the components of this vector.
                                     #the larger the value, the less observations information that is assimilated in each iteration.
                                     #Usually values are greater for the first iteration an smaller for the rest of them. 
zero_percentaje_threshold=0.3        #RMSE and SPREAD will be computed and ploted separatedly for those events in which the percentaje
                                     #of members with qr=0 in the guess is above this threshold. This cases are supposed to be more non-linear
                                     #and more challenging. 

alpha=1 / ( ( 1/alpha ) /  np.sum( 1/alpha ) )


#Sensitivity to the ensemble spread.
    
par_qr_anal_rmse = np.zeros( n_parameter )      #RMSE over the sample
par_qr_gues_rmse = np.zeros( n_parameter )
par_dbz_anal_rmse = np.zeros( n_parameter )
par_dbz_gues_rmse = np.zeros( n_parameter )

par_qr_anal_rmse_zeros = np.zeros( n_parameter )      #RMSE over the sample for cases where more than half of the qr guess is 0.
par_dbz_anal_rmse_zeros = np.zeros( n_parameter )


par_qr_anal_bias = np.zeros( n_parameter )      #BIAS over the sample
par_qr_gues_bias = np.zeros( n_parameter )
par_dbz_anal_bias = np.zeros( n_parameter )
par_dbz_gues_bias = np.zeros( n_parameter )

par_qr_anal_sprd = np.zeros( n_parameter )      #SPREAD over the sample
par_qr_gues_sprd = np.zeros( n_parameter )
par_dbz_anal_sprd = np.zeros( n_parameter )
par_dbz_gues_sprd = np.zeros( n_parameter )

par_qr_anal_sprd_zeros = np.zeros( n_parameter )      #SPREAD over the sample
par_dbz_anal_sprd_zeros = np.zeros( n_parameter )

par_qr_inconsistencyratio = np.zeros( n_parameter )   #Percentaje of times in which the update is in the opossite direction.
par_dbz_inconsistencyratio = np.zeros( n_parameter )

par_qr_overcorrectionratio = np.zeros( n_parameter )  #Percentaje of times in which the update is "overreacting".
par_dbz_overcorrectionratio = np.zeros( n_parameter )


par_qr_anal_rmse_temp = np.zeros( n_parameter )      #RMSE over the sample
par_qr_gues_rmse_temp = np.zeros( n_parameter )
par_dbz_anal_rmse_temp = np.zeros( n_parameter )
par_dbz_gues_rmse_temp = np.zeros( n_parameter )

par_qr_anal_rmse_zeros_temp = np.zeros( n_parameter )      #RMSE over the sample for cases where more than half of the qr guess is 0.
par_dbz_anal_rmse_zeros_temp = np.zeros( n_parameter )

par_qr_anal_bias_temp = np.zeros( n_parameter )      #BIAS over the sample
par_qr_gues_bias_temp = np.zeros( n_parameter )
par_dbz_anal_bias_temp = np.zeros( n_parameter )
par_dbz_gues_bias_temp = np.zeros( n_parameter )

par_qr_anal_sprd_temp = np.zeros( n_parameter )      #SPREAD over the sample
par_qr_gues_sprd_temp = np.zeros( n_parameter )
par_dbz_anal_sprd_temp = np.zeros( n_parameter )
par_dbz_gues_sprd_temp = np.zeros( n_parameter )

par_qr_anal_sprd_zeros_temp = np.zeros( n_parameter )      #SPREAD over the sample
par_dbz_anal_sprd_zeros_temp = np.zeros( n_parameter )

par_qr_inconsistencyratio_temp = np.zeros( n_parameter )   #Percentaje of times in which the update is in the opossite direction.
par_dbz_inconsistencyratio_temp = np.zeros( n_parameter )

par_qr_overcorrectionratio_temp = np.zeros( n_parameter )  #Percentaje of times in which the update is "overreacting".
par_dbz_overcorrectionratio_temp = np.zeros( n_parameter )


for ip in range( n_parameter ) :
    
   qr_std = qr_std_min + ( ip ) * ( qr_std_max - qr_std_min ) / ( n_parameter - 1 ) 

   result=cf.da_statistics( ens_size , qr_mean , qr_std , dbz_tr , qr_model_bias , R , np.array([1]) , sample_size )


   par_qr_anal_rmse[ip]  = np.sqrt( np.mean( result['anal_qr_error'] ** 2 ) )     #RMSE over the sample
   par_qr_gues_rmse[ip]  = np.sqrt( np.mean( result['gues_qr_error'] ** 2 ) )
   par_dbz_anal_rmse[ip] = np.sqrt( np.mean( result['anal_dbz_error'] ** 2 ) )
   par_dbz_gues_rmse[ip] = np.sqrt( np.mean( result['gues_dbz_error'] ** 2 ) )

   par_qr_anal_rmse_zeros[ip]  = np.sqrt( np.mean( result['anal_qr_error'][result['zero_percentaje_guess'] > zero_percentaje_threshold ] ** 2 ) )     #RMSE over the sample
   par_dbz_anal_rmse_zeros[ip] = np.sqrt( np.mean( result['anal_dbz_error'][result['zero_percentaje_guess'] > zero_percentaje_threshold ] ** 2 ) )


   par_qr_anal_bias[ip]  = np.mean( result['anal_qr_error'] )                     #BIAS over the sample
   par_qr_gues_bias[ip]  = np.mean( result['gues_qr_error'] )
   par_dbz_anal_bias[ip] = np.mean( result['anal_dbz_error'] )
   par_dbz_gues_bias[ip] = np.mean( result['gues_dbz_error'] )

   par_qr_anal_sprd[ip]  = np.mean( result['anal_qr_sprd'] )                     #SPRD over the sample
   par_qr_gues_sprd[ip]  = np.mean( result['gues_qr_sprd'] )
   par_dbz_anal_sprd[ip] = np.mean( result['anal_dbz_sprd'] )
   par_dbz_gues_sprd[ip] = np.mean( result['gues_dbz_sprd'] )

   par_qr_anal_sprd_zeros[ip]  = np.mean( result['anal_qr_sprd'][result['zero_percentaje_guess'] > zero_percentaje_threshold ] )                     #SPRD over the sample
   par_dbz_anal_sprd_zeros[ip] = np.mean( result['anal_dbz_sprd'][result['zero_percentaje_guess'] > zero_percentaje_threshold ] )

   par_qr_inconsistencyratio[ip] = np.sum(  np.abs( result['anal_qr_error'] ) >  np.abs( result['gues_qr_error'] )  )/sample_size   #Percentaje of times in which the update is in the opossite direction.
   par_dbz_inconsistencyratio[ip] = np.sum( np.abs( result['anal_dbz_error'] ) >  np.abs( result['gues_dbz_error']  ) )/sample_size

   par_qr_overcorrectionratio[ip] = np.sum( np.sign( result['anal_qr_error'] ) !=  np.sign( result['gues_qr_error'] ) )/sample_size  #Percentaje of times in which the update is "overreacting".
   par_dbz_overcorrectionratio[ip] = np.sum( np.sign( result['anal_dbz_error'] ) !=  np.sign( result['gues_dbz_error'] ) )/sample_size

   result=cf.da_statistics( ens_size , qr_mean , qr_std , dbz_tr , qr_model_bias , R , alpha , sample_size )

   par_qr_anal_rmse_temp[ip]  = np.sqrt( np.mean( result['anal_qr_error'] ** 2 ) )     #RMSE over the sample
   par_qr_gues_rmse_temp[ip]  = np.sqrt( np.mean( result['gues_qr_error'] ** 2 ) )
   par_dbz_anal_rmse_temp[ip] = np.sqrt( np.mean( result['anal_dbz_error'] ** 2 ) )
   par_dbz_gues_rmse_temp[ip] = np.sqrt( np.mean( result['gues_dbz_error'] ** 2 ) )

   par_qr_anal_rmse_zeros_temp[ip]  = np.sqrt( np.mean( result['anal_qr_error'][result['zero_percentaje_guess'] > zero_percentaje_threshold ] ** 2 ) )     #RMSE over the sample
   par_dbz_anal_rmse_zeros_temp[ip] = np.sqrt( np.mean( result['anal_dbz_error'][result['zero_percentaje_guess'] > zero_percentaje_threshold ] ** 2 ) )

   par_qr_anal_bias_temp[ip]  = np.mean( result['anal_qr_error'] )                     #BIAS over the sample
   par_qr_gues_bias_temp[ip]  = np.mean( result['gues_qr_error'] )
   par_dbz_anal_bias_temp[ip] = np.mean( result['anal_dbz_error'] )
   par_dbz_gues_bias_temp[ip] = np.mean( result['gues_dbz_error'] )

   par_qr_anal_sprd_temp[ip]  = np.mean( result['anal_qr_sprd'] )                     #SPRD over the sample
   par_qr_gues_sprd_temp[ip]  = np.mean( result['gues_qr_sprd'] )
   par_dbz_anal_sprd_temp[ip] = np.mean( result['anal_dbz_sprd'] )
   par_dbz_gues_sprd_temp[ip] = np.mean( result['gues_dbz_sprd'] )

   par_qr_anal_sprd_zeros_temp[ip]  = np.mean( result['anal_qr_sprd'][result['zero_percentaje_guess'] > zero_percentaje_threshold ] )                     #SPRD over the sample
   par_dbz_anal_sprd_zeros_temp[ip] = np.mean( result['anal_dbz_sprd'][result['zero_percentaje_guess'] > zero_percentaje_threshold ] )

   par_qr_inconsistencyratio_temp[ip] = np.sum(  np.abs( result['anal_qr_error'] ) >  np.abs( result['gues_qr_error'] )  )/sample_size   #Percentaje of times in which the update is in the opossite direction.
   par_dbz_inconsistencyratio_temp[ip] = np.sum( np.abs( result['anal_dbz_error'] ) >  np.abs( result['gues_dbz_error']  ) )/sample_size

   par_qr_overcorrectionratio_temp[ip] = np.sum( np.sign( result['anal_qr_error'] ) !=  np.sign( result['gues_qr_error'] ) )/sample_size  #Percentaje of times in which the update is "overreacting".
   par_dbz_overcorrectionratio_temp[ip] = np.sum( np.sign( result['anal_dbz_error'] ) !=  np.sign( result['gues_dbz_error'] ) )/sample_size

 

#Plot.
fig , ax = plt.subplots(3,1)
fig.set_figheight(10)
fig.set_figwidth(5)
par_range = qr_std_min + ( np.arange(n_parameter) ) * ( qr_std_max - qr_std_min ) / ( n_parameter - 1 )
fig.suptitle('Results for EnsSize=' +str(ens_size)+' qr_mean=' +str(qr_mean)+'\n qr_model_bias='+str(qr_model_bias)+'R='+str(R)+'\n alpha='+str(alpha))
ax[0].plot(par_range,par_qr_anal_rmse,'-k')
ax[0].plot(par_range,par_qr_anal_rmse_temp,'-g')

ax[0].plot(par_range,par_qr_anal_sprd,'--k')
ax[0].plot(par_range,par_qr_anal_sprd_temp,'--g')

ax[0].ticklabel_format(style='sci', axis='x', scilimits=(0,0))
ax[0].set_title('Analysis RMSE and SPRD')

ax[1].plot(par_range,par_qr_anal_rmse_zeros,'-k')
ax[1].plot(par_range,par_qr_anal_rmse_zeros_temp,'-g')

ax[1].plot(par_range,par_qr_anal_sprd_zeros,'--k')
ax[1].plot(par_range,par_qr_anal_sprd_zeros_temp,'--g')

ax[1].ticklabel_format(style='sci', axis='x', scilimits=(0,0))
ax[1].set_title('Analysis RMSE and SPRD')

ax[2].plot(par_range,par_qr_anal_bias,'-k')
ax[2].plot(par_range,par_qr_anal_bias_temp,'-g')

ax[2].ticklabel_format(style='sci', axis='x', scilimits=(0,0))
ax[2].set_title('Analysis BIAS')
  
#ax[2].plot(par_range,par_qr_inconsistencyratio,'-k')
#ax[2].plot(par_range,par_qr_inconsistencyratio_temp,'-g')

#ax[2].ticklabel_format(style='sci', axis='x', scilimits=(0,0))
#ax[2].set_title('Analysis detreimental update ratio')

figname='Figure_tempering_ETKF_ESize='+str(ens_size)+'_qr_mean=' +str(qr_mean)+'_qr_model_bias='+str(qr_model_bias)+'_R='+str(R)+'.png'
plt.savefig( figname )



