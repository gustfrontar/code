#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""

Created on Tue Feb 26 23:30:13 2019

@author: jruiz

"""
import numpy as np
import matplotlib.pyplot as plt

def calc_ref( q , assume_graupel = False )  :
    
    nor=8.0e6        #[m^-4]
    ror=1000.0e0     #[Kg/m3]
    pip=np.power( np.pi , 1.75 )   #factor
    cf =1.0e18 * 720 #factor 
    ro=1.0e0
    
    nog=4.0e4      #[m^-4]
    rog=913.0e0    #[Kg/m3] 

    if q <= 1.0e-10  :
      q = 1.0e-10
        
    z= cf * ( np.power( ro * q , 1.75 ) )
    z= z / ( pip * ( nor ** 0.75 ) * ( ror ** 1.75 ) )
  
    if assume_graupel  :
       z= ( cf / ( pip * ( nog ** 0.75) * ( rog ** 1.75 ) ) ) ** 0.95
       z= z * ( ( ro * q ) ** 1.6625 )

    
    z = 10.0*np.log10( z )  
    
    return z

#Define qr ensemble
    
ens_size = 100     

qr_mean = 0.002
qr_std  = 0.002

dbz_tr = 10.0 
obs= 20.0
R=np.power( 5.0 , 2 )

assume_graupel = True
    
qr_ens = qr_mean+qr_std*np.random.randn( ens_size )

qr_ens[ qr_ens < 0.0 ] = 0.0 + 0.00001


#Define dbz ensemble    
dbz_ens = np.zeros( np.shape( qr_ens ) )   
for ii in range( ens_size ):
    dbz_ens[ii] = calc_ref( qr_ens[ii] , assume_graupel = assume_graupel )


qr_ens_trans = np.log10(qr_ens) 

    
plt.figure()
plt.plot( qr_ens_trans , dbz_ens , 'ob' )


dbz_ens_trunc=np.copy( dbz_ens )
dbz_ens_trunc[ dbz_ens_trunc < dbz_tr ] = dbz_tr

plt.plot( qr_ens_trans , dbz_ens_trunc , 'or' )

    
 #Fit linear regression
fit=np.polyfit(qr_ens_trans,dbz_ens, 1)   
fit_trunc=np.polyfit(qr_ens_trans,dbz_ens_trunc, 1)   

x=np.array([-1e-4,np.max(qr_ens)])
plt.plot(x,x*fit[0]+fit[1],'--b')
plt.plot(x,x*fit_trunc[0]+fit_trunc[1],'--r')
plt.grid()


#Compute updates.

cov=np.cov( np.stack((qr_ens_trans,dbz_ens), axis=0) )[0,1]
cov_trunc=np.cov( np.stack((qr_ens_trans,dbz_ens_trunc), axis=0) )[0,1]

qr_var = np.var( qr_ens_trans )
dbz_var = np.var( dbz_ens )
dbz_var_trunc = np.var( dbz_ens_trunc )
dbz_mean = np.mean( dbz_ens )
dbz_mean_trunc = np.mean( dbz_ens_trunc )


qr_update = ( cov / ( dbz_var + R ) ) * ( obs - dbz_mean )

qr_update_trunc = ( cov_trunc / ( dbz_var_trunc + R ) ) * ( obs - dbz_mean_trunc )

qr_a = np.mean( qr_ens_trans) + qr_update

qr_a_trunc = np.mean( qr_ens_trans ) + qr_update_trunc

dbz_a = calc_ref( 10**qr_a , assume_graupel = assume_graupel)
dbz_a_trunc = calc_ref( 10**qr_a_trunc , assume_graupel = assume_graupel)

dbz_a_lin= dbz_mean + ( dbz_var / ( dbz_var + R ) ) * ( obs - dbz_mean )
dbz_a_trunc_lin= dbz_mean_trunc + ( dbz_var_trunc / ( dbz_var_trunc + R ) ) * ( obs - dbz_mean_trunc )

plt.plot(qr_a,dbz_a,'ob',markersize=20.0)

plt.plot(qr_a_trunc,dbz_a_trunc,'or',markersize=20.0)


plt.plot(qr_a,dbz_a_lin,'ob',markersize=15.0)
plt.plot(qr_a_trunc,dbz_a_trunc_lin,'or',markersize=15.0)

#plt.plot(qr_mean,dbz_mean_trunc,'ok',markersize=10.0)    
#plt.plot(qr_mean,dbz_mean,'ok',markersize=10.0)   
plt.plot(np.array([0,np.max(qr_ens)]),np.array([dbz_mean_trunc,dbz_mean_trunc]),'-k',markersize=10.0) 
plt.plot(np.array([0,np.max(qr_ens)]),np.array([dbz_a_trunc,dbz_a_trunc]),'--k',markersize=10.0) 