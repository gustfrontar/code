#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""

Created on Tue Feb 26 23:30:13 2019

@author: jruiz

"""
import numpy as np
import matplotlib.pyplot as plt

def calc_ref( qr, dbz_tr )  :

    nor=8.0e6        #[m^-4]
    ror=1000.0e0     #[Kg/m3]
    pip=np.power( np.pi , 1.75 )   #factor
    cf =1.0e18 * 720 #factor
    ro=1.0e0

#    if qr <= 1.0e-10  :
#      qr = 1.0e-10
    zr=0
    if qr > 0.0:
      zr= cf * ( np.power( ro * qr , 1.75 ) )
      zr= zr / ( pip * ( np.power( nor , 0.75 ) ) * ( np.power( ror , 1.75 ) ) )

    if zr < dbz_tr:
       zr = dbz_tr
    else:
       zr = 10.0*np.log10( zr )

    return zr


#Define qr ensemble

ens_size = 100

qr_mean = 0.002
qr_std  = 0.002

dbz_tr = 10.0
obs=20.0
qr_obs=0.0

R=np.power( 5.0 , 2 )
R_qr = np.power( 10e-5 , 2 )

qr_ens = qr_mean+qr_std*np.random.randn( ens_size )

qr_ens[ qr_ens < 0.0 ] = 0.0

#Define dbz ensemble
dbz_ens = np.zeros( np.shape( qr_ens ) )
for ii in range( ens_size ):
    dbz_ens[ii] = calc_ref( qr_ens[ii], dbz_tr )

plt.figure()
plt.plot( qr_ens , dbz_ens , 'ob' )


dbz_ens_trunc=np.copy( dbz_ens )
dbz_ens_trunc[ dbz_ens_trunc < dbz_tr ] = dbz_tr

plt.plot( qr_ens , dbz_ens_trunc , 'or' )


 #Fit linear regression
fit=np.polyfit(qr_ens,dbz_ens, 1)
fit_trunc=np.polyfit(qr_ens,dbz_ens_trunc, 1)

x=np.array([-1e-4,np.max(qr_ens)])
plt.plot(x,x*fit[0]+fit[1],'--b')
plt.plot(x,x*fit_trunc[0]+fit_trunc[1],'--r')
plt.grid()


#Compute updates.

cov=np.cov( np.stack((qr_ens,dbz_ens), axis=0) )[0,1]
cov_trunc=np.cov( np.stack((qr_ens,dbz_ens_trunc), axis=0) )[0,1]

qr_var = np.var( qr_ens )
dbz_var = np.var( dbz_ens )
dbz_var_trunc = np.var( dbz_ens_trunc )
dbz_mean = np.mean( dbz_ens )
dbz_mean_trunc = np.mean( dbz_ens_trunc )


qr_update = ( cov / ( dbz_var + R ) ) * ( obs - dbz_mean )

qr_update_trunc = ( cov_trunc / ( dbz_var_trunc + R ) ) * ( obs - dbz_mean_trunc )

qr_a = qr_mean + qr_update

qr_a_trunc = qr_mean + qr_update_trunc

dbz_a = calc_ref( qr_a, dbz_tr )
dbz_a_trunc = calc_ref( qr_a_trunc, dbz_tr )

dbz_a_lin= dbz_mean + ( dbz_var / ( dbz_var + R ) ) * ( obs - dbz_mean )
dbz_a_trunc_lin= dbz_mean_trunc + ( dbz_var_trunc / ( dbz_var_trunc + R ) ) * ( obs - dbz_mean_trunc )

plt.plot(qr_a,dbz_a,'ob',markersize=20.0)

plt.plot(qr_a_trunc,dbz_a_trunc,'or',markersize=20.0)


plt.plot(qr_a,dbz_a_lin,'ob',markersize=15.0)
plt.plot(qr_a_trunc,dbz_a_trunc_lin,'or',markersize=15.0)

#Assimilate a 0 condensate observation.

qr_update_qrobs = ( qr_var / ( qr_var + R_qr ) ) * ( qr_obs - qr_mean )
qr_a_qrobs = qr_mean + qr_update_qrobs
dbz_a_qrobs = calc_ref( qr_a_qrobs, dbz_tr)
plt.plot(qr_a_qrobs,dbz_a_qrobs,'og',markersize=20.0)

#plt.plot(qr_mean,dbz_mean_trunc,'ok',markersize=10.0)
#plt.plot(qr_mean,dbz_mean,'ok',markersize=10.0)
#plt.plot(np.array([0,np.max(qr_ens)]),np.array([dbz_mean_trunc,dbz_mean_trunc]),'-k',markersize=10.0)
#plt.plot(np.array([0,np.max(qr_ens)]),np.array([dbz_a_trunc,dbz_a_trunc]),'--k',markersize=10.0)


#Update the ensemble using a particle filter.

w= np.power(dbz_ens - obs , 2) / R
w=w/np.sum(w) #Normalizo los pesos.

qr_a_pf = np.sum( qr_ens * w )
dbz_a_pf = calc_ref( qr_a_pf, dbz_tr )
plt.plot(qr_a_pf,dbz_a_pf,'om',markersize=20.0)
