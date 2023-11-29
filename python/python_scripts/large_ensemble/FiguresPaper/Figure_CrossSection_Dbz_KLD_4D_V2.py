# -*- coding: utf-8 -*-
"""
Created on Tue Nov  1 18:45:15 2016

@author:
"""

import sys
sys.path.append('../../../common_python/common_functions/')
sys.path.append('../../../common_python/common_modules/')

import scipy.ndimage.filters as spyf

import numpy as np
import datetime as dt
import ctl_reader as ctlr
import os

import common_plot_functions as cpf
import common_mask_functions as cmf

import matplotlib
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
from matplotlib import patches
import common_histogram_functions as chf
from matplotlib import cm
from matplotlib.colors import ListedColormap, LinearSegmentedColormap

nbv=1000                                      #Number of ensemble members
nbins=int(np.round(np.sqrt(nbv)))             #Number of bins used to compute the histogram
smooth_range=2                                #To smooth the histogram


basedir='/home/jruiz/share/LARGE_ENSEMBLE/output_data/'

expnames = ['/LE_D1_1km_5min_4D_OFP_V2/','/LE_D1_1km_1min_4D/']

#cen_lon = [135.28]
cen_lon = [135.29]
ini_lat = [34.91]
end_lat = [35.15]

buffer_size=0.005  #Delta longtitude to perform zonal average.

filetype=['guesgp']   #analgp , analgz , guesgp , guesgz

#Define initial and end times using datetime module.
ctimes = [dt.datetime(2013,7,13,5,30,0)]  #Initial time.

#Define the delta.
delta=dt.timedelta(seconds=60)

ctl_file = basedir + expnames[0] + '/ctl/moment0001_for.ctl'

#=========================================================
#  READ CTL FILE
#=========================================================

ctl_dict = ctlr.read_ctl( ctl_file )

nx=ctl_dict['nx']
ny=ctl_dict['nx']
nlev=ctl_dict['nz']
nt=int(1)             #Force the number of times to be one.
ctl_dict['nt']=int(1) #Force the number of times to be one.

undef=np.float32( ctl_dict['undef'] )

#=========================================================
#  READ LAT LON
#=========================================================

latlon_file = basedir + expnames[0] + '/latlon/latlon.grd'

tmp=ctlr.read_data_records(latlon_file,ctl=ctl_dict,records=np.array([0,1]))
lat=np.transpose(np.squeeze(tmp[:,:,1]),axes=[1,0])
lon=np.transpose(np.squeeze(tmp[:,:,0]),axes=[1,0])

#=========================================================
#  READ'N PLOT
#=========================================================


levels=ctl_dict['vlevels']

tick_levels=[1000,850,700,500,300,200]
levels_str=list()
levels=np.delete(levels,4,axis=0)
levels[3]=850.0
nlevs=np.size( levels )

#titles=['(a)','(b)','(c)','(d)','(e)','(f)','(g)','(h)','(i)']

#Get the level string list.
levels_str=[]
for ilev in tick_levels  :
   levels_str.append( str(int(ilev)) )


fig, axs = plt.subplots( 2,2 , figsize=[4.05,5] )
fig.subplots_adjust(wspace=0.05,hspace=0.1,bottom=0.04,left=0.12,right=0.86,top=0.99)

label_1=['(a) - 5MIN-4D','(b) - 1MIN-4D']
label_2=['(c) - 5MIN-4D','(d) - 1MIN-4D']
label_3=['(e) - 5MIN-4D','(f) - 1MIN-4D']
label_4=['(g) - 5MIN-4D','(h) - 1MIN-4D']

#mask = cmf.box_mask( cen_lon[0] - buffer_size , cen_lon[0] + buffer_size , ini_lat[0] , end_lat[0] , lon , lat )
mean_lat = 0.5*( end_lat[0] + ini_lat[0] )
min_dist_loc = np.nanargmin( (lat - mean_lat)**2 + (lon - cen_lon[0])**2  )
[cross_x,cross_y]=np.unravel_index( min_dist_loc , (nx,ny) )
cross_ys = np.nanargmin( ( lat[cross_x,:] - ini_lat[0] )**2 )
cross_ye = np.nanargmin( ( lat[cross_x,:] - end_lat[0] )**2 )
cross_lat = lat[cross_x,cross_ys:cross_ye]
print('Cross section at x=',cross_x,' and y ranging between',cross_ys,cross_ye)
print('Lon for Cross section ',lon[cross_x,cross_ys])
print('Lat for Cross section ',lat[cross_x,cross_ys],lat[cross_x,cross_ye])

#=========================================================
#  PLOT CROSS-SECTIONS OF REFLECTIVITY AND W.
#=========================================================

for iexp,my_exp in enumerate( expnames ) :

   #=========================================================
   #  READ
   #=========================================================

   my_file=basedir + expnames[iexp] + ctimes[0].strftime("%Y%m%d%H%M%S") + '/' + filetype[0] + '/moment0001.grd'
   m01=ctlr.read_data_grads(my_file,ctl_dict,masked=False,undef2nan=True)
   my_file=basedir + expnames[iexp] + ctimes[0].strftime("%Y%m%d%H%M%S") + '/' + filetype[0] + '/moment0002.grd'
   m02=ctlr.read_data_grads(my_file,ctl_dict,masked=False,undef2nan=True)
   my_file=basedir + expnames[iexp] + ctimes[0].strftime("%Y%m%d%H%M%S") + '/' + filetype[0] + '/kldistance.grd'
   kld=ctlr.read_data_grads(my_file,ctl_dict,masked=False)
   #hist_file = basedir + expnames[iexp] + ctimes[0].strftime("%Y%m%d%H%M%S") + '/' + filetype[0]  + '/histogram.grd'
   #max_file  = basedir + expnames[iexp] + ctimes[0].strftime("%Y%m%d%H%M%S") + '/' + filetype[0]  + '/ensmax.grd'
   #min_file  = basedir + expnames[iexp] + ctimes[0].strftime("%Y%m%d%H%M%S") + '/' + filetype[0]  + '/ensmin.grd'
   #hist=chf.read_histogram(hist_file,max_file,min_file,nx,ny,nbins,ctl_dict,dtypein='i2')

   w     =np.squeeze(np.delete( np.transpose( m01['w']   , axes=[1,0,2,3] ) ,4,2))[cross_x,cross_ys:cross_ye,:]
   dbz   =np.squeeze(np.delete( np.transpose( m01['dbz'] , axes=[1,0,2,3] ) ,4,2))[cross_x,cross_ys:cross_ye,:]
   wvar  =np.squeeze(np.delete( np.transpose( m02['w']   , axes=[1,0,2,3] ) ,4,2))[cross_x,cross_ys:cross_ye,:]
   wkld  =np.squeeze(np.delete( np.transpose( kld['w']   , axes=[1,0,2,3] ) ,4,2))[cross_x,cross_ys:cross_ye,:]
   dbzkld=np.squeeze(np.delete( np.transpose( kld['dbz'] , axes=[1,0,2,3] ) ,4,2))[cross_x,cross_ys:cross_ye,:]
   tkld  =np.squeeze(np.delete( np.transpose( kld['tk'] , axes=[1,0,2,3] ) ,4,2))[cross_x,cross_ys:cross_ye,:]
   wkld_plot=np.copy( wkld )
   wkld[ dbz < 30.0 ] = np.nan

   #max_loc=np.nanargmax( wkld )
   #ny_cross = cross_ye - cross_ys + 1
   #[hist_y,hist_z]=np.unravel_index( max_loc , (ny_cross,nlevs) )  
   #hist_y = hist_y + cross_ys
   #hist_x = cross_x                             
   #lon_hist=lon[hist_x,hist_y]
   #lat_hist=lat[hist_x,hist_y]
   #lev_hist=levels[hist_z]
   #print('Hist x,y,z', hist_x , hist_y , hist_z )
   #print('Hist lon,lat,lev',lon_hist,lat_hist,lev_hist )

   #PLOTTING W AND DBZ
   smin=0
   smax=70
   ncols=70
   delta = ( smax - smin )/ncols
   clevs=np.arange(smin,smax+delta,delta)

   my_map = cm.get_cmap('gist_ncar',ncols+10)
   tmp_colors = my_map( range( ncols +10 ) )
   for ii in range( 10 ) :
       tmp_colors[ii,:] =np.array([1.0,1.0,1.0,1.0])
       tmp_colors=np.delete( tmp_colors , np.arange( 70 , 80 ) , axis = 0 )
   my_map = ListedColormap( tmp_colors )


   ax = axs[0,iexp]
   minlat = cross_lat[0]
   maxlat = cross_lat[-1]
   p=ax.contourf( cross_lat , -np.log( levels )  , np.transpose( dbz ) , clevs  ,cmap=my_map)
   cpos=ax.contour(cross_lat , -np.log( levels ) , np.transpose(w),levels=[2.5,5,7.5,10],colors='k',linewidths=2.0,linestyles='solid')
   cpos=ax.contour(cross_lat , -np.log( levels ) , np.transpose(w),levels=[-5.0,-2.5],colors='k',linewidths=2.0,linestyles='dashed')
   plt.clabel(cpos, inline=1, fontsize=12,fmt='%1.0f')

   ax.set_xlim([minlat,maxlat])
   ax.set_ylim([-np.log(levels[0]),-np.log(levels[-1])+0.01])
   ytick=-np.log(tick_levels)
   ax.set_yticks(ytick)
   if iexp == 0 :
      ax.set_yticklabels(levels_str,fontsize=12,color='k')
   else         :
      ax.set_yticklabels([])
   ax.set_xticks(np.array([35.0,35.1]))
   ax.set_xticklabels([])
   #ax.set_xticklabels(np.array([34.9,35.0,35.1]),fontsize=14,color='k')
   ax.tick_params(axis='both', which='both', length=0)

   ax.grid(linewidth=0.5, color='k',alpha=0.4, linestyle='--')
   ax.text( 34.912 , -np.log( 220 ) ,label_1[iexp] ,fontsize=12,color='k',bbox={'facecolor':'white', 'alpha':0.0,'edgecolor':'white'})
   #ax.set_title(label_1[iexp],fontsize=17)
   #ax.plot( lat_hist , -np.log(lev_hist) , "bo" , markersize=10 )

   if iexp == 0 :
      #cbar_ax = fig.add_axes([0.96, 0.78, 0.015, 0.21])
      cbar_ax = fig.add_axes([0.87, 0.538, 0.025 , 0.45])
      m = plt.cm.ScalarMappable(cmap=my_map )
      m.set_array(np.transpose(dbz))
      m.set_clim(smin,smax)
      cb=plt.colorbar(m,cax=cbar_ax,boundaries=np.arange(smin,smax+delta,delta),ticks=np.arange(0,80,10),orientation='vertical')
      cb.ax.tick_params(labelsize=12)
      ax.annotate('[dBZ]',(0.85,0.5),xycoords='figure fraction',fontsize=8)

   #Plotting KLD for W and variance
   ax = axs[1,iexp]
   smin = 5.0
   smax = 40.0
   var_levs=np.arange(3.0,30,3.0)
   wkld_plot[wkld_plot > 0.39 ] = 0.39
   dbzkld[dbzkld > 0.3 ] = 0.3
   tkld[tkld > 0.2 ] = 0.2
   ncolors=35
   my_map = cm.get_cmap('cool',ncolors)
   delta = (smax-smin)/ncolors
   clevs=np.arange(smin,smax+delta,delta)
   p=ax.contourf( cross_lat , -np.log( levels ) ,  100*np.transpose(wkld_plot), clevs  , cmap=my_map)
   c=ax.contour(cross_lat , -np.log( levels ) , np.transpose(wvar),levels=var_levs,colors='r',linewidths=1.0,linestyles='solid')
   #c=ax.contour(cross_lat , -np.log( levels ) , np.transpose(dbzkld)*100,levels=np.arange(10,40,10),colors='b',linewidths=1.0,linestyles='solid')
   #plt.clabel(c, inline=1, fontsize=15,fmt='%1.1f')
   #p=ax.contourf( cross_lat , -np.log( levels ) ,  100*np.transpose(dbzkld), [5,20,35]  , cmap='Reds' , alpha=0.3)
   p=ax.contour( cross_lat , -np.log( levels ) ,  100*np.transpose(tkld), [4]  , alpha=1.0 , colors='b')
   cdbz=ax.contour(cross_lat , -np.log( levels ) , np.transpose(dbz),levels=[30.0],colors='k',linewidths=3.0,linestyles='dashed')
   ax.set_xlim([minlat,maxlat])
   ax.set_ylim([-np.log(levels[0]),-np.log(levels[-1])+0.01])
   ytick=-np.log(tick_levels)
   ax.set_yticks(ytick)
   if iexp == 0 :
      ax.set_yticklabels(levels_str,fontsize=12,color='k')
   else         :
      ax.set_yticklabels([])
   ax.set_xticks(np.array([35.0,35.1]))
   ax.set_xticklabels(np.array([34.91,35.0,35.1]),fontsize=12,color='k')
   ax.tick_params(axis='both', which='both', length=0)
   ax.grid(linewidth=0.5, color='k',alpha=0.4, linestyle='--')
   #ax.set_title(label_1[iexp],fontsize=17)
   ax.text( 34.912 , -np.log( 220 ) ,label_2[iexp] ,fontsize=12,color='k',bbox={'facecolor':'white', 'alpha':0.0,'edgecolor':'white'})
   #ax.plot( lat_hist , -np.log(lev_hist) , "bo" , markersize=10 )

   if iexp == 0 :
      #cbar_ax = fig.add_axes([0.96, 0.533, 0.015, 0.21])
      cbar_ax = fig.add_axes([0.87, 0.04, 0.025 , 0.45])
      #cbar_ax = fig.add_axes([0.59, 0.77, 0.31 , 0.011])
      m = plt.cm.ScalarMappable(cmap=my_map )
      m.set_array(np.transpose(dbz))
      m.set_clim(smin,smax)
      cb=plt.colorbar(m,cax=cbar_ax,boundaries=np.arange(smin,smax+delta,delta),orientation='vertical',ticks=np.arange(0,80,5))
      cb.ax.tick_params(labelsize=12)
      ax.annotate('$[{10}^{-2}]$',(0.85,0.005),xycoords='figure fraction',fontsize=8)


#plt.show()
plt.savefig('Figure_CrossSection_dBZ_KLD_4D.png' , format='png' , dpi=300  )
plt.close()














