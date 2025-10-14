#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Mar  4 15:34:52 2022

@author: franco
"""

import calendar
import cdsapi
import numpy as np

c = cdsapi.Client()
dataset = 'reanalysis-era5-pressure-levels'
# variables = ['potential_vorticity','relative_humidity','specific_cloud_liquid_water_content',
#              'specific_humidity','specific_rain_water_content','temperature',
#              'u_component_of_wind','v_component_of_wind', 'vertical_velocity', 'vorticity']
variables = ['relative_humidity',]
for year in range(1980, 2021):
#for year in range(2021, 2022):
    print ('YEAR ',year)
    for month in range(1,13):
    #for month in range(3,4):
        lastday1=calendar.monthrange(year,month)
        lastday=lastday1[1]
        bdate="%s%02d01"%(year,month)
        edate="%s%02d%s"%(year,month,lastday)
        dias = np.arange(1,lastday+1,1)
        dias = ['%02.0f' % (dd) for dd in dias]
        print ("######### ERA-5  #########")
        print ('get data from ', bdate,' to ',edate,' (YYYYMMDD)')
        print ("################################")
        for var in variables:
            try:
                params = {
                          'product_type': 'reanalysis',
                          'format': 'netcdf',
                          'variable': [var],
                          'pressure_level': [
                              '100', '125', '150', 
                              '175', '200','225',
                              '250', '300','350',
                              '400', '450','500',
                              '550', '600','650',
                              '700', '750','775',
                              '800', '825','850',
                              '875', '900','925',
                              '950', '975','1000',
                              ],
                          'year': year,
                          'month': '%02.0f' % month,
                          'day': dias,
                          'time': [
                              '00:00', '01:00', '02:00',
                              '03:00', '04:00', '05:00',
                              '06:00', '07:00', '08:00',
                              '09:00', '10:00', '11:00',
                              '12:00', '13:00', '14:00',
                              '15:00', '16:00', '17:00',
                              '18:00', '19:00', '20:00',
                              '21:00', '22:00', '23:00',
                              ],
                          'area': [-20, -75, -55, -40],
                          }
                fl = c.retrieve(dataset, params)
                fl.download('/datosmunin3/jruiz/datos/ERA/HR/ERA5_' + var + '_pl_%s.nc'%(bdate))
                #f1.download('/home/franco.piscitelli/prueba.nc')
                #fl.download('/home/franco/Desktop/prueba/' + var + '_pl_%s.nc'%(bdate))
                #fl.download('C:/Users/Fran/Desktop/PRUEBA' + var + '_pl_%s.nc'%(bdate))
            
            except:
                continue
