FILL_VALUE = {'float32': 1e20,
              'int32': -999,
              'str': None}

#####################
# DYNAMIC VARIABLES #
#####################
### 2D ###

aclwdnb = {'name': 'aclwdnb',
           'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'ACLWDNB'}},
           'gfs': {'name': None,
                   'level': None},
           'dtype': 'float32',
           'attrs': {'units': 'J m-2',
                     'standard_name': 'integral_wrt_time_of_surface_downwelling_longwave_flux_in_air',
                     'long_name': 'Accumulated downwelling longwave flux at bottom'}, 
           'plot': {}}


aclwupb = {'name': 'aclwupb',
           'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'ACLWUPB'}},
           'gfs': {'name': None,
                   'level': None},
           'dtype': 'float32',
           'attrs': {'units': 'J m-2',
                     'standard_name': 'integral_wrt_time_of_surface_downwelling_longwave_flux_in_air',
                     'long_name': 'Accumulated upwelling longwave flux at bottom'}, 
           'plot': {}}


acswdnb = {'name': 'acswdnb',
           'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'ACSWDNB'}},
           'gfs': {'name': None,
                   'level': None},
           'dtype': 'float32',
           'attrs': {'units': 'J m-2',
                     'standard_name': 'integral_wrt_time_of_surface_downwelling_shortwave_flux_in_air',
                     'long_name': 'Accumulated downwelling shortwave flux at bottom'}, 
           'plot': {}}


# En GFS viene negativo y en WRF positivo
cin = {'name': 'cin',
       'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'cape2d'}},
       'gfs': {'name':'cin',
               'level':'surface'},
       'dtype': 'float32',
       'attrs': {'units': 'J kg-1',
                 'standard_name': 'atmosphere_convective_inhibition',
                 'long_name': 'Maximum Convective Inhibition'}, 
       'plot': {}}


frzlev = {'name': 'frzlev',
          'wrf': {'function': 'self._get_iso0_height', 'args': {}},
          'gfs': {'name':'gh', 
                  'level':'isothermZero'},
          'dtype': 'float32',
          'attrs': {'units': 'm',
                    'standard_name': 'freezing_level_altitude',
                    'long_name': 'Height above mean sea level of 0°C isotherm'}, 
          'plot': {}}


graupelnc = {'name': 'graupelnc',
             'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'GRAUPELNC'}},
             'gfs': {'name':None,
                     'level':None},
             'dtype': 'float32',
             'attrs': {'units': 'mm',
                       'standard_name': 'graupel_fall_amount',
                       'long_name': 'Accumulated total graupel'}, 
             'plot': {}}


gust10 = {'name': 'gust10',
          'wrf': {'function': 'self._get_gust10', 'args': {}},
          'gfs': {'name':'gust',
                  'level':'surface'},
          'dtype': 'float32',
          'attrs': {'units': 'm s-1',
                    'standard_name': 'wind_speed_of_gust',
                    'long_name': '10-m wind gust'}, 
          'plot': {'name': 'RAFAGAS DE VIENTO en superficie',
                   'dim': '2D',
                   'cmap': None,
                   'units': 'kts'}}


lcl = {'name': 'lcl',
       'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'cape2d'}},
       'gfs': {'name':None,
               'level':None},
       'dtype': 'float32',
       'attrs': {'units': 'm',
                 'standard_name': 'atmosphere_lifting_condensation_level',
                 'long_name': 'Lifted Condensation Level'}, 
       'plot': {}}


lfc = {'name': 'lfc',
       'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'cape2d'}},
       'gfs': {'name':None,
               'level':None},
       'dtype': 'float32',
       'attrs': {'units': 'm',
                 'standard_name': 'atmosphere_level_of_free_convection',
                 'long_name': 'Level of Free Convection'}, 
       'plot': {}}


# Para GFS usar la variable SBCAPE
mcape = {'name': 'mcape',
         'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'cape2d'}},
         'gfs': {'name':None, 
                 'level':None},
         'dtype': 'float32',
         'attrs': {'units': 'J kg-1',
                   'standard_name': 'atmosphere_convective_available_potential_energy',
                   'long_name': 'Maximum CAPE'}, 
         'plot': {'name': 'MUCAPE', 
                  'dim': '2D',
                  'cmap': None,
                  'units': 'J kg-1'}}


mdbz = {'name': 'mdbz',
        'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'mdbz'}},
        'gfs': {'name': 'refc', 
                'level': 'atmosphere'},
        'dtype': 'float32',
        'attrs': {'units': 'dBZ',
                  'standard_name': 'equivalent_reflectivity_factor',
                  'long_name': 'Max. Reflectivity'}, 
        'plot': {'name': 'COLMAX',
                 'dim': '2D',
                 'cmap': 'smn_dbz',
                 'units': 'dBZ'}}


pp = {'name': 'pp', 
      'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'RAINNC'}},
      'gfs': {'name': 'tp',
              'level': 'surface'},
      'dtype': 'float32',
      'attrs': {'units': 'mm', 
                'standard_name':'lwe_thickness_of_precipitation_amount',
                'long_name':'Accumulated Total Precipitation'}, 
      'plot': {'name': 'PRECIPITACION ACUMULADA en {H} h',
               'dim': '2D',
               'cmap': None,
               'units': 'mm'}}


ppcal = {'name': 'ppcal', 
         'wrf': {'function': None, 'args': {'varname': None}},
         'gfs': {'name': None, 
                 'level': None},
         'dtype': 'float32',
         'attrs': {'units': 'mm', 
                   'standard_name':'lwe_thickness_of_precipitation_amount',
                   'long_name':'Calibrated Accumulated Total Precipitation'}, 
         'plot': {}}


psfc = {'name': 'psfc',
        'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'PSFC'}},
        'gfs': {'name':'sp',
                'level': 'surface'},
        'dtype': 'float32',
        'attrs': {'units': 'hPa',
                  'standard_name': 'surface_air_pressure',
                  'long_name': 'Surface Pressure'}, 
        'plot': {}}


pw = {'name': 'pw',
      'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'pw'}},
      'gfs': {'name':'pwat',
              'level':'atmosphereSingleLayer'},
      'dtype': 'float32',
      'attrs': {'units': 'kg m-2',
                'standard_name': 'atmosphere_mass_content_of_water_vapor',
                'long_name': 'Precipitable Water'}, 
      'plot': {'name': 'AGUA PRECIPITABLE',
               'dim': '2D',
               'cmap': None,
               'units': 'kg m-2'}}


q2 = {'name': 'q2',
      'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'Q2'}},
      'gfs': {'name': 'sh2',
              'level': 'heightAboveGround'},
      'dtype': 'float32',
      'attrs': {'units': 'g kg-1',
                'standard_name': 'specific_humidity',
                'long_name': '2-m Water Vapor Mixing Ratio'}, 
      'plot': {}}


rh2 = {'name': 'rh2',
       'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'rh2'}},
       'gfs': {'name': 'r2',
               'level': 'heightAboveGround'},
       'dtype': 'float32',
       'attrs': {'units': '%',
                 'standard_name': 'relative_humidity',
                 'long_name': '2-m Relative Humidity'}, 
       'plot': {}}


slp = {'name': 'slp',
       'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'slp'}},
       'gfs': {'name': 'prmsl',
               'level': 'meanSea'},
       'dtype': 'float32',
       'attrs': {'units': 'hPa',
                 'stdandard_name': 'air_pressure_at_mean_sea_level',
                 'long_name': 'Sea Level Pressure'}, 
       'plot': {}}


# Para WRF/POST usar la variable MCAPE
sbcape = {'name': 'sbcape',
          'wrf': {'function': None, 'args': {'varname': None}},
          'gfs': {'name':'cape',
                  'level':'surface'},
          'dtype': 'float32',
          'attrs': {'units': 'J kg-1',
                    'standard_name': 'atmosphere_convective_available_potential_energy',
                    'long_name': 'Surface based CAPE'}, 
          'plot': {}}


snownc = {'name': 'snownc',
          'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'SNOWNC'}},
          'gfs': {'name':None, # Esta la variable snow_depth pero no me queda claro que sea lo mismo
                  'level':None},
          'dtype': 'float32',
          'attrs': {'units': 'mm',
                    'standard_name': 'snow_ice_fall_amount',
                    'long_name': 'Accumulated total snow and ice'}, 
          'plot': {'name': 'NIEVE ACUMULADA en {H} h',
                   'dim': '2D',
                   'cmap': None,
                   'units': 'mm'}}


srh1000 = {'name': 'srh1000', 
           'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'helicity', 'top':1000}},
           'gfs': {'name': None,
                   'level': None},
           'dtype': 'float32',
           'attrs': {'units': 'm2 s-2',
                     'stdandard_name': 'storm_relative_helicity', #este standard_name no esta, lo invento
                     'long_name': 'Storm relative helicity at 1 km agl'}, 
           'plot': {}}


srh3000 = {'name': 'srh3000', 
           'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'helicity', 'top':3000}},
           'gfs': {'name': 'hlcy',
                   'level': 'heightAboveGroundLayer'},
           'dtype': 'float32',
           'attrs': {'units': 'm2 s-2',
                     'stdandard_name': 'storm_relative_helicity', #este standard_name no esta, lo invento
                     'long_name': 'Storm relative helicity at 3 km agl'}, 
           'plot': {}}


t2 = {'name': 't2',
      'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'T2'}},
      'gfs': {'name':'t2m',
              'level': 'heightAboveGround'},
      'dtype': 'float32',
      'attrs': {'units': 'K',
                'standard_name': 'air_temperature',
                'long_name': '2-m Temperature'}, 
      'plot': {'name': 'TEMPERATURA a 2 m',
               'dim': '2D',
               'cmap': None,
               'units': 'degC'}}


t2cal = {'name': 't2cal',
         'wrf': {'function': None, 'args': None},
         'gfs': {'name': None,
                 'level': 'heightAboveGround'},
         'dtype': 'float32',
         'attrs': {'units': 'K',
                   'standard_name':'air_temperature',
                   'long_name':'Calibrated 2-m temperature'}, 
         'plot': {'name': 'TEMPERATURA (cal) a 2 m',
                  'dim': '2D',
                  'cmap': None,
                  'units': 'degC'}}


td2 = {'name': 'td2',
       'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'td2'}},
       'gfs': {'name':'d2m',
               'level': 'heightAboveGround'},
       'dtype': 'float32',
       'attrs': {'units': 'K',
                 'standard_name': 'dew_point_temperature',
                 'long_name': '2-m Dew Point Temperature'}, 
       'plot': {}}


t2n = {'name': 't2n',
       'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'T2'}},
       'gfs': {'name':'tmin',
               'level': 'heightAboveGround'}, 
       'dtype': 'float32',
       'attrs': {'units': 'K', 
                 'standard_name':'air_temperature',
                 'long_name':'Minimum Temperature'}, 
       'plot': {}}


t2x = {'name': 't2x',
       'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'T2'}},
       'gfs': {'name':'tmax',
               'level': 'heightAboveGround'}, 
       'dtype': 'float32',
       'attrs': {'units': 'K', 
                 'standard_name':'air_temperature',
                 'long_name':'Maximum Temperature'}, 
       'plot': {}}


t2xcal = {'name': 't2xcal',
          'wrf': {'function': None, 'args': None},
          'gfs': {'name': None,
                  'level': None},
          'dtype': 'float32',
          'attrs': {'units': 'K', 
                    'standard_name':'air_temperature',
                    'long_name':'Calibrated Maximum Temperature'}, 
          'plot': {}}


t2ncal = {'name': 't2ncal',
          'wrf': {'function': None, 'args': None},
          'gfs': {'name': None,
                  'level': None},
          'dtype': 'float32',
          'attrs': {'units': 'K', 
                    'standard_name':'air_temperature',
                    'long_name':'Calibrated Minimum Temperature'}, 
          'plot': {}}


tsk = {'name': 'tsk',
       'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'TSK'}},
       'gfs': {'name': None, 'level': None},
       'dtype': 'float32',
       'attrs': {'units': 'K',
                 'standard_name': 'surface_temperature',
                 'long_name': 'Skin Temperature'}, 
       'plot': {}}


pblh = {'name': 'pblh',
        'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'PBLH'}},
        'gfs': {'name':'hpbl',
               'level':'surface'},
        'dtype': 'float32',
        'attrs': {'units': 'm',
                 'standard_name': 'atmosphere_boundary_layer_thickness',
                 'long_name': 'Planetary Boundary Layer Heigth'}, 
        'plot': {}}


u10 = {'name': 'u10',
       'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'uvmet10'}},
       'gfs': {'name':'u10',
               'level': 'heightAboveGround'},
       'dtype': 'float32',
       'attrs': {'units': 'm s-1',
                 'standard_name': 'eastward_wind',
                 'long_name': '10-m U Wind Component'}, 
       'plot': {}}


u10cal = {'name': 'u10cal',
          'wrf': {'function': None, 'args': None},
          'gfs': {'name': None,
                  'level': None},
          'dtype': 'float32',
          'attrs': {'units': 'm s-1',
                    'standard_name': 'eastward_wind',
                    'long_name': 'Calibrated 10-m U Wind Component'}, 
          'plot': {}}


v10 = {'name': 'v10',
       'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'uvmet10'}},
       'gfs': {'name':'v10',
               'level': 'heightAboveGround'},
       'dtype': 'float32',
       'attrs': {'units': 'm s-1',
                 'standard_name': 'northward_wind',
                 'long_name': '10-m V Wind Component'}, 
       'plot': {}}


v10cal = {'name': 'v10cal',
          'wrf': {'function': None, 'args': None},
          'gfs': {'name': None,
                  'level': None},
          'dtype': 'float32',
          'attrs': {'units': 'm s-1',
                    'standard_name': 'eastward_wind',
                    'long_name': 'Calibrated 10-m U Wind Component'}, 
          'plot': {}}


wdir10 = {'name': 'wdir10',
          #'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'uvmet10_wspd_wdir'}},  #leyendo con esta funcion de wrf-python la unidad de la direccion en m/s, cuando se 
                                                                                      #arregle descomentamos
          'wrf': {'name': None},
          'gfs': {'name': None,
                  'level': None},
          'dtype': 'float32',
          'attrs': {'units': 'degree',
                    'standard_name':'wind_from_direction',
                    'long_name': 'Wind Direction at 10m'}, 
          'plot': {}}


wspd10 = {'name': 'wspd10',
          #'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'uvmet10_wspd_wdir'}}, #Descomentar cuando se solucione el problema con la direccion del viento
          'wrf': {'name': None},
          'gfs': {'name': None,
                  'level': None},
          'dtype': 'float32',
          'attrs': {'units': 'm/s',
                    'standard_name':'wind_speed',
                    'long_name': 'Wind Speed at 10m'}, 
          'plot': {'name': 'INTENSIDAD DE VIENTO a 10 m',
                   'dim': '2D',
                   'cmap': None,
                   'units': 'kts'}}


wspd10cal = {'name': 'wspd10cal',
             'wrf': {'name': None},
             'gfs': {'name': None,
                     'level': None},
             'dtype': 'float32',
             'attrs': {'units': 'm/s',
                       'standard_name':'wind_speed',
                       'long_name': 'Calibrated Wind Speed at 10m'}, 
             'plot': {'name': 'INTENSIDAD DE VIENTO (cal) a 10 m',
                      'dim': '2D',
                      'cmap': None,
                      'units': 'kts'}}


#####################
# DYNAMIC VARIABLES #
#####################
### 3D ###

cldfra = {'name': 'cldfra',
          'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'CLDFRA'}},
          'gfs': {'name': 'tcc',
                  'level': 'isobaricInhPa'},
          'dtype': 'float32',
          'attrs': {'units': '1',
                    'standard_name': 'cloud_area_fraction_in_atmosphere_layer', 
                    'long_name': 'Cloud Fraction'}, 
          'plot': {}}


dbz = {'name': 'dbz',
       'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'dbz'}},
       'gfs': {'name': None,
               'level': None},
       'dtype': 'float32',
       'attrs': {'units': 'dBZ',
                 'standard_name': 'equivalent_reflectivity_factor',
                 'long_name': 'Reflectivity'}, 
       'plot': {'name': 'REFLECTIVIDAD',
                'dim': '3D',
                'cmap': 'smn_dbz',
                'units': 'dBZ'}}


geopt = {'name': 'geopt',
         'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'geopt'}},
         'gfs': {'name': 'gh',
                 'level': 'isobaricInhPa'},
         'dtype': 'float32',
         'attrs': {'units': 'm2 s-2',
                   'standard_name': 'geopotential_height',
                   'long_name': 'Geopotential Height'}, 
         'plot': {}}


p = {'name': 'p',
     'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'pressure'}},
     'gfs': {'name': None,
             'level': None},
     'dtype': 'float32',
     'attrs': {'units': 'hPa',
               'standard_name': 'air_pressure',
               'long_name': 'Air Pressure'}, 
     'plot': {}}


q = {'name': 'q',
     'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'QVAPOR'}},
     'gfs': {'name': 'q',
             'level': 'isobaricInhPa'},
     'dtype': 'float32',
     'attrs': {'units': 'g kg-1',
               'standard_name': 'specific_humidity',
               'long_name': 'Specific Humidity'}, 
     'plot': {}}


qc = {'name': 'qc',
      'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'QCLOUD'}},
      'gfs': {'name': 'clwmr',
              'level': 'isobaricInhPa'},
      'dtype': 'float32',
      'attrs': {'units': 'g kg-1',
                'standard_name': 'cloud_water_mixing_ratio',
                'long_name': 'Cloud Water Mixing Ratio'}, 
      'plot': {}}


qg = {'name': 'qg',
      'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'QGRAUP'}},
      'gfs': {'name': 'grle',
              'level': 'isobaricInhPa'},
      'dtype': 'float32',
      'attrs': {'units': 'g kg-1',
                'standard_name': 'graupel_mixing_ratio',
                'long_name': 'Graupel Mixing Ratio'}, 
      'plot': {}}


qi = {'name': 'qi',
      'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'QICE'}},
      'gfs': {'name': 'icmr',
              'level': 'isobaricInhPa'},
      'dtype': 'float32',
      'attrs': {'units': 'g kg-1',
                'standard_name': 'ice_mixing_ratio',
                'long_name': 'Ice Mixing Ratio'}, 
      'plot': {}}


qr = {'name': 'qr',
      'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'QRAIN'}},
      'gfs': {'name': 'rwmr',
              'level': 'isobaricInhPa'},
      'dtype': 'float32',
      'attrs': {'units': 'g kg-1',
                'standard_name': 'rain_water_mixing_ratio',
                'long_name': 'Rain Water Mixing Ratio'}, 
      'plot': {}}


qs = {'name': 'qs',
      'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'QSNOW'}},
      'gfs': {'name': 'snmr',
              'level': 'isobaricInhPa'},
      'dtype': 'float32',
      'attrs': {'units': 'g kg-1',
                'standard_name': 'snow_mixing_ratio',
                'long_name': 'Snow Mixing Ratio'}, 
      'plot': {}}


rh = {'name': 'rh',
      'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'rh'}},
      'gfs': {'name': 'r',
              'level': 'isobaricInhPa'},
      'dtype': 'float32',
      'attrs': {'units': '%',
                'standard_name': 'relative_humidity',
                'long_name': 'Relative Humidity'}, 
      'plot': {}}


smois = {'name': 'smois',
         'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'SMOIS'}},
         'gfs': {'name': None,
                 'level': None},
         'name_wrf': 'SMOIS',
         'dtype': 'float32',
         'attrs': {'units': 'm3 m-3',### como wrfout sale con m3/m3 lo pongo asi, pero segun CF deberia ir kg/m2
                   'standard_name': 'mass_content_of_water_in_soil_layer', 
                   'long_name': 'SOIL MOISTURE'}, 
         'plot': {}}


tk = {'name': 'tk',
      'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'tk'}},
      'gfs': {'name': 't',
              'level': 'isobaricInhPa'},
      'dtype': 'float32',
      'attrs': {'units': 'K',
                'standard_name': 'air_temperature',
                'long_name': 'Temperature'}, 
      'plot': {}}


td = {'name': 'td',
      'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'td'}},
      'gfs': {'name': 'td',
              'level': 'isobaricInhPa'},
      'dtype': 'float32',
      'attrs': {'units': 'K',
                'standard_name': 'dew_point_temperature',
                'long_name': 'Dew Point Temperature'}, 
      'plot': {}}


tpe = {'name': 'tpe',
       'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'eth'}},
       'gfs': {'name': None,
             'level': None},
       'dtype': 'float32',
       'attrs': {'units': 'K',
               'standard_name': 'air_equivalent_potential_temperature',
               'long_name': 'Equivalent Potential Temperature'}, 
       'plot': {}}


# Faltaria generar la coordenada vertical de niveles debajo del suelo
tslb = {'name': 'tslb',
        'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'TSLB'}},
        'gfs': {'name': 'st',
                'level': 'depthBelowLandLayer'},
        'dtype': 'float32',
        'attrs': {'units': 'K',
                  'standard_name': 'soil_temperature',
                  'long_name': 'Soil Temperature'}, 
        'plot': {}}


u = {'name': 'u',
     'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'uvmet'}},
     'gfs': {'name': 'u',
             'level': 'isobaricInhPa'},
     'dtype': 'float32',
     'attrs': {'units': 'm s-1',
               'standard_name': 'eastward_wind',
               'long_name': 'Zonal Wind Component'}, 
     'plot': {}}


v = {'name': 'v',
     'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'uvmet'}},
     'gfs': {'name': 'v',
             'level': 'isobaricInhPa'},
     'dtype': 'float32',
     'attrs': {'units': 'm s-1',
               'standard_name': 'northward_wind',
               'long_name': 'Meridional Wind Component'}, 
     'plot': {}}


w = {'name': 'w',
     'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'wa'}},
     'gfs': {'name': 'wz',
             'level': 'isobaricInhPa'},
     'dtype': 'float32',
     'attrs': {'units': 'm s-1', 
               'standard_name': 'upward_air_velocity', 
               'long_name': 'Z Wind Component'}, 
     'plot': {}}


wspd = {'name':'wspd',
        'wrf':{'name': None},
        'gfs':{'name':None,
               'level':None},
        'dtype':'float32',
        'attrs':{'units':'m/s',
                 'standard_name':'wind_speed',
                 'long_name':'Wind Speed'},
        'plot':{'name': 'INTENSIDAD DE VIENTO',
                'dim': '3D',
                'cmap': None,
                'units': 'kts'}}


z = {'name': 'z',
     'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'height'}},
     'gfs': {'name': None,
             'level': None},
     'dtype': 'float32',
     'attrs': {'units': 'm',
               'standard_name': 'height_above_mean_sea_level',
               'long_name': 'Height above mean sea level'}, 
     'plot': {}}


zagl = {'name': 'zagl',
        'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'height_agl'}},
        'gfs': {'name': None,
                'level': None},
        'dtype': 'float32',
        'attrs': {'units': 'm',
                  'standard_name': 'height',
                  'long_name': 'Height above ground level'}, 
        'plot': {}}


####################
# STATIC VARIABLES #
####################

landmask = {'name': 'landmask',
            'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'LANDMASK'}},
            'gfs': {'name':'lsm',
                    'level':'surface'},
            'dtype': 'int32',
            'attrs': {'units': '1',
                      'standard_name': 'land_binary_mask', 
                      'long_name': 'landmask'}, 
           'plot': {}}


landuse = {'name': 'landuse',
           'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'LU_INDEX'}},
           'gfs': {'name': None,
                   'level': None},
           'dtype': 'int32',
           'attrs': {'units': '1',
                     'standard_name': 'land_cover', 
                     'long_name': 'land cover'}, 
           'plot': {}}


hgt = {'name': 'hgt',
       'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'HGT'}},
       'gfs': {'name': 'orog',
               'level': 'surface'},
       'dtype': 'float32',
       'attrs': {'units': 'm',
                 'standard_name': 'height_above_mean_sea_level',
                 'long_name': 'Terrain Height'}, 
       'plot': {}}


etalev = {'name': 'etalev',
          'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'ZNU'}},
          'gfs': {'name': None,
                  'level': None},
          'dtype': 'float32',
          'attrs': {'units': '1',
                    'standard_name': 'atmosphere_sigma_coordinate',
                    'long_name': 'Eta values on half (mass) levels',
                    'positive': 'down',
                    'axis': 'Z'}}


plev = {'name': 'plev',
        'wrf': {'name': None},
        'gfs': {'name': None,
                'level': None},
        'dtype': 'float32',
        'attrs': {'units': 'hPa',
                  'standard_name': 'air_pressure',
                  'long_name': 'Pressure',
                  'positive': 'down',
                  'axis': 'Z'}}


tlev = {'name': 'tlev',
        'wrf': {'name': None},
        'gfs': {'name': None,
                'level': None},
        'dtype': 'float32',
        'attrs': {'units': 'K',
                  'standard_name': 'air_temperature',
                  'long_name': 'Temperature',
                  'positive': 'down',
                  'axis': 'Z'}}


zlev = {'name': 'zlev',
        'wrf': {'name': None},
        'gfs': {'name': None,
                'level': None},
        'dtype': 'float32',
        'attrs': {'units': 'm',
                  'standard_name': 'height',
                  'long_name': 'Height above ground level',
                  'positive': 'up',
                  'axis': 'Z'}}


member = {'name': 'member',
          'wrf': {'name': None},
          'gfs': {'name': 'number',
                  'level': None},
          'dtype': 'int32',
          'attrs': {'units': '1',
                    'standard_name':'realization', 
                    'long_name':'Ensemble member'}}


lead = {'name': 'lead',
        'wrf': {'name': None},
        'gfs': {'name':'step',
                'level': None},
        'dtype': 'int32',
        'attrs': {'standard_name': 'forecast_period',
                  'units': '', #Deberia completarse segun lo que se este haciendo
                  'long_name': 'Plazo de pronostico'}}


lat = {'name': 'lat',
        'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'XLAT'}},
        'gfs': {'name': 'latitude',
                'level': None},
        'dtype': 'float32',
        'attrs': {'units': 'degrees_north',
                  'standard_name': 'latitude',
                  'long_name': 'Latitude'}}


lon = {'name': 'lon',
         'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'XLONG'}},
         'gfs': {'name': 'longitude',
                 'level': None},
         'dtype': 'float32',
         'attrs': {'units': 'degrees_east',
                   'standard_name': 'longitude',
                   'long_name': 'Longitude'}}


x = {'name': 'x',
     'wrf': {'name': None},
     'gfs': {'name': None,
             'level': None},
     'dtype': 'float32',
     'attrs': {'units': 'm',
               'standard_name': 'projection_x_coordinate',
               'long_name': 'x-coordinate in projected coordinate system',
               'axis': 'X'}}


y = {'name': 'y',
     'wrf': {'name': None},
     'gfs': {'name': None,
             'level': None},
     'dtype': 'float32',
     'attrs': {'units': 'm',
               'standard_name': 'projection_y_coordinate',
               'long_name': 'y-coordinate in projected coordinate system',
               'axis': 'Y'}}


cipi = {'name': 'cipi',
        'wrf': {'name': None}, 
        'gfs': {'name': None,
               'level': None},
        'dtype': 'str',
        'attrs': {'units': '1',
                  'standard_name': 'platform_id', 
                  'long_name': 'Codigo de Identificacion de Punto de Interes'}}

##################
# Time variables #
##################

#El atributo units va fuera del diccionario de atributos porque lo paso en el encoding

time = {'name': 'time',
        'wrf': {'function': 'wrf.getvar', 'args': {'varname': 'XTIME'}},
        'gfs': {'name': 'valid_time',
                'level': None},
        'dtype': 'int32',
        'units': 'seconds since',
        'attrs': {'standard_name': 'time',
                  'long_name': 'seconds since',
                  'axis': 'T'}}


date = {'name': 'date',
        'wrf': {'name': None},
        'gfs': {'name': 'time',
                'level': None},
         'dtype': 'float64',
         'units': 'days since',
         'attrs': {'standard_name': 'forecast_reference_time', 
                   'long_name': 'Forecast initialization time',
                   'axis': 'T'}}

