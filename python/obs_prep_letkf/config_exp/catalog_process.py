import os

OBSPROC = int(os.environ['OBSPROC'])

monit_vars = ['u', 'u10', 'v', 'v10', 't', 't2', 'psfc', 'rh2', 'rh', 'q', 'dbz','Vr']
generic_id = -999.9

adpaut = {'id_obs': 22,
          'id_var': {'u10': 82819, 'v10': 82820, 't2': 83073, 'psfc': 14593, 'rh2': 83331},
          'errors': {'u10': 1.4,  'v10': 1.4, 't2': 2., 'psfc': 1., 'rh2': 10.}, #m/s, K, hPa, %
          'slots': [1, 2, 3, 4, 5, 6],
          'so/th': {},
          'constraints': {'qc_levels': {'t2': [-1, 0, 1, 2], 'rh2': [-1, 0],
                                        'psfc': [-1, 0, 4], 'wspd10': [-1, 0]}},
          'procs': OBSPROC, 
          'name': 'ADPAUT',
          'desc': 'Est. Automaticas',
          'vars': ['u10', 'v10', 't2', 'psfc', 'rh2']
         }

adpsfc = {'id_obs': 8,
          'id_var': {'u10': 82819, 'v10': 82820, 't2': 83073, 'psfc': 14593, 'rh2': 83331},
          'errors': {'u10': 1.4,  'v10': 1.4, 't2': 2., 'psfc': 1., 'rh2': 10.}, #m/s, K, hPa, %
          'slots': [6],
          'so/th': {},
          'constraints': {},
          'procs': OBSPROC,
          'name': 'ADPSFC',
          'desc': 'Est. Superficie',
          'vars': ['u10', 'v10', 't2', 'psfc', 'rh2']
         }

adpupa = {'id_obs': 1,
          'id_var': {'u': 2819, 'v': 2820, 't': 3073, 'rh': 3331},
          'errors': {'u': {10: 2.7, 20: 2.7, 30: 2.7, 40: 2.1, 50: 2.7, 100: 2.7, 150: 3.0, 200: 3.3, 250: 3.3, 300: 3.3, 350: 3.0, 400: 2.8, 450: 2.5, 500: 2.3, 550: 2.0, 600: 1.8, 650: 1.6, 700: 1.4, 750: 1.3, 800: 1.1, 850: 1.1, 900: 1.1, 950: 1.1, 1000: 1.1}, 'v': {10: 2.7, 20: 2.7, 30: 2.7, 40: 2.1, 50: 2.7, 100: 2.7, 150: 3.0, 200: 3.3, 250: 3.3, 300: 3.3, 350: 3.0, 400: 2.8, 450: 2.5, 500: 2.3, 550: 2.0, 600: 1.8, 650: 1.6, 700: 1.4, 750: 1.3, 800: 1.1, 850: 1.1, 900: 1.1, 950: 1.1, 1000: 1.1}, 't': 1., 'rh': {1000: 15., 850: 10.}}, #m/s, K, %
          'slots': [6],
          'so/th': {},
          'constraints': {},
          'procs': OBSPROC,
          'name': 'ADPUPA',
          'desc': 'Sondeos',
          'vars': ['u', 'v', 't', 'rh']
         }

aircft = {'id_obs': 3,
          'id_var': {'u': 2819, 'v': 2820, 't': 3073},
          'errors': {'u': 3.6,  'v': 3.6,  't': 1}, #m/s, K
          'slots': [1, 2, 3, 4, 5, 6],
          'so/th': {'method': 'so', 'dx': 30, 'dz': 25}, # km, hPa
          'constraints': {},
          'procs': OBSPROC,
          'name': 'AIRCFT',
          'desc': 'Aviones',
          'vars': ['u', 'v', 't']
         }

airsrt = {'id_obs': 21,
          'id_var': {'t': 3073, 'q': 3330},
          'errors': {'t': 'dyn',  'q': 'dyn'},
          'slots': [1, 2, 3, 4, 5, 6],
          'so/th': {}, # km, hPa
          'constraints': {'max_lev': 200}, # hPa
          'procs': OBSPROC,
          'name': 'AIRSRT',
          'desc': 'Satelite Polar Aqua',
          'vars': ['t', 'q']
         }

ascatw = {'id_obs': 20,
          'id_var': {'u10': 82819, 'v10': 82820},
          'errors': {'u10': 2.,  'v10': 2.}, #m/s
          'slots': [1, 2, 3, 4, 5, 6],
          'so/th': {}, # km, hPa
          'constraints': {},
          'procs': OBSPROC,
          'name': 'ASCATW',
          'desc': 'ASCAT Surface Winds',
          'vars': ['u10', 'v10'],
         }

awos = {'id_obs': 23, 
        'id_var': {'u10': 82819, 'v10': 82820, 't2': 83073, 'psfc': 14593, 'rh2': 83331},
        'errors': {'u10': 1.4,  'v10': 1.4, 't2': 2., 'psfc': 1., 'rh2': 10.}, #m/s, K, hPa, %
        'slots': [1, 2, 3, 4, 5, 6],
        'so/th': {},
        'constraints': {},
        'procs': OBSPROC,
        'name': 'AWOS',
        'desc': 'Sistema Automático de Observación Meteorológica',
        'vars': ['u10', 'v10', 't2', 'psfc', 'rh2'],
        }

geodmw = {'id_obs': 4,
          'id_var': {'u': 2819, 'v': 2820},
          'errors': {'u': 7.5,  'v': 7.5}, #m/s
          'slots': [6],
          'so/th': {'method': 'so', 'dx': 30, 'dz': 25}, # km, hPa
          'constraints': {},
          'procs': OBSPROC,
          'name': 'GEODMW',
          'desc': 'GOES-East-DMW',
          'vars': ['u', 'v'],
         }

geosnd = {'id_obs': 26,
          'id_var': {'t': 3073, 'rh': 3331},
          'errors': {'t': 1.9,  'rh': {950: 12, 750: 18, 450: 15}},
          'slots': [1, 2, 3, 4, 5, 6],
          'so/th': {'method': 'th', 'dx': 100, 'dz': 50}, # km, hPa
          'constraints': {'min_lev': 900, 'max_lev': 200},
          'procs': 6, # Not using OBSPROC to reduce memory usage. 6 is half of total files to process
          'name': 'GEOSND',
          'desc': 'GOES-East-SND',
          'vars': ['t', 'rh'],
         }

nucaps = {'id_obs': 24,
          'id_var': {'t': 3073, 'q': 3330},
          'errors':  {'t': {650: 1.16, 165: 0.82, 15: 1.05},  'q': {800: 1.23e-3, 450: 0.3e-3, 200: 0.02e-3}}, #Nalli et al. 201
          'slots': [1, 2, 3, 4, 5, 6],
          'so/th': {'method': 'so', 'dx': 5, 'dz': 25}, # km, hPa
          'constraints': {'max_lev': 200},
          'procs': OBSPROC,
          'name': 'NUCAPS',
          'desc': 'NUCAPS',
          'vars': ['t', 'q'],
         }

radarc = {'id_obs': 12,
          'id_var': {'dbz': 4001,'Vr': 4002},
          'errors': {'dbz': 5,'Vr': 2},
          'slots': [6],
          'so/th': {'method': 'so', 'dx': 6, 'dz': 1}, # km, km
          'constraints': {'min_nobs': 8 , 'min_nyquist_velocity' : 10.0 , 'maximum_so_var' : {'dbz' : 20.0 , 'Vr' : 5.0 }, 'valid_range' : {'dbz' : [-30.0,70] , 'Vr' : [-90,90] }}, # number (10 for dx > 2; 2 for dx <=2) #minimum nyquist velocity required to process doppler data. #Maximum allowed intra grid variance after supperogin.
          'procs': OBSPROC,
          'name': 'RADARC',
          'desc': 'Radar banda C',
          'vars': ['dbz','Vr']
         }

sfcbuy = {'id_obs': 9,
          'id_var': {'u10': 82819, 'v10': 82820, 't2': 83073, 'psfc': 14593, 'rh2': 83331},
          'errors': {'u10': 1.4,  'v10': 1.4, 't2': 2., 'psfc': 1.6, 'rh2': 10.}, #m/s, K, hPa, %
          'slots': [6],
          'so/th': {}, 
          'constraints': {},
          'procs': OBSPROC,
          'name': 'SFCBUY',
          'desc': 'Boyas',
          'vars': ['u10', 'v10', 't2', 'psfc', 'rh2'],
         }

sfcshp = {'id_obs': 9,
          'id_var': {'u10': 82819, 'v10': 82820, 't2': 83073, 'psfc': 14593, 'rh2': 83331},
          'errors': {'u10': 1.4,  'v10': 1.4, 't2': 2., 'psfc': 1.6, 'rh2': 10.}, #m/s, K, hPa, %
          'slots': [6],
          'so/th': {},
          'constraints': {},
          'procs': OBSPROC ,
          'name': 'SFCBUY',
          'desc': 'Boyas',
          'vars': ['u10', 'v10', 't2', 'psfc', 'rh2'],
         }
