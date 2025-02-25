# -*- coding: utf-8 -*-
# Observations sources
MONIT_VARS = ['u', 'u10', 'v', 'v10', 't', 't2', 'psfc', 'rh2', 'rh', 'q', 'dbz']
GENERIC_ID = -999.9

ADPAUT = {'NAME': 'ADPAUT',
          'DESC': 'Est. Automaticas',
          'VARS': ['u10', 'v10', 't2', 'psfc', 'rh2'],
          'ID_OBS': 22,
          'ID_VAR': {'u10': 82819, 'v10': 82820, 't2': 83073, 'psfc': 14593, 'rh2': 83331},
          'ERRORS': {'u10': 1.4,  'v10': 1.4, 't2': 2., 'psfc': 1., 'rh2': 10.}, #m/s, K, hPa, %
          'SLOTS': [1,2,3,4,5,6],
          'SO': {},
          'WALLTIME': 30, # s
          'OTHER': {'QC_THRESHOLD': {'t2': [-1,0,1,2], 'rh2': [-1,0],
                                     'psfc': [-1,0,4], 'wspd10': [-1,0]}},
          'PLOT': {'marker': '*', 'color': 'limegreen'},
         }

ADPSFC = {'NAME': 'ADPSFC',
          'DESC': 'Est. Superficie',
          'VARS': ['u10', 'v10', 't2', 'psfc', 'rh2'],
          'ID_OBS': 8,
          'ID_VAR': {'u10': 82819, 'v10': 82820, 't2': 83073, 'psfc': 14593, 'rh2': 83331},
          'ERRORS': {'u10': 1.4,  'v10': 1.4, 't2': 2., 'psfc': 1., 'rh2': 10.}, #m/s, K, hPa, %
          'SLOTS': [6],
          'SO': {},
          'WALLTIME': 10, # s
          'OTHER': {},
          'PLOT': {'marker': 'D', 'color': 'blueviolet'},
         }

ADPUPA = {'NAME': 'ADPUPA',
          'DESC': 'Sondeos',
          'VARS': ['u', 'v', 't', 'rh'],
          'ID_OBS': 1,
          'ID_VAR': {'u': 2819, 'v': 2820, 't': 3073, 'rh': 3331},
          'ERRORS': {'u': {10: 2.7, 20: 2.7, 30: 2.7, 40: 2.1, 50: 2.7, 100: 2.7, 150: 3.0, 200: 3.3, 250: 3.3, 300: 3.3, 350: 3.0, 400: 2.8, 450: 2.5, 500: 2.3, 550: 2.0, 600: 1.8, 650: 1.6, 700: 1.4, 750: 1.3, 800: 1.1, 850: 1.1, 900: 1.1, 950: 1.1, 1000: 1.1}, 'v': {10: 2.7, 20: 2.7, 30: 2.7, 40: 2.1, 50: 2.7, 100: 2.7, 150: 3.0, 200: 3.3, 250: 3.3, 300: 3.3, 350: 3.0, 400: 2.8, 450: 2.5, 500: 2.3, 550: 2.0, 600: 1.8, 650: 1.6, 700: 1.4, 750: 1.3, 800: 1.1, 850: 1.1, 900: 1.1, 950: 1.1, 1000: 1.1}, 't': 1., 'rh': {1000: 15., 850: 10.}}, #m/s, K, %
          'SLOTS': [6],
          'SO': {},
          'WALLTIME': 10, # s
          'OTHER': {},
          'PLOT': {'marker': 'o', 'color': 'navy'},
         }

AIRCFT = {'NAME': 'AIRCFT',
          'DESC': 'Aviones',
          'VARS': ['u', 'v', 't'],
          'ID_OBS': 3,
          'ID_VAR': {'u': 2819, 'v': 2820, 't': 3073},
          'ERRORS': {'u': 3.6,  'v': 3.6,  't': 1}, #m/s, K
          'SLOTS': [1,2,3,4,5,6],
          'SO': {'dx': 30, 'dz': 25}, # km, hPa
          'WALLTIME': 120, # s
          'OTHER': {},
          'PLOT': {'marker': '<', 'color': 'red'},
         }

AIRSRT = {'NAME': 'AIRSRT',
          'DESC': 'Satelite Polar Aqua',
          'VARS': ['t', 'q'],
          'ID_OBS': 21,
          'ID_VAR': {'t': 3073, 'q': 3330},
          'ERRORS': {'t': 'dyn',  'q': 'dyn'},
          'SLOTS': [1,2,3,4,5,6],
          'SO': {}, # km, hPa
          'WALLTIME': 60, # s
          'OTHER': {'max_lev': 200}, # hPa
          'PLOT': {'marker': 'x', 'color': 'plum'},
         }

ASCATW = {'NAME': 'ASCATW',
          'DESC': 'ASCAT Surface Winds',
          'VARS': ['u10', 'v10'],
          'ID_OBS': 20,
          'ID_VAR': {'u10': 82819, 'v10': 82820},
          'ERRORS': {'u10': 2.,  'v10': 2.}, #m/s
          'SLOTS': [1,2,3,4,5,6],
          'SO': {}, # km, hPa
          'WALLTIME': 60, # s
          'OTHER': {},
          'PLOT': {'marker': '4', 'color': 'sandybrown'},
         }

AWOS = {'NAME': 'AWOS',
        'DESC': 'Sistema Automático de Observación Meteorológica',
        'VARS': ['u10', 'v10', 't2', 'psfc', 'rh2'],
        'ID_OBS': 23, 
        'ID_VAR': {'u10': 82819, 'v10': 82820, 't2': 83073, 'psfc': 14593, 'rh2': 83331},
        'ERRORS': {'u10': 1.4,  'v10': 1.4, 't2': 2., 'psfc': 1., 'rh2': 10.}, #m/s, K, hPa, %
        'SLOTS': [1,2,3,4,5,6],
        'SO': {},
        'WALLTIME': 70, # s
        'OTHER': {},
        }

GEODMW = {'NAME': 'GEODMW',
          'DESC': 'GOES-East-DMW',
          'VARS': ['u', 'v'],
          'ID_OBS': 4,
          'ID_VAR': {'u': 2819, 'v': 2820},
          'ERRORS': {'u': 7.5,  'v': 7.5}, #m/s
          'SLOTS': [6],
          'SO': {'dx': 30, 'dz': 25}, # km, hPa
          'WALLTIME': 60, # s
          'OTHER': {},
          'PLOT': {'marker': '+', 'color': 'mediumseagreen'},
         }

GEOSND = {'NAME': 'GEOSND',
          'DESC': 'GOES-East-SND',
          'VARS': ['t', 'rh'],
          'ID_OBS': 26,
          'ID_VAR': {'t': 3073, 'rh': 3331},
          'ERRORS': {'t': 1.9,  'rh': {950: 12, 750: 18, 450: 15}},
          'SLOTS': [1, 2, 3, 4, 5, 6],
          'SO': {'dx': 16, 'dz': 25}, # km, hPa
          'WALLTIME': 90, # s
          'OTHER': {'max_lev': 200},
          'PLOT': {'marker': 'x', 'color': 'plum'}
         }

RADARC = {'NAME': 'RADARC',
          'DESC': 'Radar banda C',
          'VARS': ['dbz'], #, 'Vr'],
          'ID_OBS': 12,
          'ID_VAR': {'dbz': 4001}, #, 'Vr': 4002},
          'ERRORS': {'dbz': 5}, #, 'Vr': 2},
          'SLOTS': [6],
          'SO': {'dx': 6, 'dz': 1}, # km, km
          'WALLTIME': 60, # s
          'OTHER': {'min_nobs': 8}, # number (10 for dx > 2; 2 for dx <=2)
          'PLOT': {'marker': '.', 'color': 'cornflowerblue'},
         }

SFCBUY = {'NAME': 'SFCBUY',
          'DESC': 'Boyas',
          'VARS': ['u10', 'v10', 't2', 'psfc', 'rh2'],
          'ID_OBS': 9,
          'ID_VAR': {'u10': 82819, 'v10': 82820, 't2': 83073, 'psfc': 14593, 'rh2': 83331},
          'ERRORS': {'u10': 1.4,  'v10': 1.4, 't2': 2., 'psfc': 1.6, 'rh2': 10.}, #m/s, K, hPa, %
          'SLOTS': [6],
          'SO': {}, 
          'WALLTIME': 10, # s
          'OTHER': {},
          'PLOT': {'marker': 's', 'color': 'orangered'},
         }

SFCSHP = {'NAME': 'SFCSHP',
          'DESC': 'Barcos',
          'VARS': ['u10', 'v10', 't2', 'psfc', 'rh2'],
          'ID_OBS': 9,
          'ID_VAR': {'u10': 82819, 'v10': 82820, 't2': 83073, 'psfc': 14593, 'rh2': 83331},
          'ERRORS': {'u10': 1.4,  'v10': 1.4, 't2': 2., 'psfc': 1.6, 'rh2': 10.}, #m/s, K, hPa, %
          'SLOTS': [6],
          'SO': {},
          'WALLTIME': 10, # s
          'OTHER': {},
          'PLOT': {'marker': 's', 'color': 'orangered'},
         }
