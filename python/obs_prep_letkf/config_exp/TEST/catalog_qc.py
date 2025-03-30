#############
# QC ADPAUT #
#############

ADPAUT = {'N_DAYS_QC': 30, # Cantidad de dias que se tienen en cuenta en el calculo del QC
          'ALFA_RECHAZO': 0.995, # Rechazamos si se encuentra en el 1% mas extremo de la distribucion (0.005% en cada cola)
          'NEIGHBOR_DIST_H': 200, # Distancia horizontal para la que se buscan estaciones en el entorno [Km]
          'NEIGHBOR_DIST_V': 500, # Distancia vertical para la que se buscan estaciones en el entorno [m]
          'N_NEIGHBORS': 20, # Cantidad maxima de estaciones en el entorno que se toman en cuenta en el calculo del QC
          'MIN_RELIABLE_NEIGHBORS': 3, # Cantidad minima de estaciones en el entorno que se requieren para calcular el QC
          'MIN_DIFFERENT_OBS': 10, # Minima cantidad de observaciones diferentes que se requieren para decir que la estacion no reporta siempre los mismos valores
          'GROSS_LIMITS': {'t2': [-30 + 273.15, 60 + 273.15], 'rh2': [0, 100], # Valores que se toman para definir si una observacion se encuentra fuera de rango
                           'psfc': [500, 1100], 'wspd10': [0 , 100]},
          'CORR_THRESHOLD': {'t2':0.7, 'rh2':0.7, # Minimo valor de correlacion con el entorno para no rechazar una estacion
                             'psfc':0.7, 'wspd10':0.1}, 
          'QC_SOURCES': ['ADPAUT'], # Fuentes de datos a la que se le quiere hacer el QC
          'QC_SUPPORT_SOURCES': ['ADPSFC', 'AWOS'] # Fuentes de datos que se quieren usar para el QC pero no aplicarselo. Se supone que son datos confiables
         }   

