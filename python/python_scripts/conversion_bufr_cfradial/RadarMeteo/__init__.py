'''
radarsmn: La libreria de RadarMeteo
===================================

Repositorio para algoritmos y productos basados en datos de radares
meteorológicos de simple y doble polarización en Argentina.

Modulos
=======
    conversores
        RMA

    disdrometro

    QC
        Clutter_control
            SimPol
'''

# Version
from .version import version as __version__

# Subpaquetes
from . import conversores
from . import disdrometro
from . import QC
