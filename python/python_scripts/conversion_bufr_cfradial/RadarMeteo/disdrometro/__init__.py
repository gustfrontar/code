"""
Disdrometro (:mod: radarsmn.disdrometro)

Repositorio de lectores particulares para aplicaciones de disdrometro. Por
separado esta la posibilidad de importar el lector de radar y el de
disdrometro.

Clases y funciones
==================

    leo_parsivel
    ParsivelReader

    grafico

    leo_radar
    RadarReaderForDSD

"""

from .disdro_parsivel import leo_parsivel
from .disdro_parsivel import ParsivelReader

from .grafico_disdro import grafico

from .lector_radar import leo_radar
from .lector_radar import RadarReaderForDSD
from .lector_radar import gate_mas_cercano
