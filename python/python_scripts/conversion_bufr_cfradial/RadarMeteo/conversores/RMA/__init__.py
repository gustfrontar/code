"""
RMA (:mod: radarsmn.conversores.RMA)

Conversion desde bufr a hdf5 y CfRadial. En el repositorio existe una carpeta
bbufr con las tablas y ejecutables necesarios para convertir de bufr a hdf5.

Funciones
=========

    bufr2cfradial
    genero_nc_RMA

"""

from .genero_nc_RMA import genero_nc_RMA
from .bufr2cfradial import bufr2cfradial
