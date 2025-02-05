"""
Conversores (:mod: radarsmn.conversores)
==========================================

Repositorio de algoritmos que convierten entre formatos. Preferentemente
convierten a CfRadial usando pyart.core.write_cfradial.

Submodulos
==========
    RMA

Funciones
=========
    hdf5_to_cfrad

"""

from . import RMA

from .hdf5_to_cfrad_EZE import hdf5_to_cfrad
