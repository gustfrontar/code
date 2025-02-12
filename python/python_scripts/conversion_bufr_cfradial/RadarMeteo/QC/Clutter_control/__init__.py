"""
Clutter_control (:mod: radarsmn.QC.Clutter_control)
==========================================

Eliminacion de ecos no meteorologicos en polarizacion doble y simple.

Submodulos
==========
    SimPol

Funciones
=========
    class_meteo

"""
from . import SimPol

from .funAux import info_radar
from .funAux import predice_lda
from .funAux import predice_sin_nan_lda
from .funAux import lda_mod

from .funAux import predice_qda
from .funAux import predice_sin_na_qda
from .funAux import qda_mod

from .funAux import post_proceso

from .funAux import grafico_PPI

from .funrad import sd_zdr
from .funrad import s_rohv
from .funrad import vgz
from .funrad import sd_zh
from .funrad import echo_top
from .funrad import sd_var

from .clasificador import class_meteo
