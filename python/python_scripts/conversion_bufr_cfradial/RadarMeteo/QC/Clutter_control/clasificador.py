"""
Created on Fri Apr  8 08:54:20 2016

@author: sofia
"""
import numpy as np
import pyart
import matplotlib.pyplot as plt
import matplotlib as mpl
import netCDF4
import copy
import scipy.ndimage as nd
import numpy.ma as ma
import sys
import os
import tempfile
from . import funrad
from . import funAux
# from .funAux import *
# from szdr import sd_zdr
# from .funrad import *

# ########################## Description #####################################

# INPUT
# route: route of the .nc file
# sweep: radar's sweep
# names:
# model: lda for LDA model, qda for QDA model, lg for Logit model.
# graf: TRUE for PPI plot. DEFAULT: True


def class_meteo(route, sweep, names, model='lda', graph=False):

    # ########################## read the file ###############################

    Radar = pyart.io.read(route)
    radar = Radar.extract_sweeps([sweep])

    # ###################  deviation of Zdr and Rho smoothing #################

    nro = funrad.s_rohv(radar, names[2])
    radar.add_field_like(names[2], 'nRho', nro)
    Szdr = funrad.sd_zdr(radar, 3)  # ventana de 3x3
    # Szdr=funrad.sd_var(radar,names[3],3)
    radar.add_field_like(names[3], 'Szdr', Szdr)

    # ###################### export radar information  ####################
    p = funAux.info_radar(radar, sweep, names)

    if model == 'qda':
        a = funAux.qda_mod(p)

    # if model=='logit':
    #    os.system('Rscript logitMod.R '+p)

    else:
        a = funAux.lda_mod(p)

    b = funAux.post_proceso(route, sweep, a, p, names)

    if graph:
        funAux.grafico_PPI(route, b, sweep, names)

    return(b)
