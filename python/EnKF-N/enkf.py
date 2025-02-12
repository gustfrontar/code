#!/usr/bin/env python
# -*- coding: iso-8859-15 -*-h


#################################################################
#
#      (c) Marc Bocquet
#      Programme enkf.py
#      Cleansed release version for Alex Ayet and Alexis Hannart
#      Dernière modification: 03/04/2016
#
##################################################################

import sys

import numpy as np
import random, time
import matplotlib.pyplot as plt
import l95 as model
from enkf_call import enkf_call



######################
#  Global Variables  #
######################

# Length of the data assimilation run
Nt = 5000

# which includes some burnin
Nts = 2000

# Forecast step
Dt = 0.05

# Ensemble size
N = 20

# Observation error standard deviation
sig_obs = 1.

# Inflation factor
infl = 1.00

# Set model parameters lorenz95
F = model.F
M = model.M
dt = model.dt
print("> set model parameters", F, M, dt)




###########################
#  Data assimilation run  #
###########################

print("> run")
statistics, infa_mean, infa_median, spread_mean, spread_median, prtime = enkf_call(model, Dt, Nt, Nts, N, sig_obs, infl)

# wctime, prtime

print("> prtime(s)", prtime)
model.ShowStat(statistics)
print("> infa_mean", infa_mean, " infa_median", infa_median)
print("> spread_mean", spread_mean, " spread_median", spread_median)
