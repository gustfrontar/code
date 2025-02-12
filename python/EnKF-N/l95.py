#!/usr/bin/env python
# -*- coding: iso-8859-15 -*-h

#################################################################
#
#      (c) Marc Bocquet
#      Programme l95.py
#      Cleansed release version for Alex Ayet and Alexis Hannart
#      Dernière modification: 03/04/2016
#
##################################################################



import numpy as np
import random

from numba import double
from numba.decorators import jit, autojit

# Lorenz 95 constants
F = 8.

# State space size
M = 40

####################
# Evolution Model  #
####################

# Recommended RK4 integration step
dt = 0.05

def _f(x):
    x_dot = np.zeros([M]);
    for m in range(M):
        x_dot[m] = (x[(m+1)%M]-x[(m-2+M)%M])*x[(m-1+M)%M]-x[m]+F;
    return x_dot

f = autojit(_f)

def scheme(X, Dt):

    Nin = int(round(Dt/dt))

    # 4th order Runge-Kutta

    for iter in range(Nin):
    
        k1 = f(X)*dt
        k2 = f(X + k1/2.)*dt
        k3 = f(X + k2/2.)*dt
        k4 = f(X + k3)*dt
        X += (k1+2.*k2+2.*k3+k4)/6.


def _df(dx, x):
    x_dot = np.zeros([M])
    for m in range(M):
        x_dot[m] = (x[(m+1)%M]-x[(m-2+M)%M])*dx[(m-1+M)%M]-dx[m]+(dx[(m+1)%M]-dx[(m-2+M)%M])*x[(m-1+M)%M];
    return x_dot

df = autojit(_df)

def schemeTL(dx, Dt, x):

    Nin = int(round(Dt/dt))

    # 4th order Runge-Kutta
    for iter in range(Nin):
    
        k1 = df(dx, x)*dt
        k2 = df(dx + k1/2., x)*dt
        k3 = df(dx + k2/2., x)*dt
        k4 = df(dx + k3, x)*dt
        dx += (k1+2.*k2+2.*k3+k4)/6.



####################
# Initialisation   #
####################

def NewState():
    x = 1. + np.random.normal(0, 1., M)
    return x

def NewEnsemble(N, x):
    E = np.zeros([N,M])
    for n in range(N):
        E[n] = x + np.random.normal(0, 1., M)
    return E


######################
# Observation Model  #
######################

# Observation space size
D = 40

def Obs(x):
    y = x
    return y

#####################
# Error statistics  #
#####################


def ComputeStat(dx):
    stat = []
    for e in dx:
        stat.append(np.sqrt(np.mean(e**2)))
    rmse_mean = np.mean(stat)
    rmse_median = np.median(stat)
    return (rmse_mean, rmse_median)

def ShowStat(statistics):
    print("> rmse_mean", statistics[0], "rmse_median", statistics[1])


###############################
# Integration scheme wrappers #
###############################

def ForecastState(x, Dt):
    scheme(x, Dt) 

def ForecastEnsemble(X, Dt):
    # mscheme(X, Dt, X.shape[0])
    for n in range(X.shape[0]):
        scheme(X[n], Dt)
