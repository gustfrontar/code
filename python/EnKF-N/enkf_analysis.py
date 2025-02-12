#!/usr/bin/env python
# -*- coding: iso-8859-15 -*-h

#################################################################
#
#      (c) Marc Bocquet
#      Programme enkf_analysis.py
#      Cleansed release version for Alex Ayet and Alexis Hannart
#      Dernière modification: 03/04/2016
#
##################################################################


import numpy as np
from numpy import zeros, dot, identity, newaxis, diag, eye, sqrt
from math import log
import random
from scipy import optimize
import scipy.linalg

eps = 1.e-8


#################################################
### Analysis 1                                ###
### Analysis of the deterministic EnKF (ETKF) ###
#################################################

def Analysis1(model, y, E, sig_obs, xa):

    # Get model dimension
    M = model.M
    D = model.D

    # Determine size of the ensemble
    N = E.shape[0]

    # Compute the mean of the ensemble
    xm = E.sum(axis=0)/N

    # Compute the innovation, normalised by the error
    delta = (y - model.Obs(xm))/sig_obs

    # Compute the state anomalies
    X = E - xm

    # Compute the observation anomalies normalised by the error
    Y = model.Obs(X)/sig_obs
    
    # Compute the estimate in ensemble space
    
    # SVD of the normalised observation anomalies
    K = min(N,D)
    U, s, V = np.linalg.svd(Y, full_matrices=False)

    # Update of the state vector in ensemble space
    _s = s/(s*s+N-1)
    S = diag(_s)
    w = dot(U,dot(S,dot(V,delta)))
    xa[:M] = xm + dot(w,X)

    # Compute the posterior ensemble
    _s = 1./sqrt(1.+s*s/(N-1))
    S = diag(_s)
    T = dot(U,dot(S,U.T))
    Xa = dot(T,X)
    E[:N] = Xa + xa

    return 1.


#####################################
### Analysis 2                    ###
### Primal analysis of the EnKF-N ###
#####################################

def F(w, delta, Y, sig_obs, e, Ne):

    d = (delta - dot(w,Y))/sig_obs
    s = Ne*log(e+dot(w,w))

    return 0.5*(dot(d,d)+s)
    

def DF(w, delta, Y, sig_obs, e, Ne):

    d = (delta - dot(w,Y))/sig_obs
    zeta = Ne/(e+dot(w,w))

    return zeta*w - dot(d,Y.T)


def Analysis2(model, y, E, sig_obs, xa):

    # Get model dimension
    M = model.M
    D = model.D

    # Determine size of the ensemble
    N = E.shape[0]

    # Effective N
    Ne = N+1.
    # Ne = (N+1)*(N-1)/N

    # Parameter of the finite-size formilism
    e = 1.+1./N
    # e = N/(N-1.)

    # Compute the mean of the ensemble
    xm = E.sum(axis=0)/N

    # Compute the innovation, normalised by the error
    delta = (y - model.Obs(xm))/sig_obs

    # Compute the state anomalies
    X = E - xm

    # Compute the observation anomalies normalised by the error
    Y = model.Obs(X)/sig_obs    

    # Initial condition
    w0 = zeros(N)

    # L-BFGS-B (Nocedal variant)
    wa, Fmin, info = optimize.fmin_l_bfgs_b(F, w0, DF, args=(delta, Y, sig_obs, e, Ne), approx_grad=0, bounds=None, m=10,
                                           factr=1.e7, pgtol=1e-05, epsilon=1e-08,
                                           iprint=0, maxfun=15000, maxiter=15000, disp=None, callback=None)
    
    # Update of the state vector in ensemble space
    xa[:M] = xm + dot(wa,X)

    # Compute the posterior ensemble
    zeta = Ne/(e+dot(wa,wa))
    T = dot(Y,Y.T)+zeta*(np.identity(N)-2*zeta/Ne*dot(wa[newaxis].T,wa[newaxis]))
    # T = dot(Y,Y.T)+zeta*np.identity(N)
    U, s, V = np.linalg.svd(T, full_matrices=False)
    T = dot(U,dot(diag(sqrt((N-1.)/s)),U.T))
    E[:N] = dot(T,X) + xa

    return zeta


###################################
### Analysis 3                  ###
### Dual analysis of the EnKF-N ###
###################################

def J(zeta, delta, s, N, e, K):
    sum = e*zeta+(N+1.)*log(N/zeta)-N
    for m in range(K):
        sum += pow(delta[m],2)/(1.+pow(s[m],2)/zeta)
    return sum


def Analysis3(model, y, E, sig_obs, xa):

    # Get model dimension
    M = model.M
    D = model.D

    # Determine size of the ensemble
    N = E.shape[0]

    # Effective N
    # Ne = N+1.

    # Parameter of the finite-size formalism
    e = 1.+1./N
    # e = N/(N-1.)
    # e = (N+1.)/(N-1.)
    
    # Compute the mean of the ensemble
    xm = E.sum(axis=0)/N

    # Compute the innovation, normalised by the error
    delta = (y - model.Obs(xm))/sig_obs

    # Compute the state anomalies
    X = E - xm

    # Compute the observation anomalies normalised by the error
    Y = model.Obs(X)/sig_obs    


    # Compute the estimate in ensemble space
    
    # SVD of the normalised observation anomalies
    K = min(N,D)
    U, s, V = np.linalg.svd(Y, full_matrices=False)

    # Compute the normalised innovation vector in the mode space
    delta = dot(V,delta)

    # Compute the optimal zeta
    # _t = optimize.fminbound(J, -10., 10., (delta, s, Ne, e, K), 1e-3)
    # zeta = exp(_t)
    # zeta = optimize.fminbound(J, eps, N, (delta, s, N, e, K), 1e-5)
    zeta = optimize.fminbound(J, eps, 0.995*(N-1), (delta, s, N, e, K), 1e-8)


    # Update of the state vector in ensemble space
    _s = s/(s*s+zeta)
    #_s = s/(s*s+N-1.)
    S = diag(_s)
    wa = dot(U,dot(S,delta))
    xa[:M] = xm + dot(wa,X)

    # Compute the posterior ensemble
    T = dot(Y,Y.T)+zeta*(np.identity(N)-2*zeta/(N+1.)*dot(wa[newaxis].T,wa[newaxis]))
    # T = dot(Y,Y.T)+zeta*np.identity(N)
    U, s, V = np.linalg.svd(T, full_matrices=False)
    T = dot(U,dot(diag(sqrt((N-1.)/s)),U.T))
    Xa = dot(T,X)
    E[:N] = Xa + xa

    return zeta



##############################################################
## Inflation of the ensemble                                ##
##############################################################

    
def Inflation(E, infl):

    # Determine size of the ensemble
    N = E.shape[0]

    # Compute the mean
    xm = E.sum(axis=0)/N

    # Rescale
    E[:N] = xm + infl*(E - xm)
