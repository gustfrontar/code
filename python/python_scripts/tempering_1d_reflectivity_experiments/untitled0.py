#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar 17 10:11:51 2019

@author: jruiz
"""

import numpy as np
import matplotlib.pyplot as plt

sample_size=100000

r=1.0+0.1*np.random.randn(sample_size)
#r=0.3*np.random.randn(sample_size)

tend=np.power( np.random.randn(sample_size) + 10 , 1 )


tend_pert = tend * r

pert = tend - tend_pert 

print( np.mean( tend ) , np.mean(tend_pert) )
print( np.std( tend ) - np.std(tend_pert) )
print( np.mean(pert) , np.std(pert) )

[hist,bins]=np.histogram(tend, bins=100, range=(0,20))

[hist_pert,bins_pert]=np.histogram(tend_pert, bins=100, range=(0,20))

plt.figure()
plt.plot(bins[1:],hist,'r')
plt.plot(bins_pert[1:],hist_pert,'b')
plt.show()