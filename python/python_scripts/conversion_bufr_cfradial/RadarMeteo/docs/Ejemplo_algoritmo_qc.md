
# Algoritmo Control de Calidad


```python
%matplotlib inline
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
import clasificador
from clasificador import class_meteo
from IPython.display import display, Math, Latex
```

### A modo de resumen una breve descripción de los algoritmos implementados para el control de calidad 

## Variables:
* $\rho_{HV}$

* V

* $\sigma({Z_{DR}})$ 


## Clases:

 * $C_1$ :Meteo 
 * $C_2$: No Meteo

## Modelos
 Una simple descripción de cada uno de los modelos. Todos basados en téncinas de clasificación supervizada.  Se modela para cada pixel la probabilidad de estar en cada una de las clases y se asigna la que tiene probabilidad mayor 



 
Modelo | Descripción
-- | --
LDA | Supone que cada una de las clases se distrbuyen como una normal multivariada. Asume que la matriz de varianzas en ambas clases es igual 
QDA | Idem a LDA pero sin asumir igualdad en la matriz de covarianzas
BN  | Supone variables independientes. Estimacion de las funciones de densidad de cada una de ellas a partir de un estimador nucleo. 
LG  | Modelo de regresion. Estima las probabilidades condicionales asumiendo que éstas se modelan a partir de una función logística

Función class_meteo

#### INPUTS
* *route* :  ruta del archivo .nc


* *sweep*: elevación 


* *model*: 'lda' para modelo LDA, 'qda' para modelo QDA, 'lg' para modelo Logístico, 'bn' para modelo Bayes Naive.boost for mixture of lda,bn,qda,and logit. DEFAULT: 'lda'


* *graf*: TRUE para graficar PPI. DEFAULT: True


* *Path*: ruta en donde esta el modulo. DEFAULT: ./

Ejemplo


```python

ruta='/home/sofia/RadarMeteo/datos/cfrad.20100111_205345.000_to_20100111_205722.000_ANG120_v126_SUR.nc'
sweep = 0
graf=True
M=class_meteo(ruta,0,graf=True)


```


![png](Ejemplo_algoritmo_qc_files/Ejemplo_algoritmo_qc_5_0.png)

