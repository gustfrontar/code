"""
Created on Wed Apr  6 10:11:00 2016

@author: sofia
"""

###############################################################################
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
from .funrad import s_rohv
from .funrad import sd_zdr
import tempfile

###############################################################################


def info_radar(radar, sweep, names):

    # if cual=='Cordoba':
    mascara_dbz = radar.fields[names[0]]['data'].mask
    # else:
    #     mascara_dbz=radar.fields['dBZ']['data'].mask

    queazi = np.where(mascara_dbz == False)[0]
    querango = np.where(mascara_dbz == False)[1]

    ro = radar.get_field(sweep, 'nRho')
    value_ro = ro.get_fill_value()
    ro.data[np.where(ro.data == value_ro)] = np.nan

    szdr = radar.get_field(sweep, 'Szdr')
    value_szdr = szdr.get_fill_value()
    szdr.data[np.where(szdr.data == value_szdr)] = np.nan

    # if cual=='Cordoba':
    #     v=radar.get_field(sweep,'VRAD')
    # else:
    v = radar.get_field(sweep, names[1])

    value_v = v.get_fill_value()
    v.data[np.where(v.data == value_v)] = np.nan

    datos = np.zeros([len(queazi), 5])
    for i in range(len(queazi)):
        datos[i, 0] = ro.data[queazi[i], querango[i]]
        datos[i, 1] = szdr.data[queazi[i], querango[i]]
        datos[i, 2] = v.data[queazi[i], querango[i]]
        datos[i, 3] = queazi[i]
        datos[i, 4] = querango[i]
    return(datos)

# sale array con los datos de las variables Rho,SZDR,V en los lugares
# donde hay dato reflectividad

###############################################################################

# ################################# Funcion LDA ##############################


def predice_lda(x, alpha, mu_m, mu_nm):
    dw = alpha*np.transpose((np.matrix(x)))
    delta_1 = alpha*(np.transpose(mu_m))
    delta_2 = alpha*(np.transpose(mu_nm))

    if abs(delta_1-dw) < abs(delta_2-dw):
        salida = 1
    else:
        salida = 0
    return(salida)


def predice_sin_nan_lda(fila):
    # discriminantes
    d_lda = np.matrix([-12.28773848, 0.18936150, -0.01796824])
    d_lda_rn = np.matrix([1.67664632, -0.02728686])
    d_lda_sn = np.matrix([-13.00138925, -0.01820025])
    d_lda_vn = np.matrix([-12.2242797, 0.1963356])

    # Medias
    mu_meteo = np.matrix([0.9759186, 0.3077836, 2.8011959])
    mu_nometeo = np.matrix([0.76089505, 1.19478813, -0.07744507])

    mu_meteo_rn = np.matrix([0.3077848, 2.80122903])
    mu_meteo_sn = np.matrix([0.9759186, 2.80119591])
    mu_meteo_vn = np.matrix([0.9752396, 0.3143273])

    mu_nometeo_rn = np.matrix([1.1947888, -0.07753789])
    mu_nometeo_sn = np.matrix([0.7608951, -0.07744507])
    mu_nometeo_vn = np.matrix([0.7582490, 1.1993115])
    pred = np.nan
    tot = sum(np.isnan(fila))
    if tot == 0:
        pred = predice_lda(fila, d_lda, mu_meteo, mu_nometeo)
    if tot == 1:
        if np.isnan(fila[0]):
            pred = predice_lda(fila[1:3], d_lda_rn, mu_meteo_rn, mu_nometeo_rn)
        if np.isnan(fila[1]):
            pred = predice_lda(fila[[0, 2]], d_lda_sn, mu_meteo_sn,
                               mu_nometeo_sn)
        if np.isnan(fila[2]):
            pred = predice_lda(fila[0:2], d_lda_vn, mu_meteo_vn, mu_nometeo_vn)

    return(pred)


def lda_mod(datos):
    a = []
    for i in range(len(datos)):
        a.append(predice_sin_nan_lda(datos[i, 0:3]))
    a = np.array(a)
    return(a)

# ################################ Funcion QDA ################################


def predice_qda(x, sigma1i, sigma2i, mu_m, mu_nm, ld1, ld2):
    delta_1 = -1/2*np.matrix(x-mu_m)*sigma1i*np.transpose(x-mu_m)-1/2*ld1
    delta_2 = -1/2*np.matrix(x-mu_nm)*sigma2i*np.transpose(x-mu_nm)-1/2*ld2

    if delta_1 > delta_2:
        salida = 1
    else:
        salida = 0
    return(salida)


def predice_sin_na_qda(fila):

    # medias y sigmas estimadas

    mu_meteo = np.matrix([0.9759186, 0.3077836, 2.80119591])
    mu_nometeo = np.matrix([0.7608951, 1.1947881, -0.07744507])

    sigma_meteo_inv = np.matrix([[1.057151e+03, 81.34570771, 0.09060785],
                                [8.134571e+01, 13.40018428, 0.01883916],
                                [9.060785e-02, 0.01883916, 0.01361322]])
    sigma_nometeo_inv = np.matrix([[115.0965244, 6.235484812, 0.059295502],
                                  [6.2354848, 2.129814914, -0.001237191],
                                  [0.0592955, -0.001237191, 0.016319462]])

    log_det_meteo = -4.630421
    log_det_nometeo = -1.211155

    # rn
    mu_meteo_rn = np.matrix([0.3077836, 2.80119591])
    mu_nometeo_rn = np.matrix([1.1947881, -0.07744507])

    sigma_meteo_inv_rn = np.matrix([[7.14078944, 0.01186707],
                                   [0.01186707, 0.01360545]])
    sigma_nometeo_inv_rn = np.matrix([[1.792000448, -0.004449593],
                                     [-0.004449593, 0.016288914]])

    log_det_meteo_rn = 2.332912
    log_det_nometeo_rn = 3.534616

    # sn
    mu_meteo_sn = np.matrix([0.9759186, 2.80119591])
    mu_nometeo_sn = np.matrix([0.7608951, -0.07744507])

    sigma_meteo_inv_sn = np.matrix([[563.34233824, -0.02375515],
                                   [-0.02375515, 0.01358673]])
    sigma_nometeo_inv_sn = np.matrix([[96.84082029, 0.06291764],
                                     [0.06291764, 0.01631874]])

    log_det_meteo_sn = -2.035152
    log_det_nometeo_sn = -0.4551195

    # vn
    mu_meteo_vn = np.matrix([0.9759186, 0.3077836])
    mu_nometeo_vn = np.matrix([0.7608951, 1.1947881])

    sigma_meteo_inv_vn = np.matrix([[1056.54771, 81.22032],
                                   [81.22032, 13.37411]])
    sigma_nometeo_inv_vn = np.matrix([[114.805091, 6.238085],
                                     [6.238085, 2.129674]])

    log_det_meteo_vn = -8.927135
    log_det_nometeo_vn = -5.325853

    pred = np.nan
    tot = sum(np.isnan(fila))

    if tot == 0:
        pred = predice_qda(fila, sigma_meteo_inv, sigma_nometeo_inv, mu_meteo,
                           mu_nometeo, log_det_meteo, log_det_nometeo)

    if tot == 1:
        if np.isnan(fila[0]):
            pred = predice_qda(fila[1:3], sigma_meteo_inv_rn,
                               sigma_nometeo_inv_rn, mu_meteo_rn,
                               mu_nometeo_rn, log_det_meteo_rn,
                               log_det_nometeo_rn)
        if np.isnan(fila[1]):
            pred = predice_qda(fila[[0, 2]], sigma_meteo_inv_sn,
                               sigma_nometeo_inv_sn, mu_meteo_sn,
                               mu_nometeo_sn, log_det_meteo_sn,
                               log_det_nometeo_sn)
        if np.isnan(fila[2]):
            pred = predice_qda(fila[0:2], sigma_meteo_inv_vn,
                               sigma_nometeo_inv_vn, mu_meteo_vn,
                               mu_nometeo_vn, log_det_meteo_vn,
                               log_det_nometeo_vn)

    return(pred)


def qda_mod(datos):
    a = []
    for i in range(len(datos)):
        a.append(predice_sin_na_qda(datos[i, 0:3]))
    a = np.array(a)
    return(a)

###############################################################################

# ############################## Postproceso ##################################


def post_proceso(ruta, sweep, a, datos, names):
    radar2 = pyart.io.read(ruta)
    radar = pyart.io.read(ruta)
    # if cual=='Cordoba':
    #     Datos = radar.get_field(sweep,'TH')
    # else:#anguil
    Datos = radar.get_field(sweep, names[0])

    Azi_No_Meteo = datos[np.where(a == 0), 3]
    Rango_No_Meteo = datos[np.where(a == 0), 4]

    Azi_Meteo = datos[np.where(a == 1), 3]
    Rango_Meteo = datos[np.where(a == 1), 4]

    Azi_NaN = datos[np.where(np.isnan(a)*1 == 1), 3]
    Rango_NaN = datos[np.where(np.isnan(a)*1 == 1), 4]
    # Paso a int8
    Rango_NaN = Rango_NaN.astype(np.int)
    Azi_NaN = Azi_NaN.astype(np.int)

    Rango_Meteo = Rango_Meteo.astype(np.int)
    Azi_Meteo = Azi_Meteo.astype(np.int)

    Rango_No_Meteo = Rango_No_Meteo.astype(np.int)
    Azi_No_Meteo = Azi_No_Meteo.astype(np.int)

    Datos[Azi_NaN, Rango_NaN] = ma.masked  # Nan de la clasificacion
    Datos[Azi_Meteo, Rango_Meteo] = 1
    Datos[Azi_No_Meteo, Rango_No_Meteo] = 0

    nrays = Datos.shape[0]
    nran = Datos.shape[1]

    datos2 = ma.masked_array(np.full([nrays, nran], 10), mask=False,
                             fill_value=-128)
    datos2[:, 0] = Datos[:, 0]
    datos2[:, nran-1] = Datos[:, nran-1]
    for azi in range(1, nrays-1):
        for rango in range(1, nran-1):
            media = Datos[azi-1:azi+2, rango-1:rango+2].mean()
            if not isinstance(media, float):
                datos2[azi, rango] = 2
            else:
                total = Datos[azi-1:azi+2, rango-1:rango+2].count()
                quiero = np.round(total/2)+1
                suma = (float(quiero))/total
                if media >= suma:
                    datos2[azi, rango] = 1
                else:
                    datos2[azi, rango] = 0
    # AZI = 0
    for rango in range(1, nran-1):
        media = Datos[(np.array([0, 1, nrays-1]),
                       np.array([rango-1, rango, rango+1]))].mean()

        if not isinstance(media, float):
            datos2[0, rango] = 2
        else:
            total = Datos[(np.array([0, 1, nrays-1]),
                           np.array([rango-1, rango, rango+1]))].count()
            quiero = np.round(total/2)+1
            suma = (float(quiero))/total
            if media >= suma:
                datos2[0, rango] = 1
            else:
                datos2[0, rango] = 0

    # AZI = final
    for rango in range(1, nran-1):
        media = Datos[(np.array([0, nrays-2, nrays-1]),
                       np.array([rango-1, rango, rango+1]))].mean()

        if not isinstance(media, float):
            datos2[nrays-1, rango] = 2
        else:
            total = Datos[(np.array([0, nrays-2, nrays-1]),
                           np.array([rango-1, rango, rango+1]))].count()
            quiero = np.round(total/2)+1
            suma = (float(quiero))/total
            if media >= suma:
                datos2[nrays-1, rango] = 1
            else:
                datos2[nrays-1, rango] = 0

    return(datos2)

###############################################################################

# ######################################### Grafico PPI ######################


def grafico_PPI(ruta, datos2, sweep, names):

    Radar = pyart.io.read(ruta)
    radar2 = pyart.io.read(ruta)
    # if cual=='Cordoba':
    #     reflectividad_nueva=radar2.get_field(sweep,'TH')
    # if cual=='Anguil':
    reflectividad_nueva = radar2.get_field(sweep, names[0])
    reflectividad_nueva[np.where(datos2 == 1)] = 1
    reflectividad_nueva[np.where(datos2 == 0)] = 0
    reflectividad_nueva[np.where(datos2 == 2)] = 2

    display = pyart.graph.RadarDisplay(Radar)
    display2 = pyart.graph.RadarMapDisplay(radar2)

    f = plt.figure(figsize=[5, 15])

    plt.subplot(2, 1, 1)
    # if cual=='Anguil':
    #     display.plot_ppi('dBZ',vmin=-25,vmax=75)
    # if cual=='Cordoba':
    display.plot_ppi(names[0], vmin=-25, vmax=75)
    plt.axis('equal')
    # display.plot_range_rings([30, 60, 90, 120])
    # display.plot_cross_hair(5.)

    cmap = mpl.colors.ListedColormap(['r', 'b', 'g'])
    plt.subplot(2, 1, 2)
    # if cual=='Anguil':
    #     display2.plot_ppi('dBZ',vmin=0,vmax=2,cmap=cmap)
    # if cual=='Cordoba':
    display2.plot_ppi(names[0], vmin=0, vmax=2, cmap=cmap)
    plt.title('Clasificado')
    plt.axis('equal')
    # display.plot_range_rings([30, 60, 90, 120])
    # display.plot_cross_hair(5.)

    plt.show()
