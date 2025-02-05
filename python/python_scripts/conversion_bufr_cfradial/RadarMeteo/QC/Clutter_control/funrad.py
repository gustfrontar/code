# Funciones que se aplican sobre las variables del radar

# Esta funcion lo que hace es tomar un elemento radar y calcula el desvio
# estandar del ZDR devuelve un diccionario analogo a los diccionario de las
# fields del radar que contiene los datos del desvio(ZDR) se puede graficar:
# para ello hay que pegarlo como field a un elemento radar de una elevacion

import numpy as np
import numpy.ma as ma
import numpy as np
import pyart
import netCDF4
import numpy.ma as ma
from ..corregir_azimuth import corregir_azimuth


def sd_zdr(radar, ventana):
    ele1 = radar.extract_sweeps([0])
    pego = ma.array(np.zeros([ele1.nrays, ele1.ngates]))
    pego[pego == 0] = np.nan
    for azi in range(0, ele1.nrays):
        for rango in range(ventana, ele1.ngates-ventana):
            pego[azi, rango] = ele1.fields['ZDR']['data'][azi, rango-ventana:rango+ventana].std()

    # enmascaramos los bordes
    pego[0, :] = ma.masked
    pego[ele1.nrays-1, :] = ma.masked

    pego[:, 0] = ma.masked
    pego[:, ele1.ngates-1] = ma.masked

    return pego

# calculamos un suavizado para el valor del rho_hv, sobre una ventana de 5x5.
# se puede graficar: para ello hay que pegarlo como field a un elemento radar
# de una elevacion: ele1.add_field_like('RhoHV','srhohv',pego)
# ingresa como datos un objeto tipo radar


def s_rohv(radar, rho):
    ele1 = radar.extract_sweeps([0])
    pego = radar.get_field(0, rho, copy=False).copy()
    for azi in range(2, 360-2):
        for rango in range(2, 480-2):
            pego[azi, rango] = pego[azi-2:azi+2, rango-2:rango+2].mean()

    pego.mask = ele1.fields[rho]['data'].mask
    return pego

# def s_rohv_cba(radar):
#   ele1=radar.extract_sweeps([0])
#   pego=radar.get_field(0,'RHOHV',copy=False).copy()
#   nazi=pego.shape[0]
#   nran=pego.shape[1]

#   for azi in range (2,nazi-2):
#       for rango in range (2,nran-2):
#           pego[azi,rango]=pego[azi-2:azi+2,rango-2:rango+2].mean()

#   pego.mask=ele1.fields['RHOHV']['data'].mask
#   return pego

# Gradiente vertical de la reflectividad


def vgz(radar, tilt):  # entre la elevacion j y la j+2
    AzC = corregir_azimuth(radar)
    radar_lat = radar.latitude['data'][0]
    radar_lon = radar.longitude['data'][0]
    radar_alt = radar.altitude['data'][0]
    d2r = np.pi/180

    # constant necessary
    Re = 6370000  # radius of the earth
    ke = (4/3)
    M = []  # en meters

    # calculo la distancia proyectada hacia el punto (al radar)
    for i in range(radar.nsweeps):
        sweep = i
        ang = radar.get_elevation(sweep)[0]
        rango = radar.range['data']
        M.append(np.ones([480, 2]))

        # azi=AzC[i][j]
        M[i][:, 0] = radar_alt+np.sqrt((ke*Re)**2+2*rango*np.sin(ang*d2r)*ke*Re+rango**2)-ke*Re
        M[i][:, 1] = ke*Re*np.sin((rango*np.cos(ang*d2r))/(ke*Re))

    # calculo de vgz
    r1 = radar.extract_sweeps([tilt])
    r2 = radar.extract_sweeps([tilt+1])
    thetha1 = r1.elevation['data'][0]
    thetha2 = r2.elevation['data'][0]

    orden = r1.azimuth['data']  # order azimuth 1st sweep
    NNo = np.ones([r1.nrays], dtype=np.int)
    for rr in range(0, r1.nrays):
        q = orden[rr]
        NNo[rr] = AzC[tilt+2][int(q)]

    Vgz = np.ones([r1.nrays, r1.ngates])
    fv = radar.fields['dBZ']['_FillValue']
    for r in range(0, r1.ngates):
        s = M[tilt][r][1]
        d = abs(M[tilt+2][:, 1]-s)
        ii = d.argmin()  # buscamos el que esta arriba
        c = radar.fields['dBZ']['data'][NNo, ii]
        c[c.data == -128] = 0
        Vgz[:, r] = c-r1.fields['dBZ']['data'][:, r]
        # Vgz[:,r]=radar.fields['dBZ']['data'][NNo,ii]-r1.fields['dBZ']['data']

    Vgz[np.where(Vgz == fv)] = np.nan
    Vgz[np.where(r1.fields['dBZ']['data'].mask == True)] = np.nan
    Vgz = -Vgz/(thetha2-thetha1)

    return(Vgz)

# Esta funcion lo que hace es tomar un elemento radar y calcula el desvio
# estandar de la reflectividad devuelve un diccionario analogo a los
# diccionario de las fields del radar que contiene los datos del desvio(ZH)
# se puede graficar: para ello hay que pegarlo como field a un elemento
# radar de una elevacion


def sd_zh(radar, tilt, ventana):
    ele1 = radar.extract_sweeps([tilt])
    pego = ma.array(np.zeros([ele1.nrays, ele1.ngates]))
    pego[pego == 0] = np.nan
    for azi in range(0, ele1.nrays):
        for rango in range(ventana, ele1.ngates-ventana):
            pego[azi, rango] = ele1.fields['dBZ']['data'][azi, rango-ventana:rango+ventana].std()
    # enmascaramos los bordes
    pego[0, :] = ma.masked
    pego[ele1.nrays-1, :] = ma.masked

    pego[:, 0] = ma.masked
    pego[:, ele1.ngates-1] = ma.masked

    return(pego)


def echo_top(radar, thr):

    radar_lat = radar.latitude['data'][0]
    radar_lon = radar.longitude['data'][0]
    radar_alt = radar.altitude['data'][0]
    d2r = np.pi/180
    Re = 6370000  # radius of the earth
    ke = (4/3)
    thr = 5
    r1 = radar.extract_sweeps([0])
    EchT = np.ones([r1.nrays, r1.ngates])
    AzC = corregir_azimuth(radar)
    # Para cada azimuth
    for sazimuth in range(r1.nrays):
        data = []
        H = []
        S = []

        for i in range(radar.nsweeps-1):
            sweep = i
            ang = radar.get_elevation(sweep)[0]
            data.append(np.ones([1, 480]))
            azi = AzC[i][sazimuth-1]
            data[i] = radar.fields['dBZ']['data'].data[azi, :]
            rango = radar.range['data']

            H.append(np.ones([480]))
            # H[i][0,:]=radar_alt+np.sqrt((ke*Re)**2+2*rango*np.sin((ang-0.5)*d2r)*ke*Re+rango**2)-ke*Re
            H[i] = radar_alt+np.sqrt((ke*Re)**2+2*rango*np.sin(ang*d2r)*ke*Re+rango**2)-ke*Re
            # H[i][2,:]=radar_alt+np.sqrt((ke*Re)**2+2*rango*np.sin((ang+0.5)*d2r)*ke*Re+rango**2)-ke*Re

            S.append(np.ones([480]))
            # S[i][0,:]= ke*Re*np.arcsin((rango*np.cos((ang-0.5)*d2r))/(ke*Re))
            S[i] = ke*Re*np.arcsin((rango*np.cos(ang*d2r))/(ke*Re))
            # S[i][2,:]=ke*Re*np.arcsin((rango*np.cos((ang+0.5)*d2r))/(ke*Re))
        data = np.array(data)
        S = np.array(S)
        H = np.array(H)

        for rang in range(r1.ngates-1):
            if data[0, rang] > thr:
                ejex = S[0, rang]
                ind = rang
                for elev in range(radar.nsweeps-1):
                    dist_mas_parecida = np.min(abs(S[elev, ]-ejex))
                    ind = abs(S[1, ]-ejex).argmin()
                    if data[elev, ind] > thr:
                        if ejex > dist_mas_parecida:
                            ind1 = ind
                            ind2 = ind + 1
                        else:
                            ind1 = ind - 1
                            ind2 = ind

                        d1 = ejex-S[1, ind1]
                        d2 = ejex-S[1, ind2]
                        w1 = d1/(d1+d2)
                        w2 = 1-w1
                        r = w1*rango[ind1]+w2*rango[ind2]
                        angulo = radar.get_elevation(elev)[elev]
                        EchT[AzC[0][sazimuth-1], rang] = radar_alt+np.sqrt((ke*Re)**2+2*r*np.sin(angulo*d2r)*ke*Re+r**2)-ke*Re

    EchT = EchT/1000
    EchT = ma.masked_array(EchT)
    EchT[np.where(EchT == 0.001)] = ma.masked
    EchT.fill_value = -128

    return(EchT)

# Esta funcion lo que hace es tomar un elemento radar y calcula el desvio
# estandar de alguna variable var (sobre la elevacion mas baja) devuelve un
# diccionario analogo a los diccionario de las fields del radar #que contiene
# los datos del desvio(var)
# se puede graficar: para ello hay que pegarlo como field a un elemento
# radar de una elevacion


def sd_var(radar, var, ven):

    lista = ma.array(radar.fields[var]['data'].data.copy(),
                     mask=radar.fields[var]['data'].mask.copy())
    for elevacion in range(radar.nsweeps):
        ele1 = radar.extract_sweeps([elevacion])
        for azi in range(ven, ele1.nrays-ven):
            for rango in range(ven, ele1.ngates-ven):
                lista[azi+361*elevacion, rango] = ele1.fields[var]['data'][azi-ven:azi+ven, rango-ven:rango+ven].std()
        # Bordes
        for azi in range(-ven, ven):
            for rango in range(ven, ele1.ngates-ven):
                if azi >= 0:
                    a = ele1.fields[var]['data'][-ven+azi:, rango-ven:rango+ven]
                    b = ele1.fields[var]['data'][0:azi+ven, rango-ven:rango+ven]
                    t = ma.concatenate([a, b]).std()
                    lista[361*elevacion+azi, rango] = t
                else:
                    a = ele1.fields[var]['data'][0:azi+ven, rango-ven:rango+ven]
                    b = ele1.fields[var]['data'][azi-ven:, rango-ven:rango+ven]
                    t = ma.concatenate([a, b]).std()
                    lista[azi+361*(elevacion+1), rango] = t

    # Enmascarar los bordes del rango
    lista[:, 0:ven] = ma.masked
    lista[:, -ven:] = ma.masked
    return(lista)
