
from ..funrad import echo_top
import numpy as np
import numpy.ma as ma


##### Clasificador para radares de polarizacion simple (solo con EchoTop)

def qc_sp(radar,tr=1.5,postp=True):
    Et=echo_top(radar,0)
    r=radar.extract_sweeps([0])
    V=radar.fields['V']['data']
    clas=ma.masked_array(np.ones([r.nrays,r.ngates]))
    clas[np.where(clas==1)]=2
    clas.mask=Et.mask
    for azi in range(r.nrays):
        for ran in range(r.ngates):
            if Et[azi,ran]<=tr:
                clas[azi,ran]=0
            elif Et[azi,ran]>tr:
                clas[azi,ran]=1
            elif Et[azi,ran]>1 and Et[azi,ran]<0.5:
                if V[azi,ran]: ### si hay valor
                    if abs(V[azi,ran])<1:
                        clas[azi,ran]=0
                    else:
                        clas[azi,ran]=1
                else:
                    clas[azi,ran]=2
    datos2=clas.copy() 
    ###postproceso
    if postp:
        
        for azi in range(r.nrays):
            for ran in range(r.ngates):
                media=clas[azi-1:azi+2,ran-1:ran+2].mean()
                if not isinstance(media, float):
                    datos2[azi,ran]=2 
                else:    
                    total=clas[azi-1:azi+2,ran-1:ran+2].count()
                    quiero=np.round(total/2)+1
                    suma=(float(quiero))/total
                    if media >= suma:
                        datos2[azi,ran]=1
                    else:
                        datos2[azi,ran]=0
        #AZI = 0
        nrays=r.nrays
        for rango in range(1,r.ngates-1):
            media=clas[(np.array([0, 1, nrays-1]), np.array([rango-1,rango,rango+1]))].mean()       
            if not isinstance(media, float):
                datos2[0,rango]=2 
            else:    
                total=clas[(np.array([0, 1, nrays-1]), np.array([rango-1,rango,rango+1]))].count()
                quiero=np.round(total/2)+1
                suma=(float(quiero))/total
                if media >= suma:
                    datos2[0,rango]=1
                else:
                    datos2[0,rango]=0


        #AZI = final
        for rango in range(1,r.ngates-1):
            media=clas[(np.array([0, nrays-2,nrays-1 ]), np.array([rango-1,rango,rango+1]))].mean()    
            if not isinstance(media, float):
                datos2[nrays-1,rango]=2 
            else:    
                total=clas[(np.array([0, nrays-2,nrays-1 ]), np.array([rango-1,rango,rango+1]))].count()
                quiero=np.round(total/2)+1
                suma=(float(quiero))/total
                if media >= suma:
                    datos2[nrays-1,rango]=1
                else:
                    datos2[nrays-1,rango]=0



        datos2.mask=clas.mask 
    return(datos2)

    









