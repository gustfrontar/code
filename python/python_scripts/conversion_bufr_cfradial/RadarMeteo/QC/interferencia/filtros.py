# filtra los 'ruidos' de una variable del radar mirando si la mayoria de los vecinos son nan. 
# Parametros de entrada:(radar = objeto radar pyart, var = str de la variable a filtrar,umbral= por defecto 6)
# devuelve: objeto radar modificado, se le agrega la variable corregida de mobre var+'_cor'

import numpy as np
import pyart
import numpy.ma as ma

def scp(radar,var,umbral=6):
	azin=0	
	nsw=radar.nsweeps
	
	ref2=radar.fields[var]['data'].copy()
	for s in range(nsw):
    		r1  = radar.extract_sweeps([s])
    		ref_copy=r1.fields[var]['data'].copy()
    		nrays=r1.nrays
    		nbins=r1.ngates
    		for azi in range(1,nrays-2):
        		for ran in range(1,nbins-2):
            			cuantos=sum(sum(r1.fields[var]['data'].mask[azi-1:azi+2,ran-1:ran+2]))
            			if cuantos>umbral:
                			ref_copy[azi,ran]=ma.masked

        	ref2[azin:azin+nrays,]=ref_copy
    		azin=azin+nrays
	#ref2=ma.masked_equal(ref2, fv)  
	return(ref2)


def interf(radar,var):
	azin=0	
	nsw=radar.nsweeps
	ref2=radar.fields[var]['data'].copy()
	
	# Factor de atenuacion gaseosa para banda C
	att=0.01
	## Fillvalue
	fv=radar.fields[var]['_FillValue']
	
	## Umbrales
	offset=100  # Este offset es para determinar hasta que rango tengo contaminacion por lobulos laterales.
	refthreshold=5.0
	min_sample_size=50

	for s in range(nsw):
		r1  = radar.extract_sweeps([s])
		ref = r1.fields[var]['data']

		nrays    = r1.nrays
		nbins    = r1.ngates
		startazi = radar.sweep_start_ray_index['data'][s]
		endazi   = radar.sweep_end_ray_index['data'][s]
		rscale   = radar.range['meters_between_gates']

		r = np.arange(startazi, startazi+nbins*rscale,rscale)
		r[0]=1
		r=r/1000

	
		tmp=np.zeros((nrays,nbins))
		tmp[:,:]=ref[:,:]


		# Creo una nueva variable llamada "cref" que contendra la reflectividad corregida
		cref=np.zeros((nrays,nbins))
		cref[:,:]=fv


		# Reservo lugar para las cuentas que siguen
		dbm      = np.zeros((nbins))
		dbzrayo  = np.zeros((nrays,nbins))
		zrayo    = np.zeros((nrays,nbins))
		z        = np.zeros((nrays,nbins))
		b        = np.zeros(nrays)
		corrcoef = np.zeros(nrays)
		raux     = np.zeros(nbins)

		percentaje=np.zeros(nrays)

		for i in range(nrays):
    
			dbm[:]   = 0.0
			raux[:]  = 0.0
			contador = 0
			valid    = 0.0
    
			for j in range(offset,nbins):
                   	
				if tmp[i,j] != fv:
            
            # A continuacion, revertimos la correcciones por rango que realiza el soft. La respuesta de un spike
            # es lineal porque la potencia es constante (en ausencia de eco meteorologico)
            			
					dbm[contador]= tmp[i,j]  - 20*np.log10( r[j] ) - 2*att*r[j]
			     		raux[contador]=r[j]
			    		contador=contador+1
			    
			if contador > min_sample_size:
            
        # Aca hacemos un ajuste de primer orden (lineal) entre "rayo teorico (dbm)" y rango 
        			p=np.polyfit(r[:contador-1],dbm[:contador-1],1)
        			tmpcorr=np.corrcoef(r[:contador-1],dbm[:contador-1])
        			corrcoef[i]=tmpcorr[0,1]
        			b[i]=p[1]
           
    			else:
        			b[i]=0.0
        
	    	#Calculo la forma del rayo ajustado a los datos. 
	    	#Calculo ademas el porcentaje de pixeles en este azimuth que estan a una distancia
	    	#mas / menos refthreshold del perfil de reflectividad ajustado. Si el porcentaje es alto
	    	#este azimuth es un candidato para ser corregido.
    			for j in range(offset,nbins):
        			# Potencia teorica (dBZ)
        
        			dbzrayo[i,j]=+b[i] + 20*np.log10( r[j] ) + 2*att*r[j]
        
        			if ref[i,j] > fv:
            				valid=valid+1
        			if np.abs( dbzrayo[i,j]-tmp[i,j]) < refthreshold:
            				percentaje[i]=percentaje[i]+1
    
    			if valid > 0:        
        			percentaje[i] = percentaje[i] / valid
    			else:
        			percentaje[i]=0  

    #Si el porcentaje de pixeles que estan cerca del perfil de dbz ajustado es mayor a un cierto valor 
    #umbral (eg 0.4) entonces este azimuth esta contaminado por un rayo y tenemos que corregirlo
    			if percentaje[i] > 0.4:  
          
        			for j in range(nbins):  
            
            #Si la reflectividad esta cercana al perfil ajustado entonces esto es contaminacion
            #y nada mas
            				if np.abs(dbzrayo[i,j] - tmp[i,j]) < refthreshold:
                				cref[i,j]=fv   
            
            #Si la reflectividad es mayor que el perfil ajustado. Entonces puedo tener echo meteorologico
            #junto con la contaminacion electromagnetica.
            #En ese caso le resto la potencia asociada a la contaminacion electromagnetica al echo meteorologico.
            #para obtener la reflectividad corregida.
            				else: 
                				zrayo = np.power(10,dbzrayo[i,j]/10.0)
                				z     = np.power(10,tmp[i,j]/10.0)
    
                				if z - zrayo > 0.0:
                    #We correct the reflectivity substracting RLAN power
                    					cref[i,j]=10*np.log10(z-zrayo)
                				else:
                    					cref[i,j]=fv
    			else:
        
        			cref[i,:]=tmp[i,:]
     

	#Additional filter for the remaining echoes
              
	#TODO: consider cyclic boundary conditions in azimuth.

		npassfilter=1

		for j in range(nbins):

    			for ifilter in range(npassfilter):
        
        			for i in range(nrays-1):
            
            				if np.logical_and(cref[i-1,j]<0, cref[i+1,j]<0):
                
                				if cref[i,j]>0:
                    					cref[i,j]=fv
            	
           	 			if np.logical_and(cref[i-2,j]<0, cref[i+1,j]<0):
                
                				if cref[i,j]>0:
                	    				cref[i,j]=fv
            	
            				if np.logical_and(cref[i-1,j]>0, cref[i+1,j]>0):
                
                				if cref[i,j]==fv:
                	    				cref[i,j]=0.5*(cref[i-1,j]+cref[i+1,j])
            
            				if np.logical_and(cref[i-2,j]>0, cref[i+1,j]>0):
                
                				if cref[i,j]==fv:
                	    				cref[i,j]=0.25*cref[i-2,j]+0.75*cref[i+1,j]
            
            				if np.logical_and(cref[i-3,j]>0,cref[i+1,j]>0):
                
                				if cref[i,j] == fv:
                	    				cref[i,j]=0.10*cref[i-3,j]+0.90*cref[i+1,j]
                
                
		cref[ cref < fv ] = fv
		ref2[azin:azin+nrays,]=cref
    		azin=azin+nrays			

	ref2=ma.masked_equal(ref2, fv)  
	return(ref2)


