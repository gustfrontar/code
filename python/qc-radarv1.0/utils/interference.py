import numpy as np
from utils.qc_utils  import qc 
from sklearn import linear_model, datasets


def interference_filter(ref, undef, min_ref, r, my_conf):
    """ 
    Identificar píxeles con interferencias
    
    Este filtro discrimina píxeles con interferencia utilizando el modelo de 
    regreción RANSAC. Los píxeles que están bien ajustados por el modelo 
    corresponden a interferencias y los outliers a meteorología.

    ref : array 
        campo de reflectividad en la grilla 3D (na,nr,ne)
    undef : float 
        Valor asignado al dato faltante en el campo de reflectivida.
    min_ref : float
        Valor de reflectividad correspondiente a la situación con cielo claro.
    r : vector
        Rango del radar.
    my_conf : dict
        Diccionario con la configuración de los filtros.
    """
    ref[ref==min_ref] = undef

    offset = my_conf['offset']

    na = ref.shape[0]
    nr = ref.shape[1]
    ne = ref.shape[2]

    nx = my_conf['nx']
    ny = my_conf['ny']
    nz = my_conf['nz']

    # Tomo el campo de reflectividad y lo suavizo o no según la configuración
    if my_conf['Smooth_Ref']:
        tmp_ref = qc.box_functions_2d(datain=ref,na=na,nr=nr,ne=ne,undef=undef
                                 ,boxx=nx,boxy=ny,boxz=nz,operation='MEAN',threshold=0.0)
    else:
        tmp_ref = np.copy(ref)

    # Inicializo la matriz de índices 
    tmp_index = np.zeros(np.shape(ref)) 
    # Obtengo la configuración del filtro 
    att = my_conf['att']
    AzimuthFilter = my_conf['AzimuthFilter']
    ElevationFilter = my_conf['ElevationFilter']
    npass_filter = my_conf['npass_filter']
    percent_valid_threshold = my_conf['percent_valid_threshold']
    corr_threshold = my_conf['corr_threshold']
    ref_threshold = my_conf['ref_threshold']
    percent_ref_threshold = my_conf['percent_ref_threshold']
    azimuth_ref_diff_threshold = my_conf['azimuth_ref_diff_threshold'] 
    Power_Regression = my_conf['Power_Regression']

    # Filtro principal

    for k in range(ne):
        for i in range(na):

            # Copio el rango completo de un rayo.
            local_sref = np.copy(tmp_ref[i,:,k])
            local_sref[0 : offset] = undef

            undef_mask = local_sref!=undef 

            # Obtener los rayos vecinos.

            local_sref_m1 = np.copy(tmp_ref[i-1,:,k])
 
            if i < na-1:
                local_sref_p1 = np.copy(tmp_ref[i+1,:,k])
            else:
                local_sref_p1 = np.copy(tmp_ref[na-1,:,k])
              
            local_ref = np.copy(ref[i,:,k])
            local_ref[0:offset] = undef

            tmp_count = np.sum((undef_mask).astype(int))/nr

            #Local smooth power
            local_spower = np.power(10.0, (local_sref-20.0*np.log10(r)-2.0*att*r)/10.0)
            local_spower[local_sref==undef] = undef

            #Local power
            local_power = np.power(10.0, (ref[i,:,k]-20.0*np.log10(r)-2.0*att*r)/10.0)
            local_power[ref[i, :, k] == undef] = undef 

            local_inlier_mask = undef_mask 

            if tmp_count > percent_valid_threshold:
                try: 
                    ransac = linear_model.RANSACRegressor(random_state=1)     
                    ransac.fit(r[undef_mask].reshape(-1, 1), local_spower[undef_mask].reshape(-1, 1))
                
                    local_inlier_mask[undef_mask] = ransac.inlier_mask_

                    local_fit_power = ransac.predict(r.reshape(-1,1))[:,0]
                    local_fit_power[local_fit_power < 0.0] = 10e-10

                    local_fit_ref = 10.0*np.log10(local_fit_power)+20.0*np.log10(r)+2.0*att*r
                except ValueError:
                    print("No pude correr el estimador RANSAC")
                    continue

            else:
                local_fit_power = np.zeros(nr) 
                local_inlier_mask = np.zeros(nr).astype(bool) 

            if (np.sum(local_inlier_mask.astype(int)) >= 10):
                if (np.std(local_sref[local_inlier_mask]) > 0):
                    corrcoef = np.corrcoef(local_fit_ref[local_inlier_mask], local_sref[local_inlier_mask])[0,1]
                    corrcoef_1 = np.corrcoef(r[local_inlier_mask], local_sref[local_inlier_mask])[0,1]

                    tmp_mask = np.logical_and(local_inlier_mask, local_sref_p1 != undef) 
                    if np.sum(tmp_mask) >= 10:
                        azimuth_ref_diff = np.power(np.mean(local_sref[tmp_mask]-local_sref_p1[tmp_mask]), 2)
                        
                    else:
                        azimuth_ref_diff = 0.0
                        
                    tmp_mask = np.logical_and(local_inlier_mask, local_sref_m1!=undef)
                    
                    if np.sum(tmp_mask) >= 10:
                        azimuth_ref_diff = azimuth_ref_diff + np.power(np.mean(local_sref[tmp_mask]-local_sref_m1[tmp_mask]), 2)
                        
                    else:
                        azimuth_ref_diff = 0.0

                    azimuth_ref_diff = np.sqrt(azimuth_ref_diff/2.0)/np.mean(local_sref[local_inlier_mask]) 

                else:
                    corrcoef = np.array(0.0)
                    corrcoef_1 = 0.0
                    azimuth_ref_diff = 0.0
                    
            else:
                corrcoef = np.array(0.0)
                corrcoef_1 = 0.0
                azimuth_ref_diff = 0.0

            filter_ray = False
           
            if np.sum(local_inlier_mask)/(nr-offset) > percent_ref_threshold:
                for it in range(np.size(corr_threshold)):
                    if (corrcoef > corr_threshold[it]) & (azimuth_ref_diff > azimuth_ref_diff_threshold[it]):
                        filter_ray = True
                        
            if filter_ray:
                #This means that this ray is likely to be contaminated by interference.
                undef_mask = (local_ref != undef)
                #If the reflectivity is far from the fitted interference, and is greather than the fitted
                #Interference, then correct the power substracting the interference power.         
                tmp_mask = np.logical_and(local_sref - local_fit_ref > ref_threshold, undef_mask)  
                tmp_mask = np.logical_and(local_ref - local_fit_ref > 0, tmp_mask)
                ref[i, tmp_mask ,k] = 10.0*np.log10(local_power[tmp_mask]-local_fit_power[tmp_mask]) + 20.0*np.log10(r[tmp_mask]) + 2.0 * att * r[tmp_mask]
                #If the reflectivity is far from the fitted interference, and is smaller than the fitted interference
                #then set that pixel as an undef pixel.
                tmp_mask = np.logical_and(local_sref-local_fit_ref <= ref_threshold, undef_mask)
                ref[i, tmp_mask ,k] = undef
                tmp_index[i, tmp_mask, k] = 1.0


    #Additional filter for the remaining echoes
    #consider cyclic boundary conditions in azimuth.

    for ifilter in range( npass_filter ):
        for k in range(ne):
            for i in range(na):
                if AzimuthFilter:  #DETECT ISOLATED PIXELS IN AZIMUTH
                    if (i > 1) & (i < na-2):
                        #If we have reflectivity in only one ray but not in the neighbors this suggest an interference pattern.
                        tmp_mask = np.logical_and(ref[i-1,:,k]==undef, ref[i+1,:,k]==undef)
                        tmp_mask = np.logical_and(ref[i,:,k]!=undef, tmp_mask)
                        ref[i, :, k][tmp_mask] = undef
                        tmp_index[i, :, k][tmp_mask] = 1.0

                        tmp_mask = np.logical_and(ref[i-2,:,k]==undef, ref[i+2,:,k]== undef)
                        tmp_mask = np.logical_and(ref[i,:,k]!=undef, tmp_mask)
                        ref[i,:,k][tmp_mask] = undef
                        tmp_index[i, :, k][tmp_mask] = 1.0
 
                    elif i==na-1:
                        tmp_mask = np.logical_and(ref[i-1,:,k]==undef, ref[0,:,k]==undef)
                        tmp_mask = np.logical_and( ref[i,:,k]!=undef, tmp_mask)
                        ref[i,:,k][tmp_mask] = undef
                        tmp_index[i, :, k][tmp_mask] = 1.0

                        tmp_mask = np.logical_and(ref[i-2,:,k] == undef , ref[1,:,k] == undef )
                        tmp_mask = np.logical_and(ref[i,:,k] != undef , tmp_mask )
                        ref[i,:,k][ tmp_mask ] = undef
                        tmp_index[i,:,k][ tmp_mask ] = 1.0

                    elif i==na-2:
                        tmp_mask = np.logical_and(ref[i-1,:,k]==undef, ref[i, :, k]==undef)
                        tmp_mask = np.logical_and(ref[i,:,k]!=undef, tmp_mask)
                        ref[i, :, k][tmp_mask] = undef
                        tmp_index[i, :, k][tmp_mask] = 1.0

                        tmp_mask = np.logical_and( ref[i-2,:,k] == undef , ref[0,:,k] == undef )
                        tmp_mask = np.logical_and( ref[i,:,k]   != undef , tmp_mask )
                        ref[i,:,k][ tmp_mask ] = undef
                        tmp_index[i,:,k][ tmp_mask ] = 1.0

                    elif  i==0      :
                        tmp_mask = np.logical_and( ref[na-1,:,k] == undef , ref[i+1,:,k] == undef )
                        tmp_mask = np.logical_and( ref[i,:,k]    != undef , tmp_mask )
                        ref[i,:,k][ tmp_mask ] = undef
                        tmp_index[i,:,k][ tmp_mask ] = 1.0

                        tmp_mask = np.logical_and( ref[na-2,:,k] == undef , ref[i+2,:,k] == undef )
                        tmp_mask = np.logical_and( ref[i,:,k]    != undef , tmp_mask )
                        ref[i,:,k][ tmp_mask ] = undef
                        tmp_index[i,:,k][ tmp_mask ] = 1.0

                    elif  i==1      :

                        tmp_mask = np.logical_and( ref[i-1,:,k] == undef , ref[i+1,:,k] == undef )
                        tmp_mask = np.logical_and( ref[i,:,k]    != undef , tmp_mask )
                        ref[i,:,k][ tmp_mask ] = undef
                        tmp_index[i,:,k][ tmp_mask ] = 1.0

                        tmp_mask = np.logical_and( ref[na-1,:,k] == undef , ref[i+2,:,k] == undef )
                        tmp_mask = np.logical_and( ref[i,:,k]    != undef , tmp_mask )
                        ref[i,:,k][ tmp_mask ] = undef
                        tmp_index[i,:,k][ tmp_mask ] = 1.0


                if ElevationFilter         :   #DETECT ISOLATED PIXELS IN ELEVATION
                    if ( k > 0 ) & ( k < ne-1 ) :
                        tmp_mask = np.logical_and( ref[i,:,k-1] == undef , ref[i,:,k+1] == undef )
                        tmp_mask = np.logical_and( ref[i,:,k]    != undef , tmp_mask )
                        ref[i,:,k][ tmp_mask ] = undef
                        tmp_index[i,:,k][ tmp_mask ] = 1.0

                    if ( k == 0 )                :
                        tmp_mask = np.logical_and( ref[i,:,k+2] == undef , ref[i,:,k+1] == undef )
                        tmp_mask = np.logical_and( ref[i,:,k]    != undef , tmp_mask )
                        ref[i,:,k][ tmp_mask ] = undef
                        tmp_index[i,:,k][ tmp_mask ] = 1.0

                    if ( k == ne-1 )            :
                        tmp_mask = np.logical_and( ref[i,:,k-2] == undef , ref[i,:,k-1] == undef )
                        tmp_mask = np.logical_and( ref[i,:,k]    != undef , tmp_mask )
                        ref[i,:,k][ tmp_mask ] = undef
                        tmp_index[i,:,k][ tmp_mask ] = 1.0

    return tmp_index 
