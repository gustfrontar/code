import numpy as np

def calculate_coherence_index(v, available_mask, coherence_index, my_conf,i, k, iprev, kprev):
    """
    Calcula el índice de coherencia para cada rango.

    Parámetros
    ----------
    v : array
        Campo de velocidad doppler
    available_mask : array
    coherence_index :
    my_conf : 
    i : 
    k :
    iprev : 
    kprev :

    Devuelve
    --------
    coherence_index : 
    """
    # para cada rango chequeamos si están enmascarados los puntos de alrededor
    local_valid_mask = np.logical_and(available_mask[i,:,k], available_mask[iprev,:,kprev])
    n_valid = np.sum(local_valid_mask.astype(int)) 
    nr = coherence_index.shape[1]
    coherence_index[i,:,k][np.logical_not(local_valid_mask)]=coherence_index[i,:,k][np.logical_not(local_valid_mask)] + 1.0
    if (n_valid/nr) > my_conf['threshold_undef']:
        tmp_vi = np.copy(v[i,:,k][local_valid_mask])
        tmp_viprev = np.copy(v[iprev,:,kprev][local_valid_mask])

        if my_conf['consistency_metric'] == 'ransac':
            #Use a robust correlation method to decide which
            #elements are spatially coherent.

            ransac.fit(tmp_vi.reshape(-1, 1), tmp_viprev.reshape(-1, 1))
            inlier_mask = ransac.inlier_mask_
            outlier_mask = np.logical_not(inlier_mask)

        if my_conf['consistency_metric'] == 'constant':
            #Use a constant threshold to define spatial coherence.

            inlier_mask = np.abs(tmp_vi - tmp_viprev) < my_conf['constant_consistency_threshold'] 
            outlier_mask = np.logical_not(inlier_mask)


        corrcoef = np.corrcoef(tmp_vi[inlier_mask], tmp_viprev[inlier_mask])[0,1]

        tmp = coherence_index[i,:,k][local_valid_mask]
        tmp[outlier_mask]=tmp[outlier_mask] + 3.0
        coherence_index[i,:,k][local_valid_mask]=tmp

        tmp = coherence_index[iprev,:,kprev][local_valid_mask]
        tmp[outlier_mask] = tmp[outlier_mask] + 3.0
        coherence_index[iprev,:,kprev][local_valid_mask] = tmp

        if corrcoef < my_conf['threshold_corr']:
            coherence_index[i,:,k] = coherence_index[i,:,k] + 3.0
            coherence_index[iprev,:,kprev] = coherence_index[iprev,:,kprev] + 3.0
    else:
        coherence_index[i,:,k] = coherence_index[i,:,k] + 3.0
    return coherence_index

def azimuth_filter(coherence_index, v, undef, i, k):
    na = coherence_index.shape[0]
    if (i > 1) & (i < na-2):
        #If we have in only one ray but not in the neighbors this suggest an interference pattern.
        tmp_mask = np.logical_and(v[i-1,:,k] == undef, v[i+1,:,k] == undef)
        coherence_index[i,:,k][np.logical_and(tmp_mask, v[i,:,k] != undef)] = 10.0

        tmp_mask = np.logical_and(v[i-2,:,k] == undef, v[i+2,:,k] == undef)
        coherence_index[i,:,k][np.logical_and(tmp_mask, v[i,:,k] != undef)] = 10.0

    elif i==na-1:
        tmp_mask = np.logical_and( v[i-1,:,k] == undef , v[0,:,k] == undef)
        coherence_index[i,:,k][np.logical_and(tmp_mask, v[i,:,k] != undef)] = 10.0

        tmp_mask = np.logical_and(v[i-2,:,k] == undef, v[1,:,k] == undef)
        coherence_index[i,:,k][np.logical_and(tmp_mask, v[i,:,k] != undef)] = 10.0

    elif i==na-2:
        tmp_mask = np.logical_and(v[i-1,:,k] == undef, v[i,:,k] == undef)
        coherence_index[i,:,k][np.logical_and(tmp_mask, v[i,:,k] != undef)] = 10.0

        tmp_mask = np.logical_and(v[i-2,:,k] == undef, v[0,:,k] == undef)
        coherence_index[i,:,k][np.logical_and(tmp_mask, v[i,:,k] != undef)] = 10.0

    elif i==0:

        tmp_mask = np.logical_and(v[na-1,:,k] == undef, v[i+1,:,k] == undef)
        coherence_index[i,:,k][np.logical_and(tmp_mask, v[i,:,k] != undef)] = 10.0

        tmp_mask = np.logical_and( v[na-2,:,k] == undef, v[i+2,:,k] == undef)
        coherence_index[i,:,k][np.logical_and(tmp_mask, v[i,:,k] != undef)] = 10.0

    elif i==1:

        tmp_mask = np.logical_and(v[i-1,:,k] == undef, v[i+1,:,k] == undef)
        coherence_index[i,:,k][np.logical_and(tmp_mask, v[i,:,k] != undef)] = 10.0

        tmp_mask = np.logical_and(v[na-1,:,k] == undef, v[i+2,:,k] == undef)
        coherence_index[i,:,k][np.logical_and(tmp_mask, v[i,:,k] != undef)] = 10.0

    return coherence_index

def elevation_filter(coherence_index, v, undef, i, k):
    ne = coherence_index.shape[2]
    if (k > 0) & (k < ne-1):
        tmp_mask = np.logical_and(v[i,:,k-1] == undef, v[i,:,k+1] == undef)
        coherence_index[i,:,k][np.logical_and(tmp_mask, v[i,:,k] != undef)] = 10.0

    if (k == 0):
        tmp_mask = np.logical_and(v[i,:,k+2] == undef, v[i,:,k+1] == undef)
        coherence_index[i,:,k][np.logical_and(tmp_mask, v[i,:,k] != undef)] = 10.0

    if (k == ne-1):
        tmp_mask = np.logical_and(v[i,:,k-2] == undef, v[i,:,k-1] == undef)
        coherence_index[i,:,k][np.logical_and(tmp_mask, v[i,:,k] != undef)] = 10.0

    return coherence_index

def range_filter(coherence_index, v, undef, i, k):
    nr = coherence_index.shape[1]
    if (i > 0) & (i < nr-1):
        tmp_mask = np.logical_and(v[:,i-1,k] == undef, v[:,i+1,k] == undef)
        coherence_index[:,i,k][np.logical_and(tmp_mask, v[:,i,k] != undef)] = 10.0

    if (i == 0):
        tmp_mask = np.logical_and(v[:,i+2,k] == undef, v[:,i+1,k] == undef)
        coherence_index[:,i,k][np.logical_and(tmp_mask, v[:,i,k] != undef)] = 10.0

    if (i == nr-1):
        tmp_mask = np.logical_and(v[:,i-2,k] == undef, v[:,i-1,k] == undef)
        coherence_index[:,i,k][np.logical_and(tmp_mask, v[:,i,k] != undef)] = 10.0

    return coherence_index
