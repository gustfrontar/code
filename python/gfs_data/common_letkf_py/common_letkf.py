import numpy as np
from scipy.linalg import eigh

def letkf_core(ne, nobsl, hdxb, rloc, dep, parm_infl, minfl=0.0):
    """
    LETKF Core Algorithm
    
    Parameters:
        ne: ensemble size
        nobsl: number of observations
        hdxb: observation operator times forecast ensemble perturbations (nobsl x ne)
        rloc: localization weighting function (nobsl)
        dep: observation departure (yo-Hxb) (nobsl)
        parm_infl: covariance inflation parameter
        minfl: minimum covariance inflation parameter
        
    Returns:
        trans: transformation matrix (ne x ne)
        transm: transformation matrix mean (ne)
        pao: analysis covariance matrix in ensemble space (ne x ne)
        parm_infl: updated inflation parameter
    """
    
    if nobsl == 0:
        trans = np.eye(ne) * np.sqrt(parm_infl)
        transm = np.zeros(ne)
        pao = np.eye(ne) * (parm_infl / (ne - 1))
        return trans, transm, pao, parm_infl
    
    # hdxb Rinv
    hdxb_rinv = hdxb / rloc[:, np.newaxis]
    
    # hdxb^T Rinv hdxb
    work1 = hdxb_rinv.T @ hdxb
    
    # Apply minimum inflation if needed
    if minfl > 0.0 and parm_infl < minfl:
        parm_infl = minfl
    
    rho = 1.0 / parm_infl
    
    # hdxb^T Rinv hdxb + (m-1)I / rho
    work1 += np.eye(ne) * (ne - 1) * rho
    
    # Eigen decomposition
    eival, eivec = eigh(work1)
    
    # Pa = [hdxb^T Rinv hdxb + (m-1)I]^-1
    work1 = eivec / eival
    pa = work1 @ eivec.T
    
    # Pa hdxb_rinv^T
    work2 = pa @ hdxb_rinv.T
    
    # Pa hdxb_rinv^T dep
    work3 = work2 @ dep
    
    # T = sqrt[(m-1)Pa]
    rho = np.sqrt((ne - 1) / eival)
    work1 = eivec * rho
    trans = work1 @ eivec.T
    
    # Return values
    transm = work3
    pao = pa
    
    return trans, transm, pao, parm_infl

def weight_RTPS(ne, relax_alpha_spread, w, pa, xb):
    """
    Relaxation via LETKF weight - RTPS method
    
    Parameters:
        ne: ensemble size
        relax_alpha_spread: relaxation parameter
        w: weight matrix
        pa: analysis covariance matrix
        xb: background ensemble
        
    Returns:
        wrlx: relaxed weights
    """
    var_g = np.sum(xb**2)
    var_a = xb.T @ pa @ xb
    
    if var_g > 0.0 and var_a > 0.0:
        infl = relax_alpha_spread * np.sqrt(var_g / (var_a * (ne - 1))) - relax_alpha_spread + 1.0
        wrlx = w * infl
    else:
        wrlx = w
    
    return wrlx

def weight_RTPP(ne, relax_alpha, w):
    """
    Relaxation via LETKF weight - RTPP method
    
    Parameters:
        ne: ensemble size
        relax_alpha: relaxation parameter
        w: weight matrix
        
    Returns:
        wrlx: relaxed weights
    """
    wrlx = (1.0 - relax_alpha) * w
    np.fill_diagonal(wrlx, wrlx.diagonal() + relax_alpha)
    return wrlx

def weight_EPES(ne, relax_alpha_spreadw, w):
    """
    Relaxation via LETKF weight - EPES method
    
    Parameters:
        ne: ensemble size
        relax_alpha_spreadw: relaxation parameter
        w: weight matrix
        
    Returns:
        wrlx: relaxed weights
    """
    if relax_alpha_spreadw == 0.0:
        return w.copy()
    
    # Remove column means
    w_mean = np.mean(w, axis=0)
    wrlx = w - w_mean
    
    # Compute column variances
    w_var = np.var(wrlx, axis=0, ddof=1)
    
    # Compute inflation factor
    infl = relax_alpha_spreadw * np.sqrt(1 / np.sum(w_var))
    
    wrlx = infl * wrlx
    return wrlx

def weight_modEPES(ne, relax_alpha_spreadw, w):
    """
    Relaxation via LETKF weight - modified EPES method
    
    Parameters:
        ne: ensemble size
        relax_alpha_spreadw: relaxation parameter
        w: weight matrix
        
    Returns:
        wrlx: relaxed weights
    """
    if relax_alpha_spreadw == 0.0:
        return w.copy()
    
    # Remove column means
    w_mean = np.mean(w, axis=0)
    wrlx = w - w_mean
    
    # Compute trace
    w_trace = np.trace(wrlx)
    
    # Compute inflation factor
    infl = ne / w_trace
    
    wrlx = infl * wrlx
    return wrlximport numpy as np
from scipy.linalg import eigh

def letkf_core(ne, nobsl, hdxb, rloc, dep, parm_infl, minfl=0.0):
    """
    LETKF Core Algorithm
    
    Parameters:
        ne: ensemble size
        nobsl: number of observations
        hdxb: observation operator times forecast ensemble perturbations (nobsl x ne)
        rloc: localization weighting function (nobsl)
        dep: observation departure (yo-Hxb) (nobsl)
        parm_infl: covariance inflation parameter
        minfl: minimum covariance inflation parameter
        
    Returns:
        trans: transformation matrix (ne x ne)
        transm: transformation matrix mean (ne)
        pao: analysis covariance matrix in ensemble space (ne x ne)
        parm_infl: updated inflation parameter
    """
    
    if nobsl == 0:
        trans = np.eye(ne) * np.sqrt(parm_infl)
        transm = np.zeros(ne)
        pao = np.eye(ne) * (parm_infl / (ne - 1))
        return trans, transm, pao, parm_infl
    
    # hdxb Rinv
    hdxb_rinv = hdxb / rloc[:, np.newaxis]
    
    # hdxb^T Rinv hdxb
    work1 = hdxb_rinv.T @ hdxb
    
    # Apply minimum inflation if needed
    if minfl > 0.0 and parm_infl < minfl:
        parm_infl = minfl
    
    rho = 1.0 / parm_infl
    
    # hdxb^T Rinv hdxb + (m-1)I / rho
    work1 += np.eye(ne) * (ne - 1) * rho
    
    # Eigen decomposition
    eival, eivec = eigh(work1)
    
    # Pa = [hdxb^T Rinv hdxb + (m-1)I]^-1
    work1 = eivec / eival
    pa = work1 @ eivec.T
    
    # Pa hdxb_rinv^T
    work2 = pa @ hdxb_rinv.T
    
    # Pa hdxb_rinv^T dep
    work3 = work2 @ dep
    
    # T = sqrt[(m-1)Pa]
    rho = np.sqrt((ne - 1) / eival)
    work1 = eivec * rho
    trans = work1 @ eivec.T
    
    # Return values
    transm = work3
    pao = pa
    
    return trans, transm, pao, parm_infl

def weight_RTPS(ne, relax_alpha_spread, w, pa, xb):
    """
    Relaxation via LETKF weight - RTPS method
    
    Parameters:
        ne: ensemble size
        relax_alpha_spread: relaxation parameter
        w: weight matrix
        pa: analysis covariance matrix
        xb: background ensemble
        
    Returns:
        wrlx: relaxed weights
    """
    var_g = np.sum(xb**2)
    var_a = xb.T @ pa @ xb
    
    if var_g > 0.0 and var_a > 0.0:
        infl = relax_alpha_spread * np.sqrt(var_g / (var_a * (ne - 1))) - relax_alpha_spread + 1.0
        wrlx = w * infl
    else:
        wrlx = w
    
    return wrlx

def weight_RTPP(ne, relax_alpha, w):
    """
    Relaxation via LETKF weight - RTPP method
    
    Parameters:
        ne: ensemble size
        relax_alpha: relaxation parameter
        w: weight matrix
        
    Returns:
        wrlx: relaxed weights
    """
    wrlx = (1.0 - relax_alpha) * w
    np.fill_diagonal(wrlx, wrlx.diagonal() + relax_alpha)
    return wrlx

def weight_EPES(ne, relax_alpha_spreadw, w):
    """
    Relaxation via LETKF weight - EPES method
    
    Parameters:
        ne: ensemble size
        relax_alpha_spreadw: relaxation parameter
        w: weight matrix
        
    Returns:
        wrlx: relaxed weights
    """
    if relax_alpha_spreadw == 0.0:
        return w.copy()
    
    # Remove column means
    w_mean = np.mean(w, axis=0)
    wrlx = w - w_mean
    
    # Compute column variances
    w_var = np.var(wrlx, axis=0, ddof=1)
    
    # Compute inflation factor
    infl = relax_alpha_spreadw * np.sqrt(1 / np.sum(w_var))
    
    wrlx = infl * wrlx
    return wrlx

def weight_modEPES(ne, relax_alpha_spreadw, w):
    """
    Relaxation via LETKF weight - modified EPES method
    
    Parameters:
        ne: ensemble size
        relax_alpha_spreadw: relaxation parameter
        w: weight matrix
        
    Returns:
        wrlx: relaxed weights
    """
    if relax_alpha_spreadw == 0.0:
        return w.copy()
    
    # Remove column means
    w_mean = np.mean(w, axis=0)
    wrlx = w - w_mean
    
    # Compute trace
    w_trace = np.trace(wrlx)
    
    # Compute inflation factor
    infl = ne / w_trace
    
    wrlx = infl * wrlx
    return wrlx


