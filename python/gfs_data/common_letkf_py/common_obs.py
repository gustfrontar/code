import numpy as np
from typing import Tuple


def model_to_obs( obs_list , model_data , model_grid , var_index ) -> Tuple[np.ndarray, np.ndarray]:
    from scipy.interpolate import interpn

    nobs = len( obs_list )                 #Number of observations
    y = np.zeros( nobs )
    obs_error = np.zeros( nobs )

    #Loop over the observations and define the observation value and observation error based on obs_list. 
    for iobs , observation in enumerate( obs_list )  : 
 
        otype = observation[0]
        oloc = np.array([observation[1],observation[2],observation[3]])
        
        if observation[0] in ['t','u','v','q','gh'] :  #Conventional observation
     
            #Create the pseudo-observation as the mean of the forecast ensemble + the innovation.
            y[iobs] = interpn( model_grid , model_data[:,:,:,var_index[observation[0]] ], oloc )
            obs_error[iobs] = observation[5] 
    
    return y , obs_error 

def add_obs_error(no: int, nens: int, obs: np.ndarray, 
                 obs_error: np.ndarray, obs_bias: np.ndarray) -> np.ndarray:
    """
    Add observation error to synthetic observations.
    
    Args:
        no: Number of observations
        nens: Number of ensemble members
        obs: Observations without error shape (no, nens)
        obs_error: Observation error variance shape (no,)
        obs_bias: Systematic observation error shape (no,)
        
    Returns:
        obsout: Observations with error shape (no, nens)
    """
    tmp_error = np.sqrt(obs_error)
    obsout = obs.copy()
    
    # Generate random numbers reproducibly for each ensemble member
    rng = np.random.RandomState()
    for ie in range(nens):
        rng.seed(ie)
        randomn = rng.randn(no)
        obsout[:, ie] += tmp_error * randomn + obs_bias
    
    return obsout

def get_obs_number(ntype: str, nx: int, nt: int, 
                  time_density: float, space_density: float) -> int:
    """
    Get number of observations based on network type.
    
    Args:
        ntype: Network type ('regular', 'random')
        nx: Number of grid points
        nt: Number of times
        time_density: Temporal observation density
        space_density: Spatial observation density
        
    Returns:
        no: Number of observations
    """
    ntype = ntype.lower()
    if ntype == 'regular':
        skipx = int(1 / space_density)
        skipt = int(1 / time_density)
        tmpx = len(range(0, nx, skipx))
        tmpt = len(range(0, nt, skipt))
        no = tmpx * tmpt
    elif ntype == 'random':
        no = int(nx * space_density) * int(nt * time_density)
    elif ntype == 'fromfile':
        raise NotImplementedError("FROMFILE option is not available yet")
    else:
        raise ValueError(f"Unknown network type: {ntype}")
    
    return no

def get_obs_location(ntype: str, nx: int, nt: int, no: int,
                    time_density: float, space_density: float) -> np.ndarray:
    """
    Generate observation locations based on network type.
    
    Args:
        ntype: Network type ('regular', 'random')
        nx: Number of grid points
        nt: Number of times
        no: Number of observations
        time_density: Temporal observation density
        space_density: Spatial observation density
        
    Returns:
        obsloc: Observation locations shape (no, 2)
    """
    obsloc = np.zeros((no, 2))
    ntype = ntype.lower()
    
    if ntype == 'regular':
        skipx = int(1 / space_density)
        skipt = int(1 / time_density)
        iobs = 0
        for it in range(0, nt, skipt):
            for ix in range(0, nx, skipx):
                obsloc[iobs, 0] = ix
                obsloc[iobs, 1] = it
                iobs += 1
    elif ntype == 'random':
        rng = np.random.RandomState()
        obsloc[:, 0] = rng.rand(no) * nx
        obsloc[:, 1] = rng.rand(no) * (nt - 1) + 1
    elif ntype == 'fromfile':
        raise NotImplementedError("FROMFILE option is not available")
    else:
        raise ValueError(f"Unknown network type: {ntype}")
    
    return obsloc

def bilinear_interpolation(var: np.ndarray, ri: float, rj: float) -> float:
    """
    2D bilinear interpolation.
    
    Args:
        var: Input field shape (2, 2)
        ri: Normalized i position (0-1)
        rj: Normalized j position (0-1)
        
    Returns:
        Interpolated value
    """
    return (var[0, 0] * (1-ri) * (1-rj) +
            var[1, 0] * ri * (1-rj) +
            var[0, 1] * (1-ri) * rj +
            var[1, 1] * ri * rj)
