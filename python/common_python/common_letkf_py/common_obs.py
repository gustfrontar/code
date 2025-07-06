import numpy as np
from typing import Tuple

def model_to_obs(nx: int, no: int, nt: int, nens: int, obsloc: np.ndarray, 
                 x: np.ndarray, xloc: np.ndarray, tloc: np.ndarray, 
                 obstype: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
    """
    Observation operator for data assimilation with cyclic boundary conditions.
    
    Args:
        nx: Number of state variables
        no: Number of observations
        nt: Number of times
        nens: Number of ensemble members
        obsloc: Observation locations (space, time) shape (no, 2)
        x: State variables (space, ensemble, time) shape (nx, nens, nt)
        xloc: Space grid point locations shape (nx,)
        tloc: Time grid point locations shape (nt,)
        obstype: Observation type (1=Observe X, 2=Observe X**2) shape (no,)
        
    Returns:
        obs: State in observation space shape (no, nens)
        valid_obs: Mask indicating valid observations (1=valid) shape (no,)
    """
    
    # Initialize outputs
    obs = np.zeros((no, nens))
    valid_obs = np.ones(no, dtype=int)
    
    # Compute grid spacing
    dx = xloc[1] - xloc[0]
    dt = tloc[1] - tloc[0] if nt > 1 else 1.0
    
    # Create extended arrays for cyclic boundary conditions
    tmp_x = np.zeros((nx+2, nens, nt))
    tmp_x[1:-1] = x
    tmp_x[0] = x[-1]  # Cyclic boundary
    tmp_x[-1] = x[0]  # Cyclic boundary
    
    tmp_xloc = np.zeros(nx+2)
    tmp_xloc[1:-1] = xloc
    tmp_xloc[0] = xloc[0] - dx
    tmp_xloc[-1] = xloc[-1] + dx
    
    for io in range(no):
        # Compute normalized positions
        rx = ((obsloc[io, 0] - tmp_xloc[0]) / dx) + 1.0
        ixloc = int(np.floor(rx))
        
        if nt > 1:
            rt = ((obsloc[io, 1] - tloc[0]) / dt) + 1.0
            itloc = int(np.floor(rt))
        else:
            itloc = 0  # Only one time step
            
        # Check if observation is within bounds
        if not (1 <= ixloc <= nx+1 and 0 <= itloc < nt):
            valid_obs[io] = 0
            continue
            
        # Prepare interpolation points
        if rx == ixloc and rt == itloc:
            # Exact grid point match
            tmp_obs = np.tile(tmp_x[ixloc, :, itloc], (2, 2, 1))
        elif rx == ixloc:
            # Spatial match but temporal interpolation
            tmp_obs = np.stack([tmp_x[ixloc, :, itloc], 
                               tmp_x[ixloc, :, itloc+1]], axis=0)
            tmp_obs = np.tile(tmp_obs[:, np.newaxis, :], (1, 2, 1))
        elif rt == itloc:
            # Temporal match but spatial interpolation
            tmp_obs = np.stack([tmp_x[ixloc, :, itloc], 
                               tmp_x[ixloc+1, :, itloc]], axis=0)
            tmp_obs = np.tile(tmp_obs[np.newaxis, :, :], (2, 1, 1))
        else:
            # Full 2D interpolation
            tmp_obs = np.stack([
                np.stack([tmp_x[ixloc, :, itloc], 
                         tmp_x[ixloc, :, itloc+1]], axis=0),
                np.stack([tmp_x[ixloc+1, :, itloc], 
                         tmp_x[ixloc+1, :, itloc+1]], axis=0)
            ], axis=0)
        
        # Compute interpolation weights
        rx_weight = rx - ixloc
        rt_weight = rt - itloc if nt > 1 else 0.0
        
        # Perform interpolation for each ensemble member
        for ie in range(nens):
            if obstype[io] == 1:
                obs_val = bilinear_interpolation(tmp_obs[:, :, ie], 
                                                rx_weight, rt_weight)
            elif obstype[io] == 2:
                obs_val = bilinear_interpolation(tmp_obs[:, :, ie]**2,
                                                rx_weight, rt_weight)
            else:
                raise ValueError("ERROR: Not recognized observation type")
            obs[io, ie] = obs_val
    
    if np.sum(valid_obs) < no:
        print("[Warning]: The number of valid observations is lower than the number of input observations")
    
    return obs, valid_obs

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
