import numpy as np
from numba import jit  # For performance optimization
from typing import Tuple

def simple_letkf_wloc(nx: int, ny: int, nz: int, nbv: int, nvar: int, nobs: int,
                     hxf: np.ndarray, xf: np.ndarray, dep: np.ndarray,
                     ox: np.ndarray, oy: np.ndarray, oz: np.ndarray,
                     locs: np.ndarray, oerr: np.ndarray) -> np.ndarray:
    """
    Simple LETKF implementation with localization
    
    Args:
        nx, ny, nz: Grid dimensions
        nbv: Number of ensemble members
        nvar: Number of variables to update
        nobs: Number of observations
        hxf: Ensemble forecast in observation space (nobs, nbv)
        xf: Prior state (nx, ny, nz, nbv, nvar)
        dep: Observation departures (nobs)
        ox, oy, oz: Observation locations (nobs)
        locs: Localization scales in x, y, z
        oerr: Observation errors (nobs)
        
    Returns:
        xa: Analysis state (nx, ny, nz, nbv, nvar)
    """
    # Initialize outputs
    xa = np.zeros_like(xf)
    rloc = np.ones(nobs, dtype=np.float64)
    infl = 1.0
    
    # Convert to double precision
    oerr_rsize = oerr.astype(np.float64)
    dep_rsize = dep.astype(np.float64)
    
    # Compute ensemble mean and perturbations in observation space
    hxfmean = np.mean(hxf, axis=1, dtype=np.float64)
    hxfpert = hxf - hxfmean[:, np.newaxis]
    
    # Main assimilation loop
    for ix in range(nx):
        print(f"Processing ix = {ix+1}/{nx}")
        for iy in range(ny):
            for iz in range(nz):
                # Compute ensemble mean and perturbations in state space
                xfmean = np.mean(xf[ix, iy, iz], axis=0, dtype=np.float64)
                xfpert = xf[ix, iy, iz] - xfmean[np.newaxis, :]
                
                # Compute localization weights
                simple_loc(ix, iy, iz, ox, oy, oz, locs, nobs, rloc)
                rloc[:] = oerr_rsize / rloc
                
                # Perform LETKF update
                wa, wamean, pa = letkf_core(nbv, nobs, hxfpert, rloc, 
                                           dep_rsize, infl, minfl=1.0)
                
                # Apply weights to update state
                for iv in range(nvar):
                    xa[ix, iy, iz, :, iv] = xfmean[iv]
                    for im in range(nbv):
                        for im2 in range(nbv):
                            xa[ix, iy, iz, im, iv] += (
                                xfpert[im2, iv] * (wa[im2, im] + wamean[im2])
                            )
    return xa

@jit(nopython=True)
def simple_loc(glx: int, gly: int, glz: int, 
              olx: np.ndarray, oly: np.ndarray, olz: np.ndarray,
              locs: np.ndarray, nobs: int, rloc: np.ndarray) -> None:
    """
    Compute simple localization weights
    
    Args:
        glx, gly, glz: Grid point location
        olx, oly, olz: Observation locations (nobs)
        locs: Localization scales in x, y, z
        nobs: Number of observations
        rloc: Output localization weights (nobs)
    """
    for iobs in range(nobs):
        dist = 0.0
        if locs[0] > 0.0:
            dist += ((glx - olx[iobs])/locs[0])**2
        if locs[1] > 0.0:
            dist += ((gly - oly[iobs])/locs[1])**2
        if locs[2] > 0.0:
            dist += ((glz - olz[iobs])/locs[2])**2
        
        rloc[iobs] = np.exp(-0.5 * dist)

def calc_ref_ens(nx: int, ny: int, nz: int, nbv: int,
                qrens: np.ndarray, qsens: np.ndarray, qgens: np.ndarray,
                tens: np.ndarray, pens: np.ndarray) -> np.ndarray:
    """
    Compute reflectivity for an ensemble
    
    Args:
        nx, ny, nz: Grid dimensions
        nbv: Number of ensemble members
        qrens: Rain water mixing ratio (nx, ny, nz, nbv)
        qsens: Snow mixing ratio (nx, ny, nz, nbv)
        qgens: Graupel mixing ratio (nx, ny, nz, nbv)
        tens: Temperature (nx, ny, nz, nbv)
        pens: Pressure (nx, ny, nz, nbv)
        
    Returns:
        refens: Reflectivity (nx, ny, nz, nbv)
    """
    refens = np.zeros_like(qrens)
    
    for ix in range(nx):
        for iy in range(ny):
            for iz in range(nz):
                for im in range(nbv):
                    refens[ix, iy, iz, im] = calc_ref(
                        qrens[ix, iy, iz, im],
                        qsens[ix, iy, iz, im],
                        qgens[ix, iy, iz, im],
                        tens[ix, iy, iz, im],
                        pens[ix, iy, iz, im]
                    )
    return refens

@jit(nopython=True)
def calc_ref(qr: float, qs: float, qg: float, t: float, p: float) -> float:
    """
    Compute radar reflectivity from microphysical quantities
    
    Args:
        qr: Rain water mixing ratio (kg/kg)
        qs: Snow mixing ratio (kg/kg)
        qg: Graupel mixing ratio (kg/kg)
        t: Temperature (K)
        p: Pressure (Pa)
        
    Returns:
        ref: Reflectivity (dBZ)
    """
    # Constants
    nor = 8.0e6      # [m^-4]
    nos = 2.0e6      # [m^-4]
    nog = 4.0e6      # [m^-4]
    ror = 1000.0     # [Kg/m3]
    ros = 100.0      # [Kg/m3]
    rog = 913.0      # [Kg/m3]
    roi = 917.0      # [Kg/m3]
    ki2 = 0.176      # Dielectric factor for ice
    kr2 = 0.930      # Dielectric factor for water
    rd = 287.05      # Gas constant for dry air (J/kg/K)
    mindbz = -20.0   # Minimum dBZ value
    
    # Compute air density
    ro = p / (rd * t)
    
    # Precompute factors
    pip = np.pi ** 1.75
    cf = 1.0e18 * 720 / (pip * (nor**0.75) * (ror**-1.75)
    cf2 = 1.0e18 * 720 * ki2 * (ros**0.25) / (pip * kr2 * (nos**0.75) * (roi**-2))
    cf3 = 1.0e18 * 720 / (pip * (nos**0.75) * (ros**-1.75))
    cf4 = (1.0e18 * 720 / (pip * (nog**0.75) * (rog**-1.75))) ** 0.95
    
    # Initialize reflectivities
    zr, zs, zg = 0.0, 0.0, 0.0
    
    # Rain contribution
    if qr > 0.0:
        zr = cf * ((ro * qr)**1.75)
    
    # Snow contribution
    if qs > 0.0:
        if t <= 273.16:
            zs = cf2 * ((ro * qs)**1.75)  # Dry snow
        else:
            zs = cf3 * ((ro * qs)**1.75)  # Wet snow
    
    # Graupel contribution
    if qg > 0.0:
        zg = cf4 * ((ro * qg)**1.6625)
    
    # Total reflectivity
    ref = zr + zs + zg
    
    # Convert to dBZ
    if ref > 0.0:
        ref = 10.0 * np.log10(ref)
    else:
        ref = mindbz
    
    return max(ref, mindbz)
