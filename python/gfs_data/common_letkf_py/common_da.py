import numpy as np
from numba import jit  # For performance optimization
from typing import Tuple

var_index = {}  #Empty dictionary will be defined in the calling script.

def model_to_obs( obs_list , model_data , model_grid ) -> Tuple[np.ndarray, np.ndarray]:
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

@jit(nopython=True)
def simple_letkf( xf : np.ndarray , Y : np.ndarray , yo : np.ndarray ,
                     model_grid : tuple , obs_list : list , 
                     locs: np.ndarray, infl = 1.0 ) -> np.ndarray:
    """
    Simple LETKF implementation with localization
    
    Args:
        xf: Prior state (nx, ny, nz, nbv, nvar)
        Y: Prior state in observation space (nobs,nbv)
        yo: Observations (nobs)
        model_grid: tuple containing regular grid coordinates
        obs_list: list of tuples containing observation properties. 
        locs: Localization scales in x, y, z
        oerr: Observation errors (nobs)
        infl : Multiplicative inflation factor    
    Returns:
        xa: Analysis state (nx, ny, nz, nbv, nvar)
    """
    #Get the grid dimensions
    nx , ny , nz , nbv , nvar = np.shape( xf )
    
    nobs = len( obs_list )
    
    # Initialize outputs
    xa = np.zeros_like(xf)  #Arrau tp store the analysis ensemble.
    
    # Compute ensemble mean and perturbations in observation space
    Ymean = np.mean( Y , axis=1 )
    Ypert = Y - Ymean[:, np.newaxis]
    # Compute the ensemble mean observation departure
    dep = yo - Ymean    
    
    # Compute ensemble mean and perturbations in state space
    xfmean = np.mean(xf, axis=3)
    xfpert = xf - xfmean[:,:,:,np.newaxis,:]
    
    # Main assimilation loop
    #Iterate over x, y and z. 
    for ix in range(nx):
        print(f"Processing ix = {ix+1}/{nx}")
        for iy in range(ny):
            for iz in range(nz):
                
                # Compute localization weights
                rloc = simple_loc(model_grid[0][ix, iy, iz],model_grid[1][ix, iy, iz],model_grid[2][ix, iy, iz] 
                           , obs_list , locs, rloc)
                
                # Perform LETKF update
                wa, wamean, pa = letkf_core(Ypert , rloc, dep , infl , minfl=1.0)
                
                # Apply weights to update state
                for iv in range(nvar):
                    xa[ix, iy, iz, :, iv] = xfmean[ix,iy,iz,iv]
                    for im in range(nbv):
                        for im2 in range(nbv):
                            xa[ix, iy, iz, im, iv] += (
                                xfpert[ix,iy,iz,im2, iv] * (wa[im2, im] + wamean[im2])
                            )
    return xa

@jit(nopython=True)
def simple_loc(glx: int, gly: int, glz: int, 
               obs_list : list , locs: np.ndarray ) -> np.array :
    """
    Compute simple localization weights
    
    Args:
        glx, gly, glz: Grid point location
        olx, oly, olz: Observation locations (nobs)
        locs: Localization scales in x, y, z
        nobs: Number of observations
        rloc: Output localization weights (nobs)
    """
    rloc = np.ones( len(obs_list) )   #Localized observation error
    for iobs , observation in enumerate( obs_list ):
        dist = 0.0
        if locs[0] > 0.0:
            dist += ((glx - observation[1])/locs[0])**2
        if locs[1] > 0.0:
            dist += ((gly - observation[2])/locs[1])**2
        if locs[2] > 0.0:
            dist += ((glz - observation[3])/locs[2])**2
        
        rloc[iobs] = obs_list[iobs][5] / np.exp(-0.5 * dist)
    return rloc 
        
def letkf_core( Ypert , rloc, dep, parm_infl, minfl=0.0):
    """
    LETKF Core Algorithm
    
    Parameters:
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
    nobsl , ne = np.shape(Ypert)
    
    if nobsl == 0:
        trans = np.eye(ne) * np.sqrt(parm_infl)
        transm = np.zeros(ne)
        pao = np.eye(ne) * (parm_infl / (ne - 1))
        return trans, transm, pao, parm_infl
    
    # hdxb Rinv
    Ypert_rinv = Ypert / rloc[:, np.newaxis]
    
    # hdxb^T Rinv hdxb
    work1 = Ypert_rinv.T @ Ypert
    
    # Apply minimum inflation if needed
    if minfl > 0.0 and parm_infl < minfl:
        parm_infl = minfl
    
    rho = 1.0 / parm_infl
    
    # hdxb^T Rinv hdxb + (m-1)I / rho
    work1 += np.eye(ne) * (ne - 1) * rho
    
    # Eigen decomposition
    eival, eivec = np.linalg.eig(work1)
    
    # Pa = [hdxb^T Rinv hdxb + (m-1)I]^-1
    work1 = eivec / eival
    pa = work1 @ eivec.T
    
    # Pa hdxb_rinv^T
    work2 = pa @ Ypert_rinv.T
    
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
    cf = 1.0e18 * 720 / (pip * (nor**0.75) * (ror**-1.75) )
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
