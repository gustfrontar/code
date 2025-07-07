import numpy as np

# Constants
sigma = 5.67e-8  # Stefan-Boltzmann constant [W/m²/K⁴]
g = 9.81          # Gravity [m/s²]
cp = 1004.0        # Specific heat capacity of air [J/kg/K]
k = 0.1            # Absorption coefficient [adjustable]

def simple_radiative_transfer(T, q, p_levels):
    """
    A very simple radiative transfer model for longwave radiation.
    
    Args:
        T: Temperature profile [K] (array-like, from surface to TOA)
        q: Specific humidity profile [kg/kg] (same shape as T)
        p_levels: Pressure levels [Pa] (same shape as T)
    
    Returns:
        flux_up: Upward longwave flux at each level [W/m²]
        flux_down: Downward longwave flux at each level [W/m²]
    """
    n_layers = len(T) - 1
    
    # Compute layer optical thickness (simplified: proportional to q and dp)
    dp = np.abs(np.diff(p_levels))
    tau = k * q[:-1] * dp / (g * 100)  # Adjusted for toy model
    
    # Initialize fluxes
    flux_up = np.zeros_like(T)
    flux_down = np.zeros_like(T)
    
    # Surface upward flux = blackbody emission from surface
    flux_up[0] = sigma * T[0]**4
    
    # Upward flux calculation (surface to TOA)
    for i in range(1, n_layers + 1):
        flux_up[i] = flux_up[i-1] * np.exp(-tau[i-1]) + sigma * T[i]**4 * (1 - np.exp(-tau[i-1]))
    
    # Downward flux calculation (TOA to surface)
    flux_down[-1] = 0.0  # No downward flux from space
    for i in range(n_layers - 1, -1, -1):
        flux_down[i] = flux_down[i+1] * np.exp(-tau[i]) + sigma * T[i]**4 * (1 - np.exp(-tau[i]))
    
    return flux_up, flux_down

# Example usage:
if __name__ == "__main__":
    # Example profiles (surface to TOA)
    p_levels = np.array([1000e2, 850e2, 700e2, 500e2, 300e2, 100e2])  # Pressure [Pa]
    T = np.array([300, 280, 260, 240, 220, 200])  # Temperature [K]
    q = np.array([0.018, 0.012, 0.006, 0.001, 0.0001, 0.00001])  # Specific humidity [kg/kg]
    
    flux_up, flux_down = simple_radiative_transfer(T, q, p_levels)
    
    print("Upward flux (W/m²):", flux_up)
    print("Downward flux (W/m²):", flux_down)