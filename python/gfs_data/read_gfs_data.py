# %%
import xarray as xr
import cfgrib
import numpy as np
from datetime import datetime
import glob

def gfs_ensemble_grib_to_netcdf(input_file, output_file, 
                              bbox=None, variables=None, 
                              levels=None
                              ):
    """
    Convert GFS ensemble GRIB data to a subset NetCDF file with vertical level filtering.
    
    Parameters:
        input_file (str): Path to input GRIB file
        output_file (str): Path for output NetCDF file
        bbox (tuple): (min_lon, min_lat, max_lon, max_lat) for spatial subset
        variables (list): List of variable names to include
        levels (list): List of isobaric levels to include (in hPa)
        ensemble_members (list): List of ensemble member indices to include
        time_indices (list): List of time indices to include
    """
    
    # Open the GRIB file using cfgrib
    try:
        # Open with filters for pressure levels
        backend_kwargs = {
            'filter_by_keys': {
                'typeOfLevel': 'isobaricInhPa',
                'level': levels if levels else None
            }
        }
        
        # For ensemble data, we might need to handle multiple messages
        ds = xr.open_dataset(input_file, engine='cfgrib', 
                            backend_kwargs=backend_kwargs)
        
        # Some GRIB files need to be opened as a dataset collection
        if 'isobaricInhPa' not in ds.dims:
            ds = xr.open_mfdataset([input_file], engine='cfgrib', 
                                 combine='by_coords',
                                 backend_kwargs=backend_kwargs)
            
    except Exception as e:
        print(f"Error opening GRIB file: {e}")
        return

    # Print available variables and levels for reference
    print("Available variables:", list(ds.data_vars.keys()))
    if 'isobaricInhPa' in ds.dims:
        print("Available levels:", ds.isobaricInhPa.values)
    
    # Apply subsetting
    if variables is not None:
        ds = ds[variables]
    
    if levels is not None and 'isobaricInhPa' in ds.dims:
        ds = ds.sel(isobaricInhPa=levels)
    
    
    if bbox is not None:
        min_lon, min_lat, max_lon, max_lat = bbox
        # Handle longitude wrapping if needed
        if min_lon < 0:
            min_lon += 360
        if max_lon < 0:
            max_lon += 360
            
        # Select spatial subset
        ds = ds.where(
            (ds.longitude >= min_lon) & 
            (ds.longitude <= max_lon) & 
            (ds.latitude >= min_lat) & 
            (ds.latitude <= max_lat),
            drop=True
        )
    
    # Add metadata
    ds.attrs['history'] = f"Processed by GFS ensemble converter on {datetime.now()}"
    ds.attrs['source'] = input_file
    
    # Save to NetCDF
    try:
        encoding = {var: {'zlib': True, 'complevel': 1} for var in ds.data_vars}
        ds.to_netcdf(output_file, encoding=encoding)
        print(f"Successfully saved subset to {output_file}")
    except Exception as e:
        print(f"Error saving NetCDF file: {e}")



# %%
# Example usage
ens_size = 30
data_path = '/home/jruiz/datosdemerzel/GFSDATA/gefs.20231216/00/pgrb2b/'
ens_members = np.arange(0,ens_size+1).astype(str)    #.zfill(3)
print(ens_members)

#if __name__ == "__main__":
# Input parameters
input_grib = "gfs_ensemble.grib2"  # Replace with your GRIB file
output_nc = "gfs_subset.nc"
levels = [1000.,  900.,  800.,  700.,  600.,  500.,  400.,  300.]
# Define subset parameters
bbox = (-75, -60, -40, -20 )   # Continental US approx
variables = ['t', 'q', 'u', 'v' , 'gh']  # Temperature, humidity, wind components
ensemble_members = None #[0, 1, 2, 3]  # First few ensemble members
time_indices = [0]  # First time step
    
for imem in ens_members :
    print('My member is ',imem)
    str_mem = imem.zfill(3)
    file_list = glob.glob( data_path + str_mem + '/*.pgrb2.*' )
    print( data_path + str_mem + '/*.pgrb2.*' )
    
    print('Files to be processed: ' , file_list )
    
    for input_grib in file_list :
        output_nc = input_grib + '.nc'
        print('Converting ' + input_grib + ' into ' + output_nc)
        # Run the conversion
        gfs_ensemble_grib_to_netcdf(
            input_grib, output_nc,
            bbox=bbox,
            variables=variables,
            levels = levels 
        )
    
    


