# %%
import xarray as xr
import cfgrib
import numpy as np
from datetime import datetime
import glob

def gfs_ensemble_grib_to_netcdf(input_file, output_file, 
                              bbox=None, variables=None, 
                              levels=None,scoarsen=2,
                              ) :

    
    # Open the GRIB file using cfgrib
    try:
        # For ensemble data, we need to open with the ensemble dimension
        ds = xr.open_dataset(input_file, engine='cfgrib', 
                            backend_kwargs={'filter_by_keys': {'typeOfLevel': 'isobaricInhPa'}})
    except Exception as e:
        print(f"Error opening GRIB file: {e}")
        return

    # Print available variables for reference
    print("Available variables:", list(ds.data_vars.keys()))
    
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
        
        ds.coarsen(x=scoarsen).mean().coarsen(y=scoarsen).mean()

    # Add metadata
    ds.attrs['history'] = f"Processed by GFS ensemble converter on {datetime.now()}"
    ds.attrs['source'] = input_file
    
    # Save to NetCDF
    try:
        ds.to_netcdf(output_file)
        print(f"Successfully saved subset to {output_file}")
    except Exception as e:
        print(f"Error saving NetCDF file: {e}")
        

def merge_gfs_ensemble(grib_files, members , lead_times , output_nc, 
                      bbox=None, variables=None, 
                      levels=None,scoarsen=None):
    """
    Merge multiple GFS ensemble members and lead times into a single NetCDF file.
    
    Parameters:
        grib_files (list): List of GRIB file paths or glob pattern
        output_nc (str): Output NetCDF file path
        bbox (tuple): (min_lon, min_lat, max_lon, max_lat) for spatial subset
        variables (list): Variables to extract (None for all)
        levels (list): Pressure levels to extract (None for all)
        time_dim (str): Time dimension name ('step' or 'time')
    """
    try:
        
        # Open each file and combine into a single dataset
        ds_list = []
        for ii , file in enumerate( grib_files ):
            print(file)
            try:
                # Open with appropriate filters
                backend_kwargs = {
                    'filter_by_keys': {
                        'typeOfLevel': 'isobaricInhPa' if levels else None,
                        'level': levels
                    }
                }
                
                # Open single file
                ds = xr.open_dataset(file, engine='cfgrib', 
                                    backend_kwargs=backend_kwargs)
                # Apply subsetting
                if variables is not None:
                    ds = ds[variables]
                if levels is not None and 'isobaricInhPa' in ds.dims:
                    ds = ds.sel(isobaricInhPa=levels)
                # Apply spatial subset if requested
                if bbox:
                    min_lon, min_lat, max_lon, max_lat = bbox
                    # Handle longitude wrapping
                    if min_lon < 0:
                        min_lon += 360
                    if max_lon < 0:
                        max_lon += 360
            
                    ds = ds.where(
                        (ds.longitude >= min_lon) & 
                        (ds.longitude <= max_lon) & 
                        (ds.latitude >= min_lat) & 
                        (ds.latitude <= max_lat),
                        drop=True
                        )
                if scoarsen is not None :
                    ds = ds.coarsen(x=scoarsen).mean().coarsen(y=scoarsen).mean()
                
                # Extract member number from filename if not in data
                if 'member' not in ds.dims :
                    ds = ds.expand_dims({'member': [int( members[ii] )]})
                if 'lead_time' not in ds.dims :
                    ds = ds.expand_dims({'lead_time':[int( lead_times[ii] )]})
                
                ds_list.append(ds)
                
            except Exception as e:
                print(f"Error processing {file}: {e}")
                continue
        
        if not ds_list:
            raise ValueError("No valid GRIB files processed")
        
        # Combine all datasets along ensemble and time dimensions
        print("Merging datasets...")
        combined = xr.combine_by_coords(
            ds_list,
            combine_attrs='drop_conflicts'
        )
        


        # Add metadata
        #combined.attrs['history'] = f"Merged by GFS processor on {datetime.now()}"
        #combined.attrs['source_files'] = grib_files
        
        # Save to NetCDF with compression
        #encoding = {var: {'zlib': True, 'complevel': 4} for var in combined.data_vars}
        print(f"Saving to {output_nc}...")
        combined.to_netcdf(output_nc, mode = 'w',format='NETCDF4')
        print("Successfully saved merged ensemble data")
        
        return combined
    
    except Exception as e:
        print(f"Error in merge_gfs_ensemble: {e}")
        raise

        



# %%
# Example usage
ens_size = 30
date = '20231216'
init_time = '12'
data_path = '/home/jruiz/datosdemerzel/GFSDATA/gefs.' + date +'/' + init_time + '/pgrb2b/'
output_file = data_path + 'gfs_ens.nc'
scoarsen = 2 #Spatial coarsening factor (to reduce grid size)
ens_members = np.arange(0,ens_size+1).astype(str)    #.zfill(3)
print(ens_members)

#if __name__ == "__main__":
# Input parameters
levels = [1000.,  900.,  800.,  700.,  600.,  500.,  400.,  300.]
# Define subset parameters
bbox = (-80, -60, -30, 0 )   # Continental US approx
variables = ['t', 'q', 'u', 'v' , 'gh']  # Temperature, humidity, wind components

file_list = []
members_list = []
lead_time_list = [] 
for imem in ens_members :
    print('My member is ',imem)
    str_mem = imem.zfill(3)
    file_list += glob.glob( data_path + str_mem + '/*.pgrb2.*[!idx][!nc]' ) 
        
    members_list = []
    lead_time_list = []
    for my_file in file_list :
        members_list.append( str_mem )
        lead_time_list.append( int(my_file[-3:]) )
        print(members_list[-1],lead_time_list[-1])
    
    
# Run the merge
combined_ds = merge_gfs_ensemble(
    file_list , members_list , lead_time_list ,
    output_file,
    bbox=bbox,
    variables=variables,
    levels=levels, scoarsen = scoarsen 
)


print(combined_ds)
    
    


