import numpy as np
import pyart
#rom utils import parseTime
import pickle
from datetime import datetime
import gc

def parseTime(radar):
    timeSTR=radar.time['units'].split()[-1]
    ano,mes,resto=timeSTR.split("-")
    dia,resto=resto.split("T")
    hora,minuto,segundo=resto.split(":")
    segundo=segundo[0:-1]
    Dtime=datetime.strptime(timeSTR,"%Y-%m-%dT%H:%M:%SZ")
    return {"ano":ano,"mes":mes,"dia":dia,"hora":hora,"min":minuto,"seg":segundo,"datetime":Dtime}


def cappi(radar, config, filename=None):
    date = parseTime(radar)
    #str_date = date['datetime'].strftime('%Y%m%d_%H%M%S')
    gatefilter = pyart.filters.GateFilter(radar)
    gatefilter.exclude_below(config['QC']['name_ref'],-998)
    grid = pyart.map.grid_from_radars((radar,),
        gatefilters=(gatefilter, ),
        grid_shape=(int(config['CAPPI']['nz']),int(config['CAPPI']['nx']),int(config['CAPPI']['ny'])),
        grid_limits=((float(config['CAPPI']['grid_limits_z_i']),float(config['CAPPI']['grid_limits_z_s'])),
                     (float(config['CAPPI']['grid_limits_x_o']),float(config['CAPPI']['grid_limits_x_e'])),
                     (float(config['CAPPI']['grid_limits_y_o']),float(config['CAPPI']['grid_limits_y_e']))),
        grid_projection={'proj': 'tmerc', '_include_lon_0_lat_0': True},
        grid_origin=(radar.latitude['data'][0],radar.longitude['data'][0]),
        gridding_algo='map_gates_to_grid',
        fields=[str(config['QC']['name_ref'])], roi_func=str(config['CAPPI']['roi_func']), min_radius=float(config['CAPPI']['min_rad']),
               z_factor=float(config['CAPPI']['z_f']),xy_factor=float(config['CAPPI']['xy_f']))

    lons, lats = grid.get_point_longitude_latitude(edges=False)
    cappi_2km=grid.fields[str(config['QC']['name_ref'])]['data'][0]

    cappi_2km={"data":cappi_2km.filled(fill_value=float(config['CAPPI']['UNDEF'])),
               "lons":lons,
               "lats":lats,
               "date":date,
               "lat_radar":radar.latitude['data'][0], #solo esto no saco de grid
               "lon_radar":radar.longitude['data'][0],
               "grid":grid}

    if filename :
         with open(filename, 'wb') as handle:
            pickle.dump(cappi_2km, handle)
    gc.collect()

    return cappi_2km

