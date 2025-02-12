import pyart
import os
from datetime import datetime, timedelta
import netCDF4

def save_cfradial(local_path, radar, vol_date=None, fileformat='NETCDF4'):
    """
    Guardar el radar en un cfradial

    Parametros
    ----------
    local_path : str
        camino a la carpeta donde se va a guardar el cfradial
    radar : objeto Radar
        objeto radar que se va a guardar
    vol_date : tupla (opc)
        Tupla con fecha de inicio del escaneo y fecha de final.
    fileformat : str (opc)
        formato del archivo donde se guardará el radar. Default NETCDF4

    """
    os.makedirs(local_path, exist_ok=True)

    radar_id = radar.metadata['instrument_name']
    strategy = radar.instrument_parameters['strategy']
    file_instrument = "{}_{}".format(radar_id, strategy)
    if vol_date is None:
        vol_date_ini = radar.metadata['date']  # ya viene en el formato correcto
        vol_date_fin = netCDF4.num2date(radar.time['data'][-1], radar.time['units'])
        vol_date_fin += (timedelta(seconds=1) - timedelta(microseconds=vol_date_fin.microsecond))
        file_time = f's{vol_date_ini}.e{vol_date_fin:%Y%m%d_%H%M%S}'
    else:
        vol_date_ini = datetime.strptime(vol_date[0], '%Y-%m-%d %H:%M:%S')
        vol_date_fin = datetime.strptime(vol_date[1], '%Y-%m-%d %H:%M:%S')
        file_time = f's{vol_date_ini:%Y%m%d_%H%M%S}.e{vol_date_fin:%Y%m%d_%H%M%S}'

    filename = "{}/cfrad.{}.{}.nc".format(local_path, file_time, file_instrument)

    pyart.io.cfradial.write_cfradial(filename, radar, format=fileformat)
    print('Writing file : ',filename)
