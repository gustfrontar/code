from filters.attenuation_filter import AttenuationFilter
from filters.blocking_filter import BlockingFilter
from filters.dealiasing import DealiasingFilter
from filters.dealiasing_edge_filter import DealiasingEdgeFilter
from filters.doppler_filter import DopplerSpeckleFilter, DopplerNoiseFilter, DopplerLocalStdFilter, DopplerSpatialCoherenceFilter, LowDopplerFilter,DopplerRefFilter
from filters.echo_top_filter import EchoTopFilter
from filters.echo_filter import EchoDepthFilter
from filters.interference_filter import InterferenceFilter
from filters.low_elevation_filter import LowElevFilter
from filters.missing_ref_filter import MissingRefFilter
from filters.power_filter import PowerFilter
from filters.range_filter import RefRangeFilter, DopplerRangeFilter
from filters.ref_speckle_filter import RefSpeckleFilter
from filters.rho_filter import RhoFilter
from filters.texture_filter import ReflectivityTextureFilter, DopplerTextureFilter
from utils.super_radar import SuperRadar
import numpy as np
from datetime import datetime
import os
from utils import save_cfradial

def run_filters(filter_name, super_radar, radar, output, opt):
    """ Esta función corre un filtro dado su nombre

        Parámetros
        ----------
        filter_name : str
            Nombre del filtro
        super_radar : object
            Objeto radar extendido
        radar : object
            Objeto radar original a actualizar
        output : dict
            Diccionario con los campos corregidos
        opt : object
            Clase de configuración

        Salida
        ------
        radar : object
            Objeto radar actualizado
        output : dict
            Diccionario con los campos corregidos
    """

    try:
        # Preparamos el filtro a correr
        cmdstr="filtro = {}(super_radar, opt, output)".format(filter_name)
        l = locals()
        exec(cmdstr, globals(), l)
        filtro = l['filtro']
        # Corremos el filtro
        filtro.run()
        # Si se pudo correr actualizo los campos
        if filtro.filter_run:
            #Actualizo el radar
            radar = filtro.update_radar(radar)
            # Actualizo output
            output = filtro.output
            if opt.filters_plot:
                if ('ref' in filtro.var_update) and (opt.name_ref in radar.fields):
                    filtro.plot_ref(filter_name)
                if ('v' in filtro.var_update) and (opt.name_v in radar.fields):
                    filtro.plot_vel(filter_name)
        return(radar, output)
    except Exception as e:
        print("Corriendo {} en el radar {} me encontre con {}".format(filter_name, radar.metadata['instrument_name'], e))
        return(radar, output)



def run_all_filters(radar, opt, vol_date=None):
    """
    Esta función corre todos los filtros en un orden asignado

    Parametros
    ----------
    radar : objeto
        Objeto radar a actualizar
    opt : objeto
        Clase con las configuraciones
    vol_date : tupla (opc)
        Tupla con fecha de inicio del escaneo y fecha de final.

    Salida
    ------
    cradar : object
        Objeto radar con los campos corregidos
    output : dict
        Diccionario con los campos corregidos

  """
    filter_order = opt.filters
    output = None
    super_radar = SuperRadar(radar, opt)
    for filter_name in filter_order:
        cradar, output = run_filters(filter_name, super_radar, radar, output, opt)
    if opt.save_netcdf:
        # chequear si existe el path de guardado, sino lo creo
        if not os.path.exists(opt.netcdf_output_path):
            os.makedirs(opt.netcdf_output_path)
        # Guardar el archivo
        save_cfradial(opt.netcdf_output_path, cradar, vol_date)
    return cradar, output        

