import numpy as np
import pyart
from .base_filter import BaseFilter
from utils.qc_utils  import qc 


class RhoFilter(BaseFilter):
    """
    Filtro basado en el coeficiente de correlación

    Este filtro descarta los valores de rho por debajo de un umbral 
    
    Parámetros
    ----------
    super_radar : object 
        Objeto radar extendido
    opt : object 
        Clase de configuración 
    output : dict 
        Diccionario con los campos corregidos
    """


    def __init__(self, super_radar, opt, output=None):
        BaseFilter.__init__(self, super_radar, opt, output)    
        self.ref = self.input.get_ref()
        self.undef_ref = self.input.get_undef_ref()
        self.var_update = getattr(opt, "RhoFilter")['var_update_list']
        if self.name_rho in super_radar.fields:
            self.rho = self.input.get_rho()
            self.undef_rho = self.input.get_undef_rho()
        else:
            raise KeyError('Could not find ' + self.name_rho + ' data in the radar fields')


    def get_smooth_rho(self, filter_name):
        """Hace un suavizado del campo de correlación"""
        fopt = getattr(self.opt, filter_name)
        na, nr, ne = self.ref.shape        
        return qc.box_functions_2d(datain=self.rho, 
                                   na=na, nr=nr, ne=ne, 
                                   undef=self.undef_rho,
                                   boxx=fopt['nx'], boxy=fopt['ny'], boxz=fopt['nz'],
                                   operation='MEAN', threshold=0.0)
     
    def get_rho_index(self, filter_name):
        """ Genera la matriz de indices con el que se calculará el filtro""" 
        # Obtenemos los campos suavizados de rho y reflectividad
        index = self.get_smooth_rho(filter_name)
        smooth_ref = self.calculate_smooth_ref(self.opt, filter_name)
        # Cambio el valor de los undefs
        index[index == self.undef_rho] = self.opt.undef
        # genero una máscara para mantener las coordenadas con reflectividad alta y 
        # sin valores de reflectividad
        mask = np.logical_or(smooth_ref > getattr(self.opt,filter_name)['ref_threshold'],
                              self.output['cref'] == self.opt.no_rain_ref_val)
        index[mask] = self.opt.undef
        return index
     
     
    def run(self):
        """ Corre el filtro.
            Primero calcula un campo suavizado de rho y reflectividad.
            Enmascara los valores que no serán afectados por el filtro.
            Calcula una matriz de pesos que depende de una serie de valores de correlacion.
            Filtra los valores segun un umbral.
            Actualiza el output y/o el radar."""
        filter_name = "RhoFilter"
     
        if not self.output_init:
            self.init_output(filter_name)    
        elif not 'ref' in self.output:
            self.init_output_ref()
        # genero la matriz de indices con una mascara en los valores que no seran afectados
        # por el filtro
        index = self.get_rho_index(filter_name)
        # Ahora usamos esta matriz de indices para calcular una matriz de pesos.
        # Los pesos se obtienen interpolando la correlacion entre distintos valores de umbrales (ifx)
        # y valores de peso asignados (ify)
        # Luego se actualiza el radar
        self.update(filter_name, index, self.opt)
        self.filter_run = True
