import numpy as np
import pyart
from .base_filter import BaseFilter

class RangeFilter(BaseFilter):
    """ Filtro de rango

    Se definen un valor máximo y mínimo de velocidad y/o reflectividad en la configuración. Se 
    filtran todos los valores que no están en ese rango.
        
    super_radar : objeto
        Objeto radar extendido
    opt : objeto
        Clase de configuración
    output : dict        
        Diccionario con los campos corregidos
       
    """
    def __init__(self, super_radar, opt,output=None):
        BaseFilter.__init__(self, super_radar, opt, output)
        
        
    def get_reflectivity_range_index(self,filter_name):
        """ Obtener la matriz de indices de valores entre un rango de reflectividad """
        index = np.zeros_like(self.output['cref'])
        index[np.logical_or(self.output['cref'] > getattr(self.opt,filter_name)['max'], self.output['cref'] < getattr(self.opt,filter_name)['min'])] = 1.0
        return index
    
    def get_doppler_range_index(self, filter_name):
        """ Obtener la matriz de índices de valores entre un rango de velocidad """
        index = np.zeros_like(self.output['cv'])
        index[np.logical_or(self.output['cv'] > getattr(self.opt, filter_name)['max'], self.output['cv'] < getattr(self.opt,filter_name)['min'])] = 1.0
        return index
    
    
class RefRangeFilter(RangeFilter):
    """ 
    Filtro de valores de reflectividad

    Se filtran valores de reflectividad que no están entre un rango de valores 
    definidos.

    super_radar : objeto
        Objeto radar extendido
    opt : objeto
        Clase de configuración
    output : dict        
        Diccionario con los campos corregidos

    """
    def __init__(self, super_radar, opt, output=None):
        RangeFilter.__init__(self, super_radar, opt, output)
        self.undef_ref = self.input.get_undef_ref()
        self.var_update = getattr(opt, "RefRangeFilter")['var_update_list']
       
    
    def run(self):
        """ Correr el filtro """
        filter_name = "RefRangeFilter"
        if not self.output_init:
            self.init_output(filter_name)
        elif not 'ref' in self.output:
            self.init_output_ref()
        index = self.get_reflectivity_range_index(filter_name)
        self.update(filter_name, index, self.opt)
        self.filter_run = True
 
        
class DopplerRangeFilter(RangeFilter):
    """ 
    Filtro de valores de velocidad 

    Se filtran valores de velocidad que no están entre un rango de valores 
    definidos.

    super_radar : objeto
        Objeto radar extendido
    opt : objeto
        Clase de configuración
    output : dict        
        Diccionario con los campos corregidos
    
    """

    def __init__(self, super_radar, opt, output=None):
        RangeFilter.__init__(self, super_radar, opt, output)
        self.var_update = getattr(opt, "DopplerRangeFilter")['var_update_list']
        if self.name_v in super_radar.fields:
            self.undef_v = super_radar.get_undef_v()
        else:
            raise KeyError('Could not find ' + self.name_v + ' data in the radar fields')
      
    
    def run(self):
        """ Correr el filtro """
        filter_name = "DopplerRangeFilter"
        if not self.output_init:
            self.init_output(filter_name)
        elif not 'v' in self.output:
            self.init_output_v()
        index = self.get_doppler_range_index(filter_name)
        self.update(filter_name, index, self.opt)
        self.filter_run = True

