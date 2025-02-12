import numpy as np
import pyart
from .base_filter import BaseFilter
from utils.qc_utils  import qc 


class RefSpeckleFilter(BaseFilter):
    """
    Filtro de speckle de reflectividad

    Se filtran valores de reflectividad aislados

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
        self.var_update = getattr(opt, "RefSpeckleFilter")['var_update_list']
        self.undef_ref = self.input.get_undef_ref()     

    def calculate_box_function(self, filter_name):
        """ Calcula la cantidad de píxeles con valores de reflectividad por encima de un valor """
        fopt = getattr(self.opt,filter_name)
        radar = self.input
        na, nr, ne = radar.ref.shape
        return qc.box_functions_2d(datain=self.output['ref'], 
                                  na=na, nr=nr, ne=ne, 
                                  undef=radar.get_undef_ref(),
                                  boxx=fopt['nx'], boxy=fopt['ny'], boxz=fopt['nz'],
                                  operation='COU2', threshold=fopt['reftr'])
    
    
    def get_ref_speckle_index(self, filter_name):
        """ Calcular la matriz de índices """
        index = self.calculate_box_function(filter_name)
        index[index == self.input.undef_ref] = self.opt.undef
        index[self.output['ref'] == self.opt.no_rain_ref_val] = self.opt.undef
        return index
    
    
    def run(self):
        """ Correr el filtro """
        filter_name = "RefSpeckleFilter"
        if not self.output_init:
            self.init_output(filter_name)
        elif not 'ref' in self.output:
            self.init_output_ref()
           
        index = self.get_ref_speckle_index(filter_name)
        self.update(filter_name, index, self.opt)
        self.filter_run = True
