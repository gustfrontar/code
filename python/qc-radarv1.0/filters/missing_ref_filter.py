import numpy as np
import pyart
from .base_filter import BaseFilter
from utils.qc_utils  import qc 

class MissingRefFilter(BaseFilter):
    """ Filtro de valores faltantes de reflectividad

        Este filtro detecta extensiones con reflectividad nula dentro de ecos
        meteorológicos
        
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
        self.undef_ref = self.input.get_undef_ref()
        self.var_update = getattr(opt, "MissingRefFilter")['var_update_list']
        
        
        
    def detect_missing(self, opt):
        """ Detectar los píxeles con reflectividad faltante """
        filter_name = "MissingRefFilter"
        na, nr, ne = self.input.ref.shape
        return qc.detect_missing(self.output['ref'],
                              na=na, nr=nr, ne=ne,
                              undef=self.undef_ref,
                              min_ref=opt.no_rain_ref_val,
                              threshold=getattr(opt,filter_name)['threshold'],
                              nmissing_max=getattr(opt,filter_name)['nmissing_max'])
    
    def get_missing_ref_index(self):
        """ Obtener la matriz de índices de píxeles de reflectividad faltante """
        index = self.detect_missing(self.opt)
        index = index.astype(int)
        index[index == self.input.undef_ref] = self.opt.undef
        return index
    
    def run(self):
        """ Correr el filtro """
        filter_name = "MissingRefFilter"
        if not self.output_init:
            self.init_output(filter_name)
        elif not 'ref' in self.output:
            self.init_output_ref()
        index = self.get_missing_ref_index()
        self.update(filter_name, index, self.opt)
        self.filter_run = True

