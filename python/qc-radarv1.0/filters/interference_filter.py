import numpy as np
import pyart
from .base_filter import BaseFilter
from utils.interference import interference_filter 

class InterferenceFilter(BaseFilter):
    """ 
    Filtro de interferencias
    Este filtro se encarga de detectar los haces del radar que puedan estar interferidos.
 
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
        BaseFilter.__init__(self, super_radar, opt,output)
        self.input = super_radar
        self.opt = opt
        self.var_update = getattr(opt, "InterferenceFilter")['var_update_list']
        if self.name_ref in super_radar.fields:
            self.undef_ref = super_radar.get_undef_ref()
        else:
            raise KeyError("Variables not available in input data")
        
    def get_interference_filter(self, opt, input_field):
        """ Calcular el filtro de interferencias 
            Se clasifican los pixeles con interferencia mediante un ajuste lineal RANSAC. Luego 
            se calculan correlación entre la reflectividad interferida y la medida, y la diferencia
            media entre un acimut y sus contiguos.
        """
        filter_opt = getattr(opt, "InterferenceFilter")
        return interference_filter(input_field, 
                                   self.undef_ref, opt.no_rain_ref_val,
                                   self.input.range['data'], filter_opt)
    
    def get_interference_index(self):
        """ Calcular la matriz de índices del filtro de interferencias"""
        input_field = np.copy(self.output['ref'])
        index = self.get_interference_filter(self.opt, input_field)
        index[index == self.undef_ref] = self.opt.undef
        return index
    
    def run(self):
        """ Correr el filtro"""
        filter_name = "InterferenceFilter"
        # Definimos un diccionario donde vamos a guardar los campos corregidos
        if not self.output_init:
            self.init_output(filter_name)
        elif not 'ref' in self.output:
            self.init_output_ref()
        # Calculamos el índice
        index = self.get_interference_index()
        # Actualizamos los campos corregidos
        self.update("InterferenceFilter", index, self.opt)
        self.filter_run = True

