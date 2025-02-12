import numpy as np
import pyart
from .base_filter import BaseFilter



class LowElevFilter(BaseFilter):
    """
    Filtro de elevación baja
   
    
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
        self.ref = self.input.get_ref()
        self.undef_ref = self.input.get_undef_ref()
        self.var_update = getattr(opt, "LowElevFilter")['var_update_list']
     
    def get_n_angles(self, opt):
        """ Calcular la cantidad de niveles que están por debajo del ángulo mínimo especificado """
        angles = self.input.get_levels()[self.input.get_levels() < getattr(opt,'LowElevFilter')['min_angle']]
        return np.size(angles)
     
    def get_low_elev_index(self, opt, filter_name):
        """ Calcular el índice """
        n_angles = self.get_n_angles(opt)
        smooth_ref = self.calculate_smooth_ref(opt, filter_name)
        index = np.zeros([self.input.get_na(),self.input.get_nr(),self.input.get_ne()])
        for angle in range(n_angles): #chequear las dimensiones de mi order_variables
            index[:,:,angle] = np.logical_and(self.output['ref'][:, :, angle] > opt.no_rain_ref_val, 
                                              smooth_ref[:, :,n_angles] <= opt.no_rain_ref_val)
            index[:,:,angle][self.altitude[:, :, n_angles] > getattr(opt,filter_name)['height_thr']] = 0.0
        index[index == self.undef_ref] = opt.undef
        return index
        
    def run(self):
        """ Correr el filtro """
        filter_name = 'LowElevFilter'        
        if not self.output_init:
            self.init_output(filter_name)
        elif not 'ref' in self.output:
            self.init_output_ref()
        index = self.get_low_elev_index(self.opt,filter_name)
        self.update(filter_name, index, self.opt)
        self.filter_run = True

