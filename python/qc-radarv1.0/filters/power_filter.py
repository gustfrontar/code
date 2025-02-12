import numpy as np
import pyart
from .base_filter import BaseFilter
import matplotlib.pyplot as plt 

class PowerFilter(BaseFilter):
    """ 
        Filtro de potencia 
        para obtener la variable corregida tengo que hacer ref_cor = np.ma.masked_where(power>threshold)
        linear(ref_cor)
        Basado en https://gitlab.smn.gov.ar/mrugna/radar_python/blob/master/grafico_colmax.py#L122
      
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
        fopt = getattr(opt, "PowerFilter")
        self.a = fopt['a']
        self.C = fopt['C']
        self.qc_pot_thr = fopt['qc_pot_thr']
        self.var_update = getattr(opt, "PowerFilter")['var_update_list']
            
    def get_power(self, ref):
        """ Calcular la potencia a partir de la reflectividad """
        radar_range = self.input.range['data']/1000.
        return ref - 20*np.log10(radar_range) - 2*self.a*radar_range - self.C
    
    def get_sfc_threshold(self):
        """ Obtener el valor de umbral de superficie para cada radar """
        radar = self.input
        radar_id = radar.metadata['instrument_name']
        if radar_id in ['RMA1', 'RMA2', 'RMA3', 'RMA4', 'RMA5', 'RMA7', 'RMA8', 'RMA10', 'RM12', 'PAR']:
            sfc_threshold = 15 
            
        elif radar_id in ['RMA6']:
            sfc_threshold = 5
        
        elif radar_id in ['RMA9', 'RMA11']:
            sfc_threshold = 0
        
        elif radar_id in ['ANG']:
            sfc_threshold = 6
        
        elif radar_id in ['PER']:
            sfc_threshold = 12
        return sfc_threshold
        
                
    
    def get_power_threshold(self, power):
        """ Obtener el umbral de potencia """
        sfc_threshold = self.get_sfc_threshold()
        threshold = np.floor(np.nanmin(power[np.isfinite(power)])) + sfc_threshold 
        return threshold
    
    
    def get_corrected_ref(self):
        """ Calcular la reflectividad corregida """
        ref = np.copy(self.output['ref'])
        ref = self.input.order_variable_inv(ref, self.undef_ref)
        ref = np.ma.masked_array(ref, mask=ref== self.undef_ref)
        power = self.get_power(ref)
        mask_power = np.ma.masked_where(power < self.qc_pot_thr, power) 
        threshold = self.get_power_threshold(mask_power)
        ref_cor = np.copy(ref)
        return np.ma.where(mask_power < threshold, self.undef_ref, ref_cor), threshold, power
        
    def get_index_power(self, power):
        power = np.ma.where(power < self.qc_pot_thr, self.undef_ref, power)
        power = self.input.order_variable(power,self.undef_ref)
        power[power == self.undef_ref] = self.opt.undef
        return power

    def power_update(self, opt, filter_name, ref_corr, index_power, threshold):
        self.output['cref'] = self.input.order_variable(ref_corr,self.undef_ref)
        mask = self.output['cref'] != self.output['ref']
        self.output['qcref'][mask] = getattr(opt, filter_name)['code']
        self.output['input_ref'] = np.copy(self.output['ref'])
        self.output[filter_name]['qc_index'] = index_power
        self.output[filter_name]['threshold'] = threshold

        if getattr(opt, filter_name)['sequential']:
            self.output['ref'] = np.copy(self.output['cref'])


    def run(self):
        """ Correr el filtro """
        filter_name = 'PowerFilter'
        if not self.output_init:
            self.init_output(filter_name)
        elif not 'ref' in self.output:
            self.init_output_ref()
        self.output[filter_name] = dict()
        ref_cor, threshold, power = self.get_corrected_ref()
        index = self.get_index_power(power)
        self.power_update(self.opt, filter_name, ref_cor, index, threshold)
        self.filter_run = True

        
