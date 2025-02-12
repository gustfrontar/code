from utils.qc_utils  import qc 
from filters.base_filter import BaseFilter
from utils.topography import get_topography
import numpy as np

class BlockingFilter(BaseFilter):
    """ Filtro de bloqueo por topografía
        Este filtro identifica zonas donde el haz del radar está parcial 
        o totalmente bloqueado por la topografía. 
       
        Parámetros
        ----------
        super_radar : objeto
            Objeto radar extendido
        radar : objeto
            Objeto radar original
        opt : objeto
            Clase de configuración
        output : dict
            Diccionario con los campos corregidos

    """
    def __init__(self, super_radar, opt, output):
        BaseFilter.__init__(self, super_radar, opt, output)        
        self.input = super_radar
        self.opt = opt
        self.undef_ref = self.input.get_undef_ref()
        self.topography = get_topography(opt, super_radar)
        self.var_update = getattr(opt, "BlockingFilter")['var_update_list']
        if self.name_v in self.input.fields and 'v' in self.var_update:
            self.undef_v = self.input.get_undef_v()
        
    def get_blocking(self,):
        """ Calcula la proporción del haz bloqueado utilizando relaciones geométricas """
        radar = self.input
        na, nr, ne = radar.ref.shape
        self.altitude = radar.get_altitude()
        self.levels = radar.get_levels()
        return qc.compute_blocking(radarz=self.altitude, topo=self.topography, 
                                na=na, nr=nr, ne=ne,
                                undef=self.opt.undef,
                                radar_beam_width_v=radar.instrument_parameters['radar_beam_width_v']['data'], 
                                beam_length=radar.range['meters_between_gates'], 
                                radarrange=radar.range['data'], radarelev=self.levels) 
    
    def get_blocking_mask(self): #pensar bien esto porque lo que hace es corregir el campo de reflectividad en el output. De todas maneras termina con un update
        """ Calcular una máscara a partir de la proporción de haz bloqueado """
        index = self.get_blocking()
        if self.input.ref is not None:
            mask = np.logical_and(index>0.1, index<=0.3)
            mask = np.logical_and(mask, self.output['cref']>self.opt.no_rain_ref_val)         
            self.output['cref'][mask] += 1.0

            mask=np.logical_and(index > 0.3, index <= 0.4 ) #Agregue esta opcion que estaba en el original tmb
            mask=np.logical_and(mask , self.output['cref'] > self.opt.no_rain_ref_val )
            self.output['cref'][mask] +=  2.0
            
            mask = index>0.4
            mask = np.logical_and(mask, self.output['cref']>self.opt.no_rain_ref_val)            
            self.output['cref'][mask] += 3.0
        return index
        
        
    def run(self):
        """ Correr el filtro """
        filter_name = 'BlockingFilter'
        if not self.output_init:
            self.init_output(filter_name)
        elif not 'ref' in self.output:
            self.init_output_ref()
        if self.name_v in self.input.fields and 'v' in self.var_update:
            if not 'v' in self.output:
                self.init_output_v()
        index = self.get_blocking_mask()       
        self.update(filter_name, index, self.opt)
        self.filter_run = True

        
