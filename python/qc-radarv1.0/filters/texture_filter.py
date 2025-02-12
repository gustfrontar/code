import numpy as np
import pyart
from.base_filter import BaseFilter
from utils.qc_utils  import qc 

class TextureFilter(BaseFilter):
    """ 
    Filtro de texturas

    Base del filtro de textura de felectividad y velocidad

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

        
    def get_ref_texture_index(self, filter_name):
        """ Calcular el indice de la textura en la reflectividad """
        fopt = getattr(self.opt, filter_name)
        index = qc.compute_texture(var=self.output['ref'],
                                na=self.input.get_na(), nr=self.input.get_nr(), ne=self.input.get_ne(),
                                undef=self.undef_ref,
                                nx=fopt['index_nx'], ny=fopt['index_ny'], nz=fopt['index_nz'])
        if fopt['use_smooth_ref']:
            smooth_ref = self.calculate_smooth_ref(self.opt, filter_name)
            index[smooth_ref >= fopt['smooth_ref_tr']] = 0.0

        index[self.output['ref'] == self.undef_ref] = 0.0
        index[index == self.undef_ref] = self.opt.undef
        return index
    
    
    def get_doppler_texture_index(self, filter_name):
        """ Calcular el indice de la textura en la velocidad """
        fopt = getattr(self.opt, filter_name)
        index = qc.compute_texture(var=self.output['v'],
                                na=self.input.get_na(), nr=self.input.get_nr(), ne=self.input.get_ne(),
                                undef=self.undef_v,
                                nx=fopt['nx'], ny=fopt['ny'], nz=fopt['nz'])
        
        index[index == self.undef_v] = self.opt.undef
        return index
    

class ReflectivityTextureFilter(TextureFilter):
    """
    Filtro de textura para la reflectividad

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
        TextureFilter.__init__(self, super_radar, opt, output)
        self.ref = self.input.get_ref()
        self.undef_ref = self.input.get_undef_ref()
        self.var_update = getattr(opt, "ReflectivityTextureFilter")['var_update_list']
       
        
    def run(self):
        """ Correr el filtro """
        filter_name = "ReflectivityTextureFilter"
        if not self.output_init:
                self.init_output(filter_name)    
        elif not 'ref' in self.output:
            self.init_output_ref()
        index = self.get_ref_texture_index(filter_name)
        self.update(filter_name, index, self.opt)
        self.filter_run = True

class DopplerTextureFilter(TextureFilter):
    """
    Filtro de textura para la velocidad

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
        TextureFilter.__init__(self, super_radar, opt, output)
        self.var_update = getattr(opt, "DopplerTextureFilter")['var_update_list']
        if self.name_v in super_radar.fields:
            self.undef_v = super_radar.get_undef_v()
        else:
            raise KeyError('Could not find ' + self.name_v + ' data in the radar fields')

        
    def run(self):
        """ Corre el filtro """
        filter_name = "DopplerTextureFilter"
        if not self.output_init:
            self.init_output(filter_name) 
        elif not 'v' in self.output:
            self.init_output_v()
        index = self.get_doppler_texture_index(filter_name)
        self.update(filter_name, index, self.opt)
        self.filter_run = True

