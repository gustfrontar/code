import numpy as np
import pyart
from .base_filter import BaseFilter
from utils.qc_utils  import qc
from utils.topography import get_topography 
## TODO este filtro no se usa. Actualizar
class EchoFilter(BaseFilter):
    """
    Métodos necesarios para correr los filtros de tope y profundidad de eco

    Parámetros
    ----------

    super_radar : objeto
        Objeto radar extendido
    opt : object
         Clase de configuración 
    filter_name : str
         Nombre del filtro
    output : dict
         Diccionario con los campos corregidos
    """

    def __init__(self, super_radar, opt,  filter_name, output=None):
        BaseFilter.__init__(self, super_radar, opt, output)
        self.input = super_radar
        self.opt = opt
        self.topography = get_topography(self.opt, self.input)
        if self.name_ref in super_radar.fields: 
            self.ref = super_radar.get_ref()
            self.undef_ref = super_radar.get_undef_ref()
        else:
             raise KeyError("Variables not available in input data")         


   
    def get_echo_top(self, filter_opt):
        """ Calcular el tope de la nube para cada eco de radar"""
        radar = self.input
        return qc.echo_top(reflectivity=self.output['ref'],
                        heigth=radar.get_altitude()[0,:,:], 
                        rrange=radar.get_distance(),
                        na=radar.get_na(), nr=radar.get_nr(), ne=radar.get_ne(),
                        undef=self.undef_ref,
                        nx=filter_opt['nx'], ny=filter_opt['ny'], nz=filter_opt['nz'])
    
    
    def get_echo_top_fast(self, filter_opt):
        """ Calcular el tope de la nube para cada eco en modo rápido """
        radar = self.input
        [index,data_2d] = qc.echo_top_fast(reflectivity=self.output['ref'],
                        heigth=radar.altitude[0], 
                        rrange=radar.get_distance(),
                        na=radar.get_na(), nr=radar.get_nr(), ne=radar.get_ne(),
                        undef=self.undef_ref,
                        nx=filter_opt['nx'],ny=filter_opt['ny'],nz=filter_opt['nz'])
        index[index != self.undef_ref] = index[index != self.undef_ref] - self.topography[index != self.undef_ref]

        # No se eliminan ecos donde la altura del haz sea menor que el parametro 'heigthtr' que representa la altura del echo-top
        na, nr, ne = self.input.ref.shape

        tmp_max_z = self.altitude[:,:,ne-1] - self.topography[:,:,ne-1]

        tmp_max_z = np.repeat(tmp_max_z[:,:,np.newaxis],ne,axis=2)

        index[tmp_max_z < filter_opt['heigthtr']] = self.opt.undef
        
        
        
        # Definimos como undef los pixeles con valores no válidos de reflectividad
        index[self.output['ref'] == self.opt.no_rain_ref_val] = self.opt.undef
        index[index == self.undef_ref] = self.opt.undef
        return index    

    def get_max_z(self): 
        self.na = self.input.get_na()
        self.nr = self.input.get_nr()
        self.ne = self.input.get_ne()
        self.altitude = self.input.get_altitude()
        max_z = np.zeros((self.na,self.nr,self.ne))
      
        for ii in range(0, self.ne):
            max_z[:,:,ii] = self.altitude[:,:,self.ne-1]  
        return max_z
    
    def get_echo_top_index(self, opt):
        """ Calcular la matriz de índices para el tope de ecos"""
        # Primero se calcula el tope de eco con el método especificado
        if opt.filter_name.fast_computation:
            [index, data_2d] = self.get_echo_top_fast()
        else:
            [data_3d, data_2d] = self.get_echo_top()
            index=data_3d[:,:,0]
        # En la matriz index se guarda la altura del tope de eco
        # Si hay reflectividad válida calculamos que tan alto está el eco con respecto a la topografía
        index[index != self.undef_ref] = index[index != self.undef_ref] - self.topography[index != self.undef_ref]
        # Definimos como undef los pixeles con valores no válidos de reflectividad
        index[self.ref == opt.no_rain_interval] = opt.undef
        index[index == self.undef_ref] = opt.undef
        return index
    
    def get_echo_depth_index(self, opt, filter_name):
        """ Calcular la matriz de índices para la profundidad de ecos"""
        max_z = self.get_max_z() 
        fopt = getattr(opt,filter_name)
        [data_3d, data_2d] = qc.echo_top(reflectivity=self.output['ref'],
                                         heigth=self.altitude[0,:,:],
                                         rrange=self.input.get_distance(),
                                         na=self.na,nr=self.nr,ne=self.ne,
                                         undef=self.undef_ref,
                                         nx=fopt['nx'],ny=fopt['ny'],nz=fopt['nz'])
        index = data_3d[:,:,:,2]
        index[index == self.undef_ref] = opt.undef
        # Do not consider this filter when the maximim height is the specified thresholds
        # i.e pixels close to the radar
        index[max_z < getattr(opt,filter_name)['heigthtr']] = 1.0e6
        return index
    
    
        
class EchoDepthFilter(EchoFilter):
    def __init__(self, super_radar, opt, output=None):
        EchoFilter.__init__(self, super_radar, opt, 'EchoDepthFilter', output)
        self.var_update = getattr(opt,"EchoDepthFilter")['var_update_list']

        
    def run(self):
        """ Correr el filtro """
        filter_name = "EchoDepthFilter" 
        if not self.output_init:
            self.init_output(filter_name)
        elif not 'ref' in self.output:
            self.init_output_ref()      
        index = self.get_echo_depth_index(self.opt, filter_name)
        self.update(filter_name, index, self.opt)
        self.filter_run = True

