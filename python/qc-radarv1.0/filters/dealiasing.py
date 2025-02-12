from.base_filter import BaseFilter
import numpy as np
from pyart.correct.region_dealias import dealias_region_based

## TODO : Este filtro no es como los otros

class DealiasingFilter(BaseFilter):
    """
    Filtro de Dealiasing

    super_radar : objeto
         Objeto radar extendido
    opt : objeto
         Clase de configuración
    output : dict
         Diccionario con los campos corregidos
    """
    def __init__(self, super_radar, opt, output = None):
        BaseFilter.__init__(self, super_radar, opt, output)
        self.input = super_radar
        self.opt = opt 
        self.var_update = getattr(opt, "DealiasingFilter")['var_update_list']
        if self.name_v in super_radar.fields:
            self.v = super_radar.get_v()
            self.undef_v = super_radar.get_undef_v()
        else:
            raise KeyError('Could not find ' + self.name_v + ' data in the radar fields')
           
        
    def calculate_dealiasing(self, filter_name,field_name):
        """ Calcula el dealiasing """
        return dealias_region_based(self.input, interval_splits= getattr(self.opt,filter_name)['interval_split'], interval_limits=None, 
                 skip_between_rays=getattr(self.opt,filter_name)['skip_between_ray'], skip_along_ray=getattr(self.opt,filter_name)['skip_along_ray'],
                 centered=True,
                 nyquist_vel=None, check_nyquist_uniform=True,
                 gatefilter=None, rays_wrap_around=True, keep_original=True, set_limits=True,
                 vel_field=field_name, corr_vel_field=None)    
    
    def apply(self,filter_name):
        """ Agregar los campos corregidos al radar """
        # create a new variable in the current radar file where to put corrected wind data.
        self.input.add_field_like(self.name_v, 'cv', self.input.fields[self.name_v]['data'], True)
        tmp = self.input.order_variable_inv(self.output['v'], self.undef_v) 
        self.input.fields['cv']['data'] = np.ma.masked_array(tmp , mask=tmp== self.undef_v)
        dealiasing = self.calculate_dealiasing(filter_name,'cv')
        self.input.fields['cv']['data'] = dealiasing['data']

    def dealiasing_update(self, opt, filter_name):
        cv = self.input.order_variable(self.input.fields['cv']['data'],self.undef_v)
        self.output['cv'] = cv
        mask = np.logical_and( self.output['cv'] != self.output['v'], self.output['cv'] != self.undef_v)
        self.output['qcv'][mask] = getattr(opt, filter_name)['code']
        self.input.fields.pop('cv')
        vdiff = self.output['cv'] - self.output['v']
        self.output['input_v'] = np.copy(self.output['v'])
        self.output[filter_name]['vdiff'] = vdiff
        self.output[filter_name]['vda'] = np.copy(self.output['cv'])
        self.output[filter_name]['qc_index'] = vdiff
        self.output['input_v'] = np.copy(self.output['v'])

        if getattr(opt, filter_name)['sequential']:
            self.output['v'] = np.copy(self.output['cv'])
       
    def run(self):
        filter_name = 'DealiasingFilter'
        if self.name_v in self.input.fields:
            if not self.output_init:
                self.init_output(filter_name)
            elif not 'v' in self.output:
                self.init_output_v()                    
            self.output[filter_name] = dict()
            self.apply(filter_name)
            self.dealiasing_update(self.opt, filter_name)
            self.filter_run = True
