from utils.qc_utils  import qc 
from filters.base_filter import BaseFilter

class AttenuationFilter(BaseFilter):
    """ 
    Filtro de atenuación

    Este filtro calcula el índice de atenuación para cada pixel. Corrige los que 
    se pueden corregir y enmascara el resto.
    La metodología está basada en el trabajo de Berne y Uijlenjoet, 2006 y los 
    parámetros se especificaron como se indica en Delrieu 2009.

    Para correrlo se puede usar la función run_filter con el nombre ´ AttenuationFilter´

    Parametros
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
    
    def __init__(self, super_radar, opt, output=None):
        BaseFilter.__init__(self, super_radar, opt, output)
        self.input = super_radar
        self.opt = opt
        self.undef_ref = self.input.get_undef_ref()
        filter_name = "AttenuationFilter"
        self.var_update = getattr(opt, "AttenuationFilter")['var_update_list']
        

    def get_attenuation(self, opt, filter_name):
        # Calcular la atenuación 
        beaml = self.input.range['data'][1] - self.input.range['data'][0]
        na, nr, ne = self.input.ref.shape
        return qc.get_attenuation(var=self.output['cref'],
                               na=na, nr=nr, ne=ne,
                               undef=self.undef_ref,
                               beaml=beaml,
                               cal_error=getattr(opt, filter_name)['attcalerror'],
                               is_power=getattr(opt, filter_name)['is_power'],
                               coefs=getattr(opt, filter_name)['att_coefs'],
                               mindbz=opt.no_rain_ref_val)

    def get_att_index(self, opt, filter_name):
        # Calcular el índice para enmascarar
        [index, corrected_ref] = self.get_attenuation(opt, filter_name)
        index[index == self.input.undef_ref] = self.opt.undef
        if getattr(opt,filter_name)['attenuation_correction']: #Agregue esto que estaba en version original
            self.output['cref'] = corrected_ref

        return index


    def run(self):
        # Correr el filtro
        filter_name = 'AttenuationFilter'
        # Inicializar el output si no está definido
        if not self.output_init:
            self.init_output(filter_name)
        elif not 'ref' in self.output:
            self.init_output_ref()
        # Calcular los índices para hacer la actualización
        index = self.get_att_index(self.opt, filter_name)
        # Actualizar radar y output
        self.update(filter_name, index, self.opt)
        self.filter_run = True
