from .echo_filter import EchoFilter

class EchoTopFilter(EchoFilter):
    """ Filtro de tope de eco
        Este filtro calcula la altura del tope de la nube y filtra los ecos que 
        tienen un tope a baja altura.

        Parámetros
        ----------
        super_radar : object
            Objeto radar extendido
        opt : object
             Clase de configuración
        output : dict
             Diccionario con los campos corregidos
    """
    def __init__(self, super_radar, opt, output = None):
        EchoFilter.__init__(self, super_radar, opt, 'EchoTopFilter', output)
        self.var_update = getattr(opt,"EchoTopFilter")['var_update_list'] 
        
    def run(self):
        """ Correr el filtro """ 
        # Crear un diccionario con los campos corregidos
        if not self.output_init:
            self.init_output('EchoTopFilter')
        elif not 'ref' in self.output:
            self.init_output_ref()
        # Hay dos métodos para calcular el filtro: rápido o lento
        # el método rápido solo detecta bases y topes de nubes
        # el método lento hace un escaneo 3D de la nube  
        filter_opt = getattr(self.opt, 'EchoTopFilter')
        if filter_opt['fast_computation']:
            index = self.get_echo_top_fast(filter_opt)
        else:
            [tmp_data_3d,tmp_data_2d] = self.get_echo_top(filter_opt)
            index = tmp_data_3d[:,:,:,0]

        # actualizar los campos corregidos
        self.update('EchoTopFilter', index, self.opt)
        self.filter_run = True

