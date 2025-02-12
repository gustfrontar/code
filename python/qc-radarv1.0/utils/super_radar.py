"""

qc-radar.core.super_radar
=========================

An extension to pyart Radar object class.

.. autosummary::
    :toctree: generated/


"""
import pyart
from pyart.core.radar import Radar
import numpy as np 
import matplotlib.pyplot as plt

class SuperRadar(Radar):
    """ 
    Una clase que extiende un objeto radar para guardar algunas variables que se 
    suelen usar en el control de calidad del radar. 
 
    Attributes
    ----------
    opt : dict
        Opciones
    name_ref : str
        Nombre de la variable de reflectividad
    name_v : str
        Nombre de la variable velocidad
    name_rho : str
        Nombre de la variable de correlacion
    ref : array
        reflectividad en 3 dimensiones: [azimut, rango, elevaciones]
    v : array
        velocidad doppler en 3 dimensiones: [azimut, rango, elevaciones]
    altitude : array
        altitud en 3 dimensiones: [azimut, rango, elevaciones]
    x : array
        coordenada x en 3 dimensiones
    y : array
        coordenada y en 3 dimensiones
    rho : array
        variable de correlación en 3 dimensiones
    order_azimuth : array 
        variable que indica el ángulo correspondiente a cada valor de azimut
    undef_ref : float
        valor undef de reflectividad
    undef_v : float
        valor undef de velocidad
    undef_rho : float
        valor undef de correlacion
    na : int
        dimensión de azimuth
    ne : int
        dimensión de las elevaciones
    nr : int
        dimensión del rango
    levels : list
        elevaciones del radar 
    distance : array
        distancia de cada punto al radar
    topography : array
        altura de la topografía
    """
    def __init__(self, radar, opt):
        for key in radar.fields.keys():
            radar.fields[key].update( {'is_checked' : 'False'} )
        
        self.__dict__.update(radar.__dict__)
        
        self.opt = opt
        self.name_ref = opt.name_ref
        self.name_v = opt.name_v
        self.name_rho = opt.name_rho
        
        #ordered
        self.ref = None
        self.v = None
        self.altitude = None
        self.x = None
        self.y = None
        self.rho = None
        #calc
        self.order_azimuth = None
        self.undef_ref = None
        self.undef_v = None
        self.undef_rho = None
        self.na = None
        self.ne = None
        self.nr = None
        self.levels = None
        self.distance = None
        self.topography = None
        
                    
    #def order_variable(self, data):
    #    """ 
    #    Convierte los campos 2D (rango, azimut) del radar a campos 3D (azimut, rango, elevaciones)
    #    """
    #    if self.levels is None:
    #        self.levels = self.set_levels()
    #    slices = [self.get_slice(lev) for lev in np.arange(len(self.levels))]
    #    #print('slices',slices)
    #    if self.metadata['instrument_name']=='PAR':
    #        #data_ = np.array([data[lev][-361:-1,:] for lev in slices])
    #        data_ = np.array([data[lev][-361:-1,:] for lev in slices])
    #    else:
    #        data_ = np.array([data[lev] for lev in slices])
    #    #print('data_',data_)
    #    #print('moveaxis',np.moveaxis(data_, 0, -1))
    #    return np.moveaxis(data_, 0, -1)
    def order_variable(self,data,undef):
        """ 
        Convierte los campos 2D (rango, azimut) del radar a campos 3D (azimut, rango, elevaciones)
        """
        ray_angle_res = self.get_ray_angle_res()
        levels = self.get_levels()
        order_azimuth = self.get_order_azimuth()
        na=np.size(order_azimuth)
        ne=np.size(levels)
        nr=np.size(self.range['data'].data)
        nb = self.azimuth['data'].shape[0]
        var = np.ones( (nb,nr) )
        order_var = np.zeros((na,nr,ne))
        var[:] = data
        order_var = np.zeros((na,nr,ne))
        order_n   = np.zeros((na,nr,ne),dtype='int')
        current_lev = self.elevation['data'][0]
        ilev = np.where(levels == current_lev)[0]
        for iray in range(0, nb):   #Loop over all the rays
            #Check if we are in the same elevation.
            if self.elevation['data'][iray] != current_lev:
                current_lev = self.elevation['data'][iray]
                ilev=np.where(levels == current_lev)[0]
            #Compute the corresponding azimuth index.
            az_index = np.round(self.azimuth['data'][iray] / ray_angle_res).astype(int)
            #Consider the case when azimuth is larger than na*ray_angle_res-(ray_angle_res/2)
            if az_index >= na:
                az_index = 0
            tmp_var = var[iray,:]
            undef_mask = tmp_var == undef
            tmp_var[ undef_mask ] = 0.0
            order_var[az_index, :, ilev] += tmp_var
            order_n[az_index, : , ilev] += np.logical_not(undef_mask).astype(int)
        order_var[order_n > 0 ] = order_var[ order_n > 0 ] / order_n[ order_n > 0 ]
        order_var[order_n == 0] = undef
        return order_var


    
    def order_variable_inv(self, variable, undef):
        """
        Convierte los campos en 3D (azimut, rango, elevaciones) a 2D (rango, azimut).
        """

        na, nr, ne  = variable.shape 

        nb = self.azimuth['data'].shape[0]

        levels = self.get_levels()

        ray_angle_res = self.get_ray_angle_res()

        current_lev = self.elevation['data'][0]
        ilev = np.where(levels == current_lev)[0]

        output_var = np.ones((nb, nr))*undef

        for iray in range(0, nb):   #Loop over all the rays
          #Check if we are in the same elevation.
            if self.elevation['data'][iray] != current_lev:
                current_lev = self.elevation['data'][iray]
                ilev=np.where(levels == current_lev)[0]

            #Compute the corresponding azimuth index.
            az_index = np.round(self.azimuth['data'][iray] / ray_angle_res).astype(int)

            #Consider the case when azimuth is larger than na*ray_angle_res-(ray_angle_res/2)
            if az_index >= na:
                az_index = 0

            output_var[iray] = variable[az_index, :, ilev]
        return output_var    
    
    def get_ref(self):
        """ 
        Obtiene el campo de reflectividad en 3 dimensiones.
        """
        if self.ref is None:
            self.set_ref()
            
        return self.ref

    
    def set_ref(self):
        """ 
        Chequea que los datos de reflectividad sean válidos y 
        los transforma a 3 dimensiones. 
        """
        self.check_variable('reflectivity') 
        reflectivity = self.fields[self.name_ref]['data'].data #Agregue .data
        self.undef_ref = self.get_undef_ref()
        self.ref = self.order_variable(reflectivity,self.undef_ref)
    
    def get_v(self):
        """ 
        Obtiene el campo de velocidad en 3 dimensiones.
        """
        if self.v is None:
            self.set_v()
        return self.v
    
    
    def set_v(self):
        """ 
        Chequea que los datos de velocidad válidos y 
        los transforma a 3 dimensiones. 
        """
        if self.name_v in self.fields.keys():
            self.check_variable(self.name_v)
            wind = self.fields[self.name_v]['data']
            self.undef_v = self.get_undef_v()
            self.v = self.order_variable(wind, self.undef_v)
            
    def get_rho(self):
        """
        Obtiene el campo de correlación en 3 dimensiones.
        """
        if self.rho is None:
            self.set_rho() 
        return self.rho
    
    
    def set_rho(self):
        """ 
        Chequea que los datos de correlación válidos y 
        los transforma a 3 dimensiones. 
        """
        if 'cross_correlation_ratio' in self.fields.keys():
            self.check_variable('cross_correlation_ratio')
            rho = self.fields['cross_correlation_ratio']['data'].data #Agregue .data
            self.undef_rho = self.get_undef_rho()
            self.rho = self.order_variable(rho, self.undef_rho)
            
            
    def get_na(self):
        """
        Obtener la dimensión de los azimut
        """
        if self.na is None:
            self.na = self.set_na()
        return self.na
    
    
    def set_na(self):
        """
        Calcula la dimensión en azimut
        """
        return self.get_order_azimuth().shape[0]
        
        
    def get_nr(self):
        """
        Obtener la dimensión del rango
        """
        if self.nr is None:
            self.set_nr()
        return self.nr
    
    
    def set_nr(self):
        """
        Calcula la dimensión en rango
        """
        self.nr = self.range['data'].shape[0] 
        
        
    def get_ne(self):
        """
        Obtener la dimensión de las elevaciones
        """
        levels = self.get_levels()
        return levels.shape[0]
        
        
    def is_checked(self, field_name):
        """
        Determina si se chequearon las máscaras y valores válidos
        """
        return eval(self.fields[field_name]['is_checked'])
    
    
    def check_variable(self, field_name):
        """
        Chequea máscaras, nan, inf y se queda con datos válidos
        """
        if not self.is_checked(field_name):
            self.set_dtype(field_name)
            self.set_mask(field_name)
            self.check_nan_inf(field_name) 
            self.fields[field_name]['is_checked'] = 'True' 
            
            
    def set_dtype(self, field_name):
        """
        Determina el datatype a float
        """
        self.fields[field_name]['data']=self.fields[field_name]['data'].astype('float64')
        
       
    def set_mask(self, field_name):
        """
        Genera una máscara donde todos los datos del campo son _FillValue
        """
        data = self.fields[field_name]['data']
        if not np.ma.is_masked(data):
            self.fields[field_name]['data'] = np.ma.masked_array(data, mask=self.fields[field_name]['data']==['_FillValue'])
            
            
    def check_nan_inf(self, field_name):
        """
        Chequea si hay datos inf o nan y los pasa a valores válidos.
        Los nans e infs se reemplazan por el valor de los campos en tiempo claro para reflectividad y _FillValue para el resto
        """
        if field_name == self.name_ref:
            ref_dic = self.fields[self.name_ref]
            no_rain_val = self.opt.no_rain_ref_val
            
            if (ref_dic['data'].data != ref_dic['_FillValue']).sum() > ref_dic['data'].data.size*0.005:
                #Set inf and nan
                ref_dic['data'].data[np.isinf(ref_dic['data'].data)] = no_rain_val
                ref_dic['data'].data[np.isnan(ref_dic['data'].data)] = no_rain_val
                ref_dic['data'].data[ref_dic['data'].data < no_rain_val] = no_rain_val
                ref_dic['data'].data[ref_dic['data'].data == ref_dic['_FillValue']] = no_rain_val
                ref_dic['data'].mask = ref_dic['data'].data == ref_dic['_FillValue']
                
        else:
            self.fields[field_name]['data'].data[np.isinf(self.fields[field_name]['data'].data)] = self.fields[field_name]['_FillValue']
            self.fields[field_name]['data'].data[np.isnan(self.fields[field_name]['data'].data)] = self.fields[field_name]['_FillValue']
            self.fields[field_name]['data'].mask = self.fields[field_name]['data'].data == self.fields[field_name]['_FillValue']
        
        
    def get_levels(self):
        """
        Obtiene la lista de las elevaciones
        """
        self.set_levels()
        return self.levels
        
        
    def set_levels(self):
        """
        Calcula las elevaciones
        """
        self.levels = np.sort(np.unique(self.elevation['data'])) 

    
    def get_ray_angle_res(self):
        """
        Obtiene la resolución angular
        """
        if self.ray_angle_res is not None:
            ray_angle_res = np.unique(self.ray_angle_res['data'])
        else:
            ray_angle_res = np.min(np.abs(self.azimuth['data'][1:] - self.azimuth['data'][0:-1]))
        return np.nanmean(ray_angle_res)

    
    def set_order_azimuth(self):
        """
        Calcula el angulo correspondiente a cada azimut.       
        """
        ray_angle_res = self.get_ray_angle_res()
        self.order_azimuth = np.arange(0.0, 360.0, ray_angle_res)
    
    
    def get_order_azimuth(self):
        """
        Obtiene el ángulo correspondiente a casa azimut.
        """
        if self.order_azimuth is None:
            self.set_order_azimuth()
        return self.order_azimuth
    

    def set_altitude(self):
        """
        Calcula la altitud.
        """
        self.altitude = self.order_variable(self.gate_altitude['data'],self.opt.undef)
        
        
    def get_altitude(self):
        """
        Obtiene la altitud.
        """
        if self.altitude is None:
            self.set_altitude()
        return self.altitude

    
    def set_x(self):
        """
        Calcula las celdas en x en 3D.
        """
        self.x = self.order_variable(self.gate_x['data'],self.opt.undef)

        
    def set_y(self):
        """
        Calcula las celdas en y en 3D.
        """
        self.y = self.order_variable(self.gate_y['data'],self.opt.undef)

        
    def set_latitude(self):
        """
        Calcula la latitud en 3D.
        """        
        self.latitude = self.order_variable(self.gate_latitude['data'], self.opt.undef)

        
    def set_longitude(self):
        """
        Calcula la longitud en 3D.
        """
        self.longitude = self.order_variable(self.gate_longitude['data'], self.opt.undef)

        
    def set_undef_ref(self):
        """
        Define el valor de undef de la reflectividad como el _FillValue.
        """
        self.undef_ref = self.fields[self.name_ref]['_FillValue']
        
        
    def get_undef_ref(self):
        """
        Obtiene el valor undef de la reflectividad
        """
        if self.undef_ref is None:
            self.set_undef_ref()
        return self.undef_ref

    def set_undef_v(self):
        """
        Define el valor de undef de la velocidad doppler como el _FillValue.
        """
        self.undef_v = self.fields[self.name_v]['_FillValue']
        
        
    def get_undef_v(self):
        """
        Obtiene el valor undef de la velocidad
        """
        if self.undef_v is None:
            self.set_undef_v()
        return self.undef_v
    
    def set_undef_rho(self):
        """
        Define el valor de undef de la correlación como el _FillValue.
        """
        self.undef_rho = self.fields[self.name_rho]['_FillValue']
        
    def get_undef_rho(self):
        """
        Obtiene el valor undef de la correlación.
        """
        if self.undef_rho is None:
            self.set_undef_rho()
        return self.undef_rho
    
    def get_distance(self):
        """ 
        Obtiene la distancia entre x e y
        """
        if self.distance is None:
            self.set_distance()
        return self.distance
    
    def set_distance(self):
        """ 
        Calcula la distancia entre x e y
        """
        x = self.get_x()
        y = self.get_y()
        
        self.distance = np.power(np.power(x[0], 2) + np.power(y[0], 2), 0.5)
    
    
    def get_x(self):
        """
        Obtiene las celdas x calculadas en 3D.
        """
        if self.x is None:
            self.set_x()
        return self.x
    
    
    def get_y(self):
        """
        Obtiene las celdas y calculadas en 3D.
        """
        if self.y is None:
            self.set_y()
        return self.y
    
    
    def get_agl_height(altitude, topography):
        """
        Calcula la altura entre el haz del radar y la topografía.
        """
        return altitude-topography

