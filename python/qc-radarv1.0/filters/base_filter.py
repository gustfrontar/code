from collections import defaultdict 
from utils.qc_utils  import qc 
import numpy as np
import matplotlib.pyplot as plt

class BaseFilter():
    """
    Base de todos los filtros

    Esta clase tiene los procesos comunes a todos los filtros

    Paramtros
    ---------
    super_radar : objeto
        Objeto radar extendido
    opt : objeto         
        Clase de configuración
    output : dict
         Diccionario con los campos corregidos

    """
    def __init__(self, super_radar, opt, output = None):
        self.input = super_radar
        self.opt = opt
        self.name_ref = opt.name_ref
        self.name_v = opt.name_v
        self.name_rho = opt.name_rho
        
        self.levels = super_radar.get_levels()
        self.altitude = super_radar.get_altitude()
        self.distance = super_radar.get_distance()
        
        self.output = output
        self.grid = None
        
        self.filter_run = False 
        if self.output is None:
            self.output_init = False
        else:
            self.output_init = True
        
        
    def init_output(self, filter_name):
        """ Iniciar el output """
        self.output_init = True
        self.output = defaultdict(list)
        var_to_update = getattr(self.opt, filter_name)['var_update_list']
        self.output['x'] = self.input.get_x()
        self.output['y'] = self.input.get_y()
        if "ref" in var_to_update:
            self.init_output_ref()
        if "v" in var_to_update:
            self.init_output_v()
            
    def init_output_ref(self):
        """ Iniciar output con reflectividad corregida """
        radar = self.input
        self.output['ref'] = radar.get_ref()
        self.output['cref'] = self.output['ref'].copy() #Defini diferente cref
        self.output['qcref'] = np.zeros_like(radar.ref)
        self.output['undef_ref'] = radar.get_undef_ref()
      
        
    def init_output_v(self):
        """ Iniciar output con velocidad corregida """
        radar = self.input
        self.output['v'] = radar.get_v()
        self.output['cv'] = self.output['v'].copy()
        self.output['qcv'] = np.zeros_like(radar.v)
        self.output['undef_v'] = radar.get_undef_v()

        
    def get_qc(self):
        """ Obtener el diccionario con las variables corregidas """
        return(self.output)
        
    def check_fields(self,field_name):
        """ Comprobar que exista una variable en los campos del radar """
        return field_name in self.super_radar.fields
    
    def get_texture(self, input_variable, undef, nx, ny, nz):
        """ Calcular la textura para una dada variable """
        return compute_texture(var=input_variable,
                               na=self.input.get_na(), nr=self.input.get_nr(), ne=self.input.get_ne(),
                               undef=undef, 
                               nx=nx, ny=ny, nz=nz)
        
    def calculate_smooth_ref(self,opt,filter_name):
        """ Calcular la reflectividad suavizada """  
        if self.ref is not None:
            na, nr, ne = self.ref.shape
        return qc.box_functions_2d(datain=self.output['ref'], 
                                   na=self.input.get_na(), nr=self.input.get_nr(), ne=self.input.get_ne(), 
                                   undef=self.undef_ref,
                                   boxx=getattr(opt,filter_name)['nx'], boxy=getattr(opt,filter_name)['ny'], boxz=getattr(opt,filter_name)['nz'], 
                                   operation='MEAN', threshold=0.0)
    
    def calculate_box_function_cou(self, filter_name, input, opt):
        return qc.box_functions_2d(datain=input, 
                                  na=self.input.get_na(), nr=self.input.get_nr(), ne=self.input.get_ne(), 
                                  undef=self.undef_v,
                                  boxx=getattr(opt, filter_name)['nx'], 
                                   boxy=getattr(opt, filter_name)['ny'], 
                                   boxz=getattr(opt, filter_name)['nz'],
                                  operation='COU2', threshold=getattr(opt, filter_name)['dvtr'])
    
    
    def calculate_weight(self, filter_name, index, opt): 
        """ Calcular el peso que se le va a dar a cada pixel segun el índice"""
        if self.input.ref is not None:
            na, nr, ne = self.input.ref.shape
        else:
            na, nr, ne = self.input.v.shape

        weigth = qc.multiple_1d_interpolation(field=index, nx=na, ny=nr, nz=ne,
                                         undef=opt.undef, xx=getattr(opt, filter_name)['ifx'], yy=getattr(opt, filter_name)['ify'], 
                                         nxx=np.size(getattr(opt, filter_name)['ifx']))
        weigth[weigth == opt.undef] == 0.0
        return weigth
    
    def update_ref(self, filter_name, index, opt, weight):
        """ Actualizar la reflectividad dado un índice y un peso"""
        if getattr(opt, filter_name)['sequential']:
            self.output['input_ref'] = np.copy(self.output['ref']) #capaz hay que agregar el campo input_ref
        
        if not getattr(opt, filter_name)['force']:
            self.output['wref'] += weight * getattr(opt, filter_name)['w']
            self.output['qcref'][weight > 0.5] = getattr(opt, filter_name)['code']
            self.output['maxw_ref'] += getattr(opt, filter_name)['w']
            
        else:
            
            if getattr(opt, filter_name)['fill_value'] == 'undef':
                tmp_mask = np.logical_and(weight > getattr(opt,filter_name)['force_value'], self.output['cref'] != self.undef_ref) 
                self.output['cref'][tmp_mask] = self.undef_ref
                if getattr(opt, filter_name)['sequential']:
                    tmp_mask = np.logical_and( weight > getattr(opt, filter_name)['force_value'] , self.output['ref'] != self.undef_ref)  
                    self.output['ref'][tmp_mask] = self.undef_ref 
                    
            elif getattr(opt, filter_name)['fill_value'] == 'min_ref':
                tmp_mask = np.logical_and(weigth > getattr(opt, filter_name)['force_value'] , self.output['cref'] != self.input.get_undef_ref()) 
                self.output['cref'][tmp_mask] = opt.no_rain_ref_val
                
                if getattr(opt, filter_name)['sequential']:
                    tmp_mask1 = np.logical_and(weigth > getattr(opt, filter_name)['force_value'], self.output['ref'] != self.input.get_undef_ref())
                    self.output['ref'][tmp_mask1] = opt.no_rain_ref_val  
                    
            else:             
                tmp_mask = np.logical_and(weight > getattr(opt, filter_name)['force_value'], self.output['cref'] != self.input.get_undef_ref()) 
                self.output['cref'][tmp_mask] = opt.filter_name.fill_value 
                
                if getattr(opt, filter_name)['sequential']:
                    tmp_mask = np.logical_and(weight > getattr(opt, filter_name)['force_value'], self.output['ref'] != self.input.get_undef_ref()) #cambie ref
                    self.output['ref'][tmp_mask] = getattr(opt, filter_name)['fill_value']
            #self.output['qcref'][weight > getattr(opt, filter_name)['force_value']] = getattr(opt, filter_name)['code'] #Puse tmp mask
            self.output['qcref'][tmp_mask] = getattr(opt, filter_name)['code'] #Puse tmp mask                       
        
    def update_v(self, filter_name, index, opt, weight):
        """ Actualizar la velocidad """
        if getattr(opt, filter_name)['sequential']:
            self.output['input_v'] = np.copy(self.output['v'])
        
        if not getattr(opt, filter_name)['force']:
            self.output['wv'] += weight * getattr(opt, filter_name)['w']
            self.output['qcv'][weight > 0.5] = getattr(opt, filter_name)['code']
            self.output['maxw_v'] += getattr(opt, filter_name)['w']
        
        else:
            
            if (getattr(opt, filter_name)['fill_value'] == 'undef') or (getattr(opt, filter_name)['fill_value'] == 'min_ref'):
               #self.output['cv'][weight > getattr(opt, filter_name)['force_value']] = self.undef_v
               tmp_mask_v = np.logical_and(weight > getattr(opt,filter_name)['force_value'], self.output['cv'] != self.undef_v)
               self.output['cv'][tmp_mask_v] = self.undef_v

               if getattr(opt, filter_name)['sequential']:
                    #self.output['v'][weight > getattr(opt, filter_name)['force_value']] = self.undef_v
                    tmp_mask_v = np.logical_and(weight > getattr(opt,filter_name)['force_value'], self.output['v'] != self.undef_v)
                    self.output['v'][tmp_mask_v] = self.undef_v
            else:
                
                self.output['cv'][weight > self(opt,filter_name)['force_value']] = getattr(opt, filter_name)['fill_value']
                
                if getattr(opt, filter_name)['sequential']:
                    self.output['v'][weight > getattr(opt, filter_name)['force_value']] = getattr(opt, filter_name)['fill_value']
            
            #self.output['qcv'][weight > getattr(opt, filter_name)['force_value']] = getattr(opt, filter_name)['code']
            self.output['qcv'][tmp_mask_v] = getattr(opt, filter_name)['code']
 
           
    def update(self, filter_name, index, opt):
        """ Actualizar los campos corregidos al radar """
        # Dado un indice calculamos el peso que se le dará a cada pixel
        weight = self.calculate_weight(filter_name, index, opt)
        # Actualizamos la reflectividad o la velocidad segun corresponda
        if ('ref' in getattr(opt,filter_name)['var_update_list']) and ('ref' in self.output):
            self.update_ref(filter_name, index, opt, weight)
        
        if ('v' in getattr(opt,filter_name)['var_update_list']) and ('v' in self.output):
            self.update_v(filter_name, index, opt, weight)
        
        if getattr(opt, filter_name)['save']:
            self.output[filter_name] = dict()
            self.output[filter_name]['qc_index'] = index
            self.output[filter_name]['weight'] = weight
            self.output[filter_name]['undef'] = opt.undef

    def update_radar(self, radar):
        """ Actualizar el radar con los campos corregidos """
        sr = self.input
        var_update = self.var_update
        self.undef_ref = self.input.get_undef_ref()
        if (self.name_ref in radar.fields) and ('ref' in var_update):
            tmp_variable = sr.order_variable_inv(self.output['cref'], self.undef_ref) ####cambie cref
            if self.opt.save['keep_original_fields']:
                radar.add_field_like(self.name_ref, 'cref', radar.fields[self.name_ref]['data'], True)
                radar.fields['cref']['data'] = np.ma.masked_array(tmp_variable, mask = (tmp_variable==self.undef_ref))
                fv = radar.fields['cref'].pop('_FillValue')
                ic =radar.fields['cref'].pop('is_checked')
                radar.fields['cref']['_FillValue'] = fv
                radar.fields['cref']['is_checked'] = ic
            else: 
                radar.fields[self.name_ref]['data']=np.ma.masked_array(tmp_variable, mask = (tmp_variable==self.undef_ref))
                fv = radar.fields[self.name_ref].pop('_FillValue')
                ic =radar.fields[self.name_ref].pop('is_checked')
                radar.fields[self.name_ref]['_FillValue'] = fv
                radar.fields[self.name_ref]['is_checked'] = ic
                 
        if (self.name_v in radar.fields) and ('v' in var_update):
            tmp_variable = sr.order_variable_inv(self.output['cv'], self.undef_v)
            if self.opt.save['keep_original_fields']:
                radar.add_field_like(self.name_v, 'cv', radar.fields[self.name_v]['data'], True)
                radar.fields['cv']['data'] = np.ma.masked_array(tmp_variable, mask = (tmp_variable==self.output['undef_v']))
                fv = radar.fields['cv'].pop('_FillValue')
                ic = radar.fields['cv'].pop('is_checked')
                radar.fields['cv']['_FillValue'] = fv
                radar.fields['cv']['is_checked'] = ic

            else:
                radar.fields[self.name_v]['data']=np.ma.masked_array(tmp_variable, mask = (tmp_variable==self.output['undef_v']))
                fv = radar.fields[self.name_v].pop('_FillValue')
                radar.fields[self.name_v].pop('is_checked')
                radar.fields[self.name_v]['_FillValue'] = fv
        return radar

    def plot_ref(self,filter_name):

        outputpath_plots = self.opt.plots_outpath  

        tmp_qc_index=np.ma.masked_array( self.output[filter_name]['qc_index'] , self.output[filter_name]['qc_index'] == self.opt.undef)
        tmp_x = np.ma.masked_array( self.output['x'] , self.output['x'] == self.opt.undef )
        tmp_y = np.ma.masked_array( self.output['y'] , self.output['y'] == self.opt.undef )
        tmp_ref=np.ma.masked_array( self.output['input_ref'] , self.output['input_ref'] == self.output['undef_ref'] )
        tmp_cref=np.ma.masked_array( self.output['cref'] , self.output['cref'] == self.output['undef_ref'] )
        date_plot = self.input.metadata['date']
        levs = self.opt.plots_elevs
        radar_id = self.input.metadata['instrument_name']
        for ilev in levs:
            fig = plt.figure(figsize=(8, 8))
            plt.subplot(2,2,1)
            plt.pcolor(tmp_x[:,:,ilev]/1e3,tmp_y[:,:,ilev]/1e3, tmp_qc_index[:,:,ilev],vmin=np.nanmin(tmp_qc_index[:,:,ilev]),vmax=np.nanmax(tmp_qc_index[:,:,ilev]) )
            plt.title('QC Index')
            plt.colorbar()
            plt.subplot(2,2,2)
            plt.pcolor(tmp_x[:,:,ilev]/1e3,tmp_y[:,:,ilev]/1e3, self.output['qcref'][:,:,ilev],cmap='Set1_r')
            plt.title('Pixels corrected (filter code)')
            plt.colorbar()
            plt.subplot(2,2,3)
            plt.pcolor(tmp_x[:,:,ilev]/1e3,tmp_y[:,:,ilev]/1e3,tmp_ref[:,:,ilev],vmin=0,vmax=70,cmap='pyart_NWSRef')
            plt.title('Original Reflectivity')
            plt.colorbar()
            plt.subplot(2,2,4)
            plt.pcolor(tmp_x[:,:,ilev]/1e3,tmp_y[:,:,ilev]/1e3, tmp_cref[:,:,ilev],vmin=0,vmax=70,cmap='pyart_NWSRef')
            plt.title('Corrected Reflectivity by ' + filter_name)
            plt.colorbar()
            plt.subplots_adjust
            plt.savefig(outputpath_plots +'/' + radar_id + '_'  + date_plot + '_' +filter_name+ '_elev' +str(ilev)+ '_ref.png',dpi=150,bbox_inches='tight',format='png')
            plt.close(fig)

    def plot_vel(self,filter_name):
        outputpath_plots = self.opt.plots_outpath
        tmp_qc_index=np.ma.masked_array( self.output[filter_name]['qc_index'] , self.output[filter_name]['qc_index'] == self.opt.undef)
        tmp_x = np.ma.masked_array( self.output['x'] , self.output['x'] == self.opt.undef )
        tmp_y = np.ma.masked_array( self.output['y'] , self.output['y'] == self.opt.undef )
        tmp_v=np.ma.masked_array( self.output['input_v'] , self.output['input_v'] == self.output['undef_v'] )
        tmp_cv=np.ma.masked_array( self.output['cv'] , self.output['cv'] == self.output['undef_v'] )

        date_plot = self.input.metadata['date']
        levs = self.opt.plots_elevs
        radar_id = self.input.metadata['instrument_name']

        for ilev in levs:
            fig = plt.figure(figsize=(8, 8))
            plt.subplot(2,2,1)
            plt.pcolor(tmp_x[:,:,ilev]/1e3,tmp_y[:,:,ilev]/1e3, tmp_qc_index[:,:,ilev],vmin=np.nanmin(tmp_qc_index[:,:,ilev]),vmax=np.nanmax(tmp_qc_index[:,:,ilev]) )
            plt.title('QC Index')
            plt.colorbar()
            plt.subplot(2,2,2)
            plt.pcolor(tmp_x[:,:,ilev]/1e3,tmp_y[:,:,ilev]/1e3, self.output['qcv'][:,:,ilev],cmap='Set1_r')
            plt.title('Pixels corrected (filter code)')
            plt.colorbar()
            plt.subplot(2,2,3)
            plt.pcolor(tmp_x[:,:,ilev]/1e3,tmp_y[:,:,ilev]/1e3,tmp_v[:,:,ilev],vmin=-30,vmax=30,cmap='pyart_NWSVel')
            plt.title('Original Doppler Velocity')
            plt.colorbar()
            plt.subplot(2,2,4)
            plt.pcolor(tmp_x[:,:,ilev]/1e3,tmp_y[:,:,ilev]/1e3, tmp_cv[:,:,ilev],vmin=-30,vmax=30,cmap='pyart_NWSVel')
            plt.title('Corrected Doppler Velocity by ' + filter_name)
            plt.colorbar()
            plt.subplots_adjust
            plt.savefig(outputpath_plots +'/' + radar_id + '_'  + date_plot + '_' + filter_name+ '_elev' +str(ilev)+ '_vel.png',dpi=150,bbox_inches='tight',format='png')
            plt.close(fig)
