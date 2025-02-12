from.base_filter import BaseFilter
from utils.qc_utils  import qc 
import numpy as np
from utils.doppler import calculate_coherence_index, azimuth_filter, elevation_filter, range_filter
from sklearn import linear_model, datasets
from utils.topography import get_topography
import matplotlib.pyplot as plt 

class DopplerFilter(BaseFilter):
    """

    """
    def __init__(self, super_radar, opt, output=None):
        BaseFilter.__init__(self, super_radar, opt, output)
        self.input = super_radar
        self.opt = opt
        if self.name_v in super_radar.fields:
             self.v = super_radar.get_v()
             self.undef_v = super_radar.get_undef_v()
        else:
             raise KeyError('Could not find ' + self.name_v + ' data in the radar fields')
        #self.tr = getattr(opt, filter_name)['threshold'] 
        #self.nf = getattr(opt, filter_name)['n_filter_pass']
     
    def get_doppler_ref_index(self,filter_name, opt):
        smooth_reflectivity = self.calculate_smooth_ref(opt, filter_name)
        index = np.zeros_like(smooth_reflectivity)
            
        if getattr(opt,filter_name)['filter_undef']:
            index[smooth_reflectivity==self.undef_ref] = 1.0
            
        index[np.logical_and(smooth_reflectivity <= getattr(opt,filter_name)['threshold'], smooth_reflectivity != self.undef_ref)] = 1.0
            
        return index
    
    def get_doppler_speckle_index(self, opt):
        tmp_input = np.abs(self.output['v'].data)
        tmp_input[self.output['v'] == self.undef_v] = opt.undef
        index = self.calculate_box_function_cou("DopplerSpeckleFilter", tmp_input, opt)
        
        index[index == self.undef_v] = opt.undef
        return index
    
    #ESte filtro es complicado. Pensar una manera de hacerlo con menos loops

    
    def get_doppler_noise_index(self, filter_opt): # pedir afuera que compute cada filtro si está el campo que necesita
        v = np.copy(self.output['v'])
        tmp_v = np.copy(self.output['v'])
        for n in range(len(filter_opt['n_filter_pass'])):
            nx = filter_opt['nx'][n]
            ny = filter_opt['ny'][n]
            nz = filter_opt['nz'][n]
            tr = filter_opt['threshold'][n]
            nf = filter_opt['n_filter_pass'][n]
            for ip in range(nf):
                smooth_v = qc.box_functions_2d(datain=tmp_v,na=self.input.get_na(),nr=self.input.get_nr(),ne=self.input.get_ne(),undef=self.undef_v
                                               ,boxx=nx,boxy=ny,boxz=nz,operation='MEAN',threshold=0.0)
                distance = np.abs(smooth_v - v)
                tmp_v = np.copy(v)
                tmp_v[distance > tr] = self.undef_v
                tmp_v[distance == self.undef_v] = self.undef_v
            v[distance > tr] = self.undef_v
            tmp_v = np.copy(v)
        tmp_index = np.logical_and(self.output['v'] != self.undef_v, v==self.undef_v).astype(int)
        tmp_index[tmp_index == self.undef_v] = self.opt.undef
        return tmp_index
    
            
     ## Doppler local STD   
    def get_box_SIGM(self, filter_name, opt):
        return qc.box_functions_2d(datain=self.output['v'],
                                    na=self.input.get_na(), nr=self.input.get_nr(), ne=self.input.get_ne(),
                                    undef=self.undef_v, 
                                    boxx=getattr(opt, filter_name)['nx'],
                                    boxy=getattr(opt, filter_name)['ny'],
                                    boxz=getattr(opt, filter_name)['nz'],
                                    operation='SIGM',threshold=0.0)
     
    def get_doppler_local_std_index(self, filter_name, opt):
        index = self.get_box_SIGM(filter_name, opt)
        index[index == self.undef_v] = opt.undef
        return index
     
     
     ##Doppler spatial coherence
    def get_doppler_spatial_coherence_filter(self, opt, v):
        """ Compute a robust correlation between a ray and its neighbors. Detect outliers based on the correlation. """
        filter_opt = getattr(opt, "DopplerSpatialCoherenceFilter")
        na = self.input.get_na()
        nr = self.input.get_nr()
        ne = self.input.get_ne()
        
        #Coherence mask starts flagging the undef values
        available_mask = (v != self.undef_v)
        coherence_index = np.zeros(np.shape(v))
        
        ransac = linear_model.RANSACRegressor(random_state=1)

        if filter_opt['compute_horizontal_coherence']:    
            for k in range(ne):
                for i in range(na):
                    if i == 0:
                        iprev = na-1
                    else:
                        iprev = i-1
                    kprev = k
                    coherence_index = calculate_coherence_index(v, available_mask, coherence_index, filter_opt, i, k, iprev, kprev)
        if filter_opt['compute_vertical_coherence']:
            for k in range(ne):
                kprev = k-1
                for i in range(na):
                    iprev = i
                    coherence_index = calculate_coherence_index(v, available_mask, coherence_index, filter_opt, i, k, iprev, kprev)

        for ifilter in range(filter_opt['npass_filter']):
            v[coherence_index > filter_opt['threshold_coherence_index']] = self.undef_v

            if filter_opt['azimuthfilter']:
                for k in range(ne):
                    for i in range(na):
                        coherence_index = azimuth_filter(coherence_index, v, self.undef_v, i, k)

            if filter_opt['elevationfilter']:
                for k in range(ne):
                    for i in range(na):
                        coherence_index = elevation_filter(coherence_index, v, self.undef_v, i, k)

            if filter_opt['rangefilter']:  
                
                for i in range(nr):
                   
                    coherence_index = range_filter(coherence_index, v, self.undef_v, i, k)

        if filter_opt['enable_speckle']:
            tr = 0.0
            nx = filter_opt['nx']
            ny = filter_opt['ny']
            nz = filter_opt['nz']
            tmp = np.abs(v)
            tmp[v==self.undef_v] = self.undef_v              

            speckle_v = qc.box_functions_2d(datain=tmp,na=na,nr=nr,ne=ne,undef=self.undef_v
                                 ,boxx=nx,boxy=ny,boxz=nz,operation='COU2',threshold=tr)
    

            coherence_index[speckle_v < filter_opt['speckle_threshold']] = 10.0
 



                  
        return coherence_index
     
    
     
    def get_doppler_spatial_coherence_index(self, opt):
        tmp_v = np.copy(self.output['v'])
        index = self.get_doppler_spatial_coherence_filter(opt,tmp_v)
        index[index == self.undef_v] = opt.undef
        return index
         
    ## Low Doppler Filter     
    def get_low_doppler(self,filter_name):
        index = np.abs(self.output['v'])
        index[(self.input.get_altitude() - self.topography)>getattr(self.opt, filter_name)['height_thr']] = 10.0
        index[index == self.undef_v] = self.opt.undef
        return index
    
    
class DopplerSpeckleFilter(DopplerFilter):
    def __init__(self, super_radar, opt, output=None):
        DopplerFilter.__init__(self, super_radar, opt, output)
        self.var_update = getattr(opt, "DopplerSpeckleFilter")['var_update_list']
        self.dvtr = getattr(opt, "DopplerSpeckleFilter")['dvtr']
        
        
    def run(self):
        filter_name = "DopplerSpeckleFilter"
        if not self.output_init:
            self.init_output(filter_name)
        elif not 'v' in self.output:
            self.init_output_v()
        index = self.get_doppler_speckle_index(self.opt)
        self.update(filter_name, index, self.opt)
        self.filter_run = True
 
        
class DopplerNoiseFilter(DopplerFilter):
    def __init__(self, super_radar, opt, output=None):
        DopplerFilter.__init__(self, super_radar, opt, output)
        self.var_update = getattr(opt, "DopplerNoiseFilter")['var_update_list']
        
    def run(self):
        filter_name = "DopplerNoiseFilter"
        if not self.output_init:
            self.init_output(filter_name)
        elif not 'v' in self.output:
            self.init_output_v()
        filter_opt = getattr(self.opt, filter_name)
        index = self.get_doppler_noise_index(filter_opt)
        self.update(filter_name, index, self.opt)
        self.filter_run = True
   
        
        
class DopplerLocalStdFilter(DopplerFilter):
    def __init__(self, super_radar, opt, output=None):
        DopplerFilter.__init__(self, super_radar, opt, output)
        self.var_update = getattr(opt, "DopplerLocalStdFilter")['var_update_list']
        
    def run(self):
        filter_name =  "DopplerLocalStdFilter"
        if not self.output_init:
            self.init_output(filter_name)
        elif not 'v' in self.output:
            self.init_output_v()
        index = self.get_doppler_local_std_index(filter_name,self.opt)
        self.update(filter_name, index, self.opt)
        self.filter_run = True

        
class DopplerSpatialCoherenceFilter(DopplerFilter):
    def __init__(self, super_radar, opt, output=None):
        DopplerFilter.__init__(self, super_radar, opt, output)
        self.var_update = getattr(opt, "DopplerSpatialCoherenceFilter")['var_update_list']        
 
    def run(self):
        filter_name = "DopplerSpatialCoherenceFilter"
        if not self.output_init:
            self.init_output(filter_name)
        elif not 'v' in self.output:
            self.init_output_v()
        index = self.get_doppler_spatial_coherence_index(self.opt)
        self.update(filter_name, index, self.opt)
        self.filter_run = True

        
class LowDopplerFilter(DopplerFilter):
    def __init__(self, super_radar, opt, output):
        DopplerFilter.__init__(self, super_radar, opt, output)
        self.topography = get_topography(opt, super_radar)
        self.var_update = getattr(opt, "LowDopplerFilter")['var_update_list']        
    
    def run(self):
        filter_name = "LowDopplerFilter"
        if not self.output_init:
            self.init_output(filter_name)
        elif not 'v' in self.output:
            self.init_output_v()
        index = self.get_low_doppler(filter_name)
        self.update(filter_name, index, self.opt)
        self.filter_run = True

class DopplerRefFilter(DopplerFilter):
    def __init__(self, super_radar, opt, output):
        DopplerFilter.__init__(self, super_radar, opt, output)
        self.var_update = getattr(opt, "DopplerRefFilter")['var_update_list']
        self.ref = super_radar.get_ref()
        self.undef_ref = super_radar.get_undef_ref()

    def run(self):
        filter_name = "DopplerRefFilter"
        if not self.output_init:
            self.init_output(filter_name)
        elif not 'v' in self.output:
            self.init_output_v()
        if not self.name_ref in self.input.fields:
            index = np.ones([self.input.get_na(), self.input.get_nr(), self.input.get_ne()])            
        else:
            if not 'ref' in self.output:
                self.init_output_ref()
            index = self.get_doppler_ref_index(filter_name, self.opt)
        self.update(filter_name, index, self.opt)
        self.filter_run = True

