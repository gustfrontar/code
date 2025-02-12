from.base_filter import BaseFilter
import pyart
import numpy as np
from utils.qc_utils import qc 

class DealiasingEdgeFilter(BaseFilter):
    def __init__(self, super_radar, opt, output):
        BaseFilter.__init__(self, super_radar, opt, output)
        self.input = super_radar
        self.opt = opt
        self.undef = opt.undef
        self.var_update = getattr(opt, "DealiasingEdgeFilter")['var_update_list']
        if self.name_v in super_radar.fields:
            self.undef_v = super_radar.get_undef_v()
            self.nyquist_v = super_radar.get_nyquist_vel(0, check_uniform=True)
        else:
            raise KeyError('Could not find ' + self.name_v + ' data in the radar fields')

    def get_max_nyquist_v(self):
         return np.max(self.nyquist_v)
     
     
    def get_doppler_edge_filter(self, nyquist_v, opt, filter_name):
        dealiasing = self.output['DealiasingFilter']
        self.na = self.input.get_na()
        self.nr = self.input.get_nr()
        self.ne = self.input.get_ne()
        return qc.doppler_edge_filter(vdiff=dealiasing['vdiff'], v=dealiasing['vda'],
                                    nx=self.na, ny=self.nr, nz=self.ne,
                                    undef=self.undef_v,
                                    nboxx=getattr(opt,filter_name)['nx'],
                                    nboxy=getattr(opt,filter_name)['ny'],
                                    nboxz=getattr(opt,filter_name)['nz'],
                                    edge_tr=nyquist_v/3.0)  
     
    def get_index(self, filter_name):
        nyquist_v = self.get_max_nyquist_v()
        index = self.get_doppler_edge_filter(nyquist_v, self.opt, filter_name).astype('float')
        index[index == self.undef_v] = self.opt.undef
        return index

    def run(self):
        filter_name = "DealiasingEdgeFilter"
        if self.name_v in self.input.fields:
            if not self.output_init:
                self.init_output(filter_name)
            elif not 'v' in self.output:
                self.init_output_v()
        if 'DealiasingFilter' in self.output:
            index = self.get_index(filter_name)
            self.update(filter_name, index, self.opt)
            self.filter_run = True
        else:
            raise KeyError('DealiasingFilter must be implemented before run this filter')


