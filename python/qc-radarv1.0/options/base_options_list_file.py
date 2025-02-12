import argparse
import os
from configparser import ConfigParser, ExtendedInterpolation
import shlex
import numpy as np

class BaseOptions():
    """
       Configuración del control de calidad de radar

       A través de esta clase se puede acceder a la lista de archivos a procesar, se
       especifican los radares que se van a controlar, y las fechas que se van a tener
       en cuenta.
       También se especifica el nombre de los campos y la configuración de los filtros.

    """

    def __init__(self,userconf):
        """Reset the class; indicates the class hasn't been initialized"""
        self.initialized = False
        self.config = ConfigParser(interpolation=ExtendedInterpolation())
        self.config.optionxform = str
        self.config.read("config.ini")
        self.config.read(userconf)
        self.initialize_qc_env()



    def initialize_qc_env(self):
        self.dataroot = self.config['System']['DATAROOT']
        self.instrument_list = self.config['Base']['INSTRUMENT_LIST'].split(',')
        self.file_ext = self.config['Base']['DATA_EXT'].split(',')
        self.start_date = self.config['Base']['QC_I_DATE']
        self.end_date = self.config['Base']['QC_E_DATE']

        self.topo_radar_datapath = self.config['System']['TOPO_RADAR_DATAPATH']
        self.topo_raw_datapath = self.config['System']['TOPO_RAW_DATAPATH']

        #### Opciones de guardar #########
        self.save_netcdf = eval(self.config['Base']['SAVE_NETCDF'])
        self.netcdf_output_path = self.config['Base']['NETCDF_OUTPATH']

        self.filters_plot = eval(self.config['Base']['FILTERS_PLOT'])
        self.plots_outpath = self.config['Base']['PLOTS_OUTPATH']
        self.plots_elevs= list(map(int,self.config['Base']['PLOTS_ELEVS'].split(',')))

        self.filters = self.config['Base']['FILTERS'].split(',')

        self.no_rain_ref_val = float(self.config['Base']['NO_RAIN_REF_VALUE'])
        self.undef = float(self.config['System']['UNDEF'])

        self.name_ref = 'reflectivity'
        self.name_v = 'velocity'
        self.name_rho = 'cross_correlation_ratio'
        self.save = dict()
        self.save['keep_original_fields'] = eval(self.config['Base']['SAVE_KEEP_ORIGINAL'])

        self.initialize_filter_options()

        return self

    def initialize_filter_options(self):
        if "AttenuationFilter" in self.filters:
            self.AttenuationFilter = dict()
            self.AttenuationFilter['flag'] = eval(self.config['AttenuationFilter']['FLAG'])
            self.AttenuationFilter['nx'] = int(self.config['AttenuationFilter']['NX'])
            self.AttenuationFilter['ny'] = int(self.config['AttenuationFilter']['NY'])
            self.AttenuationFilter['nz'] = int(self.config['AttenuationFilter']['NZ'])
            self.AttenuationFilter['save'] = eval(self.config['AttenuationFilter']['SAVE'])
            self.AttenuationFilter['ify'] = np.array(list(map(float,self.config['AttenuationFilter']['IFY'].split(','))))
            self.AttenuationFilter['ifx'] = np.array(list(map(float,self.config['AttenuationFilter']['IFX'].split(','))))
            self.AttenuationFilter['w'] = float(self.config['AttenuationFilter']['W'])
            self.AttenuationFilter['code'] = int(self.config['AttenuationFilter']['CODE'])
            self.AttenuationFilter['attcalerror'] = float(self.config['AttenuationFilter']['ATTCALERROR'])
            self.AttenuationFilter['attenuation_correction'] = eval(self.config['AttenuationFilter']['ATTENUATION_CORRECTION'])
            self.AttenuationFilter['is_power'] = eval(self.config['AttenuationFilter']['IS_POWER'])
            self.AttenuationFilter['att_coefs'] = np.array(list(map(float,self.config['AttenuationFilter']['ATT_COEFS'].split(','))))
            self.AttenuationFilter['force'] = eval(self.config['AttenuationFilter']['FORCE'])
            self.AttenuationFilter['force_value'] = float(self.config['AttenuationFilter']['FORCE_VALUE'])
            self.AttenuationFilter['order'] = [int(self.config['AttenuationFilter']['ORDER'])]
            self.AttenuationFilter['var_update_list'] = self.config['AttenuationFilter']['VAR_UPDATE_LIST'].split(',')
            self.AttenuationFilter['sequential'] = eval(self.config['AttenuationFilter']['SEQUENTIAL'])
            self.AttenuationFilter['fill_value'] = self.config['AttenuationFilter']['FILLVALUE']


        if "BlockingFilter" in self.filters:

            self.BlockingFilter = dict()
            self.BlockingFilter['flag'] = eval(self.config['BlockingFilter']['flag'])
            self.BlockingFilter['blocking_correction'] = eval(self.config['BlockingFilter']['blocking_correction'])
            self.BlockingFilter['save'] = eval(self.config['BlockingFilter']['save'])
            self.BlockingFilter['code'] = int(self.config['BlockingFilter']['code'])
            self.BlockingFilter['ify'] = np.array(list(map(float,self.config['BlockingFilter']['ify'].split(','))))
            self.BlockingFilter['ifx'] = np.array(list(map(float,self.config['BlockingFilter']['ifx'].split(','))))
            self.BlockingFilter['w'] = float(self.config['BlockingFilter']['w'])
            self.BlockingFilter['code'] = int(self.config['BlockingFilter']['code'])
            self.BlockingFilter['force'] = eval(self.config['BlockingFilter']['force'])
            self.BlockingFilter['force_value'] = float(self.config['BlockingFilter']['force_value'])
            self.BlockingFilter['order'] = [int(self.config['BlockingFilter']['order'])]
            self.BlockingFilter['var_update_list'] = self.config['BlockingFilter']['var_update_list'].split(',')
            self.BlockingFilter['sequential'] = eval(self.config['BlockingFilter']['sequential'])
            self.BlockingFilter['fill_value'] = self.config['BlockingFilter']['fill_value']


        if "DealiasingFilter" in self.filters:
            self.DealiasingFilter = dict()
            self.DealiasingFilter['flag'] = eval(self.config['DealiasingFilter']['flag'])
            self.DealiasingFilter['interval_split'] = int(self.config['DealiasingFilter']['interval_split'])
            self.DealiasingFilter['skip_between_ray'] = int(self.config['DealiasingFilter']['skip_between_ray'])
            self.DealiasingFilter['skip_along_ray'] = int(self.config['DealiasingFilter']['skip_along_ray'])
            self.DealiasingFilter['nx'] = int(self.config['DealiasingFilter']['nx'])
            self.DealiasingFilter['ny'] = int(self.config['DealiasingFilter']['ny'])
            self.DealiasingFilter['nz'] = int(self.config['DealiasingFilter']['nz'])
            self.DealiasingFilter['code'] = int(self.config['DealiasingFilter']['code'])
            self.DealiasingFilter['sequential'] = eval(self.config['DealiasingFilter']['sequential'])
            self.DealiasingFilter['order'] = [int(self.config['DealiasingFilter']['order'])]
            self.DealiasingFilter['var_update_list'] = self.config['DealiasingFilter']['var_update_list'].split(',')
            self.DealiasingFilter['fill_value'] = self.config['DealiasingFilter']['fill_value']

        if "DealiasingEdgeFilter" in self.filters:
            self.DealiasingEdgeFilter = dict()
            self.DealiasingEdgeFilter['flag'] = eval(self.config['DealiasingEdgeFilter']['flag'])
            self.DealiasingEdgeFilter['nx'] = int(self.config['DealiasingEdgeFilter']['nx'])
            self.DealiasingEdgeFilter['ny'] = int(self.config['DealiasingEdgeFilter']['ny'])
            self.DealiasingEdgeFilter['nz'] = int(self.config['DealiasingEdgeFilter']['nz'])
            self.DealiasingEdgeFilter['save'] = eval(self.config['DealiasingEdgeFilter']['save'])
            self.DealiasingEdgeFilter['ify'] = np.array(list(map(float,self.config['DealiasingEdgeFilter']['ify'].split(','))))
            self.DealiasingEdgeFilter['ifx'] = np.array(list(map(float,self.config['DealiasingEdgeFilter']['ifx'].split(','))))
            self.DealiasingEdgeFilter['w'] = float(self.config['DealiasingEdgeFilter']['w'])
            self.DealiasingEdgeFilter['code'] = int(self.config['DealiasingEdgeFilter']['code'])
            self.DealiasingEdgeFilter['force'] = eval(self.config['DealiasingEdgeFilter']['force'])
            self.DealiasingEdgeFilter['force_value'] = float(self.config['DealiasingEdgeFilter']['force_value'])
            self.DealiasingEdgeFilter['order'] = [int(self.config['DealiasingEdgeFilter']['order'])]
            self.DealiasingEdgeFilter['var_update_list'] = self.config['DealiasingEdgeFilter']['var_update_list'].split(',')
            self.DealiasingEdgeFilter['sequential'] = eval(self.config['DealiasingEdgeFilter']['sequential'])
            self.DealiasingEdgeFilter['fill_value'] = self.config['DealiasingEdgeFilter']['fill_value']


        if "DopplerLocalStdFilter" in self.filters:
            filter_name='DopplerLocalStdFilter'
            self.DopplerLocalStdFilter=dict()
            self.DopplerLocalStdFilter['flag'] = eval(self.config['DopplerLocalStdFilter']['flag'])
            self.DopplerLocalStdFilter['nx'] = int(self.config['DopplerLocalStdFilter']['nx'])
            self.DopplerLocalStdFilter['ny'] = int(self.config['DopplerLocalStdFilter']['ny'])
            self.DopplerLocalStdFilter['nz'] = int(self.config['DopplerLocalStdFilter']['nz'])
            self.DopplerLocalStdFilter['ify'] = np.array(list(map(float, self.config['DopplerLocalStdFilter']['ify'].split(','))))
            self.DopplerLocalStdFilter['ifx'] = np.array(list(map(float, self.config['DopplerLocalStdFilter']['ifx'].split(','))))
            self.DopplerLocalStdFilter['save'] = eval(self.config['DopplerLocalStdFilter']['save'])
            self.DopplerLocalStdFilter['w'] = float(self.config['DopplerLocalStdFilter']['w'])
            self.DopplerLocalStdFilter['code'] = int(self.config['DopplerLocalStdFilter']['code'])
            self.DopplerLocalStdFilter['force'] = eval(self.config['DopplerLocalStdFilter']['force'])
            self.DopplerLocalStdFilter['force_value'] = float(self.config['DopplerLocalStdFilter']['force_value'])
            self.DopplerLocalStdFilter['var_update_list'] = self.config['DopplerLocalStdFilter']['var_update_list'].split(',')
            self.DopplerLocalStdFilter['sequential'] = eval(self.config['DopplerLocalStdFilter']['sequential'])
            self.DopplerLocalStdFilter['fill_value'] = self.config['DopplerLocalStdFilter']['fill_value']


        if 'DopplerSpatialCoherenceFilter' in self.filters:
            self.DopplerSpatialCoherenceFilter = dict()
            self.DopplerSpatialCoherenceFilter['flag'] = eval(self.config['DopplerSpatialCoherenceFilter']['flag'])
            self.DopplerSpatialCoherenceFilter['nx'] = int(self.config['DopplerSpatialCoherenceFilter']['nx'])
            self.DopplerSpatialCoherenceFilter['ny'] = int(self.config['DopplerSpatialCoherenceFilter']['ny'])
            self.DopplerSpatialCoherenceFilter['nz'] = int(self.config['DopplerSpatialCoherenceFilter']['nz'])
            self.DopplerSpatialCoherenceFilter['threshold_undef'] = float(self.config['DopplerSpatialCoherenceFilter']['threshold_undef'])
            self.DopplerSpatialCoherenceFilter['threshold_corr'] = float(self.config['DopplerSpatialCoherenceFilter']['threshold_corr'])
            self.DopplerSpatialCoherenceFilter['threshold_coherence_index'] = int(self.config['DopplerSpatialCoherenceFilter']['threshold_coherence_index'])
            self.DopplerSpatialCoherenceFilter['compute_horizontal_coherence'] = eval(self.config['DopplerSpatialCoherenceFilter']['compute_horizontal_coherence'])
            self.DopplerSpatialCoherenceFilter['compute_vertical_coherence'] = eval(self.config['DopplerSpatialCoherenceFilter']['compute_vertical_coherence'])
            self.DopplerSpatialCoherenceFilter['npass_filter'] = int(self.config['DopplerSpatialCoherenceFilter']['npass_filter'])
            self.DopplerSpatialCoherenceFilter['azimuthfilter'] = eval(self.config['DopplerSpatialCoherenceFilter']['azimuthfilter'])
            self.DopplerSpatialCoherenceFilter['rangefilter'] = eval(self.config['DopplerSpatialCoherenceFilter']['rangefilter'])
            self.DopplerSpatialCoherenceFilter['elevationfilter'] = eval(self.config['DopplerSpatialCoherenceFilter']['elevationfilter'])
            self.DopplerSpatialCoherenceFilter['enable_speckle'] = eval(self.config['DopplerSpatialCoherenceFilter']['enable_speckle'])
            self.DopplerSpatialCoherenceFilter['speckle_threshold'] = float(self.config['DopplerSpatialCoherenceFilter']['speckle_threshold'])
            self.DopplerSpatialCoherenceFilter['consistency_metric'] = self.config['DopplerSpatialCoherenceFilter']['consistency_metric']
            self.DopplerSpatialCoherenceFilter['constant_consistency_threshold'] = float(self.config['DopplerSpatialCoherenceFilter']['constant_consistency_threshold'])
            self.DopplerSpatialCoherenceFilter['ify'] = np.array(list(map(float, self.config['DopplerSpatialCoherenceFilter']['ify'].split(','))))
            self.DopplerSpatialCoherenceFilter['ifx'] = np.array(list(map(float, self.config['DopplerSpatialCoherenceFilter']['ifx'].split(','))))
            self.DopplerSpatialCoherenceFilter['save'] = eval(self.config['DopplerSpatialCoherenceFilter']['save'])
            self.DopplerSpatialCoherenceFilter['w'] = float(self.config['DopplerSpatialCoherenceFilter']['w'])
            self.DopplerSpatialCoherenceFilter['code'] = int(self.config['DopplerSpatialCoherenceFilter']['code'])
            self.DopplerSpatialCoherenceFilter['force'] = eval(self.config['DopplerSpatialCoherenceFilter']['force'])
            self.DopplerSpatialCoherenceFilter['force_value'] = float(self.config['DopplerSpatialCoherenceFilter']['force_value'])
            self.DopplerSpatialCoherenceFilter['var_update_list'] = self.config['DopplerSpatialCoherenceFilter']['var_update_list']
            self.DopplerSpatialCoherenceFilter['sequential'] = self.config['DopplerSpatialCoherenceFilter']['sequential'].split(',')
            self.DopplerSpatialCoherenceFilter['fill_value'] = self.config['DopplerSpatialCoherenceFilter']['fill_value']


        if 'DopplerNoiseFilter' in self.filters:
            self.DopplerNoiseFilter = dict()
            self.DopplerNoiseFilter['flag'] = eval(self.config['DopplerNoiseFilter']['flag'])
            self.DopplerNoiseFilter['nx'] =  list(map(int, self.config['DopplerNoiseFilter']['nx'].split(',')))
            self.DopplerNoiseFilter['ny'] =  list(map(int, self.config['DopplerNoiseFilter']['ny'].split(',')))
            self.DopplerNoiseFilter['nz'] =  list(map(int, self.config['DopplerNoiseFilter']['nz'].split(',')))
            self.DopplerNoiseFilter['threshold'] = list(map(float, self.config['DopplerNoiseFilter']['threshold'].split(',')))
            self.DopplerNoiseFilter['n_filter_pass'] = list(map(int, self.config['DopplerNoiseFilter']['n_filter_pass'].split(',')))
            self.DopplerNoiseFilter['save'] = eval(self.config['DopplerNoiseFilter']['save'])
            self.DopplerNoiseFilter['ify'] = np.array(list(map(float, self.config['DopplerNoiseFilter']['ify'].split(','))))
            self.DopplerNoiseFilter['ifx'] = np.array(list(map(float, self.config['DopplerNoiseFilter']['ifx'].split(','))))
            self.DopplerNoiseFilter['w'] = float(self.config['DopplerNoiseFilter']['w'])
            self.DopplerNoiseFilter['code'] = int(self.config['DopplerNoiseFilter']['code'])
            self.DopplerNoiseFilter['force'] = eval(self.config['DopplerNoiseFilter']['force'])
            self.DopplerNoiseFilter['force_value'] = float(self.config['DopplerNoiseFilter']['force_value'])
            self.DopplerNoiseFilter['var_update_list'] = self.config['DopplerNoiseFilter']['var_update_list'].split(',')
            self.DopplerNoiseFilter['sequential'] = eval(self.config['DopplerNoiseFilter']['sequential'])
            self.DopplerNoiseFilter['fill_value'] = self.config['DopplerNoiseFilter']['fill_value']


        if 'DopplerSpeckleFilter' in self.filters:
            self.DopplerSpeckleFilter=dict()
            self.DopplerSpeckleFilter['flag'] = eval(self.config['DopplerSpeckleFilter']['flag'])
            self.DopplerSpeckleFilter['nx'] = int(self.config['DopplerSpeckleFilter']['nx'])
            self.DopplerSpeckleFilter['ny'] = int(self.config['DopplerSpeckleFilter']['ny'])
            self.DopplerSpeckleFilter['nz'] = int(self.config['DopplerSpeckleFilter']['nz'])
            self.DopplerSpeckleFilter['save'] = eval(self.config['DopplerSpeckleFilter']['save'])
            self.DopplerSpeckleFilter['ify'] = np.array(list(map(float, self.config['DopplerSpeckleFilter']['ify'].split(','))))
            self.DopplerSpeckleFilter['ifx'] = np.array(list(map(float, self.config['DopplerSpeckleFilter']['ifx'].split(','))))
            self.DopplerSpeckleFilter['w'] = float(self.config['DopplerSpeckleFilter']['w'])
            self.DopplerSpeckleFilter['code'] = int(self.config['DopplerSpeckleFilter']['code'])
            self.DopplerSpeckleFilter['dvtr'] = float(self.config['DopplerSpeckleFilter']['dvtr'])
            self.DopplerSpeckleFilter['force'] = eval(self.config['DopplerSpeckleFilter']['force'])
            self.DopplerSpeckleFilter['force_value'] = float(self.config['DopplerSpeckleFilter']['force_value'])
            self.DopplerSpeckleFilter['var_update_list'] = self.config['DopplerSpeckleFilter']['var_update_list'].split(',')
            self.DopplerSpeckleFilter['sequential'] = eval(self.config['DopplerSpeckleFilter']['sequential'])
            self.DopplerSpeckleFilter['fill_value'] = self.config['DopplerSpeckleFilter']['fill_value']


        if 'LowDopplerFilter' in self.filters:
            self.LowDopplerFilter=dict()
            self.LowDopplerFilter['flag'] = eval(self.config['LowDopplerFilter']['flag'])
            self.LowDopplerFilter['nx'] = int(self.config['LowDopplerFilter']['nx'])
            self.LowDopplerFilter['ny'] = int(self.config['LowDopplerFilter']['ny'])
            self.LowDopplerFilter['nz'] = int(self.config['LowDopplerFilter']['nz'])
            self.LowDopplerFilter['save'] = eval(self.config['LowDopplerFilter']['save'])
            self.LowDopplerFilter['ify'] = np.array(list(map(float, self.config['LowDopplerFilter']['ify'].split(','))))
            self.LowDopplerFilter['ifx'] = np.array(list(map(float, self.config['LowDopplerFilter']['ifx'].split(','))))
            self.LowDopplerFilter['w'] = float(self.config['LowDopplerFilter']['w'])
            self.LowDopplerFilter['code'] = int(self.config['LowDopplerFilter']['code'])
            self.LowDopplerFilter['force'] = eval(self.config['LowDopplerFilter']['force'])
            self.LowDopplerFilter['force_value'] = float(self.config['LowDopplerFilter']['force_value'])
            self.LowDopplerFilter['height_thr'] = float(self.config['LowDopplerFilter']['height_thr'])
            self.LowDopplerFilter['var_update_list'] = self.config['LowDopplerFilter']['var_update_list'].split(',')
            self.LowDopplerFilter['sequential'] = eval(self.config['LowDopplerFilter']['sequential'])
            self.LowDopplerFilter['fill_value'] = self.config['LowDopplerFilter']['fill_value']


        if 'EchoTopFilter' in self.filters:
            self.EchoTopFilter = dict()
            self.EchoTopFilter['flag'] = eval(self.config['EchoTopFilter']['flag'])
            self.EchoTopFilter['nx'] = int(self.config['EchoTopFilter']['nx'])
            self.EchoTopFilter['ny'] = int(self.config['EchoTopFilter']['ny'])
            self.EchoTopFilter['nz'] = int(self.config['EchoTopFilter']['nz'])
            self.EchoTopFilter['fast_computation'] = eval(self.config['EchoTopFilter']['fast_computation'])
            self.EchoTopFilter['save'] = eval(self.config['EchoTopFilter']['save'])
            self.EchoTopFilter['ify'] = np.array(list(map(float, self.config['EchoTopFilter']['ify'].split(','))))
            self.EchoTopFilter['ifx'] = np.array(list(map(float, self.config['EchoTopFilter']['ifx'].split(','))))
            self.EchoTopFilter['w'] = float(self.config['EchoTopFilter']['w'])
            self.EchoTopFilter['code'] = int(self.config['EchoTopFilter']['code'])
            self.EchoTopFilter['heigthtr'] = float(self.config['EchoTopFilter']['heigthtr'])
            self.EchoTopFilter['force'] = eval(self.config['EchoTopFilter']['force'])
            self.EchoTopFilter['force_value'] = float(self.config['EchoTopFilter']['force_value'])
            self.EchoTopFilter['var_update_list'] = self.config['EchoTopFilter']['var_update_list'].split(',')
            self.EchoTopFilter['sequential'] = eval(self.config['EchoTopFilter']['sequential'])
            self.EchoTopFilter['fill_value'] = self.config['EchoTopFilter']['fill_value']


        if 'EchoDepthFilter' in self.filters:
            self.EchoDepthFilter=dict()
            self.EchoDepthFilter['flag'] = eval(self.config['EchoDepthFilter']['flag'])
            self.EchoDepthFilter['nx'] = int(self.config['EchoDepthFilter']['nx'])
            self.EchoDepthFilter['ny'] = int(self.config['EchoDepthFilter']['ny'])
            self.EchoDepthFilter['nz'] = int(self.config['EchoDepthFilter']['nz'])
            self.EchoDepthFilter['save'] = eval(self.config['EchoDepthFilter']['save'])
            self.EchoDepthFilter['ify'] = np.array(list(map(float, self.config['EchoDepthFilter']['ify'].split(','))))
            self.EchoDepthFilter['ifx'] = np.array(list(map(float, self.config['EchoDepthFilter']['ifx'].split(','))))
            self.EchoDepthFilter['w'] = float(self.config['EchoDepthFilter']['w'])
            self.EchoDepthFilter['code'] = int(self.config['EchoDepthFilter']['code'])
            self.EchoDepthFilter['heigthtr'] = float(self.config['EchoDepthFilter']['heigthtr'])
            self.EchoDepthFilter['force'] = eval(self.config['EchoDepthFilter']['force'])
            self.EchoDepthFilter['force_value'] = float(self.config['EchoDepthFilter']['force_value'])
            self.EchoDepthFilter['var_update_list'] = self.config['EchoDepthFilter']['var_update_list'].split(',')
            self.EchoDepthFilter['sequential'] = eval(self.config['EchoDepthFilter']['sequential'])
            self.EchoDepthFilter['fill_value'] = self.config['EchoDepthFilter']['fill_value']


        if 'InterferenceFilter' in self.filters:
            self.InterferenceFilter = dict()
            self.InterferenceFilter['flag'] = eval(self.config['InterferenceFilter']['flag'])
            self.InterferenceFilter['save'] = eval(self.config['InterferenceFilter']['save'])
            self.InterferenceFilter['nx'] = int(self.config['InterferenceFilter']['nx'])
            self.InterferenceFilter['ny'] = int(self.config['InterferenceFilter']['ny'])
            self.InterferenceFilter['nz'] = int(self.config['InterferenceFilter']['nz'])
            self.InterferenceFilter['code'] = int(self.config['InterferenceFilter']['code'])
            self.InterferenceFilter['ify'] = np.array(list(map(float, self.config['InterferenceFilter']['ify'].split(','))))
            self.InterferenceFilter['ifx'] = np.array(list(map(float, self.config['InterferenceFilter']['ifx'].split(','))))
            self.InterferenceFilter['force'] = eval(self.config['InterferenceFilter']['force'])
            self.InterferenceFilter['force_value'] = float(self.config['InterferenceFilter']['force_value'])
            self.InterferenceFilter['att'] = float(self.config['InterferenceFilter']['att'])
            self.InterferenceFilter['Smooth_Ref'] = eval(self.config['InterferenceFilter']['Smooth_Ref'])
            self.InterferenceFilter['Power_Regression'] = eval(self.config['InterferenceFilter']['Power_Regression'])
            self.InterferenceFilter['offset'] = int(self.config['InterferenceFilter']['offset'])
            self.InterferenceFilter['AzimuthFilter'] = eval(self.config['InterferenceFilter']['AzimuthFilter'])
            self.InterferenceFilter['ElevationFilter'] = eval(self.config['InterferenceFilter']['ElevationFilter'])
            self.InterferenceFilter['npass_filter'] = int(self.config['InterferenceFilter']['npass_filter'])
            self.InterferenceFilter['percent_valid_threshold'] = float(self.config['InterferenceFilter']['percent_valid_threshold'])
            self.InterferenceFilter['corr_threshold'] = list(map(float, self.config['InterferenceFilter']['corr_threshold'].split(',')))
            self.InterferenceFilter['azimuth_ref_diff_threshold'] = list(map(float, self.config['InterferenceFilter']['azimuth_ref_diff_threshold'].split(',')))
            self.InterferenceFilter['ref_threshold'] = float(self.config['InterferenceFilter']['ref_threshold'])
            self.InterferenceFilter['percent_ref_threshold'] = float(self.config['InterferenceFilter']['percent_ref_threshold'])
            self.InterferenceFilter['var_update_list'] = self.config['InterferenceFilter']['var_update_list'].split(',')
            self.InterferenceFilter['sequential'] = eval(self.config['InterferenceFilter']['sequential'])
            self.InterferenceFilter['fill_value'] = self.config['InterferenceFilter']['fill_value']


        if 'LowElevFilter' in self.filters:
            self.LowElevFilter=dict()
            self.LowElevFilter['flag'] = eval(self.config['LowElevFilter']['flag'])
            self.LowElevFilter['nx'] = int(self.config['LowElevFilter']['nx'])
            self.LowElevFilter['ny'] = int(self.config['LowElevFilter']['ny'])
            self.LowElevFilter['nz'] = int(self.config['LowElevFilter']['nz'])
            self.LowElevFilter['save'] = eval(self.config['LowElevFilter']['save'])
            self.LowElevFilter['min_angle'] = float(self.config['LowElevFilter']['min_angle'])
            self.LowElevFilter['height_thr'] = float(self.config['LowElevFilter']['height_thr'])
            self.LowElevFilter['w'] = float(self.config['LowElevFilter']['w'])
            self.LowElevFilter['code'] = int(self.config['LowElevFilter']['code'])
            self.LowElevFilter['ify'] = np.array(list(map(float, self.config['LowElevFilter']['ify'].split(','))))
            self.LowElevFilter['ifx'] = np.array(list(map(float, self.config['LowElevFilter']['ifx'].split(','))))
            self.LowElevFilter['force'] = eval(self.config['LowElevFilter']['force'])
            self.LowElevFilter['force_value'] = float(self.config['LowElevFilter']['force_value'])
            self.LowElevFilter['var_update_list'] = self.config['LowElevFilter']['var_update_list'].split(',')
            self.LowElevFilter['sequential'] = eval(self.config['LowElevFilter']['sequential'])
            self.LowElevFilter['fill_value'] = self.config['LowElevFilter']['fill_value']


        if 'MissingRefFilter' in self.filters:
            self.MissingRefFilter = dict()
            self.MissingRefFilter['flag'] = eval(self.config['MissingRefFilter']['flag'])
            self.MissingRefFilter['threshold'] = float(self.config['MissingRefFilter']['threshold'])
            self.MissingRefFilter['nmissing_max'] = float(self.config['MissingRefFilter']['nmissing_max'])
            self.MissingRefFilter['save'] = eval(self.config['MissingRefFilter']['save'])
            self.MissingRefFilter['w'] = float(self.config['MissingRefFilter']['w'])
            self.MissingRefFilter['ify'] = np.array(list(map(float, self.config['MissingRefFilter']['ify'].split(','))))
            self.MissingRefFilter['ifx'] = np.array(list(map(float, self.config['MissingRefFilter']['ifx'].split(','))))
            self.MissingRefFilter['code'] = [int(self.config['MissingRefFilter']['code'])]
            self.MissingRefFilter['force'] = eval(self.config['MissingRefFilter']['force'])
            self.MissingRefFilter['force_value'] = float(self.config['MissingRefFilter']['force_value'])
            self.MissingRefFilter['var_update_list'] = self.config['MissingRefFilter']['var_update_list'].split(',')
            self.MissingRefFilter['sequential'] = eval(self.config['MissingRefFilter']['sequential'])
            self.MissingRefFilter['fill_value'] = self.config['MissingRefFilter']['fill_value']


        if 'PowerFilter' in self.filters:
            self.PowerFilter=dict()
            self.PowerFilter['a'] = float(self.config['PowerFilter']['a'])
            self.PowerFilter['C'] = float(self.config['PowerFilter']['C'])
            self.PowerFilter['qc_pot_thr'] = float(self.config['PowerFilter']['qc_pot_thr'])
            self.PowerFilter['var_update_list'] = self.config['PowerFilter']['var_update_list'].split(',')
            self.PowerFilter['sequential'] = eval(self.config['PowerFilter']['sequential'])
            self.PowerFilter['code'] = [int(self.config['PowerFilter']['code'])]

        if 'DopplerRefFilter' in self.filters:
            self.DopplerRefFilter = dict()
            self.DopplerRefFilter['flag'] = eval(self.config['DopplerRefFilter']['flag'])
            self.DopplerRefFilter['filter_undef'] = eval(self.config['DopplerRefFilter']['filter_undef'])
            self.DopplerRefFilter['threshold'] = float(self.config['DopplerRefFilter']['threshold'])
            self.DopplerRefFilter['nx'] = int(self.config['DopplerRefFilter']['nx'])
            self.DopplerRefFilter['ny'] = int(self.config['DopplerRefFilter']['ny'])
            self.DopplerRefFilter['nz'] = int(self.config['DopplerRefFilter']['nz'])
            self.DopplerRefFilter['save'] = eval(self.config['DopplerRefFilter']['save'])
            self.DopplerRefFilter['ify'] = np.array(list(map(float, self.config['DopplerRefFilter']['ify'].split(','))))
            self.DopplerRefFilter['ifx'] = np.array(list(map(float, self.config['DopplerRefFilter']['ifx'].split(','))))
            self.DopplerRefFilter['code'] = [int(self.config['DopplerRefFilter']['code'])]
            self.DopplerRefFilter['sequential'] = eval(self.config['DopplerRefFilter']['sequential'])
            self.DopplerRefFilter['var_update_list'] = self.config['DopplerRefFilter']['var_update_list'].split(',')
            self.DopplerRefFilter['force'] = eval(self.config['DopplerRefFilter']['force'])
            self.DopplerRefFilter['force_value'] = float(self.config['DopplerRefFilter']['force_value'])
            self.DopplerRefFilter['fill_value'] = self.config['DopplerRefFilter']['fill_value']

        if 'DopplerRangeFilter' in self.filters:
            self.DopplerRangeFilter=dict()
            self.DopplerRangeFilter['flag'] = eval(self.config['DopplerRangeFilter']['flag'])
            self.DopplerRangeFilter['min'] = float(self.config['DopplerRangeFilter']['min'])
            self.DopplerRangeFilter['max'] = float(self.config['DopplerRangeFilter']['max'])
            self.DopplerRangeFilter['save'] = eval(self.config['DopplerRangeFilter']['save'])
            self.DopplerRangeFilter['w'] = float(self.config['DopplerRangeFilter']['w'])
            self.DopplerRangeFilter['ify'] = np.array(list(map(float, self.config['DopplerRangeFilter']['ify'].split(','))))
            self.DopplerRangeFilter['ifx'] = np.array(list(map(float, self.config['DopplerRangeFilter']['ifx'].split(','))))
            self.DopplerRangeFilter['code'] = int(self.config['DopplerRangeFilter']['code'])
            self.DopplerRangeFilter['force'] = eval(self.config['DopplerRangeFilter']['force'])
            self.DopplerRangeFilter['force_value'] = float(self.config['DopplerRangeFilter']['force_value'])
            self.DopplerRangeFilter['var_update_list'] = self.config['DopplerRangeFilter']['var_update_list'].split(',')
            self.DopplerRangeFilter['sequential'] = eval(self.config['DopplerRangeFilter']['sequential'])
            self.DopplerRangeFilter['fill_value'] = self.config['DopplerRangeFilter']['fill_value']

        if 'RefRangeFilter' in self.filters:
            self.RefRangeFilter=dict()
            self.RefRangeFilter['flag'] = eval(self.config['RefRangeFilter']['flag'])
            self.RefRangeFilter['min'] = float(self.config['RefRangeFilter']['min'])
            self.RefRangeFilter['max'] = float(self.config['RefRangeFilter']['max'])
            self.RefRangeFilter['save'] = eval(self.config['RefRangeFilter']['save'])
            self.RefRangeFilter['w'] = float(self.config['RefRangeFilter']['w'])
            self.RefRangeFilter['ify'] = np.array(list(map(float, self.config['RefRangeFilter']['ify'].split(','))))
            self.RefRangeFilter['ifx'] = np.array(list(map(float, self.config['RefRangeFilter']['ifx'].split(','))))
            self.RefRangeFilter['code'] = [int(self.config['RefRangeFilter']['code'])]
            self.RefRangeFilter['force'] = eval(self.config['RefRangeFilter']['force'])
            self.RefRangeFilter['force_value'] = float(self.config['RefRangeFilter']['force_value'])
            self.RefRangeFilter['var_update_list'] = self.config['RefRangeFilter']['var_update_list'].split(',')
            self.RefRangeFilter['sequential'] = eval(self.config['RefRangeFilter']['sequential'])
            self.RefRangeFilter['fill_value'] = self.config['RefRangeFilter']['fill_value']

        if 'RefSpeckleFilter' in self.filters:
            self.RefSpeckleFilter = dict()
            self.RefSpeckleFilter['flag'] = eval(self.config['RefSpeckleFilter']['flag'])
            self.RefSpeckleFilter['nx'] = int(self.config['RefSpeckleFilter']['nx'])
            self.RefSpeckleFilter['ny'] = int(self.config['RefSpeckleFilter']['ny'])
            self.RefSpeckleFilter['nz'] = int(self.config['RefSpeckleFilter']['nz'])
            self.RefSpeckleFilter['save'] = eval(self.config['RefSpeckleFilter']['save'])
            self.RefSpeckleFilter['ify'] = np.array(list(map(float, self.config['RefSpeckleFilter']['ify'].split(','))))
            self.RefSpeckleFilter['ifx'] = np.array(list(map(float, self.config['RefSpeckleFilter']['ifx'].split(','))))
            self.RefSpeckleFilter['w'] = float(self.config['RefSpeckleFilter']['w'])
            self.RefSpeckleFilter['code'] = [int(self.config['RefSpeckleFilter']['code'])]
            self.RefSpeckleFilter['reftr'] = float(self.config['RefSpeckleFilter']['reftr'])
            self.RefSpeckleFilter['force'] = eval(self.config['RefSpeckleFilter']['force'])
            self.RefSpeckleFilter['force_value'] = float(self.config['RefSpeckleFilter']['force_value'])
            self.RefSpeckleFilter['var_update_list'] = self.config['RefSpeckleFilter']['var_update_list'].split(',')
            self.RefSpeckleFilter['sequential'] = eval(self.config['RefSpeckleFilter']['sequential'])
            self.RefSpeckleFilter['fill_value'] = self.config['RefSpeckleFilter']['fill_value']


        if 'RhoFilter' in self.filters:
            self.RhoFilter = dict()
            self.RhoFilter['flag'] = eval(self.config['RhoFilter']['flag'])
            self.RhoFilter['nx'] = int(self.config['RhoFilter']['nx'])
            self.RhoFilter['ny'] = int(self.config['RhoFilter']['ny'])
            self.RhoFilter['nz'] = int(self.config['RhoFilter']['nz'])
            self.RhoFilter['save'] = eval(self.config['RhoFilter']['save'])
            self.RhoFilter['ify'] = np.array(list(map(float, self.config['RhoFilter']['ify'].split(','))))
            self.RhoFilter['ifx'] = np.array(list(map(float, self.config['RhoFilter']['ifx'].split(','))))
            self.RhoFilter['w'] = float(self.config['RhoFilter']['w'])
            self.RhoFilter['code'] = [int(self.config['RhoFilter']['code'])]
            self.RhoFilter['force'] = eval(self.config['RhoFilter']['force'])
            self.RhoFilter['force_value'] = float(self.config['RhoFilter']['force_value'])
            self.RhoFilter['var_update_list'] = self.config['RhoFilter']['var_update_list'].split(',')
            self.RhoFilter['sequential'] = eval(self.config['RhoFilter']['sequential'])
            self.RhoFilter['fill_value'] = self.config['RhoFilter']['fill_value']
            self.RhoFilter['ref_threshold'] = float(self.config['RhoFilter']['ref_threshold'])


        if 'DopplerTextureFilter' in self.filters:
            self.DopplerTextureFilter = dict()
            self.DopplerTextureFilter['flag'] = eval(self.config['DopplerTextureFilter']['flag'])
            self.DopplerTextureFilter['nx'] = int(self.config['DopplerTextureFilter']['nx'])
            self.DopplerTextureFilter['ny'] = int(self.config['DopplerTextureFilter']['ny'])
            self.DopplerTextureFilter['nz'] = int(self.config['DopplerTextureFilter']['nz'])
            self.DopplerTextureFilter['save'] = eval(self.config['DopplerTextureFilter']['save'])
            self.DopplerTextureFilter['ify'] = np.array(list(map(float, self.config['DopplerTextureFilter']['ify'].split(','))))
            self.DopplerTextureFilter['ifx'] = np.array(list(map(float, self.config['DopplerTextureFilter']['ifx'].split(','))))
            self.DopplerTextureFilter['w'] = float(self.config['DopplerTextureFilter']['w'])
            self.DopplerTextureFilter['code'] = [int(self.config['DopplerTextureFilter']['code'])]
            self.DopplerTextureFilter['force'] = eval(self.config['DopplerTextureFilter']['force'])
            self.DopplerTextureFilter['force_value'] = float(self.config['DopplerTextureFilter']['force_value'])
            self.DopplerTextureFilter['var_update_list'] = self.config['DopplerTextureFilter']['var_update_list'].split(',')
            self.DopplerTextureFilter['sequential'] = eval(self.config['DopplerTextureFilter']['sequential'])
            self.DopplerTextureFilter['fill_value'] = self.config['DopplerTextureFilter']['fill_value']


        if 'ReflectivityTextureFilter' in self.filters:
            self.ReflectivityTextureFilter=dict()
            self.ReflectivityTextureFilter['flag'] = eval(self.config['ReflectivityTextureFilter']['flag'])
            self.ReflectivityTextureFilter['index_nx'] = int(self.config['ReflectivityTextureFilter']['index_nx'])
            self.ReflectivityTextureFilter['index_ny'] = int(self.config['ReflectivityTextureFilter']['index_ny'])
            self.ReflectivityTextureFilter['index_nz'] = int(self.config['ReflectivityTextureFilter']['index_nz'])
            self.ReflectivityTextureFilter['nx'] = int(self.config['ReflectivityTextureFilter']['nx'])
            self.ReflectivityTextureFilter['ny'] = int(self.config['ReflectivityTextureFilter']['ny'])
            self.ReflectivityTextureFilter['nz'] = int(self.config['ReflectivityTextureFilter']['nz'])
            self.ReflectivityTextureFilter['use_smooth_ref'] = eval(self.config['ReflectivityTextureFilter']['use_smooth_ref'])
            self.ReflectivityTextureFilter['smooth_ref_tr'] = float(self.config['ReflectivityTextureFilter']['smooth_ref_tr'])
            self.ReflectivityTextureFilter['save'] = eval(self.config['ReflectivityTextureFilter']['save'])
            self.ReflectivityTextureFilter['ify'] = np.array(list(map(float, self.config['ReflectivityTextureFilter']['ify'].split(','))))
            self.ReflectivityTextureFilter['ifx'] = np.array(list(map(float, self.config['ReflectivityTextureFilter']['ifx'].split(','))))
            self.ReflectivityTextureFilter['w'] = float(self.config['ReflectivityTextureFilter']['w'])
            self.ReflectivityTextureFilter['code'] = int(self.config['ReflectivityTextureFilter']['code'])
            self.ReflectivityTextureFilter['force'] = eval(self.config['ReflectivityTextureFilter']['force'])
            self.ReflectivityTextureFilter['force_value'] = float(self.config['ReflectivityTextureFilter']['force_value'])
            self.ReflectivityTextureFilter['var_update_list'] = self.config['ReflectivityTextureFilter']['var_update_list'].split(',')
            self.ReflectivityTextureFilter['sequential'] = eval(self.config['ReflectivityTextureFilter']['sequential'])
            self.ReflectivityTextureFilter['fill_value'] = self.config['ReflectivityTextureFilter']['fill_value']



    def print_options(self,opt):
        """ Print and save options
        """
        message = ''
        message += '----------------- Options ---------------\n'
        for k, v in sorted(vars(opt).items()):
            comment = ''
            default = self.parser.get_default(k)
            if v != default:
                comment = '\t[default: %s]' % str(default)
            message += '{:>25}: {:<30}{}\n'.format(str(k), str(v), comment)
        message += '----------------- End -------------------'
        print(message)
