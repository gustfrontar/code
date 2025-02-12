import numpy as np 
import pysteps
import glob
from datetime import datetime
from datetime import timedelta
import pickle
import os 

conf_experiment = os.environ['conf_experiment']
my_conf_exp = 'import conf_'+ conf_experiment + ' as conf'
exec(my_conf_exp)

GeneralConf = conf.GeneralConf
ModelConf   = conf.ModelConf

def parseTimeFFN(filename):
    timeSTR=filename.split(".")[1]
    Dtime=datetime.strptime(timeSTR,"%Y%m%d_%H%M%S")
    ano=Dtime.year
    mes=Dtime.month
    dia=Dtime.day
    hora=Dtime.hour
    minuto=Dtime.minute
    segundo=Dtime.second
    return {"ano":ano,"mes":mes,"dia":dia,"hora":hora,"min":minuto,"seg":segundo,"datetime":Dtime}


BASEDIRCAPPI = GeneralConf['DataBase']+'/'+GeneralConf['data']+'/'+GeneralConf['rad']+'/'+GeneralConf['date']+'/'+GeneralConf['path_ref']


print(BASEDIRCAPPI)


list_cappis = glob.glob(BASEDIRCAPPI + '/*.pckl')
list_cappis.sort()

arch_ini = [s for s in list_cappis if GeneralConf['date_start_mv'] in s]
print(arch_ini)
print(list_cappis.index(arch_ini[0]))
start_list = list_cappis.index(arch_ini[0])

arch_end = [s for s in list_cappis if GeneralConf['date_end_mv'] in s]
print(arch_end)
print(list_cappis.index(arch_end[0]))
end_list = list_cappis.index(arch_end[0])

nx = ModelConf['nx']
ny = ModelConf['ny']
mint_scan = ModelConf['mint_scan']
maxt_scan = ModelConf['maxt_scan']
OUTPUTMV =GeneralConf['DataBase']+'/'+GeneralConf['data']+'/'+GeneralConf['rad']+'/'+GeneralConf['date']+ GeneralConf['filedir_mv']
os.makedirs(OUTPUTMV, exist_ok=True)



for i in range(start_list,end_list-ModelConf['dt']):
    file_cappi = list_cappis[i]
    print('field 0',file_cappi)
    file_cappi1 = list_cappis[i+ModelConf['dt']]
    print('field 1', file_cappi1)
    date = parseTimeFFN(file_cappi)
    date1 = parseTimeFFN(file_cappi1)
    fecha = date['datetime'].strftime('%Y%m%d_%H%M%S')
    fecha1 = date1['datetime'].strftime('%Y%m%d_%H%M%S')
    sec_imag = timedelta.total_seconds(date1["datetime"]-date["datetime"])
    dt = sec_imag
    if sec_imag <= maxt_scan and sec_imag > mint_scan:
        with open(file_cappi,'rb') as handle:
            cappi_2km = pickle.load(handle)
        with open(file_cappi1,'rb') as handle:
            cappi_2km1 = pickle.load(handle)
        fields = np.empty(((2,nx,ny)))
        fields[0,:,:] = cappi_2km['data']
        fields[1,:,:] = cappi_2km1['data']
        fields[fields<=-998]=np.nan
        fields[fields<=ModelConf['ref_umb']]=ModelConf['ref_min']
        motion = pysteps.motion.vet.vet(fields, sectors=ModelConf['sectors'], smooth_gain=ModelConf['smooth'], first_guess=ModelConf['first_guess'],  intermediate_steps=ModelConf['inst_steps'], verbose=ModelConf['verb'], indexing=ModelConf['yx'], padding=ModelConf['padding'], options=ModelConf['options'])
        mv_fields={"u_motions": motion[0,:,:],
                   "v_motions": motion[1,:,:],
                   "motion":motion,
                   "dt":sec_imag}
        with open(OUTPUTMV+ "/mv."+fecha1+".pckl", 'wb') as handle:
            pickle.dump(mv_fields, handle)

