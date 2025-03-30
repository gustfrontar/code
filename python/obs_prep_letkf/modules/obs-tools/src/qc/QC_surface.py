import os, shutil, sys, glob, re
from datetime import datetime, timedelta
import pandas as pd
import numpy as np
from copy import deepcopy
from scipy.stats import t as t_student
import common_obs
from catalog_qc import ADPAUT as conf_QC
import catalog_obs as ctlg_obs
import multiprocessing as mp
QCPROC = int(os.environ['QCPROC'])
SRCDIR = os.environ['SRCDIR']
OBSREPO = os.environ['REPODIR']
QCDIR = os.environ['QCDIR'] + '/surface'
TOOLSDIR = os.environ['TOOLSDIR']

sys.path.append(f'{SRCDIR}/process')
############
# FLAGS QC #
############
FLAG_NOQC = -1
FLAG_GOOD = 0
FLAG_MEAN = 1
FLAG_STD = 2
FLAG_CORR = 4
FLAG_MISS = 8

#Tomo 0.995 porque quiero dejar 0.5 en cada cola de la distribucion y asi sacar solo el 1% mas extremo
alfa_rechazo = conf_QC['ALFA_RECHAZO']

furthest_neighbor_H = conf_QC['NEIGHBOR_DIST_H']
furthest_neighbor_V = conf_QC['NEIGHBOR_DIST_V']

#Numero de estaciones cercanas que se van a considerar para aplicar el QC
N_neighbors = conf_QC['N_NEIGHBORS']

#Minimo numero de estaciones cercanas confiables que se requieren para aplicar el QC
min_reliable_neighbors = conf_QC['MIN_RELIABLE_NEIGHBORS']

#Minimo de observaciones con valores distintos que considero para decir que la estacion no reporta siempre lo mismo
min_different_obs = conf_QC['MIN_DIFFERENT_OBS']

QC_SOURCES = conf_QC['QC_SOURCES']
QC_SUPPORT_SOURCES = conf_QC['QC_SUPPORT_SOURCES']

################
# GROSS LIMITS #
################
gross_limits = conf_QC['GROSS_LIMITS']

###########################
# Long term QC thresholds #
###########################
ltqccorrtr  = conf_QC['CORR_THRESHOLD']   #Threshold for the mean correlation between the station and its neighbors.


def proc_filename(filename, ini, end, read_data, ctlg):

    if os.stat(filename).st_size == 0:
        return pd.DataFrame()

    # Read data
    try:
       data = read_data(filename)
    except RuntimeError as err:
       return pd.DataFrame()

    if data.empty: return pd.DataFrame()
    
    # Filter data outside time range
    data = common_obs.filter_time(data, ini, end)
    if data.empty: return pd.DataFrame()

    # Filter data
    data = common_obs.apply_filters(ctlg, data, ['Lon', 'Lat', 'Lev', 'DateTime'])
    if data.empty: return pd.DataFrame()

    return data


def get_ADPAUT_data(OBSREPO, DATE_INI, DATE_END, VARS):
    """
    Funcion que genera los datos de las estaciones automaticas en el formato necesario
    
    OBSREPO: Path a las observaciones
    DATE_INI: Fecha inicial para la que busco observaciones
    DATE_END: Fecha final para la que busco observaciones
    VARS: Variables observadas que quiero conservar
    """

    import ADPAUT as ADPAUT_fun

    file_list_ADPAUT = ADPAUT_fun.get_files(f'{OBSREPO}/ADPAUT/', DATE_INI, DATE_END)

    if file_list_ADPAUT.empty:
        return pd.DataFrame(), pd.DataFrame()

    #arg_list = [(filename, DATE_INI, DATE_END, ADPAUT_fun.read_data, ctlg_obs.adpaut) for filename in file_list_ADPAUT['Path']]
    pool_out = []
    for my_file in file_list_ADPAUT['Path'] :
        pool_out.append( proc_filename( my_file , DATE_INI, DATE_END, ADPAUT_fun.read_data, ctlg_obs.adpaut ) )

    #with mp.Pool(min(QCPROC, len(arg_list))) as pool:
    #    pool_out = pool.starmap(proc_filename, arg_list)
    ADPAUT_data = pd.concat(pool_out)

    ADPAUT_metadata = ADPAUT_data[['ID', 'Prop', 'Lat', 'Lon', 'Lev']].drop_duplicates(ignore_index = True)
    ADPAUT_metadata[['Prop', 'ID']] = ADPAUT_metadata[['Prop', 'ID']].astype(str)
    ADPAUT_metadata = ADPAUT_metadata.set_index(['Prop', 'ID'])
    ADPAUT_metadata = ADPAUT_metadata.rename({'Lat': 'latitud', 'Lon': 'longitud', 'Lev': 'altura'}, axis = 1)

    ADPAUT_data = ADPAUT_data[VARS + ['ID', 'Prop', 'DateTime']]
    ADPAUT_data[['ID', 'Prop']] = ADPAUT_data[['ID', 'Prop']].astype(str)
    ADPAUT_data = ADPAUT_data.set_index(['Prop', 'ID'])

    common_index = ADPAUT_metadata.index.intersection(ADPAUT_data.index).sort_values().unique()
    ADPAUT_data = ADPAUT_data.loc[common_index]
    ADPAUT_metadata = ADPAUT_metadata.loc[common_index]

    return ADPAUT_data, ADPAUT_metadata

def get_ADPSFC_data(OBSREPO, DATE_INI, DATE_END, VARS):
    """
    Funcion que genera los datos de las estaciones convencionales del SMN en el formato necesario
    
    OBSREPO: Path a las observaciones
    DATE_INI: Fecha inicial para la que busco observaciones
    DATE_END: Fecha final para la que busco observaciones
    VARS: Variables observadas que quiero conservar
    """
    import ADPSFC as ADPSFC_fun

    file_list_ADPSFC = ADPSFC_fun.get_files(f'{OBSREPO}/ADPSFC/', DATE_INI, DATE_END)
    if file_list_ADPSFC.empty:
        return pd.DataFrame(), pd.DataFrame()

    #arg_list = [(filename, DATE_INI, DATE_END, ADPSFC_fun.read_data, ctlg_obs.adpsfc) for filename in file_list_ADPSFC['Path']]
    pool_out = []
    for my_file in file_list_ADPSFC['Path'] :
        pool_out.append( proc_filename( my_file , DATE_INI, DATE_END, ADPSFC_fun.read_data, ctlg_obs.adpsfc ) ) 
    #with mp.Pool(min(QCPROC, len(arg_list))) as pool:
    #    pool_out = pool.starmap(proc_filename, arg_list)
    ADPSFC_data = pd.concat(pool_out)

    ADPSFC_data.loc[:, 'Prop'] = 'SMN_conv'

    ADPSFC_metadata = ADPSFC_data[['ID', 'Prop', 'Lat', 'Lon', 'Lev']].drop_duplicates(ignore_index = True)
    ADPSFC_metadata[['Prop', 'ID']] = ADPSFC_metadata[['Prop', 'ID']].astype(str)
    ADPSFC_metadata = ADPSFC_metadata.set_index(['Prop', 'ID'])
    ADPSFC_metadata = ADPSFC_metadata.rename({'Lat': 'latitud', 'Lon': 'longitud', 'Lev': 'altura'}, axis = 1)

    ADPSFC_data = ADPSFC_data[VARS + ['ID', 'Prop', 'DateTime']]
    ADPSFC_data[['ID', 'Prop']] = ADPSFC_data[['ID', 'Prop']].astype(str)
    ADPSFC_data = ADPSFC_data.set_index(['Prop', 'ID'])

    common_index = ADPSFC_metadata.index.intersection(ADPSFC_data.index).sort_values().unique()
    ADPSFC_data = ADPSFC_data.loc[common_index]
    ADPSFC_metadata = ADPSFC_metadata.loc[common_index]

    return ADPSFC_data, ADPSFC_metadata


def get_AWOS_data(OBSREPO, DATE_INI, DATE_END, VARS):
    """
    Funcion que genera los datos de los AWOS en el formato necesario
    
    OBSREPO: Path a las observaciones
    DATE_INI: Fecha inicial para la que busco observaciones
    DATE_END: Fecha final para la que busco observaciones
    VARS: Variables observadas que quiero conservar
    """

    import AWOS as AWOS_fun

    file_list_AWOS = AWOS_fun.get_files(f'{OBSREPO}/AWOS/', DATE_INI, DATE_END)
    if file_list_AWOS.empty:
        return pd.DataFrame(), pd.DataFrame()

    #arg_list = [(filename, DATE_INI, DATE_END, AWOS_fun.read_data, ctlg_obs.awos) for filename in file_list_AWOS['Path']]
    pool_out = []
    for my_file in file_list_AWOS['Path'] :
        pool_out.append( proc_filename( my_file , DATE_INI, DATE_END, AWOS_fun.read_data, ctlg_obs.awos ) )
    #with mp.Pool(min(QCPROC, len(arg_list))) as pool:
    #pool_out = pool.starmap(proc_filename, arg_list)
    AWOS_data = pd.concat(pool_out)

    AWOS_data.loc[:, 'Prop'] = 'AWOS'

    AWOS_metadata = AWOS_data[['ID', 'Prop', 'Lat', 'Lon', 'Lev']].drop_duplicates(ignore_index = True)
    AWOS_metadata[['Prop', 'ID']] = AWOS_metadata[['Prop', 'ID']].astype(str)
    AWOS_metadata = AWOS_metadata.set_index(['Prop', 'ID'])
    AWOS_metadata = AWOS_metadata.rename({'Lat': 'latitud', 'Lon': 'longitud', 'Lev': 'altura'}, axis = 1)

    AWOS_data['wspd10'] = np.hypot(AWOS_data['u10'], AWOS_data['v10'])
    AWOS_data = AWOS_data[VARS + ['ID', 'Prop', 'DateTime']]
    AWOS_data[['ID', 'Prop']] = AWOS_data[['ID', 'Prop']].astype(str)
    AWOS_data = AWOS_data.set_index(['Prop', 'ID'])

    common_index = AWOS_metadata.index.intersection(AWOS_data.index).sort_values().unique()
    AWOS_data = AWOS_data.loc[common_index]
    AWOS_metadata = AWOS_metadata.loc[common_index]

    return AWOS_data, AWOS_metadata


def gross_check(data, VARS):
    """
    Funcion que elimina observaciones fuera de rango

    data: dataframe con observaciones
    VARS: lista de variables a revisar
    """


    for var in VARS:
        gross_flag = np.logical_or(data[var] < gross_limits[var][0], data[var] > gross_limits[var][1])
        data.loc[gross_flag, var] = np.nan      
    
    return data


def remove_repeated_obs(data, VARS):
    """
    Funcion que elimina las observaciones que se consideran repetidas

    data: dataframe con observaciones
    VARS: lista de variables a revisar
    """

    #Cuento la cantidad de observaciones diferentes de cada estacion y si es menor al permitido, flagueo la 
    #variable para esa estacion
    #TODO Pensar si no es mejor eliminar series continuas de datos repetidos. ¿Como hacer con las estaciones que tiene diferente frecuencia?
    for var in VARS:
        nunique_values = data[var].groupby(['Prop', 'ID']).nunique()
        index2flag = nunique_values.where(nunique_values < min_different_obs).dropna().index

        data.loc[index2flag, var] = np.nan
 
    return data


def flag_missing_data(data, df_QC, VARS):
    """
    Funcion que flaguea las estaciones que no tienen datos

    data: dataframe con observaciones
    df_QC: dataframe con los flags de calidad del dato
    VARS: lista de variables a revisar
    """

    for var in VARS:
        data_available = np.isnan(data[var]).groupby(['Prop', 'ID']).all()
        index_no_data = data_available[data_available].index

        index_only_sources = index_no_data.intersection(df_QC.index)
        df_QC.loc[index_only_sources, var] = FLAG_MISS

    return df_QC
    

def get_distance(lat_from, lat_to, lon_from, lon_to, H_from = 0, H_to = 0):
    """
    Funcion que calcula la distancia y la diferencia de altura entre 2 puntos o entre un punto y una 
    lista de puntos

    lat_from: latitud desde la que se miden las distancias
    lon_from: longitud desde la que se miden las distancias
    H_from: altura desde la que se mide la diferencia
    lat_to: latitud o lista de latitudes hasta la/s que se quiere medir la distancia
    lon_to: longitud o lista de longitudes hasta la/s que se quiere medir la distancia
    H_to: altura o lista de alturas de la/s que se quiere medir la diferencia
    """

    root = (np.sin((np.deg2rad(lat_from) - np.deg2rad(lat_to))/2)**2
            ) + np.cos(np.deg2rad(lat_from))*np.cos(np.deg2rad(lat_to))*(
            np.sin((np.deg2rad(lon_from) - np.deg2rad(lon_to))/2)**2)

    dist_H = 2*6371*np.arcsin(np.sqrt(root))
    dist_H.name = 'dist_H'

    dist_V = np.abs(H_from - H_to)
    dist_V.name = 'dist_V'

    dist = pd.concat([dist_H, dist_V], axis = 1)

    return dist


def get_neighbors(df_QC, metadata, VARS):
    """
    Funcion que devuelve las estaciones cercanas para cada estacion y variable

    df_QC: dataframe con los flags de calidad del dato
    metadata: dataframe con la metadata de las estaciones
    VARS: variables a las que se quiere calcular las estaciones cercanas
    """

    indexes = df_QC.index
    metadata_indexes = metadata.index

    #Dataframe que va a tener la lista de estaciones cercanas a cada estacion para cada variable
    data_neighbors = pd.DataFrame(data = None, index = indexes, columns = VARS, dtype = object)

    for index in indexes:
        if index not in metadata_indexes:
            data_neighbors.loc[index, :] = [[]]
            continue
        height, lat, lon = metadata.loc[index].squeeze()
        #Calculo la distancia de la estacion a todas las demas
        dist = get_distance(lat, metadata['latitud'], lon, metadata['longitud'], height, metadata['altura'])
        dist = dist.sort_values(by = 'dist_H')[1:] # No considero la menor distancia, es la propia estacion
        dist = dist[dist['dist_H'] < furthest_neighbor_H]
        dist = dist[dist['dist_V'] < furthest_neighbor_V]
        for var in VARS:
            data_neighbors.loc[index, var] = dist.index.tolist()#to_numpy()

    return data_neighbors


def QC_var(data, metadata, df_QC_var, neighbors, var):
    """
    data: dataframe con observaciones
    metadata: dataframe con la metadata de las estaciones
    df_QC_var: dataframe con los flags de calidad del dato de la variable
    neighbors: dataframe con las estaciones cercanas para cada estacion y variable
    var: variable sobre la que se quiere hacer el QC
    """

    indexes = df_QC_var.index

    for index_station in indexes:

        if df_QC_var.loc[(index_station), var] == FLAG_MISS:
            continue

        index_neighbors = neighbors.at[index_station, var]

        data_station = data.loc[index_station, [var, 'fechaHora']]

        n_data_station = len(data_station)

        data_QC = data_station.reset_index()[[var, 'fechaHora']].set_index('fechaHora')
        data_QC = data_QC.rename({var:'station'}, axis = 1)

        dates_station = data_QC.index

        diff_mean = []
        diff_std = []
        freq_weight = []
        QC_neighbors = []
        flag_support = []
        n = 0
        for index_n in index_neighbors:
            col = f'neighbor_{n}'
            neighbor_is_support = (index_n not in indexes)
            if (not neighbor_is_support) and (df_QC_var.at[index_n, var] > 0): continue # No tengo en cuenta a los vecinos que ya no pasaron el QC y que no son datos de soporte

            data_neighbor = data.loc[index_n, [var, 'fechaHora']].dropna()

            dates_intersection = dates_station.intersection(data_neighbor['fechaHora'])
            n_common_data = len(dates_intersection)

            if n_common_data/n_data_station < 0.1: continue # No tengo en cuenta los vecinos en que la serie en comun es muy corta
            
            data_neighbor = data_neighbor.reset_index()[[var, 'fechaHora']].set_index('fechaHora')
            data_neighbor = data_neighbor.rename({var:col}, axis = 1)
            data_QC = data_QC.join(data_neighbor, how = 'left')

            tmp = data_QC.loc[dates_intersection, ['station', col]]

            diff_mean.append(float(tmp[col].mean()) - float(tmp['station'].mean()))
            diff_std.append(float(tmp[col].std()) - float(tmp['station'].std()))
            freq_weight.append(n_common_data/n_data_station)
            QC_neighbors.append(index_n)
            flag_support.append(neighbor_is_support)

            n += 1

        #Si ninguna de las estaciones cercanas paso el QC no puedo aplicarlo.
        if len(QC_neighbors) < min_reliable_neighbors:
            df_QC_var.at[index_station, var] += FLAG_NOQC
            continue

        estadisticos = data_QC.corr()['station'].iloc[1:]
        estadisticos.name = 'correlacion'
        estadisticos = estadisticos.to_frame()

        estadisticos['media'] = diff_mean
        estadisticos['desvio'] = diff_std
        estadisticos['is_support'] = flag_support

        freq_weight = np.array(freq_weight)

        loc_s = metadata.loc[index_station, ['latitud', 'longitud', 'altura']].squeeze()
        loc_n = metadata.loc[QC_neighbors, ['latitud', 'longitud', 'altura']]
        dist = get_distance(loc_s['latitud'], loc_n['latitud'], loc_s['longitud'], loc_n['longitud'], loc_s['altura'], loc_n['altura'])

        dist_h = dist['dist_H']
        dist_h[dist_h < 20] = 20  

        dist_v = dist['dist_V']
        dist_v[dist_v < 100] = 100

        weights = ((1/dist_h) * (1/dist_v) * freq_weight).values 

        estadisticos['weights'] = weights

        for neighbor in estadisticos.sort_values('correlacion').index:
            if estadisticos.at[neighbor, 'is_support']: continue # No chequeo las estaciones de soporte, confio que el dato es bueno
            n = int(neighbor.strip('neighbor_'))
            tmp_weights = deepcopy(weights)
            tmp_weights[n] = 0

            deg_freedom = np.sum(tmp_weights > 0) - 1

            if deg_freedom == 0:
                continue

            mean_mean, mean_std =  weighted_avg_and_std(estadisticos['media'], tmp_weights)
            std_mean, std_std =  weighted_avg_and_std(estadisticos['desvio'], tmp_weights)
            corr_mean, corr_std =  weighted_avg_and_std(estadisticos['correlacion'], tmp_weights)

            t_mean = t_student.sf(estadisticos['media'].iat[n], deg_freedom, mean_mean, mean_std)
            t_std = t_student.sf(estadisticos['desvio'].iat[n], deg_freedom, std_mean, std_std)
            t_corr = t_student.sf(estadisticos['correlacion'].iat[n], deg_freedom, corr_mean, corr_std)

            if t_mean < 0.5: t_mean = 1 - t_mean
            if t_std < 0.5: t_std = 1 - t_std
            if t_corr < 0.5: t_corr = 1 - t_corr

            if abs(t_mean) > alfa_rechazo or abs(t_std) > alfa_rechazo or abs(t_corr) > alfa_rechazo:
                weights[n] = 0.0


        #Si no se tiene la cantidad suficiente de estaciones cercanas validas, no se aplica el QC
        if np.sum(weights > 0) < min_reliable_neighbors:
            df_QC_var.at[index_station, var] += FLAG_NOQC
            continue

        estadisticos['weights'] = weights

        #Elimino las estaciones cercanas que tienen peso 0
        estadisticos = estadisticos.where(estadisticos['weights'] != 0).dropna()

        estadisticos = estadisticos.sort_values(by = 'weights', ascending = False)

        estadisticos = estadisticos.iloc[:N_neighbors]

        mean_mean, mean_std = weighted_avg_and_std(estadisticos['media'], estadisticos['weights'])
        std_mean, std_std = weighted_avg_and_std(estadisticos['desvio'], estadisticos['weights'])
        corr_mean, corr_std = weighted_avg_and_std(estadisticos['correlacion'], estadisticos['weights'])

        deg_freedom = len(estadisticos) - 1

        t_mean = t_student.sf(0, deg_freedom, mean_mean, mean_std)
        t_std = t_student.sf(0, deg_freedom, std_mean, std_std)

        if t_mean < 0.5: t_mean = 1 - t_mean
        if t_std < 0.5: t_std = 1 - t_std

        if t_mean > alfa_rechazo:
            df_QC_var.at[index_station, var] += FLAG_MEAN
    
        if t_std > alfa_rechazo:
            df_QC_var.at[index_station, var] += FLAG_STD

        if corr_mean < ltqccorrtr[var] or np.ma.is_masked(corr_mean):
            df_QC_var.at[index_station, var] += FLAG_CORR
 
    return df_QC_var
        

def long_term_QC(data, metadata, df_QC, neighbors):
    """
    Funcion que realiza el control de calidad

    data: dataframe con observaciones
    metadata: dataframe con la metadata de las estaciones
    df_QC: dataframe con los flags de calidad del dato
    neighbors: dataframe con las estaciones cercanas para cada estacion y variable
    """


    #arg_list = []
    #for var in df_QC.columns:
    #    arg_list.append((data, metadata, df_QC[[var]], neighbors, var))

    pool_out = []
    for my_var in df_QC.columns :
        pool_out.append( QC_var( data , metadata , df_QC[[my_var]] , neighbors , my_var ) )

    #with mp.Pool(min(QCPROC, len(arg_list))) as pool:
    #    pool_out = pool.starmap(QC_var, arg_list)

    df_QC_update = pd.concat(pool_out, axis = 1)

    return df_QC_update


def weighted_avg_and_std(values, weights):
    """
    Funcion que devuelve la media y el desvio pesados

    values: valores sobre los que se quiere calcular la media/desvio
    weights: pesos para calcular la media/desvio
    
    """
    weights = weights/np.sum(weights)

    #Hay casos en que la correlacion de una estacion daba nan, entonces lo enmascaro para hacer estas cuentas
    values_mask = np.ma.MaskedArray(values, mask = np.isnan(values))

    average = np.average(values_mask, weights = weights)
    # Fast and numerically precise:
    variance = np.average((values_mask - average)**2, weights = weights)

    return (average, np.sqrt(variance))


if __name__ == "__main__":

    exit_code = 0

    #RUNDIR = ENVVARS['RUNDIR']

    DATE_END = datetime.strptime( sys.argv[1] , '%Y%m%d%H%M%S' )
    DATE_INI = DATE_END - timedelta(days = conf_QC['N_DAYS_QC'])

    read_functions = {'ADPAUT': get_ADPAUT_data, 'ADPSFC': get_ADPSFC_data, 'AWOS': get_AWOS_data}

    #Variables a las que se les va a aplicar el QC
    VARS = ['t2', 'rh2', 'psfc', 'wspd10']

    #Leo la metadata
    filename_stationsQC = f'{QCDIR}/stations.QC'
    if not os.path.exists(filename_stationsQC):
        os.makedirs(QCDIR, exist_ok = True)
        stations_QC_template = TOOLSDIR + '/templates/stations.QC'
        shutil.copy(stations_QC_template, filename_stationsQC)
    stations_QC = pd.read_json(filename_stationsQC)
    stations_QC = stations_QC.set_index(['idEstacion', 'idPropietario'])
    stations_QC[[f'QC_{var}' for var in VARS]] = -999

    #Leo las observaciones
    print(f'Reading data from stations ({DATE_INI:%Y/%m/%d} - {DATE_END:%Y/%m/%d})')
    source_obs_list = []
    source_metadata_list = []
    for source in conf_QC['QC_SOURCES']:
        data, metadata = read_functions[source](OBSREPO, DATE_INI, DATE_END, VARS)
        source_obs_list.append(data)
        source_metadata_list.append(metadata)

    source_obs = pd.concat(source_obs_list)
    if source_obs.empty:
        print('No source observations were found', file = sys.stderr)
        sys.exit(1)

    source_obs = source_obs.sort_index()
    source_metadata = pd.concat(source_metadata_list)
    source_obs = source_obs.sort_index()

    support_obs_list = []
    support_metadata_list = []
    for source in conf_QC['QC_SUPPORT_SOURCES']:
        data, metadata = read_functions[source](OBSREPO, DATE_INI, DATE_END, VARS)
        support_obs_list.append(data)
        support_metadata_list.append(metadata)

    if support_obs_list:
        support_obs = pd.concat(support_obs_list)
        support_obs = support_obs.sort_index()
        support_metadata = pd.concat(support_metadata_list)
        support_metadata = support_metadata.sort_index()
    else:
        support_obs = pd.DataFrame()
        support_metadata = pd.DataFrame()

    if support_obs.empty:
        print('No support observations were found', file = sys.stderr)
        exit_code = 2

    total_data = pd.concat([source_obs, support_obs])
    total_data = total_data.sort_index()
    total_data = total_data.rename({'DateTime': 'fechaHora'}, axis = 1)
    total_metadata = pd.concat([source_metadata, support_metadata], sort = True)

    #Aplico un gross check para eliminar datos fuera de rango
    total_data = gross_check(total_data, VARS)

    #Elimino estaciones que reporten casi todos los datos iguales
    total_data = remove_repeated_obs(total_data, VARS)

    total_data = total_data.dropna(how = 'all', subset = VARS)
    total_data = total_data.drop_duplicates()
    data_index = total_data.index.drop_duplicates()
    total_metadata = total_metadata.loc[data_index]

    # Chequeo metadata duplicada porque paso que una estacion del SMN (Merlo Bs. As.) cambio de altura en los datos que llegan de la base 
    # y el codigo rompe al pensar que hay mas estaciones.
    duplicated_metadata = total_metadata.index.duplicated(keep = 'last')
    if any(duplicated_metadata):
        print('Founded duplicated metadata', file = sys.stderr)
        print(total_metadata[total_metadata.index.duplicated(keep = False)], file = sys.stderr)
        total_metadata = total_metadata[~duplicated_metadata]
        exit_code = 2

    index_QC = total_metadata.index.intersection(source_obs.index.unique(), sort = True)

    #Genero el dataframe que va a tener el resultado de aplicar el QC
    df_QC = pd.DataFrame(data = FLAG_GOOD, index = index_QC, columns = VARS)

    #Aplico un flag de dato faltante a los casos en que la estacion para alguna variable no tenga dato
    df_QC = flag_missing_data(total_data, df_QC, VARS)

    #Obtengo la lista de estaciones cercanas para cada estaciones y variable
    neighbors = get_neighbors(df_QC, total_metadata, VARS)

    #Aplico el QC
    print('Applying the quality control')
    df_QC = long_term_QC(total_data, total_metadata, df_QC, neighbors)

    #Renombro las columnas del df del QC para que coincida con las de stations.QC
    df_QC = df_QC.rename(columns = {var: f'QC_{var}' for var in VARS})

    df_QC = df_QC.reset_index()
    df_QC = df_QC.rename({'Prop': 'idPropietario', 'ID': 'idEstacion'}, axis = 1)
    df_QC[['idEstacion', 'idPropietario']] = df_QC[['idEstacion', 'idPropietario']].astype(int)
    df_QC = df_QC.set_index(['idEstacion', 'idPropietario'])
    stations_QC.loc[df_QC.index, ['QC_wspd10', 'QC_t2', 'QC_psfc', 'QC_rh2']] = df_QC

    stations_QC = stations_QC.reset_index()

    #Guardo el resultado del QC para cada estacion y variable
    print(f'Saving QC to {filename_stationsQC}')
    stations_QC.to_json(filename_stationsQC, orient = 'records')

    sys.exit(exit_code)
