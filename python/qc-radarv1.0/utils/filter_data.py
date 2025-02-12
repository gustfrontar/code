import pyart
import os
from datetime import datetime, timedelta


def filter_data(df, opt):
    """
    Filtrar los datos del dataframe usando los parametros especificados
    en las opciones

    Parametros
    ----------
    df : DataFrame
        Dataframe de pandas con los datos de los radares. Tiene que tener 
        las columnas 'radar_id', 'ext' y los indices son las fechas.
    opt : objeto
        Opciones

    Devuelve
    --------
    df_valid : DataFrame
        Dataframe con los datos que cumplen las condiciones especificadas en
        las opciones
    """
    instruments = opt.instrument_list
    file_ext = opt.file_ext
    df_valid = df[df['radar_id'].isin(instruments)] # Filter valid origin radar
    df_valid = df_valid[df_valid['ext'].isin(file_ext)] # Filter valid file extension
    init = datetime.strptime(opt.start_date, "%Y%m%d_%H%M%S")
    endt = datetime.strptime(opt.end_date, "%Y%m%d_%H%M%S")
    mask = (df_valid.index < endt) & (df_valid.index>=init)
    df_valid = df_valid.loc[mask]
    return df_valid

