''' Read  csv file, clean the data and save in an npz
    Read a npz file, define a set of variables with high correlation and
    return a matrix with the time series
'''
import numpy as np
import pandas as pd
import datetime
import matplotlib.dates as mdates
import matplotlib.pyplot as plt
#import stats

def read_npz(dat_fname='dat/oil_day_openval.npz'):
    dat=np.load(dat_fname)
    return dat['day'],dat['price'],dat['company']

def load_historical_prices(npz_filepath):
    """
    Carga los datos historicos de precios desde un archivo NPZ
    y los convierte a arrays de NumPy organizados.
    
    Parametros:
    - npz_filepath: Ruta al archivo NPZ generado por filter_historical_prices
    
    Retorna:
    - Un diccionario con los siguientes elementos:
      - 'fechas': Array de fechas como objetos datetime
      - 'precios': Array 2D con los precios (filas=fechas, columnas=tickers)
      - 'tickers': Lista de simbolos bursatiles
      - 'df': DataFrame de pandas con los datos organizados
      - 'metadata': Informacion adicional guardada en el archivo
    """
    try:
        # Cargar el archivo NPZ
        datos_cargados = np.load(npz_filepath, allow_pickle=True)
        
        # Extraer los arrays de datos
        days, start_date =  transform_fechas2dias(datos_cargados['fechas'])
        prices = datos_cargados['datos']
        company = datos_cargados['tickers']
        
        
        return days,prices,company,start_date
    
    except Exception as e:
        print(f"Error al cargar el archivo NPZ: {e}")

        return None
    
def transform_fechas2dias(fechas):
    #fechas=npz_data["fechas"]
    # Convertir fechas almacenadas en timestamps a formato datetime
    fechas = pd.to_datetime(fechas)

    # Determinar la fecha inicial
    start_date = fechas[0]  # Se asume que las fechas están ordenadas

    # Calcular los días transcurridos desde la fecha inicial
    return (fechas - start_date).days, start_date

def load_ts(assets=None,dat_fname='dat/oil_day_closeval.npz'):
    day,price,company = read_npz(dat_fname=dat_fname)

    prices=[]
    for asset in assets:
        j=np.where(company == asset)

        prices.append(price[j].squeeze())

    prices=np.array(prices)
    start_date = datetime.datetime(2020, 1, 1)
    dates = np.array([start_date + datetime.timedelta(days=int(d)) for d in day])

    return day, dates, prices.T

def load_n_ts(jref=1,nvar=5,dat_fname='dat/oil_day_closeval.npz'):
    day,price,company = read_npz(dat_fname=dat_fname)
    ts0=price[jref] # -1 es exxon
    if nvar < 0: nvar=len(company)
    ncompany = len(company)
    corr=np.zeros(ncompany-1)
    for i in range(ncompany-1):
        corr[i] = np.corrcoef(ts0,price[i])[0,1]

    idx = np.argsort(corr)
    corr_ordered= corr[idx]
    idx = np.append(idx,jref) # agrego la referencia
    
    company_ordered=company[idx]#company[:-1][idx]
    price_ordered = price[idx]
    print(day.shape)
    #price[idx]
#    for i in range(ncompany-1):
#        print(corr_ordered[i],company_ordered[i])

    print( company_ordered[::-1][:6] )
    print( price_ordered[::-1][:6] )
    #print(i,np.corrcoef(ts0,price[i])[0,1])
    start_date = datetime.datetime(2020, 1, 1)
    dates = np.array([start_date + datetime.timedelta(days=int(d)) for d in day])

    return day, dates,price_ordered[::-1][:nvar+1].T, company_ordered[::-1][:nvar+1]
    
def clean_data(day,price,company): #dat_fname='dat/oil_day_openval.npz'):
#    dat=np.load(dat_fname)
#    day,price,company = dat['day'],dat['open_val'],dat['company']

    #Eliminamos los tiempos en los que todos los precios son NaNs 
    #Fines de semana? Feriados? Etc. Dejamos los NaNs individuales de cada serie
    #que pueden corresponder a fusiones, creaciones de nuevas empresas, bancarrotas, etc.
    ncompany,nt = price.shape    
    print('Total de tiempos: ',nt)

    mask_nan = np.sum( np.isnan( price ),0) < price.shape[0] 
    #nt_correct = np.count_nonzero(~np.isnan(price[0,:]))
    #mask_nan=np.logical_not(np.isnan(price[0,:])) 
    dt = day[0,mask_nan]
    print('Dias habiles: ', np.sum( mask_nan ) )
    price = price[:,mask_nan]

    #prices,company1 =  [], [] #np.zeros(price.shape[0],nt_correct)

    #for i in range(ncompany):
    #    if nt_correct == np.count_nonzero(~np.isnan(price[i,:])):
    #        prices.append( price[i,mask_nan] )
    #        company1.append(company[i])
                            
    #price = np.array(prices)
    #company = np.array(company1)
    return dt,price,company

def csv2npz(init_date='2015-01-01',end_date='2023-12-31',
            var_type='close',
            folder='./dat/',
            industry_type='Oil, Gas & Consumable Fuels'):

    # reading csv file 
    df = pd.read_csv(folder+"stock_metadata.csv")
    df_dat = pd.read_csv(folder+"historical_prices.csv")
    df_company = df[df['industry'] == industry_type]
    df_dat['date'] = pd.to_datetime(df_dat['date'])

    print('Finished reading csv')
    init_date=pd.to_datetime(init_date)
    end_date = pd.to_datetime(end_date)
    date_range = pd.date_range(start=init_date, end=end_date, freq='D')

    julians, opens, company = [], [], []
    df_all = pd.DataFrame({'date': date_range})

    print('Collecting time series')
    for index, row in df_company.iterrows():
        df_ts = df_dat[df_dat['symbol'] == row['symbol']].copy()

        df_ts=df_ts[(df_ts['date'] >= init_date) & (df_ts['date'] <= end_date)]
        df_ts.loc[:, 'julian'] = (df_ts['date'] - init_date).dt.days

    #    df_ts['julian'] = (df_rangets['date']-init_date).dt.days

        df_rangets=pd.merge(df_all, df_ts[['date', 'julian', var_type]], on='date', how='left')

        julians.append(df_rangets['julian'].values)
        opens.append(df_rangets[var_type].values)
        company.append(row['symbol'])

    day = np.array(julians)
    price = np.array(opens)
    company = np.array(company)
    print('Company list', company )
    dt,price,company = clean_data(day,price,company)
    #plt.plot( price[0,:] )
    #plt.plot( price[2,:] )
    #plt.show()
    print(dt)
    print('Antes de guardar: ',dt.shape)
    print('Cantidad de empresas: ',len(company))

    np.savez_compressed(folder+ 'oil_' + datetime.datetime.strftime(init_date,'%Y-%m-%d') + '-' + datetime.datetime.strftime(end_date,'%Y-%m-%d') + '_' + var_type + '.npz' , day=dt , price=price , company=company)

if __name__=="__main__":
    csv2npz(init_date='2000-01-01',end_date='2023-12-31')
    quit()
    day,price,company = load_n_ts(jref=-1,nvar=5,)
    mean,var = stats.meanvar_ts(day,price)

    start_date = datetime.datetime(2020, 1, 1)
    dates = [start_date + datetime.timedelta(days=int(d)) for d in day]

    for j in range(2):
        figfile=f'tmp/ts_vars{j+1}.png'
        fig, ax = plt.subplots(1,3,figsize=(9,3))
        for i in range(3):
            print((price[i+j*3]-mean[i+j*3]).std())
            ax[i].plot(dates, price[i+j*3])
            ax[i].plot(dates, mean[i+j*3],'--')
            ax[i].fill_between(dates, mean[i+j*3]-1.96*var[i+j*3]**.5,
                               mean[i+j*3]+1.96*var[i+j*3]**0.5,alpha=0.25,
                               )
            #ax[i].xaxis.set_major_locator(mdates.DayLocator(interval=360))
            ax[i].xaxis.set_major_locator(mdates.YearLocator())
            ax[i].set(xlabel='Date',ylabel=company[i+j*3])
            ax[i].xaxis.set_tick_params(rotation=45) 
    #    ax[2].legend()
        plt.tight_layout()
        fig.savefig(figfile)
        plt.close()
