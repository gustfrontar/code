import re, sys, os
import pandas as pd
sys.path += [os.environ['RUNDIR'], f'{os.environ["UTILSDIR"]}/py-lib']
import catalog_download as ctlg_download
import common_obs
import common

ctlg = common.merge_catalog(ctlg_download.awos, 'obs', 'awos')

def get_location(df):

    if 'MID' in df['Long_var']:
        df['Location'] = 'MID'
    elif 'RWY' in df['Long_var']:
        df['Location'] = 'RWY'
    else:
        df['Location'] = 'No_Loc'

    return df

def parse_data(path):

    filename = f'{path}/asm-awos.lst'

    var_units = ['WIND_SPEED;METRES_PER_SECOND', 'AIR_TEMPERATURE;DEGREES_CELSIUS', 'AIR_PRESSURE_QFE;HECTO_PASCALS',
                 'RELATIVE_HUMIDITY;PERCENT', 'WIND_DIRECTION;DEGREES']
    columns = ['ID', 'DateTime', 'Long_var', 'Variable', 'Unit', 'Value', 'Dummy', 'Group_type', 'Group_period']

    list_data = []
    with open(filename, 'r') as f:
        for line in f.readlines():
            for v_u in var_units:
                matches = re.findall(f'([^\s]+){v_u}([^\s]+)', line)
                for match in matches:
                    mensaje = v_u.join(match)
                    mensaje = mensaje.replace('|', '')
                    list_data.append(mensaje.split('"')[0].split(';'))


    df = pd.DataFrame(list_data, columns = columns)
    if df.empty:
        sys.exit(common_obs.EC_WARNING)

    df['DateTime'] = pd.to_datetime(df['DateTime'])
    df['DateTime'] = df['DateTime'].dt.tz_convert('UTC').dt.tz_localize(None)

    df = df.apply(get_location, axis = 1) #Obtengo del Long_var si el dato es de cabecera de la pista o del medio
    df = df.drop(['Dummy', 'Long_var'], axis = 1)
    df['Group_period'] = df['Group_period'].replace(r'^\s*$', 'No_Period', regex=True)

    return df


def write_files(df, REPODIR):

    ini_date = df['DateTime'].min().round('5min')
    end_date = df['DateTime'].max().round('5min')

    dates = pd.date_range(ini_date, end_date, freq = '5min')
    intervalos = pd.IntervalIndex.from_breaks(dates)

    for inter in intervalos:
        inter_ini = inter.left
        inter_end = inter.right

        inter_mask = (df['DateTime'] >= inter_ini) * (df['DateTime'] < inter_end)
        df_inter = df[inter_mask]
        print(f'{REPODIR}/{ctlg["name"]}/awos_s{inter_ini:%Y%m%d%H%M%S}_e{inter_end:%Y%m%d%H%M%S}.csv')   
        df_inter.to_csv(f'{REPODIR}/{ctlg["name"]}/awos_s{inter_ini:%Y%m%d%H%M%S}_e{inter_end:%Y%m%d%H%M%S}.csv', index = False, header = True)


def main():

    REPODIR = os.environ['REPODIR']
    path = f'{os.environ["OBSDIRIN"]}/desarrollo'

   # Output directory
    pathout = '{}/{}'.format(REPODIR, ctlg['name'])
    os.makedirs(pathout, exist_ok=True)

    df = parse_data(path)

    write_files(df, REPODIR)


if __name__ == '__main__':

   common_obs.set_walltime(ctlg['walltime'])
   try:
      main()
   except TimeoutError as e:
      print(e, file = sys.stderr)
      sys.exit(common_obs.EC_ERROR)

