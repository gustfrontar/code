import glob
import shutil
import os
from datetime import datetime, timedelta
import subprocess
import sys
sys.path.append(os.environ['EXPDIR'])
import catalog_sources as src
import util as ut
import pandas as pd

def get_files(pathout, dates):

    user = 'awos'
    pswd = 'awos123'
    ip = '10.10.23.20'
    for d in dates:
        year = d.year
        month = d.month
        day = d.day
        hour = d.hour
        date_pathout = f'{pathout}/{year}/{month:02d}/{day:02d}/{hour:02d}/'
        os.makedirs(date_pathout, exist_ok = True)
        command = f"set net:max-retries 1 ; mirror -r --verbose /home/ftp/awos/{year}/{month:02d}/{day:02d}/{hour:02d}/ {date_pathout} ; exit"

        EC = subprocess.run(f'lftp sftp://{user}:{pswd}@{ip} -e "{command}"', shell = True, timeout = 2*60, capture_output = True, text = True)
        if EC.returncode != 0:
            print(EC.stderr, file = sys.stderr)

    return 

def organize_data(pathout):

    date_notmove = (datetime.now() - timedelta(days = 1)).timestamp()

    for full_path_filename in sorted(glob.glob(f'{pathout}/C*')):

        if os.path.getmtime(full_path_filename) > date_notmove:
            continue

        remove = False
        with open(full_path_filename) as f:
            try:
                last_line = f.readlines()[-3]
                last_time = last_line.split(';')[1]
                time = datetime.strptime(last_time, '"%Y-%m-%dT%H:%M:%S.000%Z:00"') + timedelta(hours = 3)

                year = time.year
                month = time.month
                day = time.day
                hour = time.hour
            except:
                remove = True

        if remove:
            os.remove(full_path_filename)
            continue

        filename = full_path_filename.split('/')[-1]

        folder = f'{pathout}/{year}/{month:02d}/{day:02d}/{hour:02d}/'
        os.makedirs(folder, exist_ok = True)

        shutil.move(full_path_filename, f'{folder}/{filename}')


def main(args):

    REPODIR = os.environ['REPODIR']

    ana_date = ut.parse_date(args)
    ini, end = ut.get_awin_dates(ana_date)

    dates = pd.date_range(datetime(ini.year, ini.month, ini.day, ini.hour), 
                          datetime(end.year, end.month, end.day, end.hour), freq = 'h')

   # Output directory
    pathout = '{}/{}'.format(REPODIR, src.AWOS['NAME'])
    os.makedirs(pathout, exist_ok=True)

    #Download AWOS data from rayo
    get_files(pathout, dates)

    #Organize data in folders by date
    organize_data(pathout)

if __name__ == '__main__':

   ut.set_walltime(src.AWOS['WALLTIME'])
   try:
      main(sys.argv[1:])
   except TimeoutError as e:
      print(e, file = sys.stderr)
      sys.exit(ut.EC_ERROR)
    
