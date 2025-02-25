# -*- coding: utf-8 -*-
import sys, os, requests, subprocess, shlex
sys.path.append(os.environ['EXPDIR'])
import catalog_sources as src

import util as ut
import tarfile as tar
import urllib.request
from pandas import to_datetime, date_range

def download_amdar(REPODIR):

   print('Downloading AMDAR observations')

   EC = 0

   try:
      url = 'http://amdar.smn.gob.ar/amdar/asim'
      tarfile = 'amdar_asim.tar'
      pathout = f'{REPODIR}/{src.AIRCFT["NAME"]}/AMDAR'
      os.makedirs(pathout, exist_ok = True)

      urllib.request.urlretrieve(f'{url}/{tarfile}', f'{pathout}/{tarfile}')

      with tar.open(f'{pathout}/{tarfile}') as f:
         f.extractall(pathout)

      os.remove(f'{pathout}/{tarfile}')
   except Exception as e:
      print('Download AMDAR fail', file = sys.stderr)
      print(e, file = sys.stderr)
      EC += 1

   return EC

def download_AA(REPODIR, ana_date):

   print('Downloading AA observations')

   EC = 0

   ini, end = ut.get_awin_dates(ana_date)

   user = 'aerolineas2'
   pswd = '#ftp_aaa!14dnpt.' 
   ftp = 'ftp.smn.gob.ar'
   pathout = f'{REPODIR}/{src.AIRCFT["NAME"]}/AA'
   os.makedirs(pathout, exist_ok = True)

   down_dates = date_range(to_datetime(ini).floor('1h'), to_datetime(end).floor('1h'), freq = '1h')

   for date in down_dates:
      lftp_cmd = f'lftp -u {user},{pswd} {ftp} -e  "debug off; set xfer:clobber on; set net:max-retries 1; mget -O {pathout} entrada/*_{date:%Y%m%d%H}*txt ; exit"'
      CP = subprocess.run(shlex.split(lftp_cmd), capture_output = True, text = True, timeout = src.AIRCFT['WALLTIME'])
      if CP.returncode != 0:
         EC += CP.returncode
         print(CP.stderr, file = sys.stderr)

   return EC


def main(args):

   REPODIR = os.environ['REPODIR']
   ana_date = ut.parse_date(args)

   EC = 0

   EC += download_amdar(REPODIR)
   EC += download_AA(REPODIR, ana_date)   

   return EC

if __name__ == '__main__':

   ut.set_walltime(src.AIRCFT['WALLTIME'])
   try:
      EC = main(sys.argv[1:])
   except TimeoutError as e:
      print(e, file = sys.stderr)
      sys.exit(ut.EC_ERROR)

   if EC != 0:
      sys.exit(ut.EC_ERROR)

