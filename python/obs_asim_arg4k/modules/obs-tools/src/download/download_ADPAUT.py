import sys, os
sys.path.append(os.environ['EXPDIR'])
import catalog_sources as src
import util as ut

import urllib.request
import pandas as pd
from datetime import datetime, timedelta

def download_file(filein, fileout, filetype):
   try:
      urllib.request.urlretrieve(filein, fileout)
   except Exception as e:
      print('Download fail: {} {}'.format(src.ADPAUT['NAME'], filetype), file = sys.stderr)
      print(e, file = sys.stderr)
      return

def main(args):

   # Parse input parameters into date
   ana_date = ut.parse_date(args)

   # Analysis window (considering slots)
   ini, end = ut.get_awin_dates(ana_date)
   #itime = (ini - timedelta(minutes=st.OBSFREC/2.)).strftime('%Y-%m-%dT%H:%M:%S')
   #etime = (end + timedelta(minutes=st.OBSFREC/2.)).strftime('%Y-%m-%dT%H:%M:%S')
   itime = ini.strftime('%Y-%m-%dT%H:%M:%S')
   etime = end.strftime('%Y-%m-%dT%H:%M:%S')
   ini = ini.strftime('%Y%m%d%H%M%S')
   end = end.strftime('%Y%m%d%H%M%S')

   # Output directory
   pathout = '{}/{}'.format(os.environ['REPODIR'], src.ADPAUT['NAME'])
   os.makedirs(pathout, exist_ok=True)

   # Download stations 
#   filein = 'http://192.168.5.213:8080/aws-api/stations/'
#   fileout = '{}/stations.json'.format(pathout)
#   download_file(filein, fileout, 'stations')

   # Get station owners
   try:
      owners = pd.read_json('http://192.168.5.213:8080/aws-api/propietaries')
      fileout = '{}/owners.json'.format(pathout)
      owners.to_json(fileout)
   except:
      print('Download fail: ADPAUT owners', file = sys.stderr)
      sys.exit()

   # Download data for each owner
   for id_ in owners['id'].values:
      filein = 'http://192.168.5.213:8080/aws-api/observations/{}/{}/{}'.format(id_, itime, etime)
      fileout = '{}/prop{}_s{}_e{}.json'.format(pathout, str(id_).zfill(2), ini, end)
      download_file(filein, fileout, 'data')


if __name__ == '__main__':

   ut.set_walltime(src.ADPAUT['WALLTIME'])
   try:
      main(sys.argv[1:])
   except TimeoutError as e:
      print(e, file = sys.stderr)
      sys.exit(ut.EC_ERROR)
     
