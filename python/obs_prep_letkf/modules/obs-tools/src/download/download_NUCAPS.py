import sys, os, subprocess, shlex
sys.path += [os.environ['RUNDIR'], f'{os.environ["UTILSDIR"]}/py-lib']
import catalog_download as ctlg_download
import common_obs
import common

from pandas import to_datetime, date_range

ctlg = common.merge_catalog(ctlg_download.nucaps, 'obs', 'nucaps')

def download_NUCAPS(REPODIR, ana_date, source):

   print(f'Downloading NUCAPS-{source} observations')

   EC = 0

   ini, end = common_obs.get_awin_dates(ana_date)
   user = 'SMN'
   pswd = '5Mn2024'
   ftp = '200.16.81.24'
   lftp_sets = 'set ftp:ssl-allow no; debug off; set net:max-retries 1'
   pathout = f'{REPODIR}/{ctlg["name"]}/{source}'
   DOWNPROC = int(os.environ['DOWNPROC'])
   os.makedirs(pathout, exist_ok = True)

   down_dates = date_range(to_datetime(ini).floor('1h'), to_datetime(end).floor('1h'), freq = '1h')
   for date in down_dates:
      lftp_cmd = f'lftp {ftp} -u {user},{pswd} -e  "{lftp_sets}; cd soletop/{source}; mirror -r -P{DOWNPROC} -I NUCAPS-EDR_v3r0_*_s{date:%Y%m%d%H}*.nc . {pathout}; exit"'
      CP = subprocess.run(shlex.split(lftp_cmd), capture_output = True, text = True, timeout = ctlg['walltime'])
      if CP.returncode != 0:
         EC += CP.returncode
         print(CP.stderr, file = sys.stderr)

   return EC

def main(args):

   REPODIR = os.environ['REPODIR']
   ana_date = common_obs.parse_date(args)

   EC = download_NUCAPS(REPODIR, ana_date, 'JPSS')
   EC = download_NUCAPS(REPODIR, ana_date, 'METOP')

   return EC


if __name__ == '__main__':

   common_obs.set_walltime(ctlg['walltime'])
   try:
      EC = main(sys.argv[1:])
   except TimeoutError as e:
      print(e, file = sys.stderr)
      sys.exit(common_obs.EC_ERROR)

   if EC != 0:
      sys.exit(common_obs.EC_ERROR)

