# -*- coding: utf-8 -*-
import sys, os
sys.path += [os.environ['RUNDIR'], f'{os.environ["UTILSDIR"]}/py-lib']
import catalog_download as ctlg_download
import common_obs
import common

ctlg = common.merge_catalog(ctlg_download.airsrt, 'obs', 'airsrt')

def main(args):

   REPODIR = os.environ['REPODIR']
   ana_date = common_obs.parse_date(args)

   collection = 'AIRS2RET_NRT'
   outdir = f'{REPODIR}/{ctlg["name"]}'
   box = eval(os.environ['DOMAIN'])
   provider = 'GES_DISC'
   extension = '.hdf'
   timeout = ctlg['walltime']

   EC = common_obs.download_from_podaac(ana_date.date(), collection, outdir, box = box, provider = provider, extension = extension, timeout = timeout)

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

