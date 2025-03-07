# -*- coding: utf-8 -*-
import sys, os
sys.path += [os.environ['RUNDIR'], f'{os.environ["UTILSDIR"]}/py-lib']
import catalog_download as ctlg_download
import common_obs
import common

ctlg = common.merge_catalog(ctlg_download.ascatw, 'obs', 'ascatw')

def main(args):

   sat2download = ['ASCATB', 'ASCATC']

   REPODIR = os.environ['REPODIR']
   ana_date = common_obs.parse_date(args)

   outdir = f'{REPODIR}/{ctlg["name"]}'
   box = eval(os.environ['DOMAIN'])
   timeout = ctlg['walltime']/len(sat2download)

   EC = 0
   for ASCAT in sat2download:
      collection = f'{ASCAT}-L2-25km'
      EC += common_obs.download_from_podaac(ana_date.date(), collection, outdir, box = box, timeout = timeout)

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

