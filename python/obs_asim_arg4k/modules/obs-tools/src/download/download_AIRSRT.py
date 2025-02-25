# -*- coding: utf-8 -*-
import sys, os
sys.path.append(os.environ['EXPDIR'])
import catalog_sources as src

import util as ut

def main(args):

   REPODIR = os.environ['REPODIR']
   ana_date = ut.parse_date(args)

   collection = 'AIRS2RET_NRT'
   outdir = f'{REPODIR}/{src.AIRSRT["NAME"]}'
   box = eval(os.environ['DOMAIN'])
   provider = 'GES_DISC'
   extension = '.hdf'
   timeout = src.AIRSRT['WALLTIME']

   EC = ut.download_from_podaac(ana_date.date(), collection, outdir, box = box, provider = provider, extension = extension, timeout = timeout)

   return EC

if __name__ == '__main__':

   ut.set_walltime(src.AIRSRT['WALLTIME'])
   try:
      EC = main(sys.argv[1:])
   except TimeoutError as e:
      print(e, file = sys.stderr)
      sys.exit(ut.EC_ERROR)

   if EC != 0:
      sys.exit(ut.EC_ERROR)

