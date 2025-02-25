# -*- coding: utf-8 -*-
import sys, os
sys.path.append(os.environ['EXPDIR'])
import catalog_sources as src

import util as ut

def main(args):

   sat2download = ['ASCATB', 'ASCATC']

   REPODIR = os.environ['REPODIR']
   ana_date = ut.parse_date(args)

   outdir = f'{REPODIR}/{src.ASCATW["NAME"]}'
   box = eval(os.environ['DOMAIN'])
   timeout = src.ASCATW['WALLTIME']/len(sat2download)

   EC = 0
   for ASCAT in sat2download:
      collection = f'{ASCAT}-L2-25km'
      EC += ut.download_from_podaac(ana_date.date(), collection, outdir, box = box, timeout = timeout)

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

