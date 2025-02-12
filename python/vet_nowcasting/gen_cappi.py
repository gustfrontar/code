import numpy as np 
import os
import sys
import glob
import pyart
from datetime import datetime
from cappi import cappi , parseTime
from configparser import ConfigParser, ExtendedInterpolation
import gc


BASEDIRDATA = '../casos_estudio/'
OUTCAPPI = 'cappis'

config = ConfigParser(interpolation=ExtendedInterpolation())
config.read("cappi.ini")

casos = ['20190210']
radares = ['RMA1']


for caso in casos:
    for radar in radares :
        FILEDIR = BASEDIRDATA + '/' + radar + '/' +caso+ '/QC/'
        OUTPUTCAPPIS = FILEDIR + '/' + OUTCAPPI + '/'

        print(FILEDIR)

        FileList = np.sort(glob.glob(FILEDIR+'/*.nc'))
        endlist = len(FileList)
        print(FileList)
        for i in range(endlist):
            try:
                fileread = FileList[i]
                print(fileread)
                radar = pyart.io.read( fileread )
                MyDate = parseTime( radar )
                print( MyDate )
                fecha = os.path.basename( fileread )[6:21]
                cappi_2km = cappi(radar,config, OUTPUTCAPPIS + "/cappi2km." + fecha + ".pckl")
            except KeyError:
                print('KeyError')
                continue
        radar=[]
        gc.collect()
