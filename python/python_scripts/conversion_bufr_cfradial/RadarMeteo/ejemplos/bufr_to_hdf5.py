# -*- coding: utf-8 -*-
"""
Created on Wed Jun  1 11:13:01 2016

@author: Luciano Vidal
"""
#=============================================================================#
#                     BUFR to H5 conversion routine                           #
#=============================================================================#

import os, fnmatch
from time import clock

#=============================================================================#

start_time = clock()

#=============================================================================#

# Configuration section

# BUFR data format folder
#folder="RMA1_20151107T2122Z"
folder="RMA0_20150528T2232Z"

# bufr2hdf5 executable path
path_conv="/home/luciano/bbufr/tests/bufr2hdf5"

# Tables path
path_tables = "/home/luciano/bbufr/tables/"

# IN and OUT folders
input_path  = "/home/luciano/TRABAJO_SMN/RadarMeteo/datos/"+folder+"/"
output_path = "/home/luciano/TRABAJO_SMN/RadarMeteo/datos/"+folder+"/"

#=============================================================================#

# List of BUFR file names
fname=fnmatch.filter(os.listdir(input_path), '*.BUFR')

for ii in range(0, len(fname)):
    
    print fname[ii]
    input_file  = input_path + fname[ii]
    output_file = output_path + fname[ii][:-5] + ".H5"
    os.system(path_conv + " -d " + path_tables + " " + input_file + " " + output_file)

#=============================================================================#

end_time = clock()

print 'It took',end_time - start_time,'seconds'

#=============================================================================#