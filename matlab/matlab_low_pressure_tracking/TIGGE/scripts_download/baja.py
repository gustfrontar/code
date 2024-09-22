#!/usr/bin/python
from ecmwf import ECMWFDataServer
server = ECMWFDataServer(
       'http://tigge-portal.ecmwf.int/d/dataserver/',
       'fbcd8ae1ee6ae59293d50cd363cce857',
       'jruiz@cima.fcen.uba.ar'
    )
server.retrieve({
    'dataset' : "tigge",
    'step'    : "0/to/168/by/6",
    'levtype' : "pl",
    'levelist' : "500",
    'date'    : "20080701/to/20080731",
    'time'    : "12",
    'origin'  : "cwao",
    'type'    : "cf",
    'param'   : "geopotential",
    'grid'    : "2.0/2.0",
    'target'  : "./HGT_ENS_cwao_200807.grib" 
    })
