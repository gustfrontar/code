import os
from configparser import ConfigParser, ExtendedInterpolation

config = ConfigParser(interpolation=ExtendedInterpolation())
config.optionxform = str
config.read("config.ini")

maxTHR = config["System"]["THREADS"]
os.environ["MKL_NUM_THREADS"] = maxTHR
os.environ["NUMEXPR_NUM_THREADS"] = maxTHR
os.environ["OMP_NUM_THREADS"] = maxTHR
