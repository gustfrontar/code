import os
import glob

for dire in glob.iglob('/home/sinarame/10.10.63.21/L2/RMA0/*/*/*/*/*/*.png'):
    if dire.split('_')[3]==('01' or '02'):
        os.system('cp -v {dire} /home/sinarame/imagenes_para_GPM/'.format(dire=dire))
