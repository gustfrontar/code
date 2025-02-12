import glob
import os

lista=glob.glob('/home/martin/SMN/RMA/RMA1/*')
print lista

for i in lista:
    print i.split('.')[0]
    os.system('~/tmp/bbufr/tests/bufr2hdf5 -d ~/tmp/bbufr/tables {input} {output}.H5'.format(input=i, output=i.split('.')[0]))

