radar='Bariloche'

filename='/home/martin/RadarMeteo/GPM/{radar}_Predict.txt'.format(radar=radar)
file_out='/home/martin/RadarMeteo/GPM/{radar}_Filtro.txt'.format(radar=radar)

f=open(filename,'r')
out=open(file_out,'w')

for i in f:
    if int(i.split('|')[-1])<125:
        out.write(i)
    
f.close()
out.close()
