from datetime import datetime
import os

file_out='/home/sinarame/Bariloche_Filtro.txt'

out=open(file_out,'r')

radar='RMA0'

for i in out:
    fecha=i.split('|')[2]
    anio=fecha[0:4]
    mes=fecha[5:7]
    dia=fecha[8:10]
    hora=fecha[11:13]

    comando='wget -r -l2 --no-parent http://10.10.63.21/L2/{radar}/{anio}/{mes}/{dia}/{hora}/'.format(
        radar=radar, anio=anio, mes=mes, dia=dia, hora=hora)

    os.system(comando)
    #print anio, mes, dia, hora

out.close()

