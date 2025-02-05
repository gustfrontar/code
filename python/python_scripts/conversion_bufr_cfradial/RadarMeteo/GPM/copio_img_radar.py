'''
Este programa copia los gif remotos correspondientes a las pasadas de GPM en un radar determinado.

Tiene como entrada el archivo 'filtrado', el nombre del radar en Sol1 y el repositorio local.
'''

from datetime import datetime
import os

file_out='/home/martin/Descargas/Parana_Filtro.txt'

out=open(file_out,'r')

radar='par'

local='/home/martin/Documentos/SMN/GPM/'

for i in out:
    fecha=i.split('|')[2]
    anio=int(fecha[0:4])
    mes=int(fecha[5:7])
    dia=int(fecha[8:10])
    hora=int(fecha[11:13])
    minu=int(round(int(fecha[14:16]),-1))

    if minu==60:
        minu=0
        hora=hora+1
        if hora==24:
            hora=0
            dia=dia+1
            # falta agregar si se pasa de dia en el mes.......


    d=datetime(anio, mes, dia, hora, minu)
    d2=datetime(anio, mes, dia, hora, minu+1)
    aniomesdia=d.strftime('%Y%m%d')
    horaminu=d.strftime('%H%M')
    horaminu2=d2.strftime('%H%M')

    #local='/home/martin/Documentos/SMN/GPM/'

    if anio==2014:
        print anio, d

        remoto='mrugna@10.10.23.168:/yanina-sol-ms1/radar/{nombre}/{anio}/{fecha1}/gif/*.{fecha2}.{hora}.0.gif'.format(
            nombre=radar, anio=str(anio), fecha1=aniomesdia, fecha2=aniomesdia, hora=horaminu)

        os.system('scp {remoto} {local}'.format(remoto=remoto, local=local))

        remoto='mrugna@10.10.23.168:/yanina-sol-ms1/radar/{nombre}/{anio}/{fecha1}/gif/*.{fecha2}.{hora}.0.gif'.format(
            nombre=radar, anio=str(anio), fecha1=aniomesdia, fecha2=aniomesdia, hora=horaminu2)

        os.system('scp {remoto} {local}'.format(remoto=remoto, local=local))
    elif anio==2015:
        print anio, d

        remoto='mrugna@10.10.23.168:/yanina-sol-ms1/radar/{nombre}/{anio}/{fecha1}/gif/*.{fecha2}.{hora}.0.gif'.format(
            nombre=radar, anio=str(anio), fecha1=aniomesdia, fecha2=aniomesdia, hora=horaminu)

        os.system('scp {remoto} {local}'.format(remoto=remoto, local=local))

        remoto='mrugna@10.10.23.168:/yanina-sol-ms1/radar/{nombre}/{fecha1}/gif/*.{fecha2}.{hora}.0.gif'.format(
            nombre=radar, anio=str(anio), fecha1=aniomesdia, fecha2=aniomesdia, hora=horaminu)

        os.system('scp {remoto} {local}'.format(remoto=remoto, local=local))

        remoto='mrugna@10.10.23.168:/yanina-sol-ms1/radar/{nombre}/{anio}/{fecha1}/gif/*.{fecha2}.{hora}.0.gif'.format(
            nombre=radar, anio=str(anio), fecha1=aniomesdia, fecha2=aniomesdia, hora=horaminu2)

        os.system('scp {remoto} {local}'.format(remoto=remoto, local=local))

        remoto='mrugna@10.10.23.168:/yanina-sol-ms1/radar/{nombre}/{fecha1}/gif/*.{fecha2}.{hora}.0.gif'.format(
            nombre=radar, anio=str(anio), fecha1=aniomesdia, fecha2=aniomesdia, hora=horaminu2)

        os.system('scp {remoto} {local}'.format(remoto=remoto, local=local))
    elif anio==2016:
        print anio, d

        remoto='mrugna@10.10.23.168:/yanina-sol-ms1/radar/{nombre}/{fecha1}/gif/*.{fecha2}.{hora}.0.gif'.format(
            nombre=radar, anio=str(anio), fecha1=aniomesdia, fecha2=aniomesdia, hora=horaminu)

        os.system('scp {remoto} {local}'.format(remoto=remoto, local=local))

        remoto='mrugna@10.10.23.168:/yanina-sol-ms1/radar/{nombre}/{fecha1}/gif/*.{fecha2}.{hora}.0.gif'.format(
            nombre=radar, anio=str(anio), fecha1=aniomesdia, fecha2=aniomesdia, hora=horaminu2)

        os.system('scp {remoto} {local}'.format(remoto=remoto, local=local))

out.close()
