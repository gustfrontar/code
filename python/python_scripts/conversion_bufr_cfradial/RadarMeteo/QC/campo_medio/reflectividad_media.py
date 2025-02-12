# -*- coding: utf-8 -*-

import sys
from datetime import date, timedelta

from copio_calculo_matlab import calculo
from grafico_campo_medio import grafico

# Radar de entrada
radar='PAR'
#radar=str(sys.argv[1])
# Dia de entrada
dt=date.today() - timedelta(days=1)
fecha=dt.strftime('%Y%m%d')

datos=calculo(radar, fecha)

grafico(radar, fecha, datos)
