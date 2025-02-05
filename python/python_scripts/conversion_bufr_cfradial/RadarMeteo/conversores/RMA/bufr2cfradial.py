from __future__ import print_function

import shlex
import subprocess
import os
import sys
import glob

from .genero_nc_RMA import genero_nc_RMA


def bufr2cfradial(path_datos, excluir_TDR=True):
    """
    Este archivo convierte archivos BUFR provenientes de los RMA a
    CfRadial para poder tener un formato uniforme.

    Argumentos:
    -----------
        path_datos : str
            Donde estan los BUFR

        excluir_TDR : bool
            Como bbufr todavia no reconoce los archivos de ZDR, se
            pueden eliminar de la lista para convertir. Es True por
            defecto.

    """

    # Convierto de bufr a hdf5

    locate_RM = ['locate', 'RadarMeteo/conversores/RMA/bbufr']
    lista_en_bytes_RM = subprocess.check_output(locate_RM)
    lista_RM = lista_en_bytes_RM.decode()
    lista_path = lista_RM.split('\n')
    lista_path.sort(key=lambda x: len(x))
    for i in lista_path:
        if locate_RM[1]+'/' not in i and locate_RM[1] in i:
            path_bbufr_RM = i

    bbufr_split = path_bbufr_RM.split('/')
    path_RM = os.path.join('/', *bbufr_split[1:-3])

    locate_bbufr = ['locate', 'bbufr/tests/bufr2hdf5']
    lista_en_bytes_bbufr = subprocess.check_output(locate_bbufr)
    lista_bbufr = lista_en_bytes_bbufr.decode()
    lista_path_bbufr = lista_bbufr[:-1].split('\n')
    lista_path_bbufr.sort(key=lambda x: len(x))
    path_bbufr = lista_path_bbufr[0]

    lista_bufr = glob.glob(path_datos + '/*.BUFR')

    # Puedo sacar los TDR porque los convierte mal, por ahora
    if excluir_TDR:
        for i in lista_bufr:
            if '_TDR_' in lista_bufr:
                lista.remove(i)

    for i in lista_bufr:
        nombre = i.split('/')[-1]
        archivo = nombre.split('.')[0]

        comando = '{bufr2hdf5} -d {path_2}/tables '\
            '{input}.BUFR {output}.H5'.format(bufr2hdf5=path_bbufr,
                                              path_2=path_bbufr_RM,
                                              input=path_datos+'/'+archivo,
                                              output=path_datos+'/'+archivo)

        args = shlex.split(comando)
        subprocess.check_call(args)

    # Convierto de hdf5 a cfradial
    genero_nc_RMA(path_datos, save='nc')

    # Borro los H5
    subprocess.call('rm ' + path_datos + '/*.H5', shell=True)

if __name__ == "__main__":
    # No parece andar correctamente porque se usan import relativos
    # Tal vez con python3 se soluciona
    path_datos = str(sys.argv[1])

    bufr2cfradial(path_datos)
