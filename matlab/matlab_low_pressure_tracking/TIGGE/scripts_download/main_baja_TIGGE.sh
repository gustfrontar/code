#/bin/bash
#Este script baja datos del TIGGE en forma mensual.

#NOMBRES DE LOS CENTROS EN LA BASE DE DATOS TIGGE
#ammc Australia
#babj China
#cwao Canada
#ecmf ECMWF
#egrr UKMET
#kwbc NCEP
#lfpw Meteofrance
#rjtd JMA
#rksl KMA
#sbsj CPTEC
export http_proxy="proxy.fcen.uba.ar:8080"
export ftp_proxy="proxy.fcen.uba.ar:8080"

INIMONTH=January
INIYEAR=2011

ENDMONTH=December
ENDYEAR=2011

DATAPATH=/home/jruiz/TIGGE/
FILEPREFIX="HGT_ENS"           #EL nombre del archivo grib comienza con esto, sigue con el nombre del centro de origen y luego con la fecha.
VARIABLE="geopotential" #geopotential heigth, temperature, u , v, etc
LEVELS="500"               #Separar los niveles por una barra.
TIEMPOINICIAL="0"
TIEMPOFINAL="168"
INCREMENTO="6"
INICIALIZACION="12"            #Separar por barras si se desea mas de una.
ORIGEN="cwao"                  #Centro que genero los pronosticos. (Ver http://www.ecmwf.int/research/EU_projects/ENSEMBLES/data/atmosphere_archiving_GRIBheaders.html)
TIPODENIVEL="pl"               #pl niveles de presion, sl niveles de superficie / single levels
RESOLUCION=2.0                 #Resolucion en grados.
FORECASTTYPE="pf"              #PF perturbed forecast (ensemble), CF control forecast.
MEMBERS="all"                  #Chose all or list desired members separated by "/"
USERID="fbcd8ae1ee6ae59293d50cd363cce857"
USERMAIL="jruiz@cima.fcen.uba.ar"
DATASERVER="http://tigge-portal.ecmwf.int/d/dataserver/"

#Create a temporal folder

CURRENTFOLDER=`pwd`
TMPFOLDER=$CURRENTFOLDER/TMP_$ORIGEN$FORECASTTYPE$INIYEAR$INIMONTH$VARIABLE$INICIALIZACION
mkdir $TMPFOLDER

cp ./ecmwf.py $TMPFOLDER
cd $TMPFOLDER


ENDDATENUMBER=`date --date "1 hours 00:00 01 $ENDMONTH $ENDYEAR" +%Y%m`
CURRENTMONTH=$INIMONTH
CURRENTYEAR=$INIYEAR
CURRENTDATENUMBER=`date --date "1 hours 00:00 01 $CURRENTMONTH $CURRENTYEAR" +%Y%m`

#Agregamos el path actual donde esta guardado el script ecmwf.py al path de python de forma tal que haga el envio del pedido y la bajada de los datos.
CURRENTPATH=`pwd`
export PYTHONPATH=$PYTHONPATH:$CURRENTPATH
echo "La variable PYTHONPATH es : " $PYTHONPATH


while [ $CURRENTDATENUMBER -le $ENDDATENUMBER ]
do
#Generamos el script para python
TMPCDATE=`date --date "1 hours 00:00 01 $CURRENTMONTH $CURRENTYEAR" +%Y%m%d`
FILENAMEDATE=`date --date "1 hours 00:00 01 $CURRENTMONTH $CURRENTYEAR" +%Y%m`
case ${CURRENTMONTH} in
 January) DAYEND=31;;
 February) DAYEND=29;;
 March) DAYEND=31;;
 April) DAYEND=30;;
 May) DAYEND=31;;
 June) DAYEND=30;;
 July) DAYEND=31;;
 August) DAYEND=31;;
 September) DAYEND=30;;
 October) DAYEND=31;;
 November) DAYEND=30;;
 December) DAYEND=31;;
 *) echo "Bad Month --> ${CURRENTMONTH} <--  EXITING!!..."; exit 8;;
esac

TMPCDATE2=`date --date "1 hours 00:00 01 $CURRENTMONTH $CURRENTYEAR" +%Y%m`
TMPCDATE2=${TMPCDATE2}${DAYEND}

echo "#!/usr/bin/python"                                                    > ./baja.py
echo "from ecmwf import ECMWFDataServer"                                   >> ./baja.py
echo "server = ECMWFDataServer("                                           >> ./baja.py
echo "       '$DATASERVER',"                                               >> ./baja.py
echo "       '$USERID',"                                                   >> ./baja.py
echo "       '$USERMAIL'"                                                  >> ./baja.py
echo "    )"                                                               >> ./baja.py
echo "server.retrieve({"                                                   >> ./baja.py
echo "    'dataset' : \"tigge\","                                          >> ./baja.py
echo "    'step'    : \"$TIEMPOINICIAL/to/$TIEMPOFINAL/by/$INCREMENTO\","  >> ./baja.py
echo "    'number'  : \"$MEMBERS\","                                       >> ./baja.py
echo "    'levtype' : \"$TIPODENIVEL\","                                   >> ./baja.py
echo "    'levelist' : \"$LEVELS\","                                       >> ./baja.py
echo "    'date'    : \"$TMPCDATE/to/$TMPCDATE2\","                        >> ./baja.py
echo "    'time'    : \"$INICIALIZACION\","                                >> ./baja.py
echo "    'origin'  : \"$ORIGEN\","                                        >> ./baja.py
echo "    'type'    : \"${FORECASTTYPE}\","                                >> ./baja.py  #USE PF for perturbations and CF for control
echo "    'param'   : \"$VARIABLE\","                                      >> ./baja.py
echo "    'grid'    : \"$RESOLUCION/$RESOLUCION\","                        >> ./baja.py
echo "    'target'  : \"./${FILEPREFIX}_${ORIGEN}_${FILENAMEDATE}.grib\" " >> ./baja.py
echo "    })"                                                              >> ./baja.py

TMPMONTH=`date --date "1 months 00:00 01 $CURRENTMONTH $CURRENTYEAR" +%B`
TMPYEAR=`date --date "1 months 00:00 01 $CURRENTMONTH $CURRENTYEAR" +%y`
CURRENTMONTH=$TMPMONTH
CURRENTYEAR=$TMPYEAR
CURRENTDATENUMBER=`date --date "1 hours 00:00 01 $CURRENTMONTH $CURRENTYEAR" +%Y%m`
cd $TMPFOLDER
chmod +x ./baja.py
#Solicita y baja los datos del portal de datos TIGGE-ECMWF usando la interface MARS
./baja.py >> log.bajaTIGGE

#Muevo el archivo que baje a su destino final.
mkdir -p $DATAPATH/$ORIGEN/
mv ./${FILEPREFIX}_${ORIGEN}_${FILENAMEDATE}.grib $DATAPATH/$ORIGEN/


done
