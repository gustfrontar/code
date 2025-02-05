clear all
close all
%*************************************************************************
% La idea de este script es cargar los datos de un ensemble (el super
% ensemble por ejemplo) y realizar las siguientes acciones.
% - Determinar si el n�mero de miembros es mayor a un n�mero minimo. Esto
% es de particular importancia sobre todo en el superensemble donde el
% numero de miembros puede ser variable de acuerdo con la disponibilidad de
% los mismos.
% - En base a los modelos presentes buscamos los 15 dias previos en los
% cuales esos modelos esten disponibles.
% - Si consigo una cierta cantidad de datos (1000 por ejemplo) construyo
% los rank histograms con esa cantidad de datos.
% - Uso el rank histogram y los datos para calibrar el pron�stico de
% precipitaci�n para ese dia. Y obtengo un set de datos de pronosticos
% probabilisticos calibrados.
% 
% Vamos a trabajar en base
% a estaciones con una matriz inicial que tiene el siguiente formato:
% tiene tantas columas como miembros + 1 (la verificaci�n). La primera
% columna es la verificacion que puede ser CMORPH o GTS para las estaciones
% en las cuales se calcula precipitaci�n en el super ensemble.
% Las demas columnas son los diferentes modelos regionales y globales que
% componen el super-ensemble o los diferentes miembros del ensemble
% regional.
% Existe una tercera variable ademas de p24 y p48 que es est_data. Esta
% variable contiene en la primera columna la fecha que se usar� para buscar
% los 15 d�as previos disponibles.
% Funciones que usa:
% prob_cal_fun   : calcula la probabilidad calibrada
% prob_cal2_fun  : calcula la probabilidad no calibrada
% prob_cal3_fun  : calcula la probabilidad en base a un pronostico
% deterministico.
% pre_cal_fun    : Calcula las varianzas y los rank histograms.
% pre_cal_fun2   : Calcula la relacion prob vs pp para un pronostico
% deterministico.
%*************************************************************************
% Juan Ruiz - 2007 (Este ya perdi la cuenta que numero es...)
%*************************************************************************
%PARAMETROS MODIFICABLES

%Region y fuente de datos:
region='sur'
source='gts'

%Directorio donde estan los archivos.
path='d:/trabajos/TrabajoSLAF/precipitacion/slaf2006/matlab/'



%Numero de miembros del ensamble

ens=10;      %Numero total de miembros del ensemble.



%Numero de puntos (observaciones previas) que vamos a usar para la
%calibracion.

cal_size=1000; %1000 equivale para cmo a 15 dias previos aprox.


%Umbrales para los cuales vamos a calcular las probabilidades.

umbral=[0.01 0.10 0.25 0.5 1 1.5 2]*25.4;

%Inicializo algunas variables
fechas=[]; %Se va a ir llenando con las fechas
flag1=[]; %Va a ir indicando si ese dia se calibro o no.

verif_24=[];
verif_48=[];
prob_nocal_24=[];
prob_nocal_48=[];
prob_cal_24=[];
prob_cal_48=[];
prob_cal2_24=[];
prob_cal2_48=[];
prob_cal3_24=[];
prob_cal3_48=[];
prob_cal4_24=[];
prob_cal4_48=[];
est_data_24=[];
est_data_48=[];


%INICIAMOS EL CALCULO
%**************************************************************************

%Abro el archivo.
load(strcat(path,'slaf2006_',region,'_',source,'.mat'));


%En este punto aparecen 3 variables p24 (prono de pp y su verificacion a 24
%horas). p48 (idem a 48 horas) y est_data (dia yyyymmdd y id de estacion
%junto con su lat y lon para cada punto de p24 y p48).

%Archivo de salida
salida=strcat(path,'slaf2006_prob3_',region,'_',source,'.mat');

%Este archivo de salida en formato texto contiene los rank histograms
%utilizados, el numero de miembros para cada d�a y otros datos que nos
%permieten controlar como esta funcionando el script.
salidatxt=fopen(strcat(path,'slaf2006_prob_',region,'_',source,'.txt'),'wt');

%Archivo con los rank histograms y sigmas de corte para la calibracion
%estatica.

load(strcat('d:/trabajos/TrabajoSLAF/precipitacion/slaf2005/matlab/calibracion_estatica_2005_',region,'_',source,'.mat'));
%Uso este archivo solo para cargar flag24 y flag48 que me van a decir
%cuando anduvo la calibracion del SMES.
load(strcat('d:/trabajos/TrabajoSLAF/precipitacion/superensemble/matlab/','superensemble_prob_',region,'_',source,'.mat'),'flag24','flag48');
flag24_smes=flag24;clear flag24;
flag48_smes=flag48;clear flag48;
flag24=[];flag48=[];


%**************************************************************************
%COMIENZA EL CICLO EN TIEMPO.
%**************************************************************************
% Ini_date y end_date marcan las fechas de inicio y final de los
% pronosticos, pero el pronóstico calibrado va a tener que arrancar n_days
% despues porque necesita informacion sobre la verificacion de los
% pronosticos previos.

%Fecha de inicio.
%El formato es dd-mmm-yyyy (el mes son las 3 primeras letras en ingles)

ini_date='01-Oct-2006';
end_date='31-Dec-2006';

%Genero un numero que identifica la fecha de arranque del pronostico
%calibrado. En principio el script se da cuenta solo de cuantos dias tiene
%que dejar pasar para poder juntar el numero necesario de observaciones
%previas para realizar la calibracion.

date_num=datenum(ini_date);

%Hago lo mismo para la fecha de fin y comienzo de los pronosticos

ini_date_num=datenum(ini_date);
end_date_num=datenum(end_date);

while (date_num <= end_date_num)
    
    
%**************************************************************************
%   PRIMER PASO, BUSCO TODOS LOS PRONOSTICOS PARA EL DIA ACTUAL Y VERIFICO
%   QUE LA CANTIDAD DE MIEMBROS SEA LA ADECUADA
%**************************************************************************
paso1_24=0;
paso1_48=0;
%Calculo la fecha de hoy en numero

date_vec=datevec(date_num);
hoy_num=(date_vec(1))*1e4+date_vec(2)*1e2+date_vec(3); %yyyymmdd

fprintf(salidatxt,'La fecha es: %6.0f\n',hoy_num);

%Busco todos los pronosticos para hoy.

i_hoy=find(est_data(:,1)==hoy_num);
    
aux_24=p24(i_hoy,:);
aux_48=p48(i_hoy,:);
aux_data=est_data(i_hoy,:);

%Vemos cuantos miembros tenemos

%Calculo las dimensiones de cada matriz.
[filas24 columnas24]=size(aux_24);
[filas48 columnas48]=size(aux_48);

   i_nonan24=find(any(isnan(aux_24(:,:)),2)==0);
   ensemble_hoy_24=aux_24(i_nonan24,:);
   est_datahoy_24=aux_data(i_nonan24,:);  
    
   i_nonan48=find(any(isnan(aux_48(:,:)),2)==0);
   ensemble_hoy_48=aux_48(i_nonan48,:);
   est_datahoy_48=aux_data(i_nonan48,:); 

if(length(i_nonan24) > 0)
   paso1_24=1;
end
if(length(i_nonan48) > 0)
   paso1_48=1;
end


%Borro algunas variables que no voy a seguir usando.
clear   date_vec i_hoy aux_24 aux_48 i_nonan24 i_nonan48

%**************************************************************************
%   FIN DEL CONTROL INICIAL. (PRIMER PASO)   
%**************************************************************************

%**************************************************************************
%     PASO 2: EN BASE AL ARCHIVO BUSCO 15 DIAS DONDE TENGA PRONOSTICOS DE
%     LOS MODELOS PRESELECCIONADOS PARA PODER CALIBRAR EL PRONO.
%**************************************************************************

paso2_24=0;
paso2_48=0;

%Primero busco todos los dias previos al dia de hoy.

i_archivo=find(est_data(:,1) < hoy_num);

if(paso1_24 ==1)
aux_24=p24(i_archivo,:);
%Busco aquellos dias/puntos en los que alguno de los pron�sticos es NaN.
i_nonan24=find(any(isnan(aux_24),2)==0);
aux_fecha=est_data(i_archivo,1);
%Me quedo solo con aquellos dias/puntos donde todos los pronosticos estan.
aux2_24=aux_24(i_nonan24,:);
fecha_24=aux_fecha(i_nonan24);
%Tengo que ver cuantas observaciones me quedaron para poder hacer la
%calibracion

[filas_arch_24 columnas_arch_24]=size(aux2_24);
%Si tengo mas de 1000 observaciones previas entonces asunto arreglado

if(filas_arch_24 > cal_size)
    ensemble_arch_24=aux2_24(filas_arch_24-cal_size:filas_arch_24,:);
    paso2_24=1; %Indica que el paso 2 termino con exito para las 24 horas.
        
else
    paso2_24=0; %no se calibra el ensemble.
end
end

if(paso1_48==1)
 
aux_48=p48(i_archivo,:);
i_nonan48=find(any(isnan(aux_48),2)==0);
aux_fecha=est_data(i_archivo,1);
aux2_48=aux_48(i_nonan48,:);
fecha_48=aux_fecha(i_nonan48);
[filas_arch_48 columnas_arch_48]=size(aux2_48);
if(filas_arch_48 > cal_size)
    ensemble_arch_48=aux2_48(filas_arch_48-cal_size:filas_arch_48,:);
    paso2_48=1; %Indica que el paso 2 termino con exito para las 48 horas.
    
else
        paso2_48=0;
end

end

clear aux_24 aux_48 i_nonan24 i_nonan48 aux_fecha aux2_24 aux2_48


%**************************************************************************
%   FIN DE LA BUSQUEDA DE CAL_SIZE DATOS PREVIOS. (SEGUNDO PASO)   
%**************************************************************************


%**************************************************************************
%       TERCER PASO: ACA VIENE LA CALIBRACION PROPIAMENTE DICHA USANDO LAS
%       FUNCIONES PARA EL CALCULO DE LOS RANK HISTOGRAMS 
%**************************************************************************

if(paso2_24==1 & flag24_smes(date_num-ini_date_num+1)) %Solo ingresamos en este 
%paso si el paso previo resulto favorable. Ademas le pide que ese dia la
%calibracion del SMES este disponible.

%Primer paso para la calibracion: Calculo rank histograms de acuerdo a la
%dispersion del ensemble.
[filas24 columnas24]=size(ensemble_arch_24);
[rank_hist_24 low_std_24 hi_std_24] = pre_cal_fun(ensemble_arch_24(:,1),ensemble_arch_24(:,2:columnas24),0);

mean_arch24=nanmean(ensemble_arch_24(:,2:columnas24),2);
clear pcal_24 pcal2_24
[pcal_24 ncal_24 rango_lluvia]=pre_cal_fun2(ensemble_arch_24(:,1),mean_arch24,umbral,1);
[pcal2_24 ncal2_24 rango_lluvia]=pre_cal_fun2(ensemble_arch_24(:,1),ensemble_arch_24(:,2),umbral,1);

fprintf(salidatxt,'Los modelos aprobados en el segundo paso para el pron�stico a 24 horas son\n');
fprintf(salidatxt,'%2.0f\n',columnas24);
fprintf(salidatxt,'Cantidad de pronos: %2.0f\n',length(ensemble_hoy_24(:,1)));
fprintf(salidatxt,'LOW STD, HI STD %3.2f %3.2f\n',low_std_24,hi_std_24);
fprintf(salidatxt,'Rank histograms\n');
fprintf(salidatxt,' %5.3f',rank_hist_24(1,:));
fprintf(salidatxt,' \n');
fprintf(salidatxt,' %5.3f',rank_hist_24(2,:));
fprintf(salidatxt,' \n');
fprintf(salidatxt,' %5.3f',rank_hist_24(3,:));
fprintf(salidatxt,' \n');
fprintf(salidatxt,' %5.3f',rank_hist_24(4,:));
fprintf(salidatxt,' \n');


mean_hoy_24=nanmean(ensemble_hoy_24(:,2:columnas24),2);

prob_nocal_aux=prob_cal_fun2(ensemble_hoy_24(:,2:columnas24),umbral);
prob_cal_aux=prob_cal_fun(ensemble_arch_24(:,1),ensemble_hoy_24(:,2:columnas24),rank_hist_24,[low_std_24 hi_std_24],umbral,1,2);
prob_cal_aux2=prob_cal_fun(ensemble_arch_24(:,1),ensemble_hoy_24(:,2:columnas24),rank_hist_24_estatico,[low_std_24_estatico hi_std_24_estatico],umbral,1,2);
prob_cal_aux3=prob_cal_fun3(mean_hoy_24,pcal_24,umbral,rango_lluvia);          %Calculo probabilidad en base a la media del ensemble.
prob_cal_aux4=prob_cal_fun3(ensemble_hoy_24(:,2),pcal2_24,umbral,rango_lluvia); %Calculo probabilidad en base al control.

%Aca deber�a agergar una calibracion est�tica.

%Voy guardando todas las probabilidades en una sola matriz
prob_nocal_24=[prob_nocal_24;prob_nocal_aux];
prob_cal_24=[prob_cal_24; prob_cal_aux];
prob_cal2_24=[prob_cal2_24; prob_cal_aux2];
prob_cal3_24=[prob_cal3_24; prob_cal_aux3];
prob_cal4_24=[prob_cal4_24; prob_cal_aux4];
est_data_24=[est_data_24 ;est_datahoy_24];

%Construyo un vector con la fecha y las observaciones.
verif_aux(:,1)=hoy_num*ones(length(ensemble_hoy_24(:,1)),1);        %Guardo la fehca en la primera columan.
verif_aux(:,2)=ensemble_hoy_24(:,1);   %Guardo los datos en la segunda.
verif_24=[verif_24;verif_aux];

clear verif_aux clear prob_nocal_aux prob_cal_aux rank_hist_24 prob_cal_aux2 prob_cal_aux3 prob_cal_aux4

end

if(paso2_48==1 & flag48_smes(date_num-ini_date_num+1)) %Solo ingresamos en este paso si el paso previo result� exitoso.

%Primer paso para la calibracion: Calculo rank histograms de acuerdo a la
%dispersion del ensemble.
[filas48 columnas48]=size(ensemble_arch_48);
[rank_hist_48 low_std_48 hi_std_48] = pre_cal_fun(ensemble_arch_48(:,1),ensemble_arch_48(:,2:columnas48),1);

mean_arch48=nanmean(ensemble_arch_48(:,2:columnas48),2);
clear pcal_48 pcal2_48
[pcal_48 ncal_48 rango_lluvia]=pre_cal_fun2(ensemble_arch_48(:,1),mean_arch48,umbral,1);
[pcal2_48 ncal2_48 rango_lluvia]=pre_cal_fun2(ensemble_arch_48(:,1),ensemble_arch_48(:,2),umbral,1);

fprintf(salidatxt,'Los modelos aprobados en el segundo paso para el pron�stico a 48 horas son\n');
fprintf(salidatxt,'%2.0f\n',columnas48);
fprintf(salidatxt,'Cantidad de pronos: %2.0f\n',length(ensemble_hoy_48(:,1)));
fprintf(salidatxt,'LOW STD, HI STD %3.2f %3.2f\n',low_std_24,hi_std_24);
fprintf(salidatxt,'Rank histograms\n');
fprintf(salidatxt,' %5.3f',rank_hist_48(1,:));
fprintf(salidatxt,' \n');
fprintf(salidatxt,' %5.3f',rank_hist_48(2,:));
fprintf(salidatxt,' \n');
fprintf(salidatxt,' %5.3f',rank_hist_48(3,:));
fprintf(salidatxt,' \n');
fprintf(salidatxt,' %5.3f',rank_hist_48(4,:));
fprintf(salidatxt,' \n');

mean_hoy_48=nanmean(ensemble_hoy_48(:,2:columnas48),2);

prob_nocal_aux=prob_cal_fun2(ensemble_hoy_48(:,2:columnas48),umbral);
prob_cal_aux=prob_cal_fun(ensemble_arch_48(:,1),ensemble_hoy_48(:,2:columnas48),rank_hist_48,[low_std_48 hi_std_48],umbral,1,2);
prob_cal_aux2=prob_cal_fun(ensemble_arch_48(:,1),ensemble_hoy_48(:,2:columnas48),rank_hist_48_estatico,[low_std_48_estatico hi_std_48_estatico],umbral,1,2);
prob_cal_aux3=prob_cal_fun3(mean_hoy_48,pcal_48,umbral,rango_lluvia);          %Calculo probabilidad en base a la media del esnemble.
prob_cal_aux4=prob_cal_fun3(ensemble_hoy_48(:,2),pcal2_48,umbral,rango_lluvia); %Calculo probabilidad en base al control.

%aca tengo que agregar la calibracion est�tica.

prob_nocal_48=[prob_nocal_48 ; prob_nocal_aux];
prob_cal_48=[prob_cal_48 ;prob_cal_aux];
prob_cal2_48=[prob_cal2_48 ; prob_cal_aux2];
prob_cal3_48=[prob_cal3_48 ; prob_cal_aux3];
prob_cal4_48=[prob_cal4_48 ; prob_cal_aux4];
est_data_48=[est_data_48 ;est_datahoy_48];

%Construyo un vector con la fecha y las observaciones.
verif_aux(:,1)=hoy_num*ones(length(ensemble_hoy_48(:,1)),1);        %Guardo la fehca en la primera columan.
verif_aux(:,2)=ensemble_hoy_48(:,1);                                %Guardo los datos en la segunda.
verif_48=[verif_48;verif_aux];

clear verif_aux clear prob_nocal_aux prob_cal_aux rank_hist_48  rank_hist_24 aux_data prob_cal_aux2 prob_cal_aux3 prob_cal_aux4
clear ensemble_arch_24 ensemble_arch_48 mean_arch24 mean_arch48 ensemble_hoy_24 mean_hoy_24 mean_hoy_48 ensemble_hoy_48 fecha_24 fecha_48
clear i_archivo est_datahoy_24 est_datahoy_48


hi_std_24
hi_std_48



end

%Voy a llevar un control de que dias esta el ensemble y se calibra y cuales
%no.
datestr(date_num,'yyyy/mm/dd')
fechas=[fechas;datestr(date_num,'yyyy/mm/dd')];
if (paso2_24==1)
    flag=1;
else
    flag=0;
end

flag24=[flag24;flag];

if (paso2_48==1)
    flag=1;
else
    flag=0;
end

flag48=[flag48;flag];


%**************************************************************************
%   FIN DE LA CALIBRACION. (TERCER PASO)   
%**************************************************************************


%**************************************************************************
date_num=date_num+1;
end %ESTE ES EL END DEL CICLO SOBRE LAS FECHAS!!
%**************************************************************************


%**************************************************************************



save(salida,'prob_nocal_24','prob_nocal_48','prob_cal_24','prob_cal_48','prob_cal2_24','prob_cal2_48','prob_cal3_24','prob_cal3_48'...
            ,'prob_cal4_24','prob_cal4_48','verif_24','verif_48','est_data_24','est_data_48','flag24','flag48');


