clear all
close all
%Solo calibra en las fechas en las que el SLAF calibro para poder llegar a
%un numero de dias comun entre ambos sets de datos.
%*************************************************************************
% La idea de este script es cargar los datos de un ensemble (el super
% ensemble por ejemplo) y realizar las siguientes acciones.
% - Determinar si el n�mero de miembros es mayor a un n�mero minimo. Esto
% es de particular importancia sobre todo en el superensemble donde el
% numero de miembros puede ser variable de acuerdo con la disponibilidad de
% los mismos.
% - En base a los modelos presentes buscamos los 15 dias previos en los
% cuales esos modelos esten disponibles. Eventuualmente si no conseguimos
% 15 dias previos podemos reducir a�n mas el n�mero de modelos hasta
% siempre y cuando no baje del umbral.
% - Si consigo una cierta cantidad de datos (1000 por ejemplo) construyo
% los rank histograms con esa cantidad de datos.
% - Uso el rank histogram y los datos para calibrar el pron�stico de
% precipitaci�n para ese dia. Y obtengo un set de datos de pronosticos
% probabilisticos calibrados.
% 
% Como este script est� orientado al superensemble vamos a trabajar en base
% a estaciones con una matriz inicial que tiene el siguiente formato:
% tiene tantas columas como miembros + 1 (la verificaci�n). La primera
% columna es la verificacion que puede ser CMORPH o GTS para las estaciones
% en las cuales se calcula precipitaci�n en el super ensemble.
% Las demas columnas son los diferentes modelos regionales y globales que
% componen el super-ensemble.
% Existe una tercera variable ademas de p24 y p48 que es est_data. Esta
% variable contiene en la primera columna la fecha que se usar� para buscar
% los 15 d�as previos disponibles.
% Funciones que usa:
% test_arch_fun  : detecta que miembros del ensemble puedo sacar para
% conseguir una base historica reciente mas completa.
% prob_cal_fun   : calcula la probabilidad calibrada
% prob_cal2_fun  : calcula la probabilidad no calibrada
% pre_cal_fun    : Calcula las varianzas y los rank histograms.
%*************************************************************************
% Juan Ruiz - 2007 (Primer script del a�o!!!)
%*************************************************************************
%PARAMETROS MODIFICABLES

%Region y fuente de datos:
region='sur'
source='gts'

%Directorio donde estan los archivos.
path='d:/trabajos/TrabajoSLAF/precipitacion/superensemble/matlab/'



%Numero de miembros del ensamble

i_bem=2; %Numero de columna correspondiente al modelo que va a ser tomado como el "mejor" miembro del ensemble.

ens_min=10;  %Numero minimo de miembros para hacer la calibracion.


%Numero de puntos (observaciones previas) que vamos a usar para la
%calibracion.

cal_size=1000; %1000 equivale para cmo a 15 dias previos aprox.

%Umbral en porcentaje de datos faltantes que vamos a tolerar para un dado
%d�a para un dado modelo.

umbral_faltantes=0.30; 

%Umbrales para los cuales vamos a calcular las probabilidades.

umbral=[0.01 0.10 0.25 0.5 1 1.5 2]*25.4;

%Inicializo algunas variables


fechas=[];
verif_24=[];
verif_48=[];
prob_nocal_24=[];
prob_nocal_48=[];
prob_cal_24=[];
prob_cal_48=[];
prob_cal3_24=[];
prob_cal3_48=[];
prob_cal4_24=[];
prob_cal4_48=[];
est_data_24=[];
est_data_48=[];


%INICIAMOS EL CALCULO
%**************************************************************************

%Abro el archivo.
load(strcat(path,'superensemble_',region,'_',source,'.mat'));
%Uso este archivo solo para cargar flag24 y flag48 que me van a decir
%cuando anduvo la calibracion del SMES.
load(strcat('d:/trabajos/TrabajoSLAF/precipitacion/slaf2006/matlab/','slaf2006_prob3_',region,'_',source,'.mat'),'flag24','flag48');
flag24_slaf=flag24;clear flag24;
flag48_slaf=flag48;clear flag48;
flag24=[];
flag48=[];


%En este punto aparecen 3 variables p24 (prono de pp y su verificacion a 24
%horas). p48 (idem a 48 horas) y est_data (dia yyyymmdd y id de estacion
%junto con su lat y lon para cada punto de p24 y p48).

%Archivo de salida
salida=strcat(path,'superensemble_prob3_',region,'_',source,'.mat');

%Este archivo de salida en formato texto contiene los rank histograms
%utilizados, el numero de miembros para cada d�a y otros datos que nos
%permieten controlar como esta funcionando el script.
salidatxt=fopen(strcat(path,'superensemble_prob_',region,'_',source,'.txt'),'wt');


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

%Calculo los faltantes en cada matriz.
faltantes24=sum(isnan(aux_24));
faltantes48=sum(isnan(aux_48));

%Calculo el porcentaje de datos faltantes sobre el total

if(filas24 > 0)

fratio24=faltantes24./filas24;
i_aprobado24=find(fratio24 < umbral_faltantes);
i_bem24=find(i_aprobado24==i_bem);  %Busco a que nuevo i corresponde el i_bem para el prono a 24 horas.
if(length(i_bem24)==0)
    i_bem24=0
end

else 
    
i_aprobado24=0;
error24=1
end

if(filas48 > 0)
fratio48=faltantes48./filas48;
i_aprobado48=find(fratio48 < umbral_faltantes);
i_bem48=find(i_aprobado48==i_bem);  %Busco a que nuevo i corresponde el i_bem para el prono a 48 horas.
if(length(i_bem48)==0)
    i_bem48=0
end
    
else
    
i_aprobado48=0;
error48=1 
end

%Si el numero de miembros aprobados es menor que el numero minimo requerido
%entonces desaprobamos el dia asignandole a i_aprobadoxx el valor de 0.

if(length(i_aprobado24)< ens_min)
    date_vec
    i_aprobado24=0;
    error24=2
end
if(length(i_aprobado48)< ens_min)
    i_aprobado48=0;
    error48=2
end

fprintf(salidatxt,'Los modelos aprobados en el primer paso para el pron�stico a 24 horas son\n');
fprintf(salidatxt,'%2.0f\n',length(i_aprobado24));
fprintf(salidatxt,'Los modelos aprobados en el primer paso para el pron�stico a 48 horas son\n');
fprintf(salidatxt,'%2.f\n',length(i_aprobado48));

%Me quedo solo con los modelos aprobados en este paso. En el paso
%siguiente, si vuelvo a reducir el n�mero de modelos, tengo que recalcular
%la variable ensemble_hoy_xx

if(i_aprobado24 ~= 0)
i_nonan24=find(any(isnan(aux_24(:,i_aprobado24)),2)==0);
ensemble_hoy_24=aux_24(i_nonan24,i_aprobado24);
est_datahoy_24=aux_data(i_nonan24,:);
end
if(i_aprobado48 ~= 0)
i_nonan48=find(any(isnan(aux_48(:,i_aprobado48)),2)==0);
ensemble_hoy_48=aux_48(i_nonan48,i_aprobado48);
est_datahoy_48=aux_data(i_nonan48,:);
end

%Borro algunas variables que no voy a seguir usando.
clear fratio24 fratio48 faltantes24 faltantes48 i_hoy aux_24 aux_48 i_nonan24 i_nonan48 %date_vec

%**************************************************************************
%   FIN DE LA SELECCION INICIAL DE MODELOS. (PRIMER PASO)   
%**************************************************************************

%**************************************************************************
%     PASO 2: EN BASE AL ARCHIVO BUSCO 15 DIAS DONDE TENGA PRONOSTICOS DE
%     LOS MODELOS PRESELECCIONADOS PARA PODER CALIBRAR EL PRONO.
%     En este paso, usamos la funcion test_arch_fun que dado datos viejos
%     del ensemble busca que modelos podemos descartar de manera tal que
%     aument la cantidad de datos que no sean nan para ese ensemble siempre
%     y cuando la cantidad de miembros no baje por debajo del umbral.
%**************************************************************************

paso2_24=0;
paso2_48=0;

%Primero busco todos los dias previos al dia de hoy.

i_archivo=find(est_data(:,1) < hoy_num);

%Me quedo con los dias previos solo de los modelos aprobados.

if(i_aprobado24~=0)
aux_24=p24(i_archivo,i_aprobado24);
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
    [i_a ensemble_arch_24]=test_arch_fun(aux_24,cal_size,ens_min);
    if(i_a==0)
        paso2_24=0;
        error24=3
        clear i_a
    else
        paso2_24=1;
        %reduje el numero de miembros tengo que reducir ensemble_hoy_xx
        temp=ensemble_hoy_24(:,i_a);
        i_bem24=find(i_bem24==i_a);
        clear ensemble_hoy_24;
        ensemble_hoy_24=temp;
        clear temp;
        clear i_a
    end 
end

end

if(i_aprobado48~=0)
    
aux_48=p48(i_archivo,i_aprobado48);
i_nonan48=find(any(isnan(aux_48),2)==0);
aux_fecha=est_data(i_archivo,1);
aux2_48=aux_48(i_nonan48,:);
fecha_48=aux_fecha(i_nonan48);
[filas_arch_48 columnas_arch_48]=size(aux2_48);
if(filas_arch_48 > cal_size)
    ensemble_arch_48=aux2_48(filas_arch_48-cal_size:filas_arch_48,:);
    paso2_48=1; %Indica que el paso 2 termino con exito para las 48 horas.
    
else
    [i_a ensemble_arch_48]=test_arch_fun(aux_48,cal_size,ens_min);
    if(i_a==0)
        paso2_48=0;
        error24=3
        clear i_a
    else
        paso2_48=1;
        %reduje el numero de miembros tengo que reducir ensemble_hoy_xx
        temp=ensemble_hoy_48(:,i_a);
        i_bem48=find(i_bem48==i_a);
        clear ensemble_hoy_48;
        ensemble_hoy_48=temp;
        clear temp;
        clear i_a
    end 
end

end

clear aux_24 aux_48 i_a i_aprobado24 i_aprobado48 i_nonan24 i_nonan48 aux_fecha aux2_24 aux2_48


%**************************************************************************
%   FIN DE LA BUSQUEDA DE CAL_SIZE DATOS PREVIOS. (SEGUNDO PASO)   
%**************************************************************************


%**************************************************************************
%       TERCER PASO: ACA VIENE LA CALIBRACION PROPIAMENTE DICHA USANDO LAS
%       FUNCIONES PARA EL CALCULO DE LOS RANK HISTOGRAMS 
%**************************************************************************

if(paso2_24==1 & flag24_slaf(date_num-ini_date_num+1)) %Solo ingresamos en este paso si el paso previo result� exitoso.

%Primer paso para la calibracion: Calculo rank histograms de acuerdo a la
%dispersion del ensemble.
[filas24 columnas24]=size(ensemble_arch_24);
[rank_hist_24 low_std_24 hi_std_24] = pre_cal_fun(ensemble_arch_24(:,1),ensemble_arch_24(:,2:columnas24),1);

mean_arch24=nanmean(ensemble_arch_24(:,2:columnas24),2);
[pcal_24 ncal_24 rango_lluvia]=pre_cal_fun2(ensemble_arch_24(:,1),mean_arch24,umbral,1);
if(length(i_bem24) == 1 & i_bem24 ~= 0 )
[pcal2_24 ncal2_24 rango_lluvia]=pre_cal_fun2(ensemble_arch_24(:,1),ensemble_arch_24(:,i_bem24),umbral,1);
end

mean_hoy_24=nanmean(ensemble_hoy_24(:,2:columnas24),2);

prob_nocal_aux=prob_cal_fun2(ensemble_hoy_24(:,2:columnas24),umbral);
prob_cal_aux=prob_cal_fun(ensemble_arch_24(:,1),ensemble_hoy_24(:,2:columnas24),rank_hist_24,[low_std_24 hi_std_24],umbral,1,1);
prob_cal_aux3=prob_cal_fun3(mean_hoy_24,pcal_24,umbral,rango_lluvia);          %Calculo probabilidad en base a la media del ensemble.
if(length(i_bem24) == 1 & i_bem24 ~= 0 )
prob_cal_aux4=prob_cal_fun3(ensemble_hoy_24(:,i_bem24),pcal2_24,umbral,rango_lluvia); %Calculo probabilidad en base 
else
prob_cal_aux4=prob_cal_aux3*NaN; %Si no est� el modelo entonces prob_cal_aux se llena con nans.
end

%Voy guardando todas las probabilidades en una sola matriz
prob_nocal_24=[prob_nocal_24;prob_nocal_aux];
prob_cal_24=[prob_cal_24; prob_cal_aux];
prob_cal3_24=[prob_cal3_24; prob_cal_aux3];
prob_cal4_24=[prob_cal4_24; prob_cal_aux4];

est_data_24=[est_data_24 ;est_datahoy_24];

%Construyo un vector con la fecha y las observaciones.
verif_aux(:,1)=hoy_num*ones(length(ensemble_hoy_24(:,1)),1);        %Guardo la fehca en la primera columan.
verif_aux(:,2)=ensemble_hoy_24(:,1);   %Guardo los datos en la segunda.

verif_24=[verif_24;verif_aux];

clear verif_aux clear prob_nocal_aux prob_cal_aux prob_cal_aux3 prob_cal_aux4 rank_hist_24

date_vec
end

if(paso2_48==1 & flag48_slaf(date_num-ini_date_num+1)) %Solo ingresamos en este paso si el paso previo result� exitoso.

%Primer paso para la calibracion: Calculo rank histograms de acuerdo a la
%dispersion del ensemble.
[filas48 columnas48]=size(ensemble_arch_48);
[rank_hist_48 low_std_48 hi_std_48] = pre_cal_fun(ensemble_arch_48(:,1),ensemble_arch_48(:,2:columnas48),1);

mean_arch48=nanmean(ensemble_arch_48(:,2:columnas48),2);
[pcal_48 ncal_48 rango_lluvia]=pre_cal_fun2(ensemble_arch_48(:,1),mean_arch48,umbral,1);
if(length(i_bem48) == 1 & i_bem48 ~= 0 )
[pcal2_48 ncal2_48 rango_lluvia]=pre_cal_fun2(ensemble_arch_48(:,1),ensemble_arch_48(:,i_bem48),umbral,1);
end

mean_hoy_48=nanmean(ensemble_hoy_48(:,2:columnas48),2);

prob_nocal_aux=prob_cal_fun2(ensemble_hoy_48(:,2:columnas48),umbral);
prob_cal_aux=prob_cal_fun(ensemble_arch_48(:,1),ensemble_hoy_48(:,2:columnas48),rank_hist_48,[low_std_48 hi_std_48],umbral,1,1);
prob_cal_aux3=prob_cal_fun3(mean_hoy_48,pcal_48,umbral,rango_lluvia);          %Calculo probabilidad en base a la media del ensemble.
if(length(i_bem48) == 1 & i_bem48 ~= 0 )
prob_cal_aux4=prob_cal_fun3(ensemble_hoy_48(:,i_bem48),pcal2_48,umbral,rango_lluvia); %Calculo probabilidad en base 
else
prob_cal_aux4=prob_cal_aux3*NaN; %Si no est� el modelo entonces prob_cal_aux se llena con nans.
end


prob_nocal_48=[prob_nocal_48 ; prob_nocal_aux];
prob_cal_48=[prob_cal_48 ;prob_cal_aux];
prob_cal3_48=[prob_cal3_48; prob_cal_aux3];
prob_cal4_48=[prob_cal4_48; prob_cal_aux4];
est_data_48=[est_data_48 ;est_datahoy_48];


%Construyo un vector con la fecha y las observaciones.
verif_aux(:,1)=hoy_num*ones(length(ensemble_hoy_48(:,1)),1);        %Guardo la fecha en la primera columan.
verif_aux(:,2)=ensemble_hoy_48(:,1);   %Guardo los datos en la segunda.
verif_48=[verif_48;verif_aux];

clear verif_aux clear prob_nocal_aux prob_cal_aux prob_cal_aux3 prob_cal_aux4 rank_hist_48  rank_hist_24 aux_data 
clear ensemble_arch_24 ensemble_arch_48 ensemble_hoy_24 ensemble_hoy_48 fecha_24 fecha_48
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



save(salida,'prob_nocal_24','prob_nocal_48','prob_cal_24','prob_cal_48','prob_cal3_24','prob_cal3_48','prob_cal4_24',...
            'prob_cal4_48','verif_24','verif_48','est_data_24','est_data_48','flag24','flag48');


