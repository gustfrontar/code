clear all
close all
%*************************************************************************
% PAra la Region Norte!!!
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

%Numero de miembros del ensamble
ens=19;      %Numero total de miembros del ensemble.
ens_min=9;  %Numero minimo de miembros para hacer la calibracion.


%Numero de puntos (observaciones previas) que vamos a usar para la
%calibracion.
cal_size=1000; %1000 equivale para cmo a 30 dias previos aprox.

%Umbral en porcentaje de datos faltantes que vamos a tolerar para un dado
%d�a para un dado modelo.
umbral_faltantes=0.30; 

%Umbrales para los cuales vamos a calcular las probabilidades.
umbral=[0.01 0.10 0.25 0.5 1 1.5 2]*25.4;

%Inicializo algunas variables

verif_24=[];
verif_48=[];
prob_nocal_24=[];
prob_nocal_48=[];
prob_cal_24=[];
prob_cal_48=[];
est_data_24=[];
est_data_48=[];




%************************Region Norte**************************
lat_min_n=-20; 
lat_max_n=15;




%INICIAMOS EL CALCULO
%**************************************************************************
%Abro el archivo.
load pp_superensemble.mat;

%En este punto aparecen 2 variables pp_ensemble_24 y pp_ensemble_48, las
%cuales tienen en la primera columna la fecha, en la segunda la estacion,
%en la tercera la latitud, en la cuarta la longitud, en la quinta la
%observacion, en la sexta los datos Cmophr, en la septima los datos "trmm"
%y de la octava a la vigesimasexta tiene los rponosticos de los distontos
%modelos ordenados como se ve en el script precip_superensemble.m


%Archivo de salida
salida=strcat('ppsmes_norte','.mat');

%Este archivo de salida en formato texto contiene los rank histograms
%utilizados, el numero de miembros para cada d�a y otros datos que nos
%permieten controlar como esta funcionando el script.
salidatxt=fopen(strcat('superensemble_prob_norte','.txt'),'wt');

%Este archivo de salida en formato texto contiene los datos de probabilidad
%de precipitacion
salidatxt_n=fopen(strcat('prob_smes_n','.txt'),'wt');



%**************************************************************************
%COMIENZA EL CICLO EN TIEMPO.
%**************************************************************************
% Ini_date y end_date marcan las fechas de inicio y final de los
% pronosticos, pero el pronóstico calibrado va a tener que arrancar n_days
% despues porque necesita informacion sobre la verificacion de los
% pronosticos previos.

%Fecha de inicio.
%El formato es dd-mmm-yyyy (el mes son las 3 primeras letras en ingles)
%Genero la fecha q voy a usar para buscar en la base de datos
S=load('-ascii', 'fech.txt');
aa=num2str(S);
b=datenum(aa,'yyyymmdd');
c=b;
d=datevec(c);
ini_date=datestr(d, 'dd-mmm-yyyy'); %Fecha para el CMORPH y fecha para el pronostico WRF


%Genero un numero que identifica la fecha de arranque del pronostico
%calibrado. En principio el script se da cuenta solo de cuantos dias tiene
%que dejar pasar para poder juntar el numero necesario de observaciones
%previas para realizar la calibracion.
date_num=datenum(ini_date);

%Hago lo mismo para la fecha de fin y comienzo de los pronosticos
ini_date_num=datenum(ini_date);
   
    


%**************************************************************************
%   PRIMER PASO, BUSCO TODOS LOS PRONOSTICOS PARA EL DIA ACTUAL Y VERIFICO
%   QUE LA CANTIDAD DE MIEMBROS SEA LA ADECUADA
%**************************************************************************

%Calculo la fecha de hoy en numero
date_vec=datevec(date_num);
hoy_num=(date_vec(1))*1e4+date_vec(2)*1e2+date_vec(3); %yyyymmdd

fprintf(salidatxt,'La fecha es: %6.0f\n',hoy_num);

%Busco todos los pronosticos para hoy.
i_hoy_24=find(pp_ensemble_24(:,1)==hoy_num & pp_ensemble_24(:,3)<lat_max_n & pp_ensemble_24(:,3)>lat_min_n);
i_hoy_48=find(pp_ensemble_48(:,1)==hoy_num & pp_ensemble_48(:,3)<lat_max_n & pp_ensemble_48(:,3)>lat_min_n);


% Genero un matriz solo con los datos del dia de hoy para los pronosticos
% (saco todo lo extra como ser estacion, lat, long, observacion, etc)
aux_24=pp_ensemble_24(i_hoy_24,8:26);
aux_48=pp_ensemble_48(i_hoy_48,8:26);

% Genero una matriz con los datos de fecha, estacion, lat y long
aux_data_24=pp_ensemble_24(i_hoy_24,:);
aux_data_24=aux_data_24(:,1:4);
aux_data_48=pp_ensemble_48(i_hoy_48,:);
aux_data_48=aux_data_48(:,1:4);

%Vemos cuantos miembros tenemos

%Calculo las dimensiones de cada matriz.
[filas24 columnas24]=size(aux_24);
filas_pepe=filas24;
[filas48 columnas48]=size(aux_48);
filas_pepi=filas48;

%Calculo los faltantes en cada matriz.
faltantes24=sum(isnan(aux_24));
faltantes48=sum(isnan(aux_48));

%Calculo el porcentaje de datos faltantes sobre el total
if(filas24 > 0)
    fratio24=faltantes24./filas24;
    i_aprobado24=find(fratio24 < umbral_faltantes);
else 
    i_aprobado24=0;
end

if(filas48 > 0)
    fratio48=faltantes48./filas48;
    i_aprobado48=find(fratio48 < umbral_faltantes);
else
    i_aprobado48=0;
end

%Si el numero de miembros aprobados es menor que el numero minimo requerido
%entonces desaprobamos el dia asignandole a i_aprobadoxx el valor de 0.
if(length(i_aprobado24)< ens_min)
    i_aprobado24=0;
end
if(length(i_aprobado48)< ens_min)
    i_aprobado48=0;
end

fprintf(salidatxt,'Los modelos aprobados en el primer paso para el pron�stico a 24 horas son\n');
fprintf(salidatxt,'%2.0f\n',length(i_aprobado24));
fprintf(salidatxt,'Los modelos aprobados en el primer paso para el pron�stico a 48 horas son\n');
fprintf(salidatxt,'%2.f\n',length(i_aprobado48));

%Me quedo solo con los modelos aprobados en este paso. En el paso
%siguiente, si vuelvo a reducir el n�mero de modelos, tengo que recalcular
%la variable ensemble_hoy_xx

if(i_aprobado24 ~= 0)
    i_nonan24_hoy=find(any(isnan(aux_24(:,i_aprobado24)),2)==0);
    ensemble_hoy_24=aux_24(i_nonan24_hoy,i_aprobado24);
    est_datahoy_24=aux_data_24(i_nonan24_hoy,:);
end
if(i_aprobado48 ~= 0)
    i_nonan48_hoy=find(any(isnan(aux_48(:,i_aprobado48)),2)==0);
    ensemble_hoy_48=aux_48(i_nonan48_hoy,i_aprobado48);
    est_datahoy_48=aux_data_48(i_nonan48_hoy,:);
end

%Borro algunas variables que no voy a seguir usando.
clear fratio24 fratio48 faltantes24 faltantes48 date_vec i_hoy aux_24 aux_48 




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



%Primero busco todos los dias previos al dia de hoy.
i_archivo_24=find(pp_ensemble_24(:,5) >= 0 & pp_ensemble_24(:,1) < hoy_num & pp_ensemble_24(:,3)<lat_max_n & pp_ensemble_24(:,3)>lat_min_n);
i_archivo_48=find(pp_ensemble_48(:,5) >= 0 & pp_ensemble_48(:,1) < hoy_num & pp_ensemble_48(:,3)<lat_max_n & pp_ensemble_48(:,3)>lat_min_n);



%Aux_24_2 y aux_48_2 tienen en la primera columna los datos que vamos a
%utilizar para la calibracion (Sea GTS CMORPH o TRMM), por eso tiene una
%columna más que la variable aux_24 y aux_48 del paso anterior. En los
%pasos siguientes usamos la variable i_archivo_24 que se refiere a las
%columnas que corresponden a los modelos analizados en el paso anterior,
%para referir esos numeros de columnas a las variables aux_24_2 y aux_48_2
%de este paso tenemos que sumarle 1. 

%Me quedo con los dias previos solo de los modelos aprobados.
if(i_aprobado24~=0)
    obs_24=pp_ensemble_24(i_archivo_24,5);
    aux_24=pp_ensemble_24(i_archivo_24,[i_aprobado24+7]);
    %Busco aquellos dias/puntos en los que alguno de los pron�sticos es NaN.
    i_nonan24=find(any(isnan(aux_24),2)==0);
    aux_fecha=pp_ensemble_24(i_archivo_24,1);
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
        if(i_a==0 | i_a(1) ~= 1 )
            paso2_24=0;
            clear i_a
        else
            paso2_24=1;
            %reduje el numero de miembros tengo que reducir ensemble_hoy_xx
            temp=ensemble_hoy_24(:,i_a);
            clear ensemble_hoy_24;
            ensemble_hoy_24=temp;
            clear temp;
            clear i_a
        end 
    end
else
    paso2_24=0;
end


if(i_aprobado48~=0)
    obs_48=pp_ensemble_48(i_archivo_48,5);
    aux_48=pp_ensemble_48(i_archivo_48,[i_aprobado48+7]);
    i_nonan48=find(any(isnan(aux_48),2)==0);
    aux_fecha=pp_ensemble_48(i_archivo_48,1);
    aux2_48=aux_48(i_nonan48,:);
    fecha_48=aux_fecha(i_nonan48);
    [filas_arch_48 columnas_arch_48]=size(aux2_48);
    if(filas_arch_48 > cal_size)
        ensemble_arch_48=aux2_48(filas_arch_48-cal_size:filas_arch_48,:);
        paso2_48=1; %Indica que el paso 2 termino con exito para las 48 horas.
    else
        [i_a ensemble_arch_48]=test_arch_fun(aux_48,cal_size,ens_min);
        if(i_a==0 | i_a(1) ~= 1 )
            paso2_48=0;
            clear i_a
        else
            paso2_48=1;
            %reduje el numero de miembros tengo que reducir ensemble_hoy_xx
            temp=ensemble_hoy_48(:,i_a);
            clear ensemble_hoy_48;
            ensemble_hoy_48=temp;
            clear temp;
            clear i_a
        end 
    end
else
    paso2_48=0;
end

clear aux_24 aux_48 i_a i_aprobado24 i_aprobado48 i_nonan24 i_nonan48 aux_fecha aux2_24 aux2_48 columnas24 filas24 columnas48 filas48




%**************************************************************************
%   FIN DE LA BUSQUEDA DE CAL_SIZE DATOS PREVIOS. (SEGUNDO PASO)   
%**************************************************************************


%**************************************************************************
%       TERCER PASO: ACA VIENE LA CALIBRACION PROPIAMENTE DICHA USANDO LAS
%       FUNCIONES PARA EL CALCULO DE LOS RANK HISTOGRAMS 
%**************************************************************************

if(paso2_24==1) %Solo ingresamos en este paso si el paso previo result� exitoso.
    %Primer paso para la calibracion: Calculo rank histograms de acuerdo a la
    %dispersion del ensemble.
    [filas24 columnas24]=size(ensemble_arch_24); 
    [rank_hist_24 low_std_24 hi_std_24] = pre_cal_fun_smes(obs_24,ensemble_arch_24,0)
    
    
    figure
subplot (2,2,1)
bar(rank_hist_24(1,:))
title('Dispersion baja','FontSize',13)
xlabel('Intervalos de Probabilidad','FontSize',12)
ylabel('Probabilidad','FontSize',12)
%set(gca,'YTick',[0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9])
% Dispersion media
subplot (2,2,2)
bar(rank_hist_24(2,:))
title('Dispersion media','FontSize',13)
xlabel('Intervalos de Probabilidad','FontSize',12)
ylabel('Probabilidad','FontSize',12)
% Dispersion alta
subplot (2,2,3)
bar(rank_hist_24(3,:))
title('Dispersion alta','FontSize',13)
xlabel('Intervalos de Probabilidad','FontSize',12)
ylabel('Probabilidad','FontSize',12)
% Dispersion total
subplot (2,2,4)
bar(rank_hist_24(4,:))
title('Dispersion Total','FontSize',13)
xlabel('Intervalos de Probabilidad','FontSize',12)
ylabel('Probabilidad','FontSize',12)
archivo=strcat('histo','_24_n');
print('-depsc',archivo)
close 1
    
    
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

    % La variable ensemble_hoy_24 tiene solo los pronosticos (sin observaciones)
    prob_nocal_aux=prob_cal_fun2(ensemble_hoy_24,umbral);
    % La variable ensemble_arch_24 tiene los pronosticos nada mas
    prob_cal_aux=prob_cal_fun_smes(obs_24,ensemble_hoy_24,rank_hist_24,[low_std_24 hi_std_24],umbral,1,1);

    %Voy guardando todas las probabilidades en una sola matriz
    prob_nocal_24=[prob_nocal_24;prob_nocal_aux];
    prob_cal_24=[prob_cal_24; prob_cal_aux];
    est_data_24=[est_data_24 ;est_datahoy_24];

    %Construyo un vector con la fecha y las observaciones.
    verif_aux(:,1)=hoy_num*ones(length(ensemble_hoy_24(:,1)),1);        %Guardo la fehca en la primera columan.
    verif_aux(:,2)=ensemble_hoy_24(:,1);   %Guardo los datos en la segunda.
    verif_24=[verif_24;verif_aux];
    
    clear verif_aux clear prob_nocal_aux prob_cal_aux rank_hist_24
else
    est_datahoy_24=aux_data_24(:,:);
    est_data_24=[est_data_24 ;est_datahoy_24];
    fila_24=size(est_datahoy_24,1);
    verif_aux_24(:,1)=pp_ensemble_24(i_hoy_24,1);        %Guardo la fehca en la primera columan.
    verif_aux_24(:,2)=pp_ensemble_24(i_hoy_24,5);   
    verif_24=[verif_24;verif_aux_24];
    prob_cal_24=NaN(fila_24,7);
    prob_nocal_24=NaN(fila_24,7);
end

if(paso2_48==1) %Solo ingresamos en este paso si el paso previo result� exitoso.
    %Primer paso para la calibracion: Calculo rank histograms de acuerdo a la
    %dispersion del ensemble.
    [filas48 columnas48]=size(ensemble_arch_48);
    [rank_hist_48 low_std_48 hi_std_48] = pre_cal_fun_smes(obs_48,ensemble_arch_48,0);

    
    figure
subplot (2,2,1)
bar(rank_hist_48(1,:))
title('Dispersion baja','FontSize',13)
xlabel('Intervalos de Probabilidad','FontSize',12)
ylabel('Probabilidad','FontSize',12)
%set(gca,'YTick',[0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9])
% Dispersion media
subplot (2,2,2)
bar(rank_hist_48(2,:))
title('Dispersion media','FontSize',13)
xlabel('Intervalos de Probabilidad','FontSize',12)
ylabel('Probabilidad','FontSize',12)
% Dispersion alta
subplot (2,2,3)
bar(rank_hist_48(3,:))
title('Dispersion alta','FontSize',13)
xlabel('Intervalos de Probabilidad','FontSize',12)
ylabel('Probabilidad','FontSize',12)
% Dispersion total
subplot (2,2,4)
bar(rank_hist_48(4,:))
title('Dispersion Total','FontSize',13)
xlabel('Intervalos de Probabilidad','FontSize',12)
ylabel('Probabilidad','FontSize',12)
archivo=strcat('histo','_48_n');
print('-depsc',archivo)
close 1
    
    
    
    
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

    % La variable ensemble_hoy_24 tiene solo los pronosticos (sin
    % observaciones)
    prob_nocal_aux=prob_cal_fun2(ensemble_hoy_48,umbral);
    % La variable ensemble_arch_24 tiene los pronosticos nada mas
    prob_cal_aux=prob_cal_fun_smes(obs_48,ensemble_hoy_48,rank_hist_48,[low_std_48 hi_std_48],umbral,1,1);

    prob_nocal_48=[prob_nocal_48 ; prob_nocal_aux];
    prob_cal_48=[prob_cal_48 ;prob_cal_aux];
    est_data_48=[est_data_48 ;est_datahoy_48];

    %Construyo un vector con la fecha y las observaciones.
    verif_aux(:,1)=hoy_num*ones(length(ensemble_hoy_48(:,1)),1);        %Guardo la fehca en la primera columan.
    verif_aux(:,2)=ensemble_hoy_48(:,1);   %Guardo los datos en la segunda.
    verif_48=[verif_48;verif_aux];  
    
    clear verif_aux clear prob_nocal_aux prob_cal_aux rank_hist_48  rank_hist_24 aux_data 
    %clear ensemble_arch_24 ensemble_arch_48 ensemble_hoy_24 ensemble_hoy_48 fecha_24 fecha_48
    clear i_archivo est_datahoy_24 est_datahoy_48
else
     est_datahoy_48=aux_data_48(:,:);
    est_data_48=[est_data_48 ;est_datahoy_48];
    fila_48=size(est_datahoy_48,1);
    verif_aux_48(:,1)=pp_ensemble_48(i_hoy_48,1);       %Guardo la fehca en la primera columan.
    verif_aux_48(:,2)=pp_ensemble_48(i_hoy_48,5);
    verif_48=[verif_48;verif_aux_48];
    prob_cal_48=NaN(fila_48,7);
    prob_nocal_48=NaN(fila_48,7);
end


%**************************************************************************
%   FIN DE LA CALIBRACION. (TERCER PASO)   
%**************************************************************************

% Genero una matriz con los datos q me quiero guardar para poder guardarlos
% en un txt
matriz_final=[est_data_24(:,1) est_data_24(:,2) est_data_24(:,3) est_data_24(:,4) prob_cal_24(:,1) prob_cal_48(:,1) prob_cal_24(:,2) prob_cal_48(:,2) prob_cal_24(:,5) prob_cal_48(:,5) prob_cal_24(:,7) prob_cal_48(:,7) prob_nocal_24(:,1) prob_nocal_48(:,1) prob_nocal_24(:,2) prob_nocal_48(:,2) prob_nocal_24(:,5) prob_nocal_48(:,5) prob_nocal_24(:,7) prob_nocal_48(:,7)];
tam_matriz=size(matriz_final,1)

% Hago un for sobre las filas de mi matriz para acomodarmela como la quiero
% y q no me aparezcan numeros exponenciales
for i=1:tam_matriz
    fprintf(salidatxt_n,' %8.0f %5.0f %6.2f %6.2f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f',matriz_final(i,1), matriz_final(i,2),matriz_final(i,3),matriz_final(i,4),matriz_final(i,5),matriz_final(i,6),matriz_final(i,7),matriz_final(i,8),matriz_final(i,9),matriz_final(i,10),matriz_final(i,11),matriz_final(i,12),matriz_final(i,13),matriz_final(i,14),matriz_final(i,15),matriz_final(i,16),matriz_final(i,17),matriz_final(i,18),matriz_final(i,19),matriz_final(i,20));
    fprintf(salidatxt_n,' \n');
end



% Me genero un archivo de texto para cada dia donde guardo la probabilidad
% de precipitacion
hoy_dia=num2str(hoy_num)
path_out=strcat('/WRFV2/wrfsi/domains/operativo/archivo/prob_smes/prob_smes_',hoy_dia,'_N','.txt')
out_txt=fopen(path_out,'w');
for i=1:tam_matriz
    fprintf(out_txt,' %8.0f %5.0f %6.2f %6.2f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f %8.5f',matriz_final(i,1), matriz_final(i,2),matriz_final(i,3),matriz_final(i,4),matriz_final(i,5),matriz_final(i,6),matriz_final(i,7),matriz_final(i,8),matriz_final(i,9),matriz_final(i,10),matriz_final(i,11),matriz_final(i,12),matriz_final(i,13),matriz_final(i,14),matriz_final(i,15),matriz_final(i,16),matriz_final(i,17),matriz_final(i,18),matriz_final(i,19),matriz_final(i,20));
    fprintf(out_txt,' \n');
end




%Me genero una matriz con el mismo largo de las observaciones asi dps lo
%puedo comparar.
%Probabilidad calibrada
pro_c_24=NaN(filas_pepe,7);
pro_c_24(i_nonan24_hoy,:)=prob_cal_24;
prob_cal_24=pro_c_24

probabi_24=NaN(filas_pepe,8);
probabi_24(:,1)=hoy_num;
probabi_24(:,2:8)=prob_cal_24;

pro_c_48=NaN(filas_pepi,7);
pro_c_48(i_nonan48_hoy,:)=prob_cal_48;
prob_cal_48=pro_c_48

probabi_48=NaN(filas_pepi,8);
probabi_48(:,1)=hoy_num;
probabi_48(:,2:8)=prob_cal_48;

%Probabilidad no calibrada
pro_nc_24=NaN(filas_pepe,7);
pro_nc_24(i_nonan24_hoy,:)=prob_nocal_24;
prob_nocal_24=pro_nc_24

probabino_24=NaN(filas_pepe,8);
probabino_24(:,1)=hoy_num;
probabino_24(:,2:8)=prob_nocal_24;

pro_nc_48=NaN(filas_pepi,7);
pro_nc_48(i_nonan48_hoy,:)=prob_nocal_48;
prob_nocal_48=pro_nc_48

probabino_48=NaN(filas_pepi,8);
probabino_48(:,1)=hoy_num;
probabino_48(:,2:8)=prob_nocal_48;


%Guarda los datos nuevos si es q existe el archivo y sino crea el archivo
%para guardar los datos
file=fopen('ppsmes_norte.mat');
if(file ~= -1)
%Va guardando todos los dias, el nuevo dato 
load ppsmes_norte.mat
nocal_24=[nocal_24;probabino_24];
nocal_48=[nocal_48;probabino_48];
cal_24=[cal_24;probabi_24];
cal_48=[cal_48;probabi_48];
save(salida,'nocal_24','nocal_48','cal_24','cal_48');
end


%Genera el archivo q va ir guardando los datos de la region WRF
%Donde el primer lugar corresponde a los umbrales, el segundo a
%los distintos pronosticos, el tercero a las variables y el cuarto a los
%tiempos
if(file==-1)
nocal_24=probabino_24;
nocal_48=probabino_48;
cal_24=probabi_24;
cal_48=probabi_48;
save(salida,'nocal_24','nocal_48','cal_24','cal_48');
end





