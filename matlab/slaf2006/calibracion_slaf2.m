clear all
close all
%*************************************************************************
% La idea es usar 2 calibraciones estáticas, una usando CMORPH y la otra
% usando GTS como fuente de datos. Las estaciones utilizadas y el periodo
% utilizado coincide en ambos casos, por lo que la comparacion es válida
% entre ambos set de datos.
% Este script trabaja con las salidas del script precip_gtsvscmo que esta
% en la carpeta de scripts/slaf2005
%*************************************************************************
% Juan Ruiz - 2007 (Este ya perdi la cuenta que numero es...)
%*************************************************************************
%PARAMETROS MODIFICABLES

%Region y fuente de datos:
region='norte'
source='gts'

%Directorio donde estan los archivos.
path='d:/trabajos/TrabajoSLAF/precipitacion/slaf2006/matlab/'

%Numero de miembros del ensamble

ens=10;      %Numero total de miembros del ensemble.

%Umbrales para los cuales vamos a calcular las probabilidades.

umbral=[0.01 0.10 0.25 0.5 1 1.5 2]*25.4;


%INICIAMOS EL CALCULO
%**************************************************************************

%Archivo con los rank histograms y sigmas de corte para la calibracion
%estatica.
load(strcat('d:/trabajos/TrabajoSLAF/precipitacion/slaf2005/matlab/slaf2005_',region,'_cmogts.mat'));
p24_estatico=p24;
p48_estatico=p48;
clear p24 p48


%Abro el archivo.
load(strcat(path,'slaf2006_',region,'_',source,'.mat'));


%En este punto aparecen 3 variables p24 (prono de pp y su verificacion a 24
%horas). p48 (idem a 48 horas) y est_data (dia yyyymmdd y id de estacion
%junto con su lat y lon para cada punto de p24 y p48).

%Archivo de salida
salida=strcat(path,'slaf2006_prob_',region,'_cmogts.mat');

[filas24 columnas24]=size(p24);
[filas48 columnas48]=size(p48);

   i_nonan24=find(any(isnan(p24(:,:)),2)==0);
   aux24=p24(i_nonan24,:);
   aux24_2=est_data(i_nonan24,:);
    
   i_nonan48=find(any(isnan(p48(:,:)),2)==0);
   aux48=p48(i_nonan48,:);
   aux48_2=est_data(i_nonan48,:); 

   clear p24 p48 est_data
   p24=aux24;
   p48=aux48;
   est_data_24=aux24_2;
   est_data_48=aux48_2;
   clear aux24 aux48 aux24_2 aux48_2 i_nonnan24 i_nonan48

%**************************************************************************
%      ACA VIENE LA CALIBRACION PROPIAMENTE DICHA USANDO LAS
%       FUNCIONES PARA EL CALCULO DE LOS RANK HISTOGRAMS
% Como es calibracion estatica puedo calibrar todo el periodo completo en 
% un solo paso.
%**************************************************************************

%mean24=nanmean(p24(:,2:columnas24),2);

prob_nocal_24=prob_cal_fun2(p24(:,2:columnas24),umbral);
prob_cal_24=prob_cal_fun(p24_estatico(:,2),p24(:,2:columnas24),rank_hist_24_gts,[low_std_24_gts hi_std_24_gts],umbral,1,2); %calibro con gts 
prob_cal2_24=prob_cal_fun(p24_estatico(:,2),p24(:,2:columnas24),rank_hist_24_cmo,[low_std_24_cmo hi_std_24_cmo],umbral,1,2);%calibro con cmorph
prob_cal3_24=prob_cal_fun3(p24(:,2),pcal2_24_gts,umbral,rango_lluvia);          %Calculo probabilidad en base al control gts.
prob_cal4_24=prob_cal_fun3(p24(:,2),pcal2_24_cmo,umbral,rango_lluvia);          %Calculo probabilidad en base al control cmo.

%Construyo un vector con las observaciones.

verif_24=p24(:,1);

prob_nocal_48=prob_cal_fun2(p48(:,2:columnas48),umbral);
prob_cal_48=prob_cal_fun(p48_estatico(:,2),p48(:,2:columnas24),rank_hist_48_gts,[low_std_48_gts hi_std_48_gts],umbral,1,2);
prob_cal2_48=prob_cal_fun(p48_estatico(:,2),p48(:,2:columnas24),rank_hist_48_cmo,[low_std_48_cmo hi_std_48_cmo],umbral,1,2);
prob_cal3_48=prob_cal_fun3(p48(:,2),pcal2_48_gts,umbral,rango_lluvia);          %Calculo probabilidad en base al control gts.
prob_cal4_48=prob_cal_fun3(p48(:,2),pcal2_48_cmo,umbral,rango_lluvia);          %Calculo probabilidad en base al control cmo.


%Construyo un vector con la fecha y las observaciones.

verif_48=p48(:,1);


%**************************************************************************
%   FIN DE LA CALIBRACION.
%**************************************************************************


%**************************************************************************



save(salida,'prob_nocal_24','prob_nocal_48','prob_cal_24','prob_cal_48','prob_cal2_24','prob_cal2_48','prob_cal3_24','prob_cal3_48','prob_cal4_24','prob_cal4_48','verif_24','verif_48','est_data_24','est_data_48');


