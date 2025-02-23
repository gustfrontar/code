clear all
close all
%*************************************************************************
%Este programa crea el conjunto de datos de precipitacon del Super Ensemble
%*************************************************************************
% Juan Ruiz - 2006
%*************************************************************************
%Vamos a usar la funcion precip_model_fun para generar el conjunto.

genera=1; %Si genera es 1 usamos los archivos .mat en base a los datos ya cargados 
%si genera es 0 volvemos a correr el script que carga los datos.
region=1; %1 es sur y 2 es norte
fuente=2; %1 es gts 2 es cmorph


if(genera==0)
    if(fuente==1)
    ini_date='01-Oct-2006';
    end
    if(fuente==2)
    ini_date='01-Sep-2006';
    end
    end_date='31-Dec-2006';
[p24eta40 p48eta40]=precip_model_fun('ETAxx',region,'12',5,5,ini_date,end_date,fuente);
[p24eta40 p48eta40]=precip_model_fun('ETAxx',region,'00',5,5,ini_date,end_date,fuente);
[p24eta20 p48eta20]=precip_model_fun('ETA20',region,'12',5,5,ini_date,end_date,fuente);
[p24eta20 p48eta20]=precip_model_fun('ETA20',region,'00',5,5,ini_date,end_date,fuente);
[p24t213x p48t213x]=precip_model_fun('T213x',region,'12',4,5,ini_date,end_date,fuente);
[p24t213x p48t213x]=precip_model_fun('T213x',region,'00',5,5,ini_date,end_date,fuente);
[p24b_uba p48b_uba]=precip_model_fun('B_UBA',region,'12',9,9,ini_date,end_date,fuente);
[p24b_uba p48b_uba]=precip_model_fun('B_UBA',region,'00',9,9,ini_date,end_date,fuente);
[p24hrmxx p48hrmxx]=precip_model_fun('HRMxx',region,'12',9,9,ini_date,end_date,fuente);
[p24hrmxx2 p48hrmxx2]=precip_model_fun('HRMxx',region,'00',9,9,ini_date,end_date,fuente);
[p24rpsas p48rpsas]=precip_model_fun('RPSAS',region,'12',5,5,ini_date,end_date,fuente);
[p24rpsas p48rpsas]=precip_model_fun('RPSAS',region,'00',5,5,ini_date,end_date,fuente);
[p24sfavn p48sfavn]=precip_model_fun('CATTx',region,'00',9,9,ini_date,end_date,fuente);
[p24sfavn p48sfavn]=precip_model_fun('WRFAR',region,'00',9,9,ini_date,end_date,fuente);
[p24sfavn p48sfavn]=precip_model_fun('WRFAR',region,'12',9,9,ini_date,end_date,fuente);
[p24etaum p48etaum]=precip_model_fun('ETAUM',region,'00',5,5,ini_date,end_date,fuente);
[p24etaum p48etaum]=precip_model_fun('MRFxx',region,'00',3,3,ini_date,end_date,fuente);
[p24etaum p48etaum]=precip_model_fun('SFENM',region,'12',3,3,ini_date,end_date,fuente);
[p24etaum p48etaum]=precip_model_fun('SFENM',region,'00',3,3,ini_date,end_date,fuente);
end

if(genera==1)
path='D:\trabajos\TrabajoSLAF\precipitacion\superensemble\matlab\'
if(region==1)
    reg='sur'
end
if(region==2)
     reg='norte'
end
if(fuente==1)
    fue='gts';
end
if(fuente==2)
    fue='cmo';
end


load(strcat(path,'ETAxx_',reg,'_',fue,'_12.mat'));
est_data=precip24(:,1:4);
p24(:,1)=precip24(:,5);
p24(:,2)=precip24(:,6);
p48(:,1)=precip48(:,5);
p48(:,2)=precip48(:,6);
clear precip24 precip48
load(strcat(path,'ETAxx_',reg,'_',fue,'_00.mat'));
p24(:,3)=precip24(:,6);
p48(:,3)=precip48(:,6);
clear precip24 precip48
load(strcat(path,'ETA20_',reg,'_',fue,'_12.mat'));
p24(:,4)=precip24(:,6);
p48(:,4)=precip48(:,6);
clear precip24 precip48
load(strcat(path,'ETA20_',reg,'_',fue,'_00.mat'));
p24(:,5)=precip24(:,6);
p48(:,5)=precip48(:,6);
clear precip24 precip48
load(strcat(path,'T213x_',reg,'_',fue,'_12.mat'));
p24(:,6)=precip24(:,6);
p48(:,6)=precip48(:,6);
clear precip24 precip48
load(strcat(path,'T213x_',reg,'_',fue,'_00.mat'));
p24(:,7)=precip24(:,6);
p48(:,7)=precip48(:,6);
clear precip24 precip48
load(strcat(path,'B_UBA_',reg,'_',fue,'_12.mat'));
p24(:,8)=precip24(:,6);
p48(:,8)=precip48(:,6);
clear precip24 precip48
load(strcat(path,'B_UBA_',reg,'_',fue,'_00.mat'));
p24(:,9)=precip24(:,6);
p48(:,9)=precip48(:,6);
clear precip24 precip48
load(strcat(path,'HRMxx_',reg,'_',fue,'_12.mat'));
p24(:,10)=precip24(:,6);
p48(:,10)=precip48(:,6);
clear precip24 precip48
load(strcat(path,'HRMxx_',reg,'_',fue,'_00.mat'));
p24(:,11)=precip24(:,6);
p48(:,11)=precip48(:,6);
clear precip24 precip48
load(strcat(path,'RPSAS_',reg,'_',fue,'_12.mat'));
p24(:,12)=precip24(:,6);
p48(:,12)=precip48(:,6);
clear precip24 precip48
load(strcat(path,'RPSAS_',reg,'_',fue,'_00.mat'));
p24(:,13)=precip24(:,6);
p48(:,13)=precip48(:,6);
clear precip24 precip48
load(strcat(path,'CATTx_',reg,'_',fue,'_00.mat'));
p24(:,14)=precip24(:,6);
p48(:,14)=precip48(:,6);
clear precip24 precip48
load(strcat(path,'WRFAR_',reg,'_',fue,'_12.mat'));
p24(:,15)=precip24(:,6);
p48(:,15)=precip48(:,6);
clear precip24 precip48
load(strcat(path,'WRFAR_',reg,'_',fue,'_00.mat'));
p24(:,16)=precip24(:,6);
p48(:,16)=precip48(:,6);
clear precip24 precip48
load(strcat(path,'ETAUM_',reg,'_',fue,'_00.mat'));
p24(:,17)=precip24(:,6);
p48(:,17)=precip48(:,6);
clear precip24 precip48
load(strcat(path,'MRFxx_',reg,'_',fue,'_00.mat')); %a este modelo y al siguiente hay que multiplicarle por 2 la precipitacion.
p24(:,18)=precip24(:,6)*2;
p48(:,18)=precip48(:,6)*2;
clear precip24 precip48
load(strcat(path,'SFENM_',reg,'_',fue,'_00.mat'));
p24(:,19)=precip24(:,6)*2;
p48(:,19)=precip48(:,6)*2;
clear precip24 precip48
load(strcat(path,'SFENM_',reg,'_',fue,'_12.mat'));
p24(:,20)=precip24(:,6)*2;
p48(:,20)=precip48(:,6)*2;
clear precip24 precip48
end

%Anulamos los pronosticos del WRF de octubre porque hay errores.
i_wrf=find(est_data(:,1)>= 20061001 & est_data(:,1)< 20061101);
p24(i_wrf,15)=NaN;
p24(i_wrf,16)=NaN;
p48(i_wrf,15)=NaN;
p48(i_wrf,16)=NaN;

%Calculo del ETS para todo el periodo (sirve para detectar errores).
%(Calculo sobre los puntos en que coinciden todos los modelos 700 aprox)

%Calculo la media del ensemble

media24=mean(p24(:,2:16),2);
media48=mean(p48(:,2:16),2);

umbral=[ 0.01 0.10 0.25 0.5 1 1.5 2]*25.4;
[ets_24 hit_rate far]=ets_fun(p24(:,1),p24(:,2:20),umbral)
[ets_48 hit_rate far]=ets_fun(p48(:,1),p48(:,[2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 20]),umbral)

[ets_media_24 hit_rate_media far_media]=ets_fun(p24(:,1),media24,umbral);
%[ets_media_48 hit_rate_media far_media]=ets_fun(p48(:,1),media48,umbral);

figure
plot(umbral,ets_24)
hold on
plot(umbral,ets_media_24,'bo-')
legend('ETA4012','ETA4000','ETA2012','ETA2000','T21312','T21300','BUBA12','BUBA00','HRM12','HRM00','RPSAS12','RPSAS00','CATT','WRFAR12','WRFAR00','ETAUMD','MRF','CPTECEM12','CPTECCEM00','MEDIA')
axis([0 50 0 0.5]);
xlabel('Umbral (mm)');
ylabel('ETS');
title('ETS 24 horas');

[bias_area24]=biasarea_fun(p24(:,1),p24(:,2:16),umbral)
[bias_area_media_24]=biasarea_fun(p24(:,1),media24,umbral)

figure
plot(umbral,bias_area24)
hold on
plot(umbral,bias_area_media_24,'bo-')
legend('ETA4012','ETA4000','ETA2012','ETA2000','T21312','T21300','BUBA12','BUBA00','HRM12','HRM00','RPSAS12','RPSAS00','CATT','WRFAR12','WRFAR00','ETAUMD','MRF','CPTECEM12','CPTECCEM00','MEDIA')
axis([0 50 0 2]);
xlabel('Umbral (mm)');
ylabel('BIAS AREAL');
title('BIAS AREAL 24 horas');

%Calculamos el n�mero de datos faltantes para cada modelo.

aux24=sum(isnan(p24));
aux48=sum(isnan(p48));

save(strcat(path,'superensemble_',reg,'_',fue,'.mat'),'p24','p48','est_data');

%Vamos a ver como es (el mundo del reves...) no!, el rank histogram del
%superensemble.


%Primero me quedo unicamente con los dias donde tengo disponibles todos los
%miembros del ensemble para ahorrar complicaciones.

[filas24 columnas24]=size(p24);
[filas48 columnas48]=size(p48);

i_nonan24=find(any(isnan(p24),2)==0);
%i_nonan48=find(any(isnan(p48),2)==0);

temp=p24(i_nonan24,:);
clear p24
p24=temp;
clear temp
%temp=p48(i_nonan48,:);
%clear p48
%p48=temp;
%clear temp




[rank_hist_24 low_std_24 hi_std_24] = pre_cal_fun(p24(:,1),p24(:,2:columnas24),1);
[rank_hist_48 low_std_48 hi_std_48] = pre_cal_fun(p48(:,1),p48(:,[2 3 4 5 6 7 8 9 10 11 12 13 15 16 17 18 19 20]),1);

%Grafico...
%**************************************************************************
figure
axis_limits=[ 0 columnas24+1 0 0.6]
subplot(241)
bar(rank_hist_24(1,:))
title('Rank 24 Baja dispersion');
axis(axis_limits);

subplot(242)
bar(rank_hist_24(2,:))
title('Rank 24 Media dispersion');
axis(axis_limits);

subplot(243)
bar(rank_hist_24(3,:))
title('Rank 24 Alta dispersion');
axis(axis_limits);

subplot(244)
bar(rank_hist_24(4,:))
title('Rank 24 Total');
axis(axis_limits);

subplot(245)
bar(rank_hist_48(1,:))
title('Rank 48 Baja dispersion');
axis(axis_limits);

subplot(246)
bar(rank_hist_48(2,:))
title('Rank 48 Media dispersion');
axis(axis_limits);

subplot(247)
bar(rank_hist_48(3,:))
title('Rank 48 Alta dispersion');
axis(axis_limits);

subplot(248)
bar(rank_hist_48(4,:))
title('Rank 48 Total');
axis(axis_limits);






