clear all
close all

%   Calcula la probabilidad de la pp usando los distintos pronosticos como
%   miembros de un esamble
%   Usa las funciones pre_cal_fun q genera la curva de referencia y usa la
%   funcion prob_cal_fun q me  genera los puntos de pronostico q quiero
%   comparar con la de referencia

load pp_wrf.mat
load pp_cmorph.mat
load lat_lon_wrf.mat 

nx=101;    %Cantidad de ptos en x
ny=110;    %Cantidad de ptos en y
med_wrf=size(pp_acum_wrf);
med_cmorph=size(pp_acum_cmorph);


% Genero la fecha de 30 dias atras, pero como los archivos estan 5 dias
% atrasados son en total 35 dias
b=datenum(date);
c=b-35;
d=datevec(c);
tiempo=datestr(d, 'yyyymmdd'); 
t=str2num(tiempo); 




% Divido en dos regiones una norte y otra sur, dividiendo en 20 de latitud
%************************Region Norte**************************

lat_min_n=-20; 
lat_max_n=-3.81697;
puntos_y_n=find(lat>=lat_min_n & lat <= lat_max_n); % Me divide la region q quiero
lat_n=lat(puntos_y_n);
ny_n=size(puntos_y_n,1);


cant_dias=15;   %Es la cant de dias q quiero tomar para analizar

% Genero un vector con el pronostico a 24hs
defa_wrf=pp_acum_wrf(puntos_y_n,:,1,med_wrf(4)-cant_dias:med_wrf(4)); % me guarda en una nueva variables los dias q quiero analizar
vector_wrf1_n=reshape(defa_wrf,nx*ny_n*size(defa_wrf,4),1);    %Me transforma las matrices de esos dias en un vector


%Genero un vector con los datos del cmorph
cant_dias=15;    %Es la cant de dias q quiero tomar para analizar
defa_cmorph_n=pp_acum_cmorph(puntos_y_n,:,med_cmorph(3)-cant_dias:med_cmorph(3)); % me guarda en una nueva variables los dias q quiero analizar
vector_cmorph_n=reshape(defa_cmorph_n,nx*ny_n*size(defa_cmorph_n,3),1);    %Me transforma las matrices de esos dias en un vector

%Genero los umbrales en mm
umbrales=[0.01 0.1 0.25 0.5 1 1.5 2] *25.4;



% Calculo la curva de referencia para el pronostico a 24 hs
[p1_n n rango_lluvia] = pre_cal_fun(vector_cmorph_n,vector_wrf1_n,umbrales,0);
clear defa_wrf



% Genero un vector con el pronostico a 48hs
defa_wrf=pp_acum_wrf(puntos_y_n,:,3,med_wrf(4)-cant_dias:med_wrf(4)); % me guarda en una nueva variables los dias q quiero analizar
vector_wrf3_n=reshape(defa_wrf,nx*ny_n*size(defa_wrf,4),1);    %Me transforma las matrices de esos dias en un vector

% Calculo la curva de referencia para el pronostico a 48 hs
[p3_n n rango_lluvia] = pre_cal_fun(vector_cmorph_n,vector_wrf3_n,umbrales,0);
clear defa_wrf



% Genero un vector con el pronostico a 72hs
defa_wrf=pp_acum_wrf(puntos_y_n,:,5,med_wrf(4)-cant_dias:med_wrf(4)); % me guarda en una nueva variables los dias q quiero analizar
vector_wrf5_n=reshape(defa_wrf,nx*ny_n*size(defa_wrf,4),1);    %Me transforma las matrices de esos dias en un vector

% Calculo la curva de referencia para el pronostico a 72 hs
[p5_n n rango_lluvia] = pre_cal_fun(vector_cmorph_n,vector_wrf5_n,umbrales,0);
clear defa_wrf



%   Ahora calculo la probabilidad para el pronostico de hoy en base a la
%   curva de referencia
%   Abro los archivo de los datos de hoy y me guardo la pp en una nueva
%   variable

S=load('-ascii', 'fecha.txt');

fech=num2str(S);
fech=fech(1:8);
b=datenum(fech,'yyyymmdd');
e=b+1;
f=b+2;
ff=b+3;
g=datevec(e);
h=datevec(f);
l=datevec(ff);
day_man=datestr(g, ' dd mmmm 12Z');
d_man=datestr(g, 'yyyymmdd12');
day_pman=datestr(h, ' dd mmmm 12Z');
d_pman=datestr(h, 'yyyymmdd12');
day_ppman=datestr(l,' dd mmmm 12Z');
d_ppman=datestr(l, 'yyyymmdd12');


[fecha prono_24 prono_36 prono_48 prono_60 prono_72]=read_fun(fech,37,'/WRFV2/wrfsi/domains/operativo/archivo/3d/');

load lat_lon_wrf.mat

nx=101; %Cantidad de ptos en x
ny=110; %Cantidad de ptos en y
puntos_y_n=find(lat>=lat_min_n & lat <= lat_max_n); % Me divide la region q quiero
lat_n=lat(puntos_y_n);
ny_n=size(puntos_y_n,1);


% Acumulo la precipitacion cada 24 horas
pp_24_n=prono_24(puntos_y_n,:,:);
pp_48_n=prono_48(puntos_y_n,:,:);
pp_72_n=prono_72(puntos_y_n,:,:);


% Genero un vector con el pronostico a 24hs
v_24_n=reshape(pp_24_n,nx*ny_n,1);    %Me transforma las matrices de ese dia en un vector
[proba_1_n] = prob_cal_fun(v_24_n,p1_n,umbrales,rango_lluvia);

% Convierto el vector de probabilidad otra vez en la matriz original
for i=1:7
    proba24_n(:,:,i)=reshape(proba_1_n(:,i),ny_n,nx);  %Me transforma el vector de probabilidad en una matriz nuevamente
end


% Genero un vector con el pronostico a 48hs
v_48_n=reshape(pp_48_n,nx*ny_n,1);    %Me transforma las matrices de ese dia en un vector
[proba_3_n] = prob_cal_fun(v_48_n,p3_n,umbrales,rango_lluvia);

% Convierto el vector de probabilidad otra vez en la matriz original
for i=1:7
    proba48_n(:,:,i)=reshape(proba_3_n(:,i),ny_n,nx);  %Me transforma el vector de probabilidad en una matriz nuevamente
end


% Genero un vector con el pronostico a 72hs
v_72_n=reshape(pp_72_n,nx*ny_n,1);    %Me transforma las matrices de ese dia en un vector
[proba_5_n] = prob_cal_fun(v_72_n,p5_n,umbrales,rango_lluvia);

% Convierto el vector de probabilidad otra vez en la matriz original
for i=1:7
    proba72_n(:,:,i)=reshape(proba_5_n(:,i),ny_n,nx);  %Me transforma el vector de probabilidad en una matriz nuevamente
end




%************************Region Sur**************************

lat_min_s=-61.46607;
lat_max_s=-20;
puntos_y_s=find(lat>=lat_min_s & lat <= lat_max_s); % Me divide la region q quiero
ny_s=size(puntos_y_s,1);
lat_s=lat(puntos_y_s);


cant_dias=15;   %Es la cant de dias q quiero tomar para analizar

% Genero un vector con el pronostico a 24hs
defa_wrf=pp_acum_wrf(puntos_y_s,:,1,med_wrf(4)-cant_dias:med_wrf(4)); % me guarda en una nueva variables los dias q quiero analizar
vector_wrf1_s=reshape(defa_wrf,nx*ny_s*size(defa_wrf,4),1);    %Me transforma las matrices de esos dias en un vector


%Genero un vector con los datos del cmorph
cant_dias=15;    %Es la cant de dias q quiero tomar para analizar
defa_cmorph=pp_acum_cmorph(puntos_y_s,:,med_cmorph(3)-cant_dias:med_cmorph(3)); % me guarda en una nueva variables los dias q quiero analizar
vector_cmorph_s=reshape(defa_cmorph,nx*ny_s*size(defa_cmorph,3),1);    %Me transforma las matrices de esos dias en un vector

%Genero los umbrales en mm
umbrales=[0.01 0.1 0.25 0.5 1 1.5 2] *25.4;



% Calculo la curva de referencia para el pronostico a 24 hs
[p1_s n rango_lluvia] = pre_cal_fun(vector_cmorph_s,vector_wrf1_s,umbrales,0);
clear defa_wrf



% Genero un vector con el pronostico a 48hs
defa_wrf=pp_acum_wrf(puntos_y_s,:,3,med_wrf(4)-cant_dias:med_wrf(4)); % me guarda en una nueva variables los dias q quiero analizar
vector_wrf3_s=reshape(defa_wrf,nx*ny_s*size(defa_wrf,4),1);    %Me transforma las matrices de esos dias en un vector

% Calculo la curva de referencia para el pronostico a 48 hs
[p3_s n rango_lluvia] = pre_cal_fun(vector_cmorph_s,vector_wrf3_s,umbrales,0);
clear defa_wrf



% Genero un vector con el pronostico a 72hs
defa_wrf=pp_acum_wrf(puntos_y_s,:,5,med_wrf(4)-cant_dias:med_wrf(4)); % me guarda en una nueva variables los dias q quiero analizar
vector_wrf5_s=reshape(defa_wrf,nx*ny_s*size(defa_wrf,4),1);    %Me transforma las matrices de esos dias en un vector

% Calculo la curva de referencia para el pronostico a 72 hs
[p5_s n rango_lluvia] = pre_cal_fun(vector_cmorph_s,vector_wrf5_s,umbrales,0);
clear defa_wrf




%   Ahora calculo la probabilidad para el pronostico de hoy en base a la
%   curva de referencia
%   Abro los archivo de los datos de hoy y me guardo la pp en una nueva
%   variable

puntos_y_s=find(lat>=lat_min_s & lat <= lat_max_s); % Me divide la region q quiero
ny_s=size(puntos_y_s,1);
lat_s=lat(puntos_y_s);


% Acumulo la precipitacion cada 24 horas
pp_24_s=prono_24(puntos_y_s,:,:);
pp_48_s=prono_48(puntos_y_s,:,:);
pp_72_s=prono_72(puntos_y_s,:,:);



% Genero un vector con el pronostico a 24hs
v_24_s=reshape(pp_24_s,nx*ny_s,1);    %Me transforma las matrices de ese dia en un vector
[proba_1_s] = prob_cal_fun(v_24_s,p1_s,umbrales,rango_lluvia);

% Convierto el vector de probabilidad otra vez en la matriz original
for i=1:7
    proba24_s(:,:,i)=reshape(proba_1_s(:,i),ny_s,nx);  %Me transforma el vector de probabilidad en una matriz nuevamente
end



% Genero un vector con el pronostico a 48hs
v_48_s=reshape(pp_48_s,nx*ny_s,1);    %Me transforma las matrices de ese dia en un vector
[proba_3_s] = prob_cal_fun(v_48_s,p3_s,umbrales,rango_lluvia);

% Convierto el vector de probabilidad otra vez en la matriz original
for i=1:7
    proba48_s(:,:,i)=reshape(proba_3_s(:,i),ny_s,nx);  %Me transforma el vector de probabilidad en una matriz nuevamente
end



% Genero un vector con el pronostico a 72hs
v_72_s=reshape(pp_72_s,nx*ny_s,1);    %Me transforma las matrices de ese dia en un vector
[proba_5_s] = prob_cal_fun(v_72_s,p5_s,umbrales,rango_lluvia);

% Convierto el vector de probabilidad otra vez en la matriz original
for i=1:7
    proba72_s(:,:,i)=reshape(proba_5_s(:,i),ny_s,nx);  %Me transforma el vector de probabilidad en una matriz nuevamente
end


%*****************************************************************
% Junto las dos matrices para graficar pero siguen estando calibradas para
% dos regiones
%*****************************************************************

merge_24(1:ny_s,:,:)=proba24_s;
merge_24(ny_s+1:ny_s+ny_n,:,:)=proba24_n;
merge_48(1:ny_s,:,:)=proba48_s;
merge_48(ny_s+1:ny_s+ny_n,:,:)=proba48_n;
merge_72(1:ny_s,:,:)=proba72_s;
merge_72(ny_s+1:ny_s+ny_n,:,:)=proba72_n;


[nx ny nz]=size(merge_24);
path_24=strcat('/WRFV2/wrfsi/domains/operativo/archivo/prob_wrf/','probwrf_',d_man,'_cmorph_24');
path_48=strcat('/WRFV2/wrfsi/domains/operativo/archivo/prob_wrf/','probwrf_',d_pman,'_cmorph_48');
path_72=strcat('/WRFV2/wrfsi/domains/operativo/archivo/prob_wrf/','probwrf_',d_ppman,'_cmorph_72');

[var]=write_file(path_24,nx,ny,nz,merge_24,1);
[var]=write_file(path_48,nx,ny,nz,merge_48,1);
[var]=write_file(path_72,nx,ny,nz,merge_72,1);





%**************************************************************************
% Me guardo la probabilidad

file=fopen('pp_proba_cmorph.mat');
if(file ~= -1)
%Va guardando todos los dias, el nuevo dato 
load pp_proba_cmorph.mat
calwrf_24=[calwrf_24;merge_24];
calwrf_48=[calwrf_48;merge_48];
calwrf_72=[calwrf_72;merge_72];
save pp_proba_cmorph.mat calwrf_24 calwrf_48 calwrf_72;
end


%Genera el archivo q va ir guardando los datos de la region WRF
%Donde el primer lugar corresponde a los umbrales, el segundo a
%los distintos pronosticos, el tercero a las variables y el cuarto a los
%tiempos
if(file==-1)
calwrf_24=merge_24;
calwrf_48=merge_48;
calwrf_72=merge_72;
save pp_proba_cmorph.mat calwrf_24 calwrf_48 calwrf_72;
end







%***********************************GRAFICOS********************************

carga_mapa

v=[5 10 20 30 40 50 60 70 80 90 100];
%vcol=[2 31 41 43 45 47 55 57 59 27 ];
vcol=[2 55 53 45 42 32 21 24 27 29];


latvar=lat;
lonvar=lon;
load coast


% Graficos para el pronostico a 24 horas
tas_24=isnan(pp_24_n(1,1))
if (tas_24~=1)
    figure
pcolor(lonvar,latvar,merge_24(:,:,1)*100)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
tit=strcat('Valido para el ', day_man);
title(tit,'FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol,0);
archivo=strcat('probacmorph1','_24');
print('-depsc',archivo)
close 1


figure
pcolor(lonvar,latvar,merge_24(:,:,2)*100)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
tit=strcat('Valido para el ', day_man);
title(tit,'FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol,0);
archivo=strcat('probacmorph2','_24');
print('-depsc',archivo)
close 1



figure
pcolor(lonvar,latvar,merge_24(:,:,5)*100)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
tit=strcat('Valido para el ', day_man);
title(tit,'FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol,0);
archivo=strcat('probacmorph5','_24');
print('-depsc',archivo)
close 1



figure
pcolor(lonvar,latvar,merge_24(:,:,7)*100)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
tit=strcat('Valido para el ', day_man);
title(tit,'FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol,0);
archivo=strcat('probacmorph7','_24');
print('-depsc',archivo)
close 1
% else
%     for k=1:7
%     nodisponible
%     numer=num2str(k);
%     archivo=strcat('proba',numer,'_24');
%     print('-depsc',archivo)
%     close 1
%     end
end




% Graficos para el pronostico a 48 horas
tas_48=isnan(pp_48_n(1,1));
if (tas_48~=1)
figure
pcolor(lonvar,latvar,merge_48(:,:,1)*100)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
tit=strcat('Valido para el ', day_pman);
title(tit,'FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol,0);
archivo=strcat('probacmorph1','_48');
print('-depsc',archivo)
close 1



figure
pcolor(lonvar,latvar,merge_48(:,:,2)*100)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
tit=strcat('Valido para el ', day_pman);
title(tit,'FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol,0);
archivo=strcat('probacmorph2','_48');
print('-depsc',archivo)
close 1



figure
pcolor(lonvar,latvar,merge_48(:,:,5)*100)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
tit=strcat('Valido para el ', day_pman);
title(tit,'FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol,0);
archivo=strcat('probacmorph5','_48');
print('-depsc',archivo)
close 1



figure
pcolor(lonvar,latvar,merge_48(:,:,7)*100)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
tit=strcat('Valido para el ', day_pman);
title(tit,'FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol,0);
archivo=strcat('probacmorph7','_48');
print('-depsc',archivo)
close 1
% else
%     for k=1:7
%     nodisponible
%     numer=num2str(k);
%     archivo=strcat('proba',numer,'_48');
%     print('-depsc',archivo)
%     close 1
%     end
end




% Graficos para el pronostico a 72 horas
tas_72=isnan(pp_72_n(1,1));
if (tas_72~=1)
figure
pcolor(lonvar,latvar,merge_72(:,:,1)*100)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
tit=strcat('Valido para el ', day_ppman);
title(tit,'FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol,0);
archivo=strcat('probacmorph1','_72');
print('-depsc',archivo)
close 1



figure
pcolor(lonvar,latvar,merge_72(:,:,2)*100)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
tit=strcat('Valido para el ', day_ppman);
title(tit,'FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol,0);
archivo=strcat('probacmorph2','_72');
print('-depsc',archivo)
close 1



figure
pcolor(lonvar,latvar,merge_72(:,:,5)*100)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
tit=strcat('Valido para el ', day_ppman);
title(tit,'FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol,0);
archivo=strcat('probacmorph5','_72');
print('-depsc',archivo)
close 1



figure
pcolor(lonvar,latvar,merge_72(:,:,7)*100)
shading flat
hold on
plot(lon_costa,lat_costa,'k')
plot(lon_pais,lat_pais,'k')
plot(lon_rios,lat_rios,'b')
plot(lon_lagos,lat_lagos,'b')
plot(lon_ciudad,lat_ciudad,'ro')
tit=strcat('Valido para el ', day_ppman);
title(tit,'FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol,0);
archivo=strcat('probacmorph7','_72');
print('-depsc',archivo)
close 1
% else
%     for k=1:7
%     nodisponible
%     numer=num2str(k);
%     archivo=strcat('proba',numer,'_72');
%     print('-depsc',archivo)
%     close 1
%     end
end



