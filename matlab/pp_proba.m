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
cant_dias=15;   %Es la cant de dias q quiero tomar para analizar
% Genero un vector con el pronostico a 24hs
defa_wrf=pp_acum_wrf(:,:,1,med_wrf(4)-cant_dias:med_wrf(4)); % me guarda en una nueva variables los dias q quiero analizar
vector_wrf1=reshape(defa_wrf,nx*ny*size(defa_wrf,4),1);    %Me transforma las matrices de esos dias en un vector

%Genero un vector con los datos del cmorph
cant_dias=15;    %Es la cant de dias q quiero tomar para analizar
defa_cmorph=pp_acum_cmorph(:,:,med_cmorph(3)-cant_dias:med_cmorph(3)); % me guarda en una nueva variables los dias q quiero analizar
vector_cmorph=reshape(defa_cmorph,nx*ny*size(defa_cmorph,3),1);    %Me transforma las matrices de esos dias en un vector
%Genero los umbrales en mm
umbrales=[0.01 0.1 0.25 0.5 1 1.5 2] *25.4;
% Calculo la curva de referencia para el pronostico a 24 hs
[p1 n rango_lluvia] = pre_cal_fun(vector_cmorph,vector_wrf1,umbrales,0);
clear defa_wrf

% Genero un vector con el pronostico a 48hs
defa_wrf=pp_acum_wrf(:,:,3,med_wrf(4)-cant_dias:med_wrf(4)); % me guarda en una nueva variables los dias q quiero analizar
vector_wrf3=reshape(defa_wrf,nx*ny*size(defa_wrf,4),1);    %Me transforma las matrices de esos dias en un vector
% Calculo la curva de referencia para el pronostico a 48 hs
[p3 n rango_lluvia] = pre_cal_fun(vector_cmorph,vector_wrf3,umbrales,0);
clear defa_wrf

% Genero un vector con el pronostico a 72hs
defa_wrf=pp_acum_wrf(:,:,5,med_wrf(4)-cant_dias:med_wrf(4)); % me guarda en una nueva variables los dias q quiero analizar
vector_wrf5=reshape(defa_wrf,nx*ny*size(defa_wrf,4),1);    %Me transforma las matrices de esos dias en un vector
% Calculo la curva de referencia para el pronostico a 72 hs
[p5 n rango_lluvia] = pre_cal_fun(vector_cmorph,vector_wrf5,umbrales,0);
clear defa_wrf

%   Ahora calculo la probabilidad para el pronostico de hoy en base a la
%   curva de referencia

% Genero un vector con el pronostico a 24hs
defa_wrf=pp_acum_wrf(:,:,1,med_wrf(4)); % me guarda en una nueva variables los dias q quiero analizar
vector_wrf_dia1=reshape(defa_wrf,nx*ny*size(defa_wrf,4),1);    %Me transforma las matrices de esos dias en un vector

[probabilidad_1] = prob_cal_fun(vector_wrf_dia1,p1,umbrales,rango_lluvia);
clear defa_wrf
% Convierto el vector de probabilidad otra vez en la matriz original
for i=1:7
    matriz_wrf_dia1(:,:,i)=reshape(probabilidad_1(:,i),ny,nx);  %Me transforma el vector de probabilidad en una matriz nuevamente
end

% Genero un vector con el pronostico a 48hs
defa_wrf=pp_acum_wrf(:,:,3,med_wrf(4)); % me guarda en una nueva variables los dias q quiero analizar
vector_wrf_dia3=reshape(defa_wrf,nx*ny*size(defa_wrf,4),1);    %Me transforma las matrices de esos dias en un vector

[probabilidad_3] = prob_cal_fun(vector_wrf_dia3,p3,umbrales,rango_lluvia);
clear defa_wrf
% Convierto el vector de probabilidad otra vez en la matriz original
for i=1:7
    matriz_wrf_dia3(:,:,i)=reshape(probabilidad_3(:,i),ny,nx);  %Me transforma el vector de probabilidad en una matriz nuevamente
end

% Genero un vector con el pronostico a 72hs
defa_wrf=pp_acum_wrf(:,:,5,med_wrf(4)); % me guarda en una nueva variables los dias q quiero analizar
vector_wrf_dia5=reshape(defa_wrf,nx*ny*size(defa_wrf,4),1);    %Me transforma las matrices de esos dias en un vector

[probabilidad_5] = prob_cal_fun(vector_wrf_dia5,p5,umbrales,rango_lluvia);
clear defa_wrf
% Convierto el vector de probabilidad otra vez en la matriz original
for i=1:7
    matriz_wrf_dia5(:,:,i)=reshape(probabilidad_5(:,i),ny,nx);  %Me transforma el vector de probabilidad en una matriz nuevamente
end

%*********************************Graficos********************************
v=[0.05 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1];
vcol=[2 31 41 43 45 47 55 57 59 27 ];
% Graficos para el pronostico a 24 horas
subplot (2,2,1)
latvar=lat;
lonvar=lon;
load coast
pcolor(lonvar,latvar,matriz_wrf_dia1(:,:,1))
shading interp
hold on
plot(long,lat,'k')
title('Probabilidad del pronostico a 24 hs para 0.25 mm','FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol);
subplot (2,2,2)
pcolor(lonvar,latvar,matriz_wrf_dia1(:,:,2))
shading interp
hold on
plot(long,lat,'k')
title('Probabilidad del pronostico a 24 hs para 2.5 mm','FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol);
subplot (2,2,3)
pcolor(lonvar,latvar,matriz_wrf_dia1(:,:,5))
shading interp
hold on
plot(long,lat,'k')
title('Probabilidad del pronostico a 24 hs para 25.4 mm','FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol);
subplot (2,2,4)
pcolor(lonvar,latvar,matriz_wrf_dia1(:,:,7))
shading interp
hold on
plot(long,lat,'k')
title('Probabilidad del pronostico a 24 hs para 50.8 mm','FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol);
archivo=strcat('pp_','proba24');
print('-depsc',archivo)
close 1
% Graficos para el pronostico a 48 horas
figure
subplot (2,2,1)
load coast
pcolor(lonvar,latvar,matriz_wrf_dia3(:,:,1))
shading interp
hold on
plot(long,lat,'k')
title('Probabilidad del pronostico a 48 hs para 0.25 mm','FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol);
subplot (2,2,2)
pcolor(lonvar,latvar,matriz_wrf_dia3(:,:,2))
shading interp
hold on
plot(long,lat,'k')
title('Probabilidad del pronostico a 48 hs para 2.5 mm','FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol);
subplot (2,2,3)
pcolor(lonvar,latvar,matriz_wrf_dia3(:,:,5))
shading interp
hold on
plot(long,lat,'k')
title('Probabilidad del pronostico a 48 hs para 25.4 mm','FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol);
subplot (2,2,4)
pcolor(lonvar,latvar,matriz_wrf_dia3(:,:,7))
shading interp
hold on
plot(long,lat,'k')
title('Probabilidad del pronostico a 48 hs para 50.8 mm','FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol);
archivo=strcat('pp_','proba48');
print('-depsc',archivo)
close 1
% Graficos para el pronostico a 72 horas
figure
subplot (2,2,1)
load coast
pcolor(lonvar,latvar,matriz_wrf_dia5(:,:,1))
shading interp
hold on
plot(long,lat,'k')
title('Probabilidad del pronostico a 72 hs para 0.25 mm','FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol);
subplot (2,2,2)
pcolor(lonvar,latvar,matriz_wrf_dia5(:,:,2))
shading interp
hold on
plot(long,lat,'k')
title('Probabilidad del pronostico a 72 hs para 2.5 mm','FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol);
subplot (2,2,3)
pcolor(lonvar,latvar,matriz_wrf_dia5(:,:,5))
shading interp
hold on
plot(long,lat,'k')
title('Probabilidad del pronostico a 72 hs para 25.4 mm','FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol);
subplot (2,2,4)
pcolor(lonvar,latvar,matriz_wrf_dia5(:,:,7))
shading interp
hold on
plot(long,lat,'k')
title('Probabilidad del pronostico a 72 hs para 50.8 mm','FontSize',13)
xlabel('Longitud','FontSize',12)
ylabel('Latitud','FontSize',12)
[colores] = plot_jrcol(v,vcol);
archivo=strcat('pp_','proba72');
print('-depsc',archivo)
close 1
