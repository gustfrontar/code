% Calcula la calibracion con 15 dias para el super ensamble haciendo 
% un histograma de probabilidades. Para esto usa las funciones rank_fun 
% y la funcion pre_cal_fun_smes. Luego grafica estos histogramas para cada
% tipo de dispersion.

clear all
close all

% Abro los archivos con los datos 
load pp_superensemble.mat

% Genero la fecha de 15 dias atras
S=load('-ascii', 'fech.txt');
fech=num2str(S);
b=datenum(fech,'yyyymmdd');
c=b-15;
d=datevec(c);
g=datevec(b);
tiempo_15=datestr(d, 'yyyymmdd'); % Fecha de 15 atras
tiempo=datestr(g, 'yyyymmdd'); % Fecha de hoy
t=str2num(tiempo_15); 

%***********************Para el pronostico a 24 hs*************************
% Divido en dos regiones una norte y otra sur, dividiendo en 20 de latitud
%************************Region Norte**************************
lat_min_n=-20; 
lat_max_n=15;
reg_n=find(pp_ensemble_24(:,1)>t & pp_ensemble_24(:,3)<lat_max_n & pp_ensemble_24(:,3)>lat_min_n); % Me qdo solo 
%con los ultimos 30 dias y con la region norte

[rank_hist low_std hi_std] = pre_cal_fun_smes(pp_ensemble_24(reg_n,5),pp_ensemble_24(reg_n,8:26),0)
%************************Region Sur**************************
lat_min_s=-81;
lat_max_s=-20;
reg_s=find(pp_ensemble_24(:,1)>t & pp_ensemble_24(:,3)<lat_max_s & pp_ensemble_24(:,3)>lat_min_s); % Me qdo solo 
%con los ultimos 30 dias y con la region sur

[rank_hist low_std hi_std] = pre_cal_fun_smes(pp_ensemble_24(reg_s,5),pp_ensemble_24(reg_s,8:26),0)


%***********************Para el pronostico a 48 hs*************************
% Divido en dos regiones una norte y otra sur, dividiendo en 20 de latitud
%************************Region Norte**************************
[rank_hist low_std hi_std] = pre_cal_fun_smes(pp_ensemble_48(reg_n,5),pp_ensemble_48(reg_n,8:26),0)

%************************Region Sur**************************
[rank_hist low_std hi_std] = pre_cal_fun_smes(pp_ensemble_48(reg_s,5),pp_ensemble_48(reg_s,8:26),0)





%**********************Grafico los histogramas*****************************
% Dispersion baja
figure
subplot (2,2,1)
bar(rank_hist(1,:))
title('Dispersion baja','FontSize',13)
xlabel('Intervalos de Probabilidad','FontSize',12)
ylabel('Probabilidad','FontSize',12)
%set(gca,'YTick',[0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9])
% Dispersion media
subplot (2,2,2)
bar(rank_hist(2,:))
title('Dispersion media','FontSize',13)
xlabel('Intervalos de Probabilidad','FontSize',12)
ylabel('Probabilidad','FontSize',12)
% Dispersion alta
subplot (2,2,3)
bar(rank_hist(3,:))
title('Dispersion alta','FontSize',13)
xlabel('Intervalos de Probabilidad','FontSize',12)
ylabel('Probabilidad','FontSize',12)
% Dispersion total
subplot (2,2,4)
bar(rank_hist(4,:))
title('Dispersion Total','FontSize',13)
xlabel('Intervalos de Probabilidad','FontSize',12)
ylabel('Probabilidad','FontSize',12)



