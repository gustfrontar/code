% Script q calcula el ets y el bias para varios dias y verifica el
% pronostico con los datos del CMORPH y tambien con los datos de
% pluviometros
% Usa los archivos generados por el script lee_precipitacion y llama a las
% funciones ets_fun y biasarea_fun
% al final de script grafica el ets y el bias para la region sur y la
% region norte para los dos tipos de verificaciones (cmorph y pluviometros)

clear all
close all

%******************************Varificacion con CMORPH*******************
% Abro los archivos donde estan los datos de pp y los datos de latitud y
% longitud
load pp_cmorph.mat
load pp_wrf.mat
load lat_lon_wrf.mat 
nx=101;    %Cantidad de ptos en x
ny=110;    %Cantidad de ptos en y
med_wrf=size(pp_acum_wrf);
med_cmorph=size(pp_acum_cmorph);




% Divido en dos regiones una norte y otra sur, dividiendo en 20 de latitud
%************************Region Norte**************************
lat_min_n=-20; 
lat_max_n=-3.81697;
puntos_y_n=find(lat>=lat_min_n & lat <= lat_max_n); % Me divide la region q quiero
ny_n=size(puntos_y_n,1);


cant_dias=60;   %Es la cant de dias q quiero tomar para analizar
% Genero una matriz con los datos del pronostico del wrf
for i=1:5
    defa_wrf=pp_acum_wrf(puntos_y_n,:,i,med_wrf(4)-cant_dias:med_wrf(4)); % me guarda en una nueva variables los dias q quiero analizar
    vector_wrf=reshape(defa_wrf,nx*ny_n*size(defa_wrf,4),1);    %Me transforma las matrices de esos dias en un vector
    matriz_wrf(:,i)=vector_wrf;
end

%Genero un vector con los datos del cmorph
defa_cmorph=pp_acum_cmorph(puntos_y_n,:,med_cmorph(3)-cant_dias:med_cmorph(3)); % me guarda en una nueva variables los dias q quiero analizar
vector_cmorph=reshape(defa_cmorph,nx*ny_n*size(defa_cmorph,3),1);    %Me transforma las matrices de esos dias en un vector

%Genero los umbrales en mm
umbrales=[0.04 0.1 0.25 0.5 1 1.5 2] *25.4;

%LLamo a las funciones para hacer los calculos
[ets_n,hit_rate_n,far_n,total_n,c_umbral_n]=ets_fun(vector_cmorph,matriz_wrf,umbrales);
[bias_area_n] = biasarea_fun(vector_cmorph,matriz_wrf,umbrales);
clear defa_wrf defa_cmorph vector_wrf matriz_wrf vector_cmorph cant_dias

total_n= num2str(total_n);
umbral_1=num2str(c_umbral_n(1,1));
umbral_2=num2str(c_umbral_n(2,1));
umbral_3=num2str(c_umbral_n(3,1));
umbral_4=num2str(c_umbral_n(4,1));
umbral_5=num2str(c_umbral_n(5,1));
umbral_6=num2str(c_umbral_n(6,1));
umbral_7=num2str(c_umbral_n(7,1));

um_n=strcat(umbral_1 ,' - ' ,  umbral_2 ,' - ' ,  umbral_3 ,' - ',  umbral_4 ,' - ',  umbral_5 ,' - ' ,  umbral_6 ,' - ',  umbral_7)
clear umbral_1 umbral_2 umbral_3 umbral_4 umbral_5 umbral_6 umbral_7 c_umbral_n umbrales lat_min_n lat_max_n puntos_y_n ny_n i


%************************Region Sur**************************
lat_min_s=-61.46607;
lat_max_s=-20;
puntos_y_s=find(lat>=lat_min_s & lat <= lat_max_s); % Me divide la region q quiero
ny_s=size(puntos_y_s,1);

cant_dias=60;   %Es la cant de dias q quiero tomar para analizar
% Genero una matriz con los datos del pronostico del wrf
for i=1:5
    defa_wrf=pp_acum_wrf(puntos_y_s,:,i,med_wrf(4)-cant_dias:med_wrf(4)); % me guarda en una nueva variables los dias q quiero analizar
    vector_wrf=reshape(defa_wrf,nx*ny_s*size(defa_wrf,4),1);    %Me transforma las matrices de esos dias en un vector
    matriz_wrf(:,i)=vector_wrf;
end

%Genero un vector con los datos del cmorph
defa_cmorph=pp_acum_cmorph(puntos_y_s,:,med_cmorph(3)-cant_dias:med_cmorph(3)); % me guarda en una nueva variables los dias q quiero analizar
vector_cmorph=reshape(defa_cmorph,nx*ny_s*size(defa_cmorph,3),1);    %Me transforma las matrices de esos dias en un vector

%Genero los umbrales en mm
umbrales=[0.04 0.1 0.25 0.5 1 1.5 2] *25.4;

%LLamo a las funciones para hacer los calculos
[ets_s,hit_rate_s,far_s,total_s,c_umbral_s]=ets_fun(vector_cmorph,matriz_wrf,umbrales);
[bias_area_s] = biasarea_fun(vector_cmorph,matriz_wrf,umbrales);
clear defa_wrf defa_cmorph vector_wrf matriz_wrf vector_cmorph cant_dias med_wrf med_cmorph


total_s= num2str(total_s);
umbral_1=num2str(c_umbral_s(1,1));
umbral_2=num2str(c_umbral_s(2,1));
umbral_3=num2str(c_umbral_s(3,1));
umbral_4=num2str(c_umbral_s(4,1));
umbral_5=num2str(c_umbral_s(5,1));
umbral_6=num2str(c_umbral_s(6,1));
umbral_7=num2str(c_umbral_s(7,1));

um_s=strcat(umbral_1 ,' - ' ,  umbral_2 ,' - ' ,  umbral_3 ,' - ',  umbral_4 ,' - ',  umbral_5 ,' - ' ,  umbral_6 ,' - ',  umbral_7)
clear umbral_1 umbral_2 umbral_3 umbral_4 umbral_5 umbral_6 umbral_7 c_umbral_s umbrales lat_min_s lat_max_s puntos_y_s ny_s


%************************************************************************
%Guarda los datos nuevos si es q existe el archivo y sino crea el archivo
%para guardar los datos
file=fopen('pp_ets_bias.mat');
if(file ~= -1)
%Va guardando todos los dias, el nuevo dato 
load pp_ets_bias.mat
med=size(pp_datos_s);
tt=med(4)+1;
pp_datos_s(:,:,1,tt)=ets_s;
pp_datos_s(:,:,2,tt)=hit_rate_s;
pp_datos_s(:,:,3,tt)=far_s;
pp_datos_s(:,:,4,tt)=bias_area_s;
pp_datos_n(:,:,1,tt)=ets_n;
pp_datos_n(:,:,2,tt)=hit_rate_n;
pp_datos_n(:,:,3,tt)=far_n;
pp_datos_n(:,:,4,tt)=bias_area_n;
save pp_ets_bias.mat pp_est_s pp_est_n pp_datos_s pp_datos_n
end


%Genera el archivo q va ir guardando los datos de la region WRF
%Donde el primer lugar corresponde a los umbrales, el segundo a
%los distintos pronosticos, el tercero a las variables y el cuarto a los
%tiempos
if(file==-1)
pp_datos_s(:,:,1,1)=ets_s;
pp_datos_s(:,:,2,1)=hit_rate_s;
pp_datos_s(:,:,3,1)=far_s;
pp_datos_s(:,:,4,1)=bias_area_s;
pp_datos_n(:,:,1,1)=ets_n;
pp_datos_n(:,:,2,1)=hit_rate_n;
pp_datos_n(:,:,3,1)=far_n;
pp_datos_n(:,:,4,1)=bias_area_n;

pp_datos_s(:,:,1,2)=ets_s;
pp_datos_s(:,:,2,2)=hit_rate_s;
pp_datos_s(:,:,3,2)=far_s;
pp_datos_s(:,:,4,2)=bias_area_s;
pp_datos_n(:,:,1,2)=ets_n;
pp_datos_n(:,:,2,2)=hit_rate_n;
pp_datos_n(:,:,3,2)=far_n;
pp_datos_n(:,:,4,2)=bias_area_n;
save pp_ets_bias.mat pp_datos_s pp_datos_n 
end

% Me qda entonces una matriz de cuatro columnas, la primera son lo
% umbrales la segunda son los distintos pronosticos (24,36,48,60 y 72) la
% tercera son las distintas variables calculadas (ets,hit_rate,far,bias) y
% la cuarta son los dias 





%***********************Verificacion con Pluviometros**********************
% Abro el archivo donde tengo los datos
load pp_est.mat
tam_est=size(pp_acum_est);

% Genero la fecha de 60 dias atras, pero como los archivos estan 5 dias
% atrasados son en total 65 dias
b=datenum(date);
c=b-60;
d=datevec(c);
tiempo=datestr(d, 'yyyymmdd'); 
t=str2num(tiempo); 
clear b c d 

val_t=find(pp_acum_est(:,1)>t); % Me qdo solo con los ultimos 60 dias

% Defino la region Norte
lat_min_n=-20; 
lat_max_n=-3.81697;
reg_n=find(pp_acum_est(val_t,2)<lat_max_n & pp_acum_est(val_t,2)>lat_min_n);

%Genero los umbrales en mm
umbrales=[0.04 0.1 0.25 0.5 1 1.5 2] *25.4;

%LLamo a las funciones para hacer los calculos
[ets_est_n,hit_rate_est_n,far_est_n,total_est_n,c_umbral_est_n]=ets_fun(pp_acum_est(reg_n,4),pp_acum_est(reg_n,5:9),umbrales);
[bias_area_est_n] = biasarea_fun(pp_acum_est(reg_n,4),pp_acum_est(reg_n,5:9),umbrales);

total_est_n= num2str(total_est_n);
umbral_1=num2str(c_umbral_est_n(1,1));
umbral_2=num2str(c_umbral_est_n(2,1));
umbral_3=num2str(c_umbral_est_n(3,1));
umbral_4=num2str(c_umbral_est_n(4,1));
umbral_5=num2str(c_umbral_est_n(5,1));
umbral_6=num2str(c_umbral_est_n(6,1));
umbral_7=num2str(c_umbral_est_n(7,1));

um_est_n=strcat(umbral_1 ,' - ' ,  umbral_2 ,' - ' ,  umbral_3 ,' - ',  umbral_4 ,' - ',  umbral_5 ,' - ' ,  umbral_6 ,' - ',  umbral_7)
clear umbral_1 umbral_2 umbral_3 umbral_4 umbral_5 umbral_6 umbral_7 c_umbral_est_n umbrales lat_min_n lat_max_n reg_n 




%**************************************************************************

% Defino la region Sur
lat_min_s=-61.46607;
lat_max_s=-20;
reg_s=find(pp_acum_est(val_t,2)<lat_max_s & pp_acum_est(val_t,2)>lat_min_s);

%Genero los umbrales en mm
umbrales=[0.01 0.1 0.25 0.5 1 1.5 2] *25.4;

%LLamo a las funciones para hacer los calculos
[ets_est_s,hit_rate_est_s,far_est_s,total_est_s,c_umbral_est_s]=ets_fun(pp_acum_est(reg_s,4),pp_acum_est(reg_s,5:9),umbrales);
[bias_area_est_s] = biasarea_fun(pp_acum_est(reg_s,4),pp_acum_est(reg_s,5:9),umbrales);

total_est_s= num2str(total_est_s);
umbral_1=num2str(c_umbral_est_s(1,1));
umbral_2=num2str(c_umbral_est_s(2,1));
umbral_3=num2str(c_umbral_est_s(3,1));
umbral_4=num2str(c_umbral_est_s(4,1));
umbral_5=num2str(c_umbral_est_s(5,1));
umbral_6=num2str(c_umbral_est_s(6,1));
umbral_7=num2str(c_umbral_est_s(7,1));

um_est_s=strcat(umbral_1 ,' - ' ,  umbral_2 ,' - ' ,  umbral_3 ,' - ',  umbral_4 ,' - ',  umbral_5 ,' - ' ,  umbral_6 ,' - ',  umbral_7)
clear umbral_1 umbral_2 umbral_3 umbral_4 umbral_5 umbral_6 umbral_7 c_umbral_est_s lat_min_s lat_max_s reg_s val_t 




%Va guardando todos los dias, el nuevo dato 
load pp_ets_bias.mat
med=size(pp_est_s);
tt=med(4)+1;
pp_est_s(:,:,1,tt)=ets_est_s;
pp_est_s(:,:,2,tt)=hit_rate_est_s;
pp_est_s(:,:,3,tt)=far_est_s;
pp_est_s(:,:,4,tt)=bias_area_est_s;
pp_est_n(:,:,1,tt)=ets_est_n;
pp_est_n(:,:,2,tt)=hit_rate_est_n;
pp_est_n(:,:,3,tt)=far_est_n;
pp_est_n(:,:,4,tt)=bias_area_est_n;
save pp_ets_bias.mat pp_est_s pp_est_n pp_datos_s pp_datos_n

%Genera el archivo q va ir guardando los datos de la region WRF
%Donde el primer lugar corresponde a los umbrales, el segundo a
%los distintos pronosticos, el tercero a las variables y el cuarto a los
%tiempos
%pp_est_s(:,:,1,1)=ets_s;
%pp_est_s(:,:,2,1)=hit_rate_est_s;
%pp_est_s(:,:,3,1)=far_est_s;
%pp_est_s(:,:,4,1)=bias_area_est_s;
%pp_est_n(:,:,1,1)=ets_est_n;
%pp_est_n(:,:,2,1)=hit_rate_est_n;
%pp_est_n(:,:,3,1)=far_est_n;
%pp_est_n(:,:,4,1)=bias_area_est_n;

%pp_est_s(:,:,1,2)=ets_est_s;
%pp_est_s(:,:,2,2)=hit_rate_est_s;
%pp_est_s(:,:,3,2)=far_est_s;
%pp_est_s(:,:,4,2)=bias_area_est_s;
%pp_est_n(:,:,1,2)=ets_est_n;
%pp_est_n(:,:,2,2)=hit_rate_est_n;
%pp_est_n(:,:,3,2)=far_est_n;
%pp_est_n(:,:,4,2)=bias_area_est_n;
%save pp_ets_bias.mat pp_est_s pp_est_n pp_datos_s pp_datos_n


clear fecha_archivo pp_acum_wrf pp_acum_cmorph pp_acum_est nx ny lat lon tam_est
load pp_wrf.mat
clear pp_acum_wrf 



%***********Genero grafico de lineas para la verificacion CMORPH************
%Genera la fecha q va a aparecer en eje x del grafico
p=fecha_archivo';
a=size(p);
aa=a(1);
e=a(1)-60;
f=p(aa,:);
ii=p(e,:);
jj=[f 0 0 0];
n=[ii 0 0 0];
tempquince=datestr(n, ' dd/mm'); %genera la fecha de hoy menos 60 dias
tempuno=datestr(jj, ' dd/mm'); %genera la fecha de hoy menos 1 dia
clear p a aa e f ii jj n  



% abro el archivo donde estan guardados los datos de ets y bias
load pp_ets_bias.mat


%****************************Region Sur**************************
% Graficos de ETS
med_s=size(pp_datos_s,4);
figure
hold on
plot(umbrales,pp_datos_s(:,1,1,med_s),'-bs','DisplayName','Pronostico a 24hs','LineWidth',1)
plot(umbrales,pp_datos_s(:,3,1,med_s),'-.r*','DisplayName','Pronostico a 48hs','LineWidth',2)
plot(umbrales,pp_datos_s(:,5,1,med_s),'--mo','DisplayName','Pronostico a 72hs','LineWidth',2)

absisas=strcat('Desde ', tempquince, ' al ', tempuno);
absisas1=strcat(' Cantidad de datos  ', total_s);
title({absisas,absisas1},'FontSize',13);
xlabel({'Umbrales en mm';'Cantidad de datos por umbral';um_s},'FontSize',12)
ylabel('ETS','FontSize',12)
xlim([0 51]) 
set(gca,'YGrid','on','XGrid','on')
legend('Pronostico a 24hs','Pronostico a 48hs','Pronostico a 72hs',1)
archivo=strcat('ets','_s');
print('-depsc',archivo)
close 1


% Graficos de Bias
figure
hold on
plot(umbrales,pp_datos_s(:,1,4,med_s),'-bs','DisplayName','Pronostico a 24hs','LineWidth',1)
plot(umbrales,pp_datos_s(:,3,4,med_s),'-.r*','DisplayName','Pronostico a 48hs','LineWidth',2)
plot(umbrales,pp_datos_s(:,5,4,med_s),'--mo','DisplayName','Pronostico a 72hs','LineWidth',2)

absisas=strcat('Desde ', tempquince, ' al ', tempuno);
title(absisas,'FontSize',13)
xlabel('Umbrales en mm','FontSize',12)
ylabel('Bias areal','FontSize',12)
xlim([0 51]) 
set(gca,'YGrid','on','XGrid','on')
legend('Pronostico a 24hs','Pronostico a 48hs','Pronostico a 72hs',1)
archivo=strcat('bias','_s');
print('-depsc',archivo)
close 1



%****************************Region Norte**************************
% Graficos de ETS
med_n=size(pp_datos_n,4);
figure
hold on
plot(umbrales,pp_datos_n(:,1,1,med_n),'-bs','DisplayName','Pronostico a 24hs','LineWidth',1)
plot(umbrales,pp_datos_n(:,3,1,med_n),'-.r*','DisplayName','Pronostico a 48hs','LineWidth',2)
plot(umbrales,pp_datos_n(:,5,1,med_n),'--mo','DisplayName','Pronostico a 72hs','LineWidth',2)

absisas=strcat('Desde ', tempquince, ' al ', tempuno);
absisas1=strcat(' Cantidad de datos  ', total_n);
title({absisas,absisas1},'FontSize',13);
xlabel({'Umbrales en mm';'Cantidad de datos por umbral';um_n},'FontSize',12)
ylabel('ETS','FontSize',12)
xlim([0 51]) 
set(gca,'YGrid','on','XGrid','on')
legend('Pronostico a 24hs','Pronostico a 48hs','Pronostico a 72hs',1)
archivo=strcat('ets','_n');
print('-depsc',archivo)
close 1


% Graficos de Bias
figure
hold on
plot(umbrales,pp_datos_n(:,1,4,med_n),'-bs','DisplayName','Pronostico a 24hs','LineWidth',1)
plot(umbrales,pp_datos_n(:,3,4,med_n),'-.r*','DisplayName','Pronostico a 48hs','LineWidth',2)
plot(umbrales,pp_datos_n(:,5,4,med_n),'--mo','DisplayName','Pronostico a 72hs','LineWidth',2)

absisas=strcat('Desde ', tempquince, ' al ', tempuno);
title(absisas,'FontSize',13)
xlabel('Umbrales en mm','FontSize',12)
ylabel('Bias areal','FontSize',12)
xlim([0 51]) 
set(gca,'YGrid','on','XGrid','on')
legend('Pronostico a 24hs','Pronostico a 48hs','Pronostico a 72hs',1)
archivo=strcat('bias','_n');
print('-depsc',archivo)
close 1



%********Genero grafico de lineas para la verificacion de PLUVIOMETROS*********
%****************************Region Sur**************************
% Graficos de ETS
med_est_s=size(pp_est_s,4);
figure
hold on
plot(umbrales,pp_est_s(:,1,1,med_est_s),'-bs','DisplayName','Pronostico a 24hs','LineWidth',1)
plot(umbrales,pp_est_s(:,3,1,med_est_s),'-.r*','DisplayName','Pronostico a 48hs','LineWidth',2)
plot(umbrales,pp_est_s(:,5,1,med_est_s),'--mo','DisplayName','Pronostico a 72hs','LineWidth',2)

absisas=strcat('Desde ', tempquince, ' al ', tempuno);
absisas1=strcat(' Cantidad de datos ', total_est_s);
title({absisas,absisas1},'FontSize',13);
xlabel({'Umbrales en mm';'Cantidad de datos por umbral';um_est_s},'FontSize',12)
ylabel('ETS','FontSize',12)
xlim([0 51]) 
set(gca,'YGrid','on','XGrid','on')
legend('Pronostico a 24hs','Pronostico a 48hs','Pronostico a 72hs',1)
archivo=strcat('ets','est_s');
print('-depsc',archivo)
close 1



% Graficos de Bias
figure
hold on
plot(umbrales,pp_est_s(:,1,4,med_est_s),'-bs','DisplayName','Pronostico a 24hs','LineWidth',1)
plot(umbrales,pp_est_s(:,3,4,med_est_s),'-.r*','DisplayName','Pronostico a 48hs','LineWidth',2)
plot(umbrales,pp_est_s(:,5,4,med_est_s),'--mo','DisplayName','Pronostico a 72hs','LineWidth',2)

absisas=strcat('Desde ', tempquince, ' al ', tempuno);
title(absisas,'FontSize',13)
xlabel('Umbrales en mm','FontSize',12)
ylabel('Bias areal','FontSize',12)
xlim([0 51]) 
set(gca,'YGrid','on','XGrid','on')
legend('Pronostico a 24hs','Pronostico a 48hs','Pronostico a 72hs',1)
archivo=strcat('bias_','est_s');
print('-depsc',archivo)
close 1



%****************************Region Norte**************************
% Graficos de ETS
med_est_n=size(pp_est_n,4);
figure
hold on
plot(umbrales,pp_est_n(:,1,1,med_est_n),'-bs','DisplayName','Pronostico a 24hs','LineWidth',1)
plot(umbrales,pp_est_n(:,3,1,med_est_n),'-.r*','DisplayName','Pronostico a 48hs','LineWidth',2)
plot(umbrales,pp_est_n(:,5,1,med_est_n),'--mo','DisplayName','Pronostico a 72hs','LineWidth',2)

absisas=strcat('Desde ', tempquince, ' al ', tempuno);
absisas1=strcat(' Cantidad de datos ', total_est_n);
title({absisas,absisas1},'FontSize',13);
xlabel({'Umbrales en mm';'Cantidad de datos por umbral';um_est_n},'FontSize',12)
ylabel('ETS','FontSize',12)
xlim([0 51]) 
set(gca,'YGrid','on','XGrid','on')
legend('Pronostico a 24hs','Pronostico a 48hs','Pronostico a 72hs',1)
archivo=strcat('ets','est_n');
print('-depsc',archivo)
close 1



% Graficos de Bias
figure
hold on
plot(umbrales,pp_est_n(:,1,4,med_est_n),'-bs','DisplayName','Pronostico a 24hs','LineWidth',1)
plot(umbrales,pp_est_n(:,3,4,med_est_n),'-.r*','DisplayName','Pronostico a 48hs','LineWidth',2)
plot(umbrales,pp_est_n(:,5,4,med_est_n),'--mo','DisplayName','Pronostico a 72hs','LineWidth',2)

absisas=strcat('Desde ', tempquince, ' al ', tempuno);
title(absisas,'FontSize',13)
xlabel('Umbrales en mm','FontSize',12)
ylabel('Bias areal','FontSize',12)
xlim([0 51]) 
set(gca,'YGrid','on','XGrid','on')
legend('Pronostico a 24hs','Pronostico a 48hs','Pronostico a 72hs',1)
archivo=strcat('bias_','est_n');
print('-depsc',archivo)
close 1


