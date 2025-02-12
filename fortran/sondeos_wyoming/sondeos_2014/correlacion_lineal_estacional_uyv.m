%Calculo correlacion lineal y corrijo para u y v
clear all
close all

load /home/juan/Dropbox/Niebla/Scripts/EZEIZAugrd_10m.mat

u_ref2=datos_estacion(1:9526,:)/0.51;  %es en m/s

clear datos_estacion

load /home/juan/Dropbox/Niebla/Scripts/EZEIZAvgrd_10m.mat

v_ref2=datos_estacion(1:9526,:)/0.51; %es en m/s, cuando multiplico lo convierto en kt

load uyvsynop_3_new.mat %en kt
load /home/juan/Dropbox/Niebla/Scripts/datos2_3hs.mat
load /home/juan/Dropbox/Niebla/Scripts/viento_ref2_modificado.mat

viento_ref=viento_ref2(1:9526,:)/0.51; %en ref2 viene en m/s
viento_syn=viento_3(1:9526,:); %hasta el dia que tienen datos, por eso el 9526
u_syn=u_synop_3(1:9526,:);
v_syn=v_synop_3(1:9526,:);
u_ref=u_ref2;
v_ref=v_ref2;

%Voy a calcular los coef de correlacion lineal para cada estacion


esnan=(~isnan(viento_ref) & ~isnan(viento_syn) & ~isnan(u_syn) & ~isnan(v_syn) & ~isnan(u_ref) & ~isnan(v_ref) );

viento_ref=viento_ref(esnan);
viento_syn=viento_syn(esnan);

u_syn=u_syn(esnan);
v_syn=v_syn(esnan);
u_ref=u_ref(esnan);
v_ref=v_ref(esnan);
















