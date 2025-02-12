% Genera los graficos de pp acumulada para WRf y para el CMORPH
% Llama a las funciones ppcon_fun y a la funcion plot_fun
% Debe tener cargado las varibles pp_wrf, lat_lon_wrf, pp_cmorph y
% lat_lon_cmorph

clear all
close all

load pp_wrf.mat
load lat_lon_wrf.mat
load pp_cmorph.mat
load lat_lon_cmorph.mat

a=size(pp_acum_wrf,3);
b=size(pp_acum_wrf,4);
c=size(pp_acum_cmorph,3);

plot_fun(lat,lon,pp_acum_wrf(:,:,a,b),pp_acum_wrf(:,:,a-1,b),pp_acum_wrf(:,:,a-2,b),pp_acum_wrf(:,:,a-3,b),pp_acum_wrf(:,:,a-4,b))
ppcon_fun(latitud_regional,longitud_regional,pp_acum_cmorph(:,:,c),'Pp acumulada en 24 hs para el CMORPH')