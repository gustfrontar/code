close all
clear all

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Este script lee un archivo en formato HDF que contiene información
% respecto del número de pasadas (muestras) mensuales en todo el mundo del
% satélite TRMM.
% El archivo esta conformado por las siguientes variables:
%
% 1) YEAR (Año)                 1x108
% 2) MONTH (Mes)                1x108
% 3) LON (Longitud)            360x80
% 4) LAT (Latitud)             360x80
% 5) SAMPLES (Muestras)    108x360x80
%
% Luego de la lectura, calcula el campo medio anual de muestras y lo grafica.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Lectura de los datos:
YEAR=double(hdfread('samples_3a25_pixels.HDF','YEAR'));
MONTH=double(hdfread('samples_3a25_pixels.HDF','MONTH'));
LON=double(hdfread('samples_3a25_pixels.HDF','LON'));
LAT=double(hdfread('samples_3a25_pixels.HDF','LAT'));
SAMPLES=double(hdfread('samples_3a25_pixels.HDF','SAMPLES'));
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generamos la media y el desvio estándar:
media_samples(:,:)=mean(squeeze(SAMPLES(:,:,:)),1);
sd_samples(:,:)=std(squeeze(SAMPLES(:,:,:)),1);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generamos los gráficos. Tener en cuenta que los valores son muestras x
% 10^4

carga_mapa;

figure

subplot(2,1,1)
pcolor(LON,LAT,media_samples)
hold on
plot(lon_costa,lat_costa,'k','LineWidth',1.5)
plot(lon_pais,lat_pais,'k','LineWidth',1.5)
grid on
shading flat
%caxis([0 3]);
axis([-180 180 -40 40 ])
title('MEDIA')
set(gca,'DataAspectRatio',[1 1 1],'XTick',[-180;-120;-60;0;60;120;180],...
    'YTick',[-40;-20;0;20;40])
run colorbar

subplot(2,1,2)
pcolor(LON,LAT,sd_samples)
hold on
plot(lon_costa,lat_costa,'k','LineWidth',1.5)
plot(lon_pais,lat_pais,'k','LineWidth',1.5)
grid on
shading flat
%caxis([0 3]);
axis([-180 180 -40 40 ])
title('DESVIO ESTÁNDAR')
set(gca,'DataAspectRatio',[1 1 1],'XTick',[-180;-120;-60;0;60;120;180],...
    'YTick',[-40;-20;0;20;40])
run colorbar