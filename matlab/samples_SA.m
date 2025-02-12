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
% Luego de la lectura, calcula el campo medio de muestras para cada mes del
% año y los grafica.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Lectura de los datos:
YEAR=double(hdfread('samples_3a25_pixels.HDF','YEAR'));
MONTH=double(hdfread('samples_3a25_pixels.HDF','MONTH'));
LON=double(hdfread('samples_3a25_pixels.HDF','LON'));
LAT=double(hdfread('samples_3a25_pixels.HDF','LAT'));
SAMPLES=double(hdfread('samples_3a25_pixels.HDF','SAMPLES'));
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generamos los campos medios mesuales:
for imes=1:12
    i=imes:12:97+imes;
   media_samples(imes,:,:)=mean(squeeze(SAMPLES(i,:,:)),1);
end

% Calculamos el campo medio anual y la desviación estándar:
med_samples(:,:)=mean(squeeze(SAMPLES(:,:,:)),1);
sd_samples(:,:)=std(squeeze(SAMPLES(:,:,:)),1);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generamos los gráficos. 

carga_mapa;

figure

subplot(2,1,1)
pcolor(LON,LAT,med_samples)
hold on
plot(lon_costa,lat_costa,'k','LineWidth',1.5)
plot(lon_pais,lat_pais,'k','LineWidth',1.5)
grid on
shading flat
%caxis([0 3]);
axis([-180 180 -50 50])
title('MEDIA')
xlabel('Longitud')
ylabel('Latitud')
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
axis([-180 180 -50 50])
title('DESVIO ESTÁNDAR')
xlabel('Longitud')
ylabel('Latitud')
set(gca,'DataAspectRatio',[1 1 1],'XTick',[-180;-120;-60;0;60;120;180],...
    'YTick',[-40;-20;0;20;40])
run colorbar

figure
pcolor(LON,LAT,med_samples)
hold on
plot(lon_costa,lat_costa,'k','LineWidth',1.5)
plot(lon_pais,lat_pais,'k','LineWidth',1.5)
shading flat
%caxis([0 3]);
axis([-85 -35 -42 12])
title('MEDIA')
xlabel('Longitud')
ylabel('Latitud')
set(gca,'DataAspectRatio',[1 1 1],'XTick',[-85;-75;-65;-55;-45;-35],...
    'YTick',[-42;-32;-22;-12;0;12])
grid on
run colorbar

figure
pcolor(LON,LAT,sd_samples)
hold on
plot(lon_costa,lat_costa,'k','LineWidth',1.5)
plot(lon_pais,lat_pais,'k','LineWidth',1.5)
shading flat
%caxis([0 3]);
axis([-85 -35 -42 12])
title('DESVIO ESTÁNDAR')
xlabel('Longitud')
ylabel('Latitud')
set(gca,'DataAspectRatio',[1 1 1],'XTick',[-85;-75;-65;-55;-45;-35],...
    'YTick',[-42;-32;-22;-12;0;12])
grid on
run colorbar