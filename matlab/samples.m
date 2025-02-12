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
pcolor(LON,LAT,squeeze(media_samples(1,:,:))-med_samples)
v=[-2000 -1500 -1000 -500 -0.5 0.5 500 1000 1500 2000];
vcol=[102 114 113 32 2 112 107 108 28];
plot_jrcol(v,vcol);
shading flat
hold on
plot(lon_costa,lat_costa,'k','LineWidth',1.5)
plot(lon_pais,lat_pais,'k','LineWidth',1.5)
grid on
caxis([-2000 2000]);
axis([-180 180 -50 50])
title('ENERO - ANOMALIA')
xlabel('Longitud')
ylabel('Latitud')
set(gca,'DataAspectRatio',[1 1 1],'XTick',[-180;-120;-60;0;60;120;180],...
    'YTick',[-40;-20;0;20;40])
colorbar('Location','South')

figure
pcolor(LON,LAT,squeeze(media_samples(2,:,:))-med_samples)
hold on
plot(lon_costa,lat_costa,'k','LineWidth',1.5)
plot(lon_pais,lat_pais,'k','LineWidth',1.5)
grid on
shading flat
%caxis([0 3]);
axis([-180 180 -50 50])
title('FEBRERO - ANOMALIA')
xlabel('Longitud')
ylabel('Latitud')
set(gca,'DataAspectRatio',[1 1 1],'XTick',[-180;-120;-60;0;60;120;180],...
    'YTick',[-40;-20;0;20;40])
colorbar('Location','South')

figure
pcolor(LON,LAT,squeeze(media_samples(3,:,:))-med_samples)
hold on
plot(lon_costa,lat_costa,'k','LineWidth',1.5)
plot(lon_pais,lat_pais,'k','LineWidth',1.5)
grid on
shading flat
%caxis([0 3]);
axis([-180 180 -50 50])
title('MARZO - ANOMALIA')
xlabel('Longitud')
ylabel('Latitud')
set(gca,'DataAspectRatio',[1 1 1],'XTick',[-180;-120;-60;0;60;120;180],...
    'YTick',[-40;-20;0;20;40])
colorbar('Location','South')

figure
pcolor(LON,LAT,squeeze(media_samples(4,:,:))-med_samples)
hold on
plot(lon_costa,lat_costa,'k','LineWidth',1.5)
plot(lon_pais,lat_pais,'k','LineWidth',1.5)
grid on
shading flat
%caxis([0 3]);
axis([-180 180 -50 50])
title('ABRIL - ANOMALIA')
xlabel('Longitud')
ylabel('Latitud')
set(gca,'DataAspectRatio',[1 1 1],'XTick',[-180;-120;-60;0;60;120;180],...
    'YTick',[-40;-20;0;20;40])
colorbar('Location','South')

figure
pcolor(LON,LAT,squeeze(media_samples(5,:,:))-med_samples)
hold on
plot(lon_costa,lat_costa,'k','LineWidth',1.5)
plot(lon_pais,lat_pais,'k','LineWidth',1.5)
grid on
shading flat
%caxis([0 3]);
axis([-180 180 -50 50])
title('MAYO - ANOMALIA')
xlabel('Longitud')
ylabel('Latitud')
set(gca,'DataAspectRatio',[1 1 1],'XTick',[-180;-120;-60;0;60;120;180],...
    'YTick',[-40;-20;0;20;40])
colorbar('Location','South')

figure
pcolor(LON,LAT,squeeze(media_samples(6,:,:))-med_samples)
hold on
plot(lon_costa,lat_costa,'k','LineWidth',1.5)
plot(lon_pais,lat_pais,'k','LineWidth',1.5)
grid on
shading flat
%caxis([0 3]);
axis([-180 180 -50 50])
title('JUNIO - ANOMALIA')
xlabel('Longitud')
ylabel('Latitud')
set(gca,'DataAspectRatio',[1 1 1],'XTick',[-180;-120;-60;0;60;120;180],...
    'YTick',[-40;-20;0;20;40])
colorbar('Location','South')

figure
pcolor(LON,LAT,squeeze(media_samples(7,:,:))-med_samples)
hold on
plot(lon_costa,lat_costa,'k','LineWidth',1.5)
plot(lon_pais,lat_pais,'k','LineWidth',1.5)
grid on
shading flat
%caxis([0 3]);
axis([-180 180 -50 50])
title('JULIO - ANOMALIA')
xlabel('Longitud')
ylabel('Latitud')
set(gca,'DataAspectRatio',[1 1 1],'XTick',[-180;-120;-60;0;60;120;180],...
    'YTick',[-40;-20;0;20;40])
colorbar('Location','South')

figure
pcolor(LON,LAT,squeeze(media_samples(8,:,:))-med_samples)
hold on
plot(lon_costa,lat_costa,'k','LineWidth',1.5)
plot(lon_pais,lat_pais,'k','LineWidth',1.5)
grid on
shading flat
%caxis([0 3]);
axis([-180 180 -50 50])
title('AGOSTO - ANOMALIA')
xlabel('Longitud')
ylabel('Latitud')
set(gca,'DataAspectRatio',[1 1 1],'XTick',[-180;-120;-60;0;60;120;180],...
    'YTick',[-40;-20;0;20;40])
colorbar('Location','South')

figure
pcolor(LON,LAT,squeeze(media_samples(9,:,:))-med_samples)
hold on
plot(lon_costa,lat_costa,'k','LineWidth',1.5)
plot(lon_pais,lat_pais,'k','LineWidth',1.5)
grid on
shading flat
%caxis([0 3]);
axis([-180 180 -50 50])
title('SEPTIEMBRE - ANOMALIA')
xlabel('Longitud')
ylabel('Latitud')
set(gca,'DataAspectRatio',[1 1 1],'XTick',[-180;-120;-60;0;60;120;180],...
    'YTick',[-40;-20;0;20;40])
colorbar('Location','South')

figure
pcolor(LON,LAT,squeeze(media_samples(10,:,:))-med_samples)
hold on
plot(lon_costa,lat_costa,'k','LineWidth',1.5)
plot(lon_pais,lat_pais,'k','LineWidth',1.5)
grid on
shading flat
%caxis([0 3]);
axis([-180 180 -50 50])
title('OCTUBRE - ANOMALIA')
xlabel('Longitud')
ylabel('Latitud')
set(gca,'DataAspectRatio',[1 1 1],'XTick',[-180;-120;-60;0;60;120;180],...
    'YTick',[-40;-20;0;20;40])
colorbar('Location','South')

figure
pcolor(LON,LAT,squeeze(media_samples(11,:,:))-med_samples)
hold on
plot(lon_costa,lat_costa,'k','LineWidth',1.5)
plot(lon_pais,lat_pais,'k','LineWidth',1.5)
grid on
shading flat
%caxis([0 3]);
axis([-180 180 -50 50])
title('NOVIEMBRE - ANOMALIA')
xlabel('Longitud')
ylabel('Latitud')
set(gca,'DataAspectRatio',[1 1 1],'XTick',[-180;-120;-60;0;60;120;180],...
    'YTick',[-40;-20;0;20;40])
colorbar('Location','South')

figure
pcolor(LON,LAT,squeeze(media_samples(12,:,:))-med_samples)
hold on
plot(lon_costa,lat_costa,'k','LineWidth',1.5)
plot(lon_pais,lat_pais,'k','LineWidth',1.5)
grid on
shading flat
%caxis([0 3]);
axis([-180 180 -50 50])
title('DICIEMBRE - ANOMALIA')
xlabel('Longitud')
ylabel('Latitud')
set(gca,'DataAspectRatio',[1 1 1],'XTick',[-180;-120;-60;0;60;120;180],...
    'YTick',[-40;-20;0;20;40])
colorbar('Location','South')
