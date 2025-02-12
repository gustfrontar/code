clear all
close all

%Evaluation of bias in the forecast.


load model_data.mat
load lluvia_obs_hires.mat
load lat_lon.mat


nforecast=size(lluvia_obs,3);
ndays=size(lluvia_obs,4);

%Compare PDF.

bins=[1 20];  %Thresholds to compute FSS.

nbins=length(bins);


%Compute FSS.
areas=[1 5 10 20 40];
%for ii=1:nforecast
%
%    [fss(:,:,ii)]=fss_fun(squeeze(lluvia_modelo(:,:,ii,:)),squeeze(lluvia_obs(:,:,ii,:)),bins,areas);
%    
%end
%save('fss.mat','fss')
load('fss.mat')


%FIGURES:

%COMPARACION VISUAL DE 2 CAMPOS
%==========================================================================
figure
subplot(1,2,1)

carga_mapa
lon_costa(lon_costa < 0)=lon_costa(lon_costa<0)+360;
lon_pais(lon_pais < 0)=lon_pais(lon_pais<0)+360;

pcolor(LON,LAT,lluvia_modelo(:,:,6,7))
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([1 2 5 10 15 20 30 40 50 60 75 100 125 150],[2 88:99 59 2])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)
title('FORECAST')

subplot(1,2,2)

pcolor(LON,LAT,lluvia_obs(:,:,6,7))
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([1 2 5 10 15 20 30 40 50 60 75 100 125 150],[2 88:99 59 2])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)
title('OBSERVATION')

%PLOT EXAMPLE FOR LARGE CIRCLE SIZE (STRONG SMOOTHING)
umbral=10;
mascara_obs=real(lluvia_obs(:,:,6,7) > umbral);
mascara_mod=real(lluvia_modelo(:,:,6,7) > umbral);
prob_obs=suaviza_fun(mascara_obs,40);
prob_mod=suaviza_fun(mascara_mod,40);

figure
subplot(1,2,1)

carga_mapa
lon_costa(lon_costa < 0)=lon_costa(lon_costa<0)+360;
lon_pais(lon_pais < 0)=lon_pais(lon_pais<0)+360;

pcolor(LON,LAT,prob_mod)
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([0:0.1:1],[2 88 89 90 91 92 93 94 95 96])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)
title('FORECAST (RADIUS 160km)')

subplot(1,2,2)

pcolor(LON,LAT,prob_obs)
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([0:0.1:1],[2 88 89 90 91 92 93 94 95 96])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)
title('OBSERVATION (RADIUS 160km)')

figure

pcolor(LON,LAT,prob_mod-prob_obs)
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([-1 -0.8 -0.6 -0.4 -0.2 -0.1 0.1 0.2 0.4 0.6 0.8 1.0],[47 46 45 44 43 2 23 24 25 26 27])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)
title('DIFFERENCE (RADIUS 160km)')


%PLOT EXAMPLE FOR SMALL CIRCLE SIZE (SOFT SMOOTHING)
umbral=10;
mascara_obs=real(lluvia_obs(:,:,6,7) > umbral);
mascara_mod=real(lluvia_modelo(:,:,6,7) > umbral);
prob_obs=suaviza_fun(mascara_obs,5);
prob_mod=suaviza_fun(mascara_mod,5);

figure
subplot(1,2,1)

carga_mapa
lon_costa(lon_costa < 0)=lon_costa(lon_costa<0)+360;
lon_pais(lon_pais < 0)=lon_pais(lon_pais<0)+360;

pcolor(LON,LAT,prob_mod)
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([0:0.1:1],[2 88 89 90 91 92 93 94 95 96])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)
title('FORECAST (RADIUS 20km)')

subplot(1,2,2)

pcolor(LON,LAT,prob_obs)
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([0:0.1:1],[2 88 89 90 91 92 93 94 95 96])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)
title('OBSERVATION (RADIUS 20km)')

figure

pcolor(LON,LAT,prob_mod-prob_obs)
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([-1 -0.8 -0.6 -0.4 -0.2 -0.1 0.1 0.2 0.4 0.6 0.8 1],[47 46 45 44 43 2 23 24 25 26 27])
hold on
plot(lon_costa,lat_costa,'k-','LineWidth',2)
plot(lon_pais,lat_pais,'k-','LineWidth',2)
title('DIFFERENCE (RADIUS 20km)')

%PLOF FSS RESULTS

horas={'03h','06h','09h','12h','15h','18h','21h','24h'};
figure 
for ii=1:8

          subplot(1,8,ii)
          hold on
          plot(areas*4,fss(1,:,ii),'bo-','LineWidth',2)
          plot(areas*4,fss(2,:,ii),'ro-','LineWidth',2)

          grid on;
          xlabel('Smoothing radius (Km)')
          ylabel(['FSS ' horas{ii}])
          axis([min(areas)*4 max(areas)*4 0 1])

end
legend('1 mm','20 mm')


%figure
%hold on
%plot(3:3:24,squeeze(fss(2,1,:)))
%plot(3:3:24,squeeze(fss(2,5,:)))





