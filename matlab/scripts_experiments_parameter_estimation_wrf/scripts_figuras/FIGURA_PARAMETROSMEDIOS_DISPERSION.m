clear all
close all

load ../EXPERIMENTOS/QFX0DNOZLOC40M_MEMNC/parameters.mat 

paraveqfx0dnozloc=parameter_time_mean;
parvarqfx0dnozloc=parameter_time_var;

partsqfx0dnozloc=parameter_spatial_average;

load ../EXPERIMENTOS/QFX0DZLOC40M_MEMNC/parameters.mat 

paraveqfx0dzloc=parameter_time_mean;
parvarqfx0dzloc=parameter_time_var;

partsqfx0dzloc=parameter_spatial_average;

load ../EXPERIMENTOS/QFX2DNOZLOC40M_MEMNC/parameters.mat 

paraveqfx2dnozloc=parameter_time_mean;
parvarqfx2dnozloc=parameter_time_var;

partsqfx2dnozloc=parameter_spatial_average;

load ../EXPERIMENTOS/QFX2DZLOC40M_MEMNC/parameters.mat 

paraveqfx2dzloc=parameter_time_mean;
parvarqfx2dzloc=parameter_time_var;

partsqfx2dzloc=parameter_spatial_average;

[xdef ydef zdef]=def_grid_grads();
[lonwrf latwrf]=meshgrid(xdef,ydef);
load coast

figure
subplot(1,2,1)
set(gca,'FontSize',15)
hold on
pcolor(lonwrf,latwrf,squeeze(paraveqfx2dnozloc(:,:,2))');shading flat;
plot(long,lat,'-k','LineWidth',2);
axis([ min(min(lonwrf)) max(max(lonwrf)) min(min(latwrf)) max(max(latwrf))])
plot_color_fun([0.2 0.4 0.6 0.8 0.9 1.1 1.2 1.4 1.6 1.8],[49 47 45 43 2 23 25 27 29],2)
title('W/O Vertical localization','FontSize',15)
xlabel('Lon')
ylabel('Lat')

subplot(1,2,2)
set(gca,'FontSize',15)
hold on
pcolor(lonwrf,latwrf,squeeze(paraveqfx2dzloc(:,:,2))');shading flat;
plot(long,lat,'-k','LineWidth',2);
axis([ min(min(lonwrf)) max(max(lonwrf)) min(min(latwrf)) max(max(latwrf))])
plot_color_fun([0.2 0.4 0.6 0.8 0.9 1.1 1.2 1.4 1.6 1.8],[49 47 45 43 2 23 25 27 29],2)
title('W. Vertical localization','FontSize',15)
xlabel('Lon')
ylabel('Lat')

figure
subplot(1,2,1)
set(gca,'FontSize',15)
hold on
pcolor(lonwrf,latwrf,squeeze(parvarqfx2dnozloc(:,:,2))');shading flat;
plot(long,lat,'-k','LineWidth',2);
axis([ min(min(lonwrf)) max(max(lonwrf)) min(min(latwrf)) max(max(latwrf))])
plot_color_fun([0 0.05 0.1 0.15 0.2 0.25 0.3 ],[2 23 24 25 27 28],2)
title('W/O Vertical localization','FontSize',15)
xlabel('Lon')
ylabel('Lat')

subplot(1,2,2)
set(gca,'FontSize',15)
hold on
pcolor(lonwrf,latwrf,squeeze(parvarqfx2dzloc(:,:,2))');shading flat;
plot(long,lat,'-k','LineWidth',2);
axis([ min(min(lonwrf)) max(max(lonwrf)) min(min(latwrf)) max(max(latwrf))])
plot_color_fun([0 0.05 0.1 0.15 0.2 0.25 0.3 ],[2 23 24 25 27 28],2)
title('W. Vertical localization','FontSize',15)
xlabel('Lon')
ylabel('Lat')
