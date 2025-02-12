clear all
close all

more off

T0=280;
P0=100000;
CP=1004;
L=2260e3;
RD=287;

%==========================================================================
%Dimensions.
[xdef ydef]=def_grid_grads;
[lonwrf latwrf]=meshgrid(xdef,ydef);


load ../FORECAST_CONTROL40M_MEMNC/verification_fnl/rmse_bias.mat

%Nota el termino de la presion de superficie va con el logaritmo, yo lo reemplaze por 1/p y como la diferencia esta al cuadrado multiplique
%la formulacion original por 1/P0^2 
TERMSE_CTRL=squeeze(  0.5*nansum(RMSEU.^2+RMSEV.^2+RMSET.^2+RMSEQ.^2*(L/(CP*T0)).^2,3)  ) + 0.5*(RD*T0/P0)*RMSEP.^2   ;

load ../FORECAST_QFX2DZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

TERMSE_PE=squeeze(  0.5*nansum(RMSEU.^2+RMSEV.^2+RMSET.^2+RMSEQ.^2*(L/(CP*T0)).^2,3)    ) + 0.5*(RD*T0/P0)*RMSEP.^2    ;

clear RMSEU RMSEV RMSET RMSEQ RMSEP

%zlevel=[1000 975 950 925 900 850 800 700 600 500 400 300 200];

load coast

figure
set(gca,'FontSize',15)

subplot(2,2,1)
set(gca,'FontSize',15)
contourf(lonwrf,latwrf, (TERMSE_PE(:,:,1) - TERMSE_CTRL(:,:,1)) ./ TERMSE_CTRL(:,:,1),[-1 -0.8 -0.6 -0.4 -0.2 -0.05 0.05 0.2 0.4 0.6 0.8 1 ] )
plot_color_fun([-1 -0.8 -0.6 -0.4 -0.2 -0.05 0.05 0.2 0.4 0.6 0.8 1 ],[49 47 45 43 42 2 22 23 25 27 29],2)
title('Analysis')
hold on
plot(long,lat,'k-','LineWidth',3)


subplot(2,2,2)
set(gca,'FontSize',15)
contourf(lonwrf,latwrf, (TERMSE_PE(:,:,5) - TERMSE_CTRL(:,:,5)) ./ TERMSE_CTRL(:,:,5),[-1 -0.8 -0.6 -0.4 -0.2 -0.05 0.05 0.2 0.4 0.6 0.8 1 ] )
plot_color_fun([-1 -0.8 -0.6 -0.4 -0.2 -0.05 0.05 0.2 0.4 0.6 0.8 1 ],[49 47 45 43 42 2 22 23 25 27 29],2)
title('24 hour forecast')
hold on
plot(long,lat,'k-','LineWidth',3)


subplot(2,2,3)
set(gca,'FontSize',15)
contourf(lonwrf,latwrf, (TERMSE_PE(:,:,9) - TERMSE_CTRL(:,:,9)) ./ TERMSE_CTRL(:,:,9),[-1 -0.8 -0.6 -0.4 -0.2 -0.05 0.05 0.2 0.4 0.6 0.8 1 ] )
plot_color_fun([-1 -0.8 -0.6 -0.4 -0.2 -0.05 0.05 0.2 0.4 0.6 0.8 1 ],[49 47 45 43 42 2 22 23 25 27 29],2)
title('48 hour forecast')
hold on
plot(long,lat,'k-','LineWidth',3)


subplot(2,2,4)
set(gca,'FontSize',15)
contourf(lonwrf,latwrf, (TERMSE_PE(:,:,13) - TERMSE_CTRL(:,:,13)) ./ TERMSE_CTRL(:,:,13),[-1 -0.8 -0.6 -0.4 -0.2 -0.05 0.05 0.2 0.4 0.6 0.8 1 ] )
plot_color_fun([-1 -0.8 -0.6 -0.4 -0.2 -0.05 0.05 0.2 0.4 0.6 0.8 1 ],[49 47 45 43 42 2 22 23 25 27 29],2)
title('72 hour forecast')
hold on
plot(long,lat,'k-','LineWidth',3)


print('FIGURA_RMSD_TOTALENERGY_FORECAST_2DZLOC.png','-dpng')
