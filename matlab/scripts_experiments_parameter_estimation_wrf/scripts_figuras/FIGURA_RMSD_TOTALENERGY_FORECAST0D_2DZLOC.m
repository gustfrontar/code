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

TERMSE_CTRL=squeeze( mean ( mean ( TERMSE_CTRL , 1 ) , 2) );

load ../FORECAST_QFX2DZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

TERMSE_PE=squeeze(  0.5*nansum(RMSEU.^2+RMSEV.^2+RMSET.^2+RMSEQ.^2*(L/(CP*T0)).^2,3)    ) + 0.5*(RD*T0/P0)*RMSEP.^2    ;

TERMSE_PE=squeeze( mean ( mean ( TERMSE_PE , 1 ) , 2) );

clear RMSEU RMSEV RMSET RMSEQ RMSEP

%zlevel=[1000 975 950 925 900 850 800 700 600 500 400 300 200];
horas=(0:6:72)

figure
set(gca,'FontSize',15)

plot( horas , TERMSE_CTRL ,'-k','LineWidth',4)
hold on
plot( horas , TERMSE_PE ,'--k','LineWidth',4)
title('Total Energy RMSD')



print('FIGURA_RMSD_TOTALENERGY_FORECAST0D_2DZLOC.png','-dpng')
