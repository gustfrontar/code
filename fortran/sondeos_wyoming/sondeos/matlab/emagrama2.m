% Esta funcion carga las curvas del emagrama generadas por emagrama.m y las plotea.
%Las adiabaticas saturadas se trazan utilizando una ecuacion similar a la ecuacion 8 del paper de Lipps y Hemler
%con la diferencia de que se ignoran los contenidos de agua liquida.
%Los parametros de entrada son ThetaMin (linea minima de valores de Theta que traza)
%Programa realizado por Juan Jose Ruiz (2006)

function out = emagrama(Pbot)

%*********************************************************************************************
%Parametros iniciales que controlan el funcionamiento del programa
%Este modulo define los parametros iniciales, el valor de las constantes termodinamicas
%*********************************************************************************************








%*********************************************************************************************
%Cargamos el archivo con las curvas del emagrama ya generadas
%*********************************************************************************************

load emagrama.mat


%********************************************************************************************************
% INTERFASE GRAFICA: LOS LOOPS QUE SIGUEN A CONTINUACION GENERAN LAS CURVAS DEL EMAGRAMA.
%********************************************************************************************************
    
    semilogy(Tseca(:,theta)-273,-(P/100))
       axis([-93 47 -1050 -150])
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[-1050 -1000 -900 -800 -700 -600 -500 -400 -300 -200 -150])
       set(gca,'YTickLabel',{'1050';'1000';'900';'800';'700';'600';'500';'400';'300';'200';'150'})
       set(gca,'Xgrid','on')
       set(gca,'Ygrid','on')

    hold on

       semilogy(Tsat1(:,rs)-273,-(P/100),'-.r')
       
       hold on
       
       semilogy(Tsat2(:,rs)-273,-(P/100),'-.r')
       
       hold on
       
       semilogy(Tsat3(:,rs)-273,-(P/100),'-.r')
       
       hold on
       
    semilogy(Tpseudo(:,theta)-273,-(P/100),'--m')

    hold on
   


out=1;












