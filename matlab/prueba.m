% GRAFICADO DE LAS CURVAS DEL EMAGRAMA
%********************************************************************************************************     
       figure
%Genero las curvas del emagrama usando la funcion emagrama.m
%Los parametros de entrada son ThetaMin (linea minima de valores de Theta que traza)
%ThetaMax maximo valor de Theta que dibuja
%DeltaTheta intervalo de las titas.
%Los valores recomendados son ThetaMin=190, ThetaMax=420 y DeltaTheta=10 (Todos en K).
%Deltar controla la cantidad de lineas de relacion de mezcla que se trazan.
     Pbot=1000;
     emagrama_byn(190,420,20,2,Pbot);
%********************************************************************************************************
% FIN DEL GRAFICADO DE LAS CURVAS DEL EMAGRAMA
%********************************************************************************************************     


%**********************************************************************************************************
% GRAFICADO DE LOS DATOS DEL ENTORNO (T Y Td). Y proceso para la parcela de superficie y para la mas inestable.
%********************************************************************************************************
       title('Diagrama aerologico (EMAGRAMA)')
       hold on
       %la parcela mas inestable
       pmax=Pbot/100;
       axis([-93 47 -1050 -150])

      % axis([180 320 -1000 -150])
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[-1000 -900 -800 -700 -600 -500 -400 -300 -200 -150])
       set(gca,'YTickLabel',{'1000';'900';'800';'700';'600';'500';'400';'300';'200';'150'})
       set(gca,'Xgrid','on')
       set(gca,'Ygrid','on')
       
       xlabel('Temperatura (C)');
       ylabel('Pres. (hPa)');
       
       clear P

       print('-dpng','-r0', 'out7.png')
       















