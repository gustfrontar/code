
%**************************************************************************
%      VerificaciÃ³n de la calibracion del pronostico probabilistico
%**************************************************************************

function [ pepe ]=verifica_calibracion_fun(obs,prob,prob2,umbral,legend)

path='d:/trabajos/TrabajoSLAF/precipitacion/superensemble/matlab/'

%obs: vector con las observaciones.
%prob: pronostico probabilistico 1. Matriz de mxn donde m es el numero de
%pronosticos que tienen que coincidir con las observaciones y m es el
%numero de umbrales (un pronóstico de probabilidad para cada umbral).
%prob2: idem pero para un pronóstico probabilístico obtenido de otra forma.
%umbral: umbrales utilizados en el cálculo del pronóstico probabilistico.
%legend: texto a agregarse en el nombre de las figuras que deberia incluir
%hora de pronóstico, fuente de la verificación region.

%**************************************************************************
%  CALCULO DE LAS CURVAS DE RELIABILITY PARA EL PRONÃ“STICO PROBABILISTICO
%  CALCULADO PREVIAMENTE.
%**************************************************************************

%Prob int son los intervalos donde voy a acumular las probabilidades
%pronosticadas.

prob_int=0:0.1:1;

[reliability n_forecast prob_ref clim_prob]= reliability_fun(obs,prob,umbral,prob_int);
[reliability_2 n_forecast_2 prob_ref clim_prob_2]= reliability_fun(obs,prob2,umbral,prob_int);

%**************************************************************************
%  CALCULO DE LOS BRIER SCORE
%**************************************************************************

[brier]=brier_fun(obs,prob,umbral);
[brier_2]=brier_fun(obs,prob2,umbral);

%**************************************************************************
%  CALCULO DE LAS CURVAS Y EL "AREA" DEL ROC
%**************************************************************************

prob_umb=0.1:0.1:0.9;

[hit far area ets bias] = roc_fun(obs,prob,umbral,prob_umb);
[hit_2 far_2 area_2 ets_2 bias_2] = roc_fun(obs,prob2,umbral,prob_umb);

%**************************************************************************
%  GRAFICO LAS CURVAS DEL ROC
%**************************************************************************

   title_graf=strcat('"AREA" DEL ROC');
   figure
   plot(umbral,area,'r');
   hold on;
   plot(umbral,area_2,'b');

   legend('no cal','cal')
   %axis([0 umbral(length(umbral)) 0 0.3]);
   hold on;
   title(title_graf)
   figura=strcat(path,legend,'.png');
   print('-dpng',figura)
   for i_umb=1:length(umbral)
   figure
   no_skill=0:0.1:1;
   title_graf=strcat('ROC para el umbral ',num2str(umbral(i_umb)));
   plot(far(:,i_umb),hit(:,i_umb),'ro-');
   hold on;
   plot(far_2(:,i_umb),hit_2(:,i_umb),'bo-');
   plot(no_skill,no_skill,'k+')
   legend('no cal','cal','no skill')
   axis([0 1 0 1]);
   hold on;
   title(title_graf)
   figura=strcat(path,legend,num2str(umbral(i_umb)),'.png');
   print('-dpng',figura)
   end


%**************************************************************************
%   GRAFICO LOS RELIABILITY DIAGRAMS.
%**************************************************************************


for i=1:length(umbral)
    no_resol_line(1:length(prob_ref))=clim_prob(i);
    title_graf=strcat('Reliavility diagram para un umbral de ',num2str(umbral(i)));
    title_graf2=strcat('Frecuencia de pronÃ³sticos para un umbral de ',num2str(umbral(i)));
figure
subplot(121)
   
   plot(prob_ref,reliability(i,:),'r');
   hold on;
   plot(prob_ref,reliability_2(i,:),'b');
   %legend('F24 calibrado','F24 no calibrado','F48 calibrado','F48 no calibrado')
   axis([0 1 0 1]);
   hold on;
   plot(prob_ref,no_resol_line,'g');
   plot(prob_ref,prob_ref,'g');
   title(title_graf)

subplot(122)
   plot(prob_ref,n_forecast(i,:),'r');
   hold on;
   plot(prob_ref,n_forecast_2(i,:),'b');

   legend('no cal','cal')
   title(title_graf2)
   %axis([0 1 0 max]);
   figura=strcat(path,verif,legend,num2str(umbral(i)),'.png');
   print('-dpng',figura)
end


%**************************************************************************
%   GRAFICO BRIER SKILL SCORE.
%**************************************************************************

   title_graf=strcat('BRIER SKILL score');
   figure
   plot(umbral,(brier-brier)./brier,'r');
   hold on;
   plot(umbral,(brier_2-brier)./brier,'b');
   legend('no cal','cal')
   axis([0 umbral(length(umbral)) -0.3 0.05]);
   hold on;
   title(title_graf)
   figura=strcat(path,legend,'BRIER.png');
   print('-dpng',figura)
   
%**************************************************************************
%   GRAFICO EL ETS PARA EL UMBRAL MAS BAJO.
%**************************************************************************

   title_graf=strcat('ETS umbral ',num2str(umbral(1)));
   figure
   plot(prob_umb,ets(:,1),'r');
   hold on;
   plot(prob_umb,ets_2(:,1),'b');
   legend('no cal','cal')
   axis([0 1 0 1]);
   hold on;
   title(title_graf)
   figura=strcat(path,legend,'ETS.png');
   print('-dpng',figura)
   
   title_graf=strcat('BIAS umbral ',num2str(umbral(1)));
   figure
   plot(prob_umb,bias(:,1),'r');
   hold on;
   plot(prob_umb,bias_2(:,1),'b');
   legend('no cal','cal')
   axis([0 1 0 3]);
   hold on;
   title(title_graf)
   figura=strcat(path,legend,'BIAS.png');
   print('-dpng',figura)

   pepe=1;
