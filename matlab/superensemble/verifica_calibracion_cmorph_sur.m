clear all
close all
%**************************************************************************
%      VerificaciÃ³n de la calibracion del pronostico probabilistico
%**************************************************************************

path='d:/trabajos/TrabajoSLAF/precipitacion/superensemble/matlab/'

region='norte'
fuente='cmo'

umbral=[0.01 0.10 0.25 0.5 1 1.5 2]*25.4;

%Cargo la probabilidad.

load(strcat(path,'superensemble_prob_',region,'_',fuente,'.mat'))

%Cargo tambien el superensemble para comparar los resultados con los
%pronosticos probabilisticos.

load(strcat(path,'superensemble_',region,'_',fuente,'.mat'));

ensemble=p24;

prob=prob_nocal_24;
prob2=prob_cal_24;
obs=verif_24(:,2);

gifname=strcat(fuente,'_',region,'_f24_');


%Primero calculo la media del ensemble

[filas columnas]=size(ensemble);

ensemble(:,columnas+1)=squeeze(nanmean(ensemble(:,2:columnas),2));


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

%Calculo hit,far,ets y bias de los miembros del ensemble y su media para
%comparar.

[ets_ens hit_ens far_ens]=ets_fun(ensemble(:,1),ensemble(:,2:columnas+1),umbral)
[bias_ens]=biasarea_fun(ensemble(:,1),ensemble(:,2:columnas+1),umbral)

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
   figura=strcat(path,gifname,'.png');
   print('-dpng',figura)
   for i_umb=1:length(umbral)
   figure
   no_skill=0:0.1:1;
   far_random=ones(length(no_skill),1)*(1-clim_prob(i_umb));
   
   title_graf=strcat('ROC para el umbral ',num2str(umbral(i_umb)));
   plot(far(:,i_umb),hit(:,i_umb),'ro-');
   hold on;
   plot(far_2(:,i_umb),hit_2(:,i_umb),'bo-');
   plot(no_skill,no_skill,'k+')
   hold on
   plot(far_ens(i_umb,1:columnas-1),hit_ens(i_umb,1:columnas-1),'b+');
   hold on
   plot(far_ens(i_umb,columnas),hit_ens(i_umb,columnas),'g+');
   plot(far_random,no_skill,'b+');
   legend('no cal','cal','no skill','members','mean','Random FAR')

   axis([0 1 0 1]);
   hold on;
   title(title_graf)
   figura=strcat(path,gifname,num2str(umbral(i_umb)),'.png');
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
   figura=strcat(path,gifname,num2str(umbral(i)),'.png');
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
   figura=strcat(path,gifname,'BRIER.png');
   print('-dpng',figura)
   
%**************************************************************************
%   GRAFICO EL ETS PARA EL UMBRAL I_UMB.
%**************************************************************************
   for i_umb=1:length(umbral);
   
       umb=umbral(i_umb);
   %Voy a comparar esto con el maximo y mínimo ETS de los miembros y el de
   %la media.
   
   ets_max=ones(length(prob_umb),1)*max(ets_ens(i_umb,1:columnas-1)); %El maximo ets del ensemble para el umbral i_umb
   ets_min=ones(length(prob_umb),1)*min(ets_ens(i_umb,1:columnas-1)); %El mínimo ets del ensemble para el umbral i_umb
   ets_mean=ones(length(prob_umb),1)*ets_ens(i_umb,columnas);     %Ets de la media
   
   
   title_graf=strcat('ETS umbral ',num2str(umbral(i_umb)));
   figure
   plot(prob_umb,ets(:,i_umb),'r');
   hold on;
   plot(prob_umb,ets_2(:,i_umb),'b');
   legend('no cal','cal','Ens max','Ens min','Ens mean')
   axis([0 1 0 1]);
   hold on;
   plot(prob_umb,ets_max,'bo');
   plot(prob_umb,ets_min,'bo');
   plot(prob_umb,ets_mean,'bo-');
   title(title_graf)
   figura=strcat(path,gifname,'ETS_',num2str(umbral(i_umb)),'.png');
   print('-dpng',figura)
   
   bias_max=ones(length(prob_umb),1)*max(bias_ens(i_umb,1:columnas-1)); %El maximo ets del ensemble para el umbral i_umb
   bias_min=ones(length(prob_umb),1)*min(bias_ens(i_umb,1:columnas-1)); %El mínimo ets del ensemble para el umbral i_umb
   bias_mean=ones(length(prob_umb),1)*bias_ens(i_umb,columnas);     %Ets de la media
   
   title_graf=strcat('BIAS umbral ',num2str(umbral(i_umb)));
   figure
   plot(prob_umb,bias(:,i_umb),'r');
   hold on;
   plot(prob_umb,bias_2(:,i_umb),'b');
   legend('no cal','cal','Ens max','Ens min','Ens mean')
   axis([0 1 0 3]);
   hold on;
   plot(prob_umb,bias_max,'bo');
   plot(prob_umb,bias_min,'bo');
   plot(prob_umb,bias_mean,'bo-');
   title(title_graf)
   figura=strcat(path,gifname,'BIAS_',num2str(umbral(i_umb)),'.png');
   print('-dpng',figura)

   end
   pepe=1;
