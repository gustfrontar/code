clear all
close all
%**************************************************************************
% Este script grafica resultados de la verificacion de la calibracion del
% pron�stico probabilistico del ensemble.
%**************************************************************************

path='d:/trabajos/TrabajoSLAF/precipitacion/slaf2006/matlab/';

archivo{1}=strcat(path,'slaf2006_verifprob_sur_gtscmo_24.mat');
archivo{2}=strcat(path,'slaf2006_verifprob_sur_gtscmo_48.mat');
archivo{3}=strcat(path,'slaf2006_verifprob_norte_gtscmo_24.mat');
archivo{4}=strcat(path,'slaf2006_verifprob_norte_gtscmo_48.mat');

%Las distintas calibraciones son:
% 1 - Pron�stico sin calibrar (democratico) en base a los miembros del
% esneble.
% 2 - Pron�stico calibrado seg�n Hamill y Colucci con calibracion estatica en base a gts.
% 3 - Pron�stico calibrado seg�n Hamill y Colucci con calibracion est�tica en base a cmorph.
% 4 - Pron�stico probabil�stico en base al control calibracion estatica en base a gts.
% 5 - Pron�stico probabil�stico en base al control calibracion estatica en base a cmorph.

for i=1:4
load(archivo{i});
reli{i}=reliability;
reli2{i}=reliability_2;
reli3{i}=reliability_3;
reli4{i}=reliability_4;
reli5{i}=reliability_5;
n{i}=n_forecast;
n2{i}=n_forecast_2;
n3{i}=n_forecast_3;
n4{i}=n_forecast_4;
n5{i}=n_forecast_5;
climp{i}=clim_prob;
bri{i}=brier;
bri2{i}=brier_2;
bri3{i}=brier_3;
bri4{i}=brier_4;
bri5{i}=brier_5;
ets_ensemble{i}=ets_ens;
hit_ensemble{i}=hit_ens;
far_ensemble{i}=far_ens;
bias_ensemble{i}=bias_ens;
hit_p{i}=hit;
far_p{i}=far;
area_p{i}=area;
ets_p{i}=ets;
bias_p{i}=bias;
hit_p2{i}=hit_2;
hit_p3{i}=hit_3;
hit_p4{i}=hit_4;
hit_p5{i}=hit_5;
far_p2{i}=far_2;
far_p3{i}=far_3;
far_p4{i}=far_4;
far_p5{i}=far_5;
area_p2{i}=area_2;
area_p3{i}=area_3;
area_p4{i}=area_4;
area_p5{i}=area_5;
ets_p2{i}=ets_2;
ets_p3{i}=ets_3;
ets_p4{i}=ets_4;
ets_p5{i}=ets_5;
bias_p2{i}=bias_2;
bias_p3{i}=bias_3;
bias_p4{i}=bias_4;
bias_p5{i}=bias_5;
area_ets_p{i}=area_ets;
area_ets_p2{i}=area_ets2;
area_ets_p3{i}=area_ets3;
area_ets_p4{i}=area_ets4;
area_ets_p5{i}=area_ets5;
reli_sup{i}=reliability_sup;
reli_inf{i}=reliability_inf;
bri_clim{i}=brier_clim;
bri_ens{i}=brier_ens;
bri_sup{i}=brier_sup;
bri_inf{i}=brier_inf;
max_ets{i}=ets_max;
min_ets{i}=ets_min;
max_bias{i}=bias_max;
min_bias{i}=bias_min;
max_hit{i}=hit_max;
min_hit{i}=hit_min;
max_far{i}=far_max;
min_far{i}=far_min;
mean_ets{i}=ets_mean;
mean_bias{i}=bias_mean;
mean_hit{i}=hit_mean;
mean_far{i}=far_mean;


end

umbral=[0.01 0.10 0.25 0.5 1 1.5 2]*25.4;

title_gra{1}='24 hour forecast south';
title_gra{2}='48 hour forecast south';
title_gra{3}='24 hour forecast north';
title_gra{4}='48 hour forecast north';



%**************************************************************************
% Reliability diagrams.
%**************************************************************************
posicion(1,:)=[0.19 0.77 0.11 0.13]; %Left bottom width heigth
posicion(2,:)=[0.63 0.77 0.11 0.13];
posicion(3,:)=[0.19 0.29 0.11 0.13];
posicion(4,:)=[0.63 0.29 0.11 0.13];


for i=1:length(umbral)
    textito=['umbral ',num2str(umbral(i))];
    figure
    for j=1:4
    no_resol_line(1:length(prob_ref))=climp{j}(i);
    subplot(2,2,j)

   plot(prob_ref,reli{j}(i,:),'k');
   %errorbar(prob_ref,reli{j}(i,:),reli_sup{j}(i,:)-reli{j}(i,:),-reli_inf{j}(i,:)+reli{j}(i,:));
   hold on;
   plot(prob_ref,reli_sup{j}(i,:),'k--');
   plot(prob_ref,reli_inf{j}(i,:),'k--');
   plot(prob_ref,reli2{j}(i,:),'r-');
   plot(prob_ref,reli3{j}(i,:),'b-');
   plot(prob_ref,reli4{j}(i,:),'rv');
   plot(prob_ref,reli5{j}(i,:),'bv');
   axis([0 1 0 1]);
   hold on;
   plot(prob_ref,no_resol_line,'g');
   plot(prob_ref,prob_ref,'g');
   title(title_gra{j})
   % Create axes
   axes('Position',posicion(j,:));
   plot(prob_ref,n{j}(i,:),'k')
   hold on
   plot(prob_ref,n2{j}(i,:),'r');
   plot(prob_ref,n3{j}(i,:),'b');
   annotation('textbox','Position',[0.42 0.00 0.16 0.06],'FitHeightToText','on','String',{textito});
  
    end
end

%**************************************************************************
% ROC diagrams.
%**************************************************************************
[filas columnas]=size(hit_ensemble{1});
   for i_umb=1:length(umbral)
   textito=['umbral ',num2str(umbral(i_umb))];
   figure
   for j=1:4
   no_skill=0:0.1:1;
   %far_random=ones(length(no_skill),1)*(1-climp{j}(i_umb));

   subplot(2,2,j)

   plot(far_p{j}(:,i_umb),hit_p{j}(:,i_umb),'k-');
   hold on;
   plot(far_p2{j}(:,i_umb),hit_p2{j}(:,i_umb),'r-');
   plot(far_p3{j}(:,i_umb),hit_p3{j}(:,i_umb),'b-');
   plot(far_p4{j}(:,i_umb),hit_p4{j}(:,i_umb),'rv');
   plot(far_p5{j}(:,i_umb),hit_p5{j}(:,i_umb),'bv');
   plot(no_skill,no_skill,'k-')
   plot(max_far{j}(:,i_umb),max_hit{j}(:,i_umb),'k--');
   plot(min_far{j}(:,i_umb),min_hit{j}(:,i_umb),'k--');

   legend('no cal','ens gts','ens cmo','ctrl gts','ctrl cmo','Location','NorthWest')

   axis([0 1 0 1]);
   hold on;
   title(title_gra{j})
   end
   annotation('textbox','Position',[0.42 0.00 0.16 0.06],'FitHeightToText','on','String',{textito});
   end
   
   
%**************************************************************************
%   GRAFICO BRIER SKILL SCORE.
%**************************************************************************

   [filas columnas]=size(bri_ens{1});
   figure
      for j=1:4
   subplot(2,2,j)
   bri_max=squeeze(max(bri_ens{j},[],2));
   bri_min=squeeze(min(bri_ens{j},[],2));
   bri_mean=bri_ens{j}(:,columnas);
   %Grafico el brier no calibrado
   plot(umbral,(bri{j}-bri_clim{j})./bri_clim{j},'k');
   hold on;
   %Grafico el brier maximo y minimo bootstrapeado.
   plot(umbral,(bri_sup{j}'-bri_clim{j})./bri_clim{j},'k--');
   plot(umbral,(bri_inf{j}'-bri_clim{j})./bri_clim{j},'k--');
   %Grafico el brier calibrado
   plot(umbral,(bri2{j}-bri_clim{j})./bri_clim{j},'r');
   plot(umbral,(bri3{j}-bri_clim{j})./bri_clim{j},'b');
   plot(umbral,(bri4{j}-bri_clim{j})./bri_clim{j},'rv');
   plot(umbral,(bri5{j}-bri_clim{j})./bri_clim{j},'bv');

   legend('no cal','ens gts','ens cmo','ctrl gts','ctrl cmo','Location','SouthEast')
   axis([0 umbral(length(umbral)) -0.5 0.5]);
   hold on;
   title(title_gra{j})
   
      end
      
%**************************************************************************
%   GRAFICOS ETS-BIAS.
%**************************************************************************     
      
   [filas columnas]=size(ets_ensemble{1});
   for i_umb=1:length(umbral)
   textito=['umbral ',num2str(umbral(i_umb))];
   figure
   for j=1:4


   subplot(2,2,j)

   plot(bias_p{j}(:,i_umb),ets_p{j}(:,i_umb),'k-');
   hold on;
   plot(bias_p2{j}(:,i_umb),ets_p2{j}(:,i_umb),'r-');
   plot(bias_p3{j}(:,i_umb),ets_p3{j}(:,i_umb),'b-');
   plot(bias_p4{j}(:,i_umb),ets_p4{j}(:,i_umb),'rv');
   plot(bias_p5{j}(:,i_umb),ets_p5{j}(:,i_umb),'bv');

   axis([0 3 0 0.7]);
   hold on;
   title(title_gra{j})
   end
   annotation('textbox','Position',[0.42 0.00 0.16 0.06],'FitHeightToText','on','String',{textito});
   end 
   
   
%**************************************************************************
%   GRAFICO AREA ROC.
%**************************************************************************

   figure
      for j=1:4
   subplot(2,2,j)

   %Grafico el area para el no calibrado
   plot(umbral,2*(area_p{j}-0.5),'k');
   hold on;
   plot(umbral,2*(area_p2{j}-0.5),'r-');
   plot(umbral,2*(area_p3{j}-0.5),'b-');
   plot(umbral,2*(area_p4{j}-0.5),'rv');
   plot(umbral,2*(area_p5{j}-0.5),'bv');
      
   legend('no cal','ens gts','ens cmo','ctrl gts','ctrl cmo','Location','SouthEast')
   axis([0 umbral(length(umbral)) 0 1]);
   hold on;
   title(title_gra{j})
   
      end
      
      
%**************************************************************************
%   GRAFICO AREA ETS-BIAS.
%**************************************************************************

   figure
      for j=1:4
   subplot(2,2,j)

   %Grafico el area para el no calibrado
   plot(umbral,(area_ets_p{j}),'k');
   hold on;
   plot(umbral,(area_ets_p2{j}),'r-');
   plot(umbral,(area_ets_p3{j}),'b-');
   plot(umbral,(area_ets_p4{j}),'rv');
   plot(umbral,(area_ets_p5{j}),'bv');
      
   legend('no cal','ens gts','ens cmo','ctrl gts','ctrl cmo','Location','SouthEast')
   axis([0 umbral(length(umbral)) 0 0.3]);
   hold on;
   title(title_gra{j})
   
      end
 
   