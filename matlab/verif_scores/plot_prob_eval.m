clear all
close all

%Evaluation of bias in the forecast.


 load model_data.mat
 load lluvia_obs_hires.mat
 load lat_lon.mat
% 
% 
% nforecast=size(lluvia_obs,3);
% ndays=size(lluvia_obs,4);
% 
% %Compare PDF.
% 
% bins=[1 20];  %Thresholds to compute FSS.
% 
% nbins=length(bins);
% 
% %Compute PROB.
% parea=40;
% for ii=1:8 %No hago todos los pronosticos por cuestiones de tiempo.
% for i_umb=1:length(bins)
%    ii
%    i_umb
%    tic
%    tmp_for=real(squeeze(lluvia_modelo(:,:,ii,:)) > bins(i_umb));
%    %tmp_obs=real(squeeze(obs(:,:,ii,:))      > bins(i_umb));
% 
%    prob_for=suaviza_fun(tmp_for,parea);
% 
%    tmp_obs=reshape(lluvia_obs(:,:,ii,:),[size(lluvia_obs,1)*size(lluvia_obs,2)*size(lluvia_obs,4) 1]);
%    tmp_prob=reshape(prob_for(:,:,:),[size(lluvia_obs,1)*size(lluvia_obs,2)*size(lluvia_obs,4) 1]);
%    [brier(ii,i_umb) breliability(ii,i_umb) bresolution(ii,i_umb) buncertainty(ii,i_umb) brier_clim(ii,i_umb)] = brier_fun(tmp_obs,tmp_prob,bins(i_umb),0.1);
%    [reliability(:,ii,i_umb) n_forecast(:,ii,i_umb) prob_ref(:,ii,i_umb)] = reliability_fun(tmp_obs,tmp_prob,bins(i_umb),0.1);
%    [hit(:,ii,i_umb) far(:,ii,i_umb) area(ii,i_umb) ets bias area_2] = roc_fun(tmp_obs,tmp_prob,bins(i_umb),[0.1:0.1:1]);
%    toc
% end  
% end
   
 
load('prob_4p.mat');

load mapas.mat
provincias(provincias(:,1) < 0,1)=360+provincias(provincias(:,1) < 0,1);
samerica(samerica(:,1) < 0,1)=360+samerica(samerica(:,1) < 0,1);
%FIGURES:

%COMPARACION VISUAL DE 2 CAMPOS
%==========================================================================
figure
subplot(1,2,1)

title('FORECAST')
pcolor(LON,LAT,lluvia_modelo(:,:,6,7))
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([1 2 5 10 15 20 30 40 50 60 75 100 125 150],[2 88:99 59 2])
hold on
plot(provincias(:,1),provincias(:,2),'-','LineWidth',1,'Color',[0.6 0.6 0.6])
plot(samerica(:,1),samerica(:,2),'-','LineWidth',2,'Color',[0.6 0.6 0.6])



subplot(1,2,2)
title('OBSERVATION')
pcolor(LON,LAT,lluvia_obs(:,:,6,7))
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([1 2 5 10 15 20 30 40 50 60 75 100 125 150],[2 88:99 59 2])
hold on
plot(provincias(:,1),provincias(:,2),'-','LineWidth',1,'Color',[0.6 0.6 0.6])
plot(samerica(:,1),samerica(:,2),'-','LineWidth',2,'Color',[0.6 0.6 0.6])


%Ploteamos los elementos de la tabla de contingencia.
umbral=10;
mascara_obs=real(lluvia_obs(:,:,6,7) > umbral);
mascara_mod=real(lluvia_modelo(:,:,6,7) > umbral);
prob_mod=suaviza_fun(mascara_mod,5);

figure

carga_mapa
lon_costa(lon_costa < 0)=lon_costa(lon_costa<0)+360;
lon_pais(lon_pais < 0)=lon_pais(lon_pais<0)+360;

pcolor(LON,LAT,prob_mod)
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([0:0.1:1],[2 88 89 90 91 92 93 94 95 96])
hold on
plot(provincias(:,1),provincias(:,2),'-','LineWidth',1,'Color',[0.6 0.6 0.6])
plot(samerica(:,1),samerica(:,2),'-','LineWidth',2,'Color',[0.6 0.6 0.6])


contour(LON,LAT,mascara_obs);
title('FORECAST AND VERIFICATION')



figure
pcolor(LON,LAT,prob_mod-mascara_obs)
xlabel('LON');ylabel('LAT')
shading flat
plot_jrcol_2009([-1 -0.8 -0.6 -0.4 -0.2 -0.1 0.1 0.2 0.4 0.6 0.8 1.0],[47 46 45 44 43 2 23 24 25 26 27])
hold on
plot(provincias(:,1),provincias(:,2),'-','LineWidth',1,'Color',[0.6 0.6 0.6])
plot(samerica(:,1),samerica(:,2),'-','LineWidth',2,'Color',[0.6 0.6 0.6])

title('DIFFERENCE')

%GRAFICOS DE RANK HISTOGRAM
%==========================================================================
figure
hour={'03hr';'06hr';'09hr';'12hr';'15hr';'18hr';'21hr';'24hr'},
for ii=4:4:size(rank_hist,1)
  subplot(1,2,ii/4)
  bar(rank_hist(ii,:)/sum(rank_hist(ii,:)),'b')
  xlabel('RANK');
  ylabel('Frequency');
  title(['Rank histogram ' hour{ii} ]);
  axis([0 enssize+2 0 0.2])
end



%GRAFICOS DE RELIABILITY DIAGRAM
%==========================================================================
figure
hour={'03hr';'06hr';'09hr';'12hr';'15hr';'18hr';'21hr';'24hr'},
for ii=4:4:size(rank_hist,1)
  subplot(2,2,ii/4)
  hold on
  plot(squeeze(prob_ref(:,ii,1)),squeeze(reliability(:,ii,1)),'b-','LineWidth',3)
  plot(squeeze(prob_ref(:,ii,2)),squeeze(reliability(:,ii,2)),'r-','LineWidth',3)
  if(ii==8);legend('1mm','20mm');end
  plot([0 1],[0 1],'k--','LineWidth',2)
  plot(squeeze(prob_ref(:,ii,1)),squeeze(reliability(:,ii,1)),'b-','LineWidth',3)
  plot(squeeze(prob_ref(:,ii,2)),squeeze(reliability(:,ii,2)),'r-','LineWidth',3)
  title(['Reliability diagram' hour{ii} ]) ;
  xlabel('Forecasted frequency')
  ylabel('Observed frequency') 
  axis([0 1 0 1])
  grid on
  subplot(2,2,ii/4+2)
  hold on
  plot(squeeze(prob_ref(:,ii,1)),squeeze(n_forecast(:,ii,1)./sum(n_forecast(:,ii,1))),'b-','LineWidth',3)
  plot(squeeze(prob_ref(:,ii,2)),squeeze(n_forecast(:,ii,2))./sum(n_forecast(:,ii,1)),'r-','LineWidth',3)
  if(ii==8);legend('1mm','20mm');end
  title(['Reliability diagram' hour{ii} ]) ;
  xlabel('Forecasted frequency')
  ylabel('Observed frequency')
  axis([0 1 0 1])
  grid on
end

%ROC DIAGRAM
%==========================================================================

figure
hour={'03hr';'06hr';'09hr';'12hr';'15hr';'18hr';'21hr';'24hr'},
for ii=4:4:size(rank_hist,1)
  subplot(1,2,ii/4)
  hold on
  plot(far(:,ii,1),hit(:,ii,1),'b-','LineWidth',3)
  plot(far(:,ii,2),hit(:,ii,2),'r-','LineWidth',3)
  if(ii==8);legend('1mm','20mm');end
  plot([0 1],[0 1],'k--','LineWidth',2)
  xlabel('False alarm ratio');
  ylabel('Hits');
  title(['ROC Diagram ' hour{ii} ]);
  axis([0 1 0 1])
end

%BSS
%==========================================================================

figure
hold on
plot(3:3:24,1-brier(:,1)./buncertainty(:,1),'bo-','LineWidth',3)
plot(3:3:24,1-brier(:,2)./buncertainty(:,2),'ro-','LineWidth',3)
legend('1mm','20mm')
grid on
xlabel('Time')
ylabel('Brier Skill Score')
title('Brier Skill Score')
axis([3 24 -0.5 0.5])



