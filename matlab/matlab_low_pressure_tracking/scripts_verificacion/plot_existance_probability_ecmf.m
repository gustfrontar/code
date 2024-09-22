clear all
close all

%Este script plotea la relacion entre el error y el spread para diferentes
%situaciones (segun hemisferio y estacion del anio).
addpath('../common_functions_vpo/');

filein='../RESULTS/ecmf/ERRORSPREAD/ERROR_SPREAD_2007040112_2010040112.mat';
outfile='../RESULTS/ecmf/ERRORSPREAD/PROB_2007040112_2010040112.mat';

load(filein)

group.minanom=[];
group.minlap=[];
group.uvel=[];
group.vvel=[];
group.trajid=[];
group.minlon=[];
group.distlatpert=[];
group.distlonpert=[];
enssize=size(group.minlon,1);
flength=size(group.minlon,2);

%==========================================================================
% CANTIDAD DE TRAYECTORIAS DE CADA MIEMBRO EN EL ENSAMBLE.
%==========================================================================

probabilidad=squeeze( mean(~isnan(group.minlat),1));
existance=double(~isnan(group.minlatanalysis));

for ii=1:enssize
   
   tmp=squeeze(~isnan(group.minlat(ii,:,:)));
   ntrajectorias(ii,:)=mean(tmp,2);
 
end

pclim=mean(existance,2);

%Calculos previos
tmpprobability=probabilidad;
tmpexistance=existance;


mask=( tmpprobability==0 ); 
tmpprobability(mask)=NaN;
tmpexistance(mask)=NaN;

%==========================================================================
%FIGURA NUMERO DE TRAYECTORIAS
%==========================================================================
horas=((1:29)-1)*6;
figure
%plot(ntrajectorias');
axis([0 168 0 1])

mean_probability=mean(probabilidad,2);
mean_existance  =mean(existance,2);

hold on
plot(horas,mean_probability,'k--','LineWidth',2)
plot(horas,mean_existance,'b--','LineWidth',2)
xlabel('Lead time')
ylabel('Association probability')
%==========================================================================

%Para la full sample
probability_total=tmpprobability;
existance_total=tmpexistance;

%Para el hemisferio sur
index=group.shemi==1;
probability_shemi=tmpprobability(:,index);
existance_shemi=tmpexistance(:,index);


%Para el hemisferio norte
index=group.nhemi==1;
probability_nhemi=tmpprobability(:,index);
existance_nhemi=tmpexistance(:,index);

%Para la estacion calida
index=group.calida==1;
probability_calida=tmpprobability(:,index);
existance_calida=tmpexistance(:,index);

%Para la estacion fria
index=group.fria==1;
probability_fria=tmpprobability(:,index);
existance_fria=tmpexistance(:,index);


%==========================================================================
% RELIABILITY DIAGRAM PARA LA PREDICCION PROBABILISTICA DE LA OCURRENCIA
% DEL SISTEMA
%==========================================================================
%Compute probability.


%RELIABILITY DIAGRAM
for ii=1:size(probability_total,1)
[reliability_total(ii,:),n_forecast_total(ii,:),prob_ref] = reliability_fun(existance_total(ii,:),probability_total(ii,:),0.1);
[reliability_shemi(ii,:),n_forecast_shemi(ii,:),prob_ref] = reliability_fun(existance_shemi(ii,:),probability_shemi(ii,:),0.1);
[reliability_nhemi(ii,:),n_forecast_nhemi(ii,:),prob_ref] = reliability_fun(existance_nhemi(ii,:),probability_nhemi(ii,:),0.1);
[reliability_calida(ii,:),n_forecast_calida(ii,:),prob_ref] = reliability_fun(existance_calida(ii,:),probability_calida(ii,:),0.1);
[reliability_fria(ii,:),n_forecast_fria(ii,:),prob_ref] = reliability_fun(existance_fria(ii,:),probability_fria(ii,:),0.1);
end

a=jet(size(reliability_total,1));
icolor=1; 

%TOTAL
figure
subplot(1,2,1)
for ii=1:4:size(reliability_total,1)
hold on
plot(prob_ref(2:end),reliability_total(ii,2:end)','LineWidth',2,'Color',a(ii,:))
end
legend('00','24','48','72','96','120','144','168');
xlabel('Forecasted probability')
ylabel('Observed frequency')
set(gca,'XGrid','On','YGrid','On')
plot([0 1],[0 1],'k--','LineWidth',2);

subplot(1,2,2)
for ii=1:4:size(reliability_total,1)
hold on
plot(prob_ref(2:end),n_forecast_total(ii,2:end)'/sum(n_forecast_total(ii,2:end)),'LineWidth',2,'Color',a(ii,:))
xlabel('Forecasted probability')
ylabel('Forecast issue frequency')
set(gca,'XGrid','On','YGrid','On')
end
legend('00','24','48','72','96','120','144','168');

%SOUTHERN HEMISPHERE
figure
subplot(1,2,1)
for ii=1:4:size(reliability_total,1)
hold on
plot(prob_ref(2:end),reliability_shemi(ii,2:end)','LineWidth',2,'Color',a(ii,:))
end
legend('00','24','48','72','96','120','144','168');
xlabel('Forecasted probability')
ylabel('Observed frequency')
set(gca,'XGrid','On','YGrid','On')
plot([0 1],[0 1],'k--','LineWidth',2);

subplot(1,2,2)
for ii=1:4:size(reliability_total,1)
hold on
plot(prob_ref(2:end),n_forecast_shemi(ii,2:end)'/sum(n_forecast_shemi(ii,2:end)),'LineWidth',2,'Color',a(ii,:))
xlabel('Forecasted probability')
ylabel('Forecast issue frequency')
set(gca,'XGrid','On','YGrid','On')
end
legend('00','24','48','72','96','120','144','168');

%NORTHERN HEMISPHERE
figure
subplot(1,2,1)
for ii=1:4:size(reliability_total,1)
hold on
plot(prob_ref(2:end),reliability_nhemi(ii,2:end)','LineWidth',2,'Color',a(ii,:))
end
legend('00','24','48','72','96','120','144','168');
xlabel('Forecasted probability')
ylabel('Observed frequency')
set(gca,'XGrid','On','YGrid','On')
plot([0 1],[0 1],'k--','LineWidth',2);

subplot(1,2,2)
for ii=1:4:size(reliability_total,1)
hold on
plot(prob_ref(2:end),n_forecast_nhemi(ii,2:end)'/sum(n_forecast_nhemi(ii,2:end)),'LineWidth',2,'Color',a(ii,:))
xlabel('Forecasted probability')
ylabel('Forecast issue frequency')
set(gca,'XGrid','On','YGrid','On')
end
legend('00','24','48','72','96','120','144','168');

%==========================================================================
% BSS AND ITS DECOMPOSITION
%==========================================================================

for ii=1:size(probability_total,1)
[brier_total(ii) breli_total(ii) bresol_total(ii) bun_total(ii)]=brier_fun(existance_total(ii,:)',probability_total(ii,:)',0.5,0.1);
[brier_shemi(ii) breli_shemi(ii) bresol_shemi(ii) bun_shemi(ii)]=brier_fun(existance_shemi(ii,:)',probability_shemi(ii,:)',0.5,0.1);
[brier_nhemi(ii) breli_nhemi(ii) bresol_nhemi(ii) bun_nhemi(ii)]=brier_fun(existance_nhemi(ii,:)',probability_nhemi(ii,:)',0.5,0.1);
[brier_calida(ii) breli_calida(ii) bresol_calida(ii) bun_calida(ii)]=brier_fun(existance_calida(ii,:)',probability_calida(ii,:)',0.5,0.1);
[brier_fria(ii) breli_fria(ii) bresol_fria(ii) bun_fria(ii)]=brier_fun(existance_fria(ii,:)',probability_fria(ii,:)',0.5,0.1);
end

%FULL SAMPLE AND BY HEMISPHERE
figure
subplot(1,3,1)
hold on
plot(horas,1-brier_total./bun_total,'k','LineWidth',2);
plot(horas,1-brier_shemi./bun_shemi,'r','LineWidth',2);
plot(horas,1-brier_nhemi./bun_nhemi,'b','LineWidth',2);
xlabel('Lead time')
set(gca,'XGrid','On','YGrid','On')
ylabel('BSS')
axis([0 168 -0.1 0.5])
title('BRIER SKILL SCORE')

subplot(1,3,2)
hold on
plot(horas,-breli_total./bun_total,'k','LineWidth',2)
plot(horas,-breli_shemi./bun_shemi,'r','LineWidth',2)
plot(horas,-breli_nhemi./bun_nhemi,'b','LineWidth',2)
xlabel('Lead time')
set(gca,'XGrid','On','YGrid','On')
ylabel('RELIABILITY')
axis([0 168 -0.5 0.0])
title('RELIABILITY')
subplot(1,3,3)
hold on
plot(horas,-bresol_total./bun_total,'k','LineWidth',2)
plot(horas,-bresol_shemi./bun_shemi,'r','LineWidth',2)
plot(horas,-bresol_nhemi./bun_nhemi,'b','LineWidth',2)
xlabel('Lead time')
set(gca,'XGrid','On','YGrid','On')
ylabel('RESOLUTION')
axis([0 168 0 0.5])
title('RESOLUTION')

%FULL SAMPLE AND BY SEASON
figure
subplot(1,3,1)
hold on
plot(horas,1-brier_total./bun_total,'k','LineWidth',2);
plot(horas,1-brier_calida./bun_calida,'r','LineWidth',2);
plot(horas,1-brier_fria./bun_fria,'b','LineWidth',2);
xlabel('Lead time')
set(gca,'XGrid','On','YGrid','On')
ylabel('BSS')
axis([0 168 -0.1 0.5])
title('BRIER SKILL SCORE')

subplot(1,3,2)
hold on
plot(horas,-breli_total./bun_total,'k','LineWidth',2)
plot(horas,-breli_calida./bun_calida,'r','LineWidth',2)
plot(horas,-breli_fria./bun_fria,'b','LineWidth',2)
xlabel('Lead time')
set(gca,'XGrid','On','YGrid','On')
ylabel('RELIABILITY')
axis([0 168 -0.5 0.0])
title('RELIABILITY')
subplot(1,3,3)
hold on
plot(horas,-bresol_total./bun_total,'k','LineWidth',2)
plot(horas,-bresol_calida./bun_calida,'r','LineWidth',2)
plot(horas,-bresol_fria./bun_fria,'b','LineWidth',2)
xlabel('Lead time')
set(gca,'XGrid','On','YGrid','On')
ylabel('RESOLUTION')
axis([0 168 0 0.5])
title('RESOLUTION')

%==========================================================================
% ROC y ROCAREA
%==========================================================================

for ii=1:size(probability_total,1)
[hit_total(ii,:) far_total(ii,:) area_total(ii) ]=roc_fun(existance_total(ii,:),probability_total(ii,:),0.1);
[hit_shemi(ii,:) far_shemi(ii,:) area_shemi(ii)]=roc_fun(existance_total(ii,:),probability_total(ii,:),0.1);
[hit_nhemi(ii,:) far_nhemi(ii,:) area_nhemi(ii)]=roc_fun(existance_total(ii,:),probability_total(ii,:),0.1);
[hit_calida(ii,:) far_calida(ii,:) area_calida(ii)]=roc_fun(existance_total(ii,:),probability_total(ii,:),0.1);
[hit_fria(ii,:) far_fria(ii,:) area_fria(ii)]=roc_fun(existance_total(ii,:),probability_total(ii,:),0.1);
end




clear group
clear tmpmean tmpctrl tmpmemb tmpsprd
clear pertlat_total pertlat_shemi pertlat_nhemi 
clear pertlon_total pertlon_shemi pertlon_nhemi
clear meanerrorlon_total meanerrorlat_total meanerrorlon_shemi meanerrorlat_shemi
clear meanerrorlat_nhemi meanerrorlon_nhemi
clear errorcovmatrix_total errorcovmatrix_nhemi errorcovmatrix_shemi
clear meanerrorlonideal_nhemi meanerrorlatideal_nhemi 
clear meanerrorlonideal_shemi meanerrorlatideal_shemi
clear coverror_nhemi coverror_shemi coverror_total
clear coverrorideal_nhemi coverrorideal_shemi coverrorideal_total
clear tmpprobability tmpexistance

save(outfile)

