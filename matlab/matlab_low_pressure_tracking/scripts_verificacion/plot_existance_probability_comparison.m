clear all
close all

%Este script plotea la relacion entre el error y el spread para diferentes
%situaciones (segun hemisferio y estacion del anio).
addpath('../common_functions_vpo/');

addpath('../common_functions_vpo/');

infile_ecmf='../RESULTS/ecmf/ERRORSPREAD/PROB_2007040112_2010040112.mat';
infile_kwbc='../RESULTS/kwbc/ERRORSPREAD/PROB_2007040112_2010040112.mat';
infile_egrr='../RESULTS/egrr/ERRORSPREAD/PROB_2007040112_2010040112.mat';

load(infile_ecmf);
brier_total_ecmf=brier_total;
breli_total_ecmf=breli_total;
bresol_total_ecmf=bresol_total;
bun_total_ecmf=bun_total;
reliability_total_ecmf=reliability_total;
n_forecast_total_ecmf=n_forecast_total;
mean_probability_ecmf=mean_probability;
mean_existance_ecmf=mean_existance;

load(infile_kwbc);
brier_total_kwbc=brier_total;
breli_total_kwbc=breli_total;
bresol_total_kwbc=bresol_total;
bun_total_kwbc=bun_total;
reliability_total_kwbc=reliability_total;
n_forecast_total_kwbc=n_forecast_total;
mean_probability_kwbc=mean_probability;
mean_existance_kwbc=mean_existance;

load(infile_egrr);
brier_total_egrr=brier_total;
breli_total_egrr=breli_total;
bresol_total_egrr=bresol_total;
bun_total_egrr=bun_total;
reliability_total_egrr=reliability_total;
n_forecast_total_egrr=n_forecast_total;
mean_probability_egrr=mean_probability;
mean_existance_egrr=mean_existance;

%FIGURA RELIABILITY DIAGRAM PARA 3 TIEMPOS DIFERENTES.

figure
subplot(2,3,1)
hold on
plot(prob_ref(2:end),reliability_total_ecmf(1,2:end),'r-','LineWidth',2)
plot(prob_ref(2:end),reliability_total_kwbc(1,2:end),'b-','LineWidth',2)
plot(prob_ref(2:end),reliability_total_egrr(1,2:end),'g-','LineWidth',2)
plot(prob_ref,prob_ref,'k--','LineWidth',2)
set(gca,'XGrid','On','YGrid','On','FontSize',15)
xlabel('Forecasted probability')
ylabel('Observed frequency')
legend('ECMWF','NCEP','UKMET')

axis([0 1 0 1])

subplot(2,3,2)
hold on
plot(prob_ref(2:end),reliability_total_ecmf(15,2:end),'r-','LineWidth',2)
plot(prob_ref(2:end),reliability_total_kwbc(15,2:end),'b-','LineWidth',2)
plot(prob_ref(2:end),reliability_total_egrr(15,2:end),'g-','LineWidth',2)
plot(prob_ref,prob_ref,'k--','LineWidth',2)
set(gca,'XGrid','On','YGrid','On','FontSize',15)
xlabel('Forecasted probability')
ylabel('Observed frequency')
axis([0 1 0 1])

subplot(2,3,3)
hold on
plot(prob_ref(2:end),reliability_total_ecmf(29,2:end),'r-','LineWidth',2)
plot(prob_ref(2:end),reliability_total_kwbc(29,2:end),'b-','LineWidth',2)
plot(prob_ref(2:end),reliability_total_egrr(29,2:end),'g-','LineWidth',2)
plot(prob_ref,prob_ref,'k--','LineWidth',2)
set(gca,'XGrid','On','YGrid','On','FontSize',15)
xlabel('Forecasted probability')
ylabel('Observed frequency')
axis([0 1 0 1])

subplot(2,3,4)
hold on
plot(prob_ref(2:end),n_forecast_total_ecmf(1,2:end)/sum(n_forecast_total_ecmf(1,2:end)),'r-','LineWidth',2)
plot(prob_ref(2:end),n_forecast_total_kwbc(1,2:end)/sum(n_forecast_total_kwbc(1,2:end)),'b-','LineWidth',2)
plot(prob_ref(2:end),n_forecast_total_egrr(1,2:end)/sum(n_forecast_total_egrr(1,2:end)),'g-','LineWidth',2)
axis([0 1 0 1])
set(gca,'XGrid','On','YGrid','On','FontSize',15)
xlabel('Probability')
ylabel('Frequency of issue')

subplot(2,3,5)
hold on
plot(prob_ref(2:end),n_forecast_total_ecmf(15,2:end)/sum(n_forecast_total_ecmf(1,2:end)),'r-','LineWidth',2)
plot(prob_ref(2:end),n_forecast_total_kwbc(15,2:end)/sum(n_forecast_total_kwbc(1,2:end)),'b-','LineWidth',2)
plot(prob_ref(2:end),n_forecast_total_egrr(15,2:end)/sum(n_forecast_total_egrr(1,2:end)),'g-','LineWidth',2)
axis([0 1 0 1])
set(gca,'XGrid','On','YGrid','On','FontSize',15)
xlabel('Probability')
ylabel('Frequency of issue')

subplot(2,3,6)
hold on
plot(prob_ref(2:end),n_forecast_total_ecmf(29,2:end)/sum(n_forecast_total_ecmf(1,2:end)),'r-','LineWidth',2)
plot(prob_ref(2:end),n_forecast_total_kwbc(29,2:end)/sum(n_forecast_total_kwbc(1,2:end)),'b-','LineWidth',2)
plot(prob_ref(2:end),n_forecast_total_egrr(29,2:end)/sum(n_forecast_total_egrr(1,2:end)),'g-','LineWidth',2)
axis([0 1 0 1])
set(gca,'XGrid','On','YGrid','On','FontSize',15)
xlabel('Probability')
ylabel('Frequency of issue')

print('-dpng','./figures/RELIABILITYDIAGRAM_EXISTANCE_COMPARISON.png')


%FULL SAMPLE BSS
figure
subplot(1,3,1)
hold on
plot(horas,1-brier_total_ecmf./bun_total_ecmf,'r','LineWidth',2);
plot(horas,1-brier_total_kwbc./bun_total_kwbc,'b','LineWidth',2);
plot(horas,1-brier_total_egrr./bun_total_egrr,'g','LineWidth',2);
set(gca,'XGrid','On','YGrid','On','FontSize',15)
xlabel('Lead time')
ylabel('BSS')
axis([0 168 -0.1 0.5])
legend('ECMWF','NCEP')
title('(a)')
subplot(1,3,2)
hold on
plot(horas,-breli_total_ecmf./bun_total_ecmf,'r','LineWidth',2)
plot(horas,-breli_total_kwbc./bun_total_kwbc,'b','LineWidth',2)
plot(horas,-breli_total_egrr./bun_total_egrr,'g','LineWidth',2)
set(gca,'XGrid','On','YGrid','On','FontSize',15)
xlabel('Lead time')
ylabel('RELIABILITY')
axis([0 168 -0.5 0.0])
title('(b)')
subplot(1,3,3)
hold on
plot(horas,-bresol_total_ecmf./bun_total_ecmf,'r','LineWidth',2)
plot(horas,-bresol_total_kwbc./bun_total_kwbc,'b','LineWidth',2)
plot(horas,-bresol_total_egrr./bun_total_egrr,'g','LineWidth',2)
set(gca,'XGrid','On','YGrid','On','FontSize',15)
ylabel('RESOLUTION')
axis([0 168 0 0.5])
title('(c)')
xlabel('Lead time')

print('-dpng','./figures/BRIER_EXISTANCE_COMPARISON.png')



%FULL SAMPLE MEAN PROBABILITY AND MEAN ANALYSIS ASOCIATION.

figure
hold on
plot(horas,mean_probability_ecmf,'r-','LineWidth',2)
plot(horas,mean_probability_kwbc,'b-','LineWidth',2)
plot(horas,mean_probability_egrr,'g-','LineWidth',2)

plot(horas,mean_existance_ecmf,'r--','LineWidth',2)
plot(horas,mean_existance_kwbc,'b--','LineWidth',2)
plot(horas,mean_existance_egrr,'g--','LineWidth',2)

set(gca,'XGrid','On','YGrid','On','FontSize',15)

axis([0 168 0 1])
xlabel('Lead time')
ylabel('Probability')

print('-dpng','./figures/MEANPROBABILITY_EXISTANCE_COMPARISON.png')


