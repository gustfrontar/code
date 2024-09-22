clear all
close all


%Este script plotea la relacion entre el error y el spread para diferentes
%situaciones (segun hemisferio y estacion del anio).

EnsSizeThre=0.5;    %Ensambles mas chicos que este porcentaje del total no van a ser tenidos en cuenta en el calculo.


infile_ecmf='../RESULTS/ecmf/ERRORSPREAD/DISTERRORLON_2007040112_2010040112.mat';
infile_kwbc='../RESULTS/kwbc/ERRORSPREAD/DISTERRORLON_2007040112_2010040112.mat';
infile_egrr='../RESULTS/egrr/ERRORSPREAD/DISTERRORLON_2007040112_2010040112.mat';

load(infile_ecmf);
ctrlbiaslon_total_ecmf=ctrlbias_total;
ctrlbiaslon_shemi_ecmf=ctrlbias_shemi;
ctrlbiaslon_nhemi_ecmf=ctrlbias_nhemi;
load(infile_kwbc);
ctrlbiaslon_total_kwbc=ctrlbias_total;
ctrlbiaslon_shemi_kwbc=ctrlbias_shemi;
ctrlbiaslon_nhemi_kwbc=ctrlbias_nhemi;

load(infile_egrr);
ctrlbiaslon_total_egrr=ctrlbias_total;
ctrlbiaslon_shemi_egrr=ctrlbias_shemi;
ctrlbiaslon_nhemi_egrr=ctrlbias_nhemi;

infile_ecmf='../RESULTS/ecmf/ERRORSPREAD/DISTERRORLAT_2007040112_2010040112.mat';
infile_kwbc='../RESULTS/kwbc/ERRORSPREAD/DISTERRORLAT_2007040112_2010040112.mat';
infile_egrr='../RESULTS/egrr/ERRORSPREAD/DISTERRORLAT_2007040112_2010040112.mat';

load(infile_ecmf);
ctrlbiaslat_total_ecmf=ctrlbias_total;
ctrlbiaslat_shemi_ecmf=ctrlbias_shemi;
ctrlbiaslat_nhemi_ecmf=ctrlbias_nhemi;

load(infile_kwbc);
ctrlbiaslat_total_kwbc=ctrlbias_total;
ctrlbiaslat_shemi_kwbc=ctrlbias_shemi;
ctrlbiaslat_nhemi_kwbc=ctrlbias_nhemi;

load(infile_egrr);
ctrlbiaslat_total_egrr=ctrlbias_total;
ctrlbiaslat_shemi_egrr=ctrlbias_shemi;
ctrlbiaslat_nhemi_egrr=ctrlbias_nhemi;

figure
horas=((1:29)-1)*6;
subplot(1,2,1)
hold on
plot(horas,ctrlbiaslon_total_ecmf,'r-','LineWidth',2);
plot(horas,ctrlbiaslon_total_kwbc,'b-','LineWidth',2);
plot(horas,ctrlbiaslon_total_egrr,'g-','LineWidth',2);

% plot(horas,ctrlbiaslon_nhemi_ecmf,'r--','LineWidth',2);
% plot(horas,ctrlbiaslon_nhemi_kwbc,'b--','LineWidth',2);
% plot(horas,ctrlbiaslon_nhemi_egrr,'g--','LineWidth',2);

axis([0 168 -5e4 5e4])
set(gca,'XGrid','On','YGrid','On','FontSize',15);
xlabel('Lead time');
ylabel('W-E bias');
ylabel('Total position error')
legend('ECMWF','NCEP','UKMET')
title('(a)')

subplot(1,2,2)
hold on
plot(horas,ctrlbiaslat_total_ecmf,'r-','LineWidth',2);
plot(horas,ctrlbiaslat_total_kwbc,'b-','LineWidth',2);
plot(horas,ctrlbiaslat_total_egrr,'g-','LineWidth',2);
% 
% plot(horas,ctrlbiaslat_nhemi_ecmf,'r--','LineWidth',2);
% plot(horas,ctrlbiaslat_nhemi_kwbc,'b--','LineWidth',2);
% plot(horas,ctrlbiaslat_nhemi_egrr,'g--','LineWidth',2);

set(gca,'XGrid','On','YGrid','On','FontSize',15);
xlabel('Lead time');
ylabel('S-N bias')
title('(b)')
axis([0 168 -5e4 5e4])

print('-dpng','./figures/BIAS_LONLAT_COMPARISON.png')










