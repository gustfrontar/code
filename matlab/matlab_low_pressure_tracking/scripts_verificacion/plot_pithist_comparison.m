clear all
close all
%Este script plotea la relacion entre el error y el spread para diferentes
%situaciones (segun hemisferio y estacion del anio).

EnsSizeThre=0.5;    %Ensambles mas chicos que este porcentaje del total no van a ser tenidos en cuenta en el calculo.


infile_ecmf='../RESULTS/ecmf/ERRORSPREAD/PITLONLAT_2007040112_2010040112.mat';
infile_kwbc='../RESULTS/kwbc/ERRORSPREAD/PITLONLAT_2007040112_2010040112.mat';
infile_egrr='../RESULTS/egrr/ERRORSPREAD/PITLONLAT_2007040112_2010040112.mat';

load(infile_ecmf);
pithistlon_total_ecmf=PitHistLon_total;
pithistlat_total_ecmf=PitHistLon_total;



load(infile_kwbc);
pithistlon_total_kwbc=PitHistLon_total;
pithistlat_total_kwbc=PitHistLon_total;


load(infile_egrr);
pithistlon_total_egrr=PitHistLon_total;
pithistlat_total_egrr=PitHistLon_total;


figure
subplot(1,3,1)
hold on
plot(0.05:0.1:0.95,pithistlon_total_ecmf(:,2),'r-','LineWidth',2);
plot(0.05:0.1:0.95,pithistlon_total_kwbc(:,2),'b-','LineWidth',2);
plot(0.05:0.1:0.95,pithistlon_total_egrr(:,2),'g-','LineWidth',2);

axis([0 1 0 0.5])
set(gca,'XGrid','On','YGrid','On','FontSize',15);
xlabel('PIT');
ylabel('Frequency')
title('(a)')

subplot(1,3,2)
hold on
plot(0.05:0.1:0.95,pithistlon_total_ecmf(:,14),'r-','LineWidth',2);
plot(0.05:0.1:0.95,pithistlon_total_kwbc(:,14),'b-','LineWidth',2);
plot(0.05:0.1:0.95,pithistlon_total_egrr(:,14),'g-','LineWidth',2);

axis([0 1 0 0.5])
set(gca,'XGrid','On','YGrid','On','FontSize',15);
xlabel('PIT');
ylabel('Frequency')
title('(b)')

subplot(1,3,3)
hold on
plot(0.05:0.1:0.95,pithistlon_total_ecmf(:,29),'r-','LineWidth',2);
plot(0.05:0.1:0.95,pithistlon_total_kwbc(:,29),'b-','LineWidth',2);
plot(0.05:0.1:0.95,pithistlon_total_egrr(:,29),'g-','LineWidth',2);

axis([0 1 0 0.5])
set(gca,'XGrid','On','YGrid','On','FontSize',15);
xlabel('PIT');
ylabel('Frequency')
title('(c)')

print('-dpng','./figures/PITHIST_COMPARISON.png')










