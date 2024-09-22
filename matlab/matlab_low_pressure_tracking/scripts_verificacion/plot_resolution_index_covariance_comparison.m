clear all
close all


%Este script plotea la relacion entre el error y el spread para diferentes
%situaciones (segun hemisferio y estacion del anio).
addpath('../common_functions_vpo/');
EnsSizeThre=0.5;    %Ensambles mas chicos que este porcentaje del total no van a ser tenidos en cuenta en el calculo.

filein_kwbc='../RESULTS/kwbc/ERRORSPREAD/COVERRORINDEX_2007040112_2010040112.mat';
filein_ecmf='../RESULTS/ecmf/ERRORSPREAD/COVERRORINDEX_2007040112_2010040112.mat';
filein_egrr='../RESULTS/egrr/ERRORSPREAD/COVERRORINDEX_2007040112_2010040112.mat';

load(filein_kwbc)
IRSS_TOTAL_KWBC=IRSS_TOTAL;
SIRSS_TOTAL_KWBC=SIRSS_TOTAL;
R_TOTAL_KWBC=R_TOTAL;
SLOPE_TOTAL_KWBC=SLOPE_TOTAL;


load(filein_ecmf)
IRSS_TOTAL_ECMF=IRSS_TOTAL;
SIRSS_TOTAL_ECMF=SIRSS_TOTAL;
R_TOTAL_ECMF=R_TOTAL;
SLOPE_TOTAL_ECMF=SLOPE_TOTAL;

load(filein_egrr)
IRSS_TOTAL_EGRR=IRSS_TOTAL;
SIRSS_TOTAL_EGRR=SIRSS_TOTAL;
R_TOTAL_EGRR=R_TOTAL;
SLOPE_TOTAL_EGRR=SLOPE_TOTAL;



%FIGURA IRSS PARA EL TOTAL
figure
hold on
horas=((1:29)-1)*6;
plot(horas(2:end),IRSS_TOTAL_ECMF(1,2:end)','r-','LineWidth',2)
plot(horas(2:end),IRSS_TOTAL_KWBC(1,2:end)','b-','LineWidth',2)
plot(horas(2:end),IRSS_TOTAL_EGRR(1,2:end),'g-','LineWidth',2)

%plot(horas(2:end),IRSS_TOTAL_ECMF(3,2:end)','r--','LineWidth',2)
%plot(horas(2:end),IRSS_TOTAL_KWBC(3,2:end)','b--','LineWidth',2)
%plot(horas(2:end),IRSS_TOTAL_EGRR(3,2:end),'g--','LineWidth',2);
legend('ECMWF','NCEP','UKMET')
xlabel('Lead time','FontSize',15)
ylabel('IRSS','FontSize',15)
set(gca,'XGrid','On','YGrid','On','FontSize',15)

print('-dpng','./figures/RESOLUTION_INDEX_COVARIANCELATLON_COMPARISON.png')

%FIGURA R Y SLOPE PARA LA MUESTRA TOTAL.
figure
subplot(1,2,1)
hold on
horas=((1:29)-1)*6;
plot(horas(2:end),R_TOTAL_ECMF(2:end)','r-','LineWidth',2)
plot(horas(2:end),R_TOTAL_KWBC(2:end)','b-','LineWidth',2)
plot(horas(2:end),R_TOTAL_EGRR(2:end)','g-','LineWidth',2)
title('(a)','FontSize',15)
set(gca,'XGrid','On','YGrid','On','FontSize',15)
legend('ECMWF','NCEP','UKMET')
xlabel('Lead time')
ylabel('R')

subplot(1,2,2)
hold on
horas=((1:29)-1)*6;
plot(horas(2:end),SLOPE_TOTAL_ECMF(2:end)','r-','LineWidth',2)
plot(horas(2:end),SLOPE_TOTAL_KWBC(2:end)','b-','LineWidth',2)
plot(horas(2:end),SLOPE_TOTAL_EGRR(2:end)','g-','LineWidth',2)
title('(b)','FontSize',15)
set(gca,'XGrid','On','YGrid','On','FontSize',15)
xlabel('Lead time')
ylabel('SLOPE')

print('-dpng','./figures/CORRELATION_COVARIANCELATLON_COMPARISON.png')

