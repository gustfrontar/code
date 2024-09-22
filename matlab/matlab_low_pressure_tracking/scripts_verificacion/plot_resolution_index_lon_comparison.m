clear all
close all


%Este script plotea la relacion entre el error y el spread para diferentes
%situaciones (segun hemisferio y estacion del anio).
addpath('../common_functions_vpo/');
EnsSizeThre=0.5;    %Ensambles mas chicos que este porcentaje del total no van a ser tenidos en cuenta en el calculo.

filein_kwbc='../RESULTS/kwbc/ERRORSPREAD/LONERRORINDEX_2007040112_2010040112.mat';
filein_ecmf='../RESULTS/ecmf/ERRORSPREAD/LONERRORINDEX_2007040112_2010040112.mat';
filein_egrr='../RESULTS/egrr/ERRORSPREAD/LONERRORINDEX_2007040112_2010040112.mat';

load(filein_kwbc)
IRSSLON_TOTAL_KWBC=IRSSLON_TOTAL;
SIRSSLON_TOTAL_KWBC=SIRSSLON_TOTAL;
BETA_TOTAL_KWBC=BETA_TOTAL;
MEANS_TOTAL_KWBC=MEANS_TOTAL;
R_TOTAL_KWBC=R_TOTAL;
SLOPE_TOTAL_KWBC=SLOPE_TOTAL;


load(filein_ecmf)
IRSSLON_TOTAL_ECMF=IRSSLON_TOTAL;
SIRSSLON_TOTAL_ECMF=SIRSSLON_TOTAL;
BETA_TOTAL_ECMF=BETA_TOTAL;
MEANS_TOTAL_ECMF=MEANS_TOTAL;
R_TOTAL_ECMF=R_TOTAL;
SLOPE_TOTAL_ECMF=SLOPE_TOTAL;

load(filein_egrr)
IRSSLON_TOTAL_EGRR=IRSSLON_TOTAL;
SIRSSLON_TOTAL_EGRR=SIRSSLON_TOTAL;
BETA_TOTAL_EGRR=BETA_TOTAL;
MEANS_TOTAL_EGRR=MEANS_TOTAL;
R_TOTAL_EGRR=R_TOTAL;
SLOPE_TOTAL_EGRR=SLOPE_TOTAL;


%FIGURA IRSS PARA EL TOTAL
figure
hold on
horas=((1:29)-1)*6;
plot(horas(2:end),IRSSLON_TOTAL_ECMF(1,2:end)','r-','LineWidth',2)
plot(horas(2:end),IRSSLON_TOTAL_KWBC(1,2:end)','b-','LineWidth',2)
plot(horas(2:end),IRSSLON_TOTAL_EGRR(1,2:end)','g-','LineWidth',2)
plot(horas(2:end),IRSSLON_TOTAL_ECMF(3,2:end)','r--','LineWidth',2)
plot(horas(2:end),IRSSLON_TOTAL_KWBC(3,2:end)','b--','LineWidth',2)
plot(horas(2:end),IRSSLON_TOTAL_EGRR(3,2:end)','g--','LineWidth',2)
axis([0 168 0 1])
legend('ECMWF','NCEP','UKMET')
title('IRSS for the total sample')
set(gca,'XGrid','On','YGrid','On','FontSize',15)
xlabel('Lead time')
ylabel('IRSS')
print('-dpng','./figures/RESOLUTION_INDEX_LON_COMPARISON.png')

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
axis([0 168 0 1])


subplot(1,2,2)
hold on
horas=((1:29)-1)*6;
plot(horas(2:end),SLOPE_TOTAL_ECMF(2:end)','r-','LineWidth',2)
plot(horas(2:end),SLOPE_TOTAL_KWBC(2:end)','b-','LineWidth',2)
plot(horas(2:end),SLOPE_TOTAL_EGRR(2:end)','g-','LineWidth',2)
title('(b)','FontSize',15)
axis([0 168 0 1.0])
set(gca,'XGrid','On','YGrid','On','FontSize',15)
xlabel('Lead time')
ylabel('SLOPE')
print('-dpng','./figures/CORRELATION_LON_COMPARISON.png')

%FIGURA BETA Y LA MEDIA DE LA DISTRIBUCION.
figure
hold on
horas=((1:29)-1)*6;

plot(horas,BETA_TOTAL_ECMF,'r-','LineWidth',2)
plot(horas,BETA_TOTAL_KWBC,'b-','LineWidth',2)
plot(horas(2:end),BETA_TOTAL_EGRR(2:end),'g-','LineWidth',2)

set(gca,'XGrid','On','YGrid','On','FontSize',15)
xlabel('Lead time')
ylabel('alfa and beta')

plot(horas,MEANS_TOTAL_ECMF/10,'r--','LineWidth',2)
plot(horas,MEANS_TOTAL_KWBC/10,'b--','LineWidth',2)
plot(horas(2:end),MEANS_TOTAL_EGRR(2:end)/10,'g--','LineWidth',2)
legend('ECMWF','NCEP','UKMET')
axis([0 168 0 1.5])
set(gca,'XGrid','On','YGrid','On','FontSize',15)
xlabel('Lead time')
print('-dpng','./figures/BETA_LON_COMPARISON.png')

