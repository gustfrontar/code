clear all
close all


%Este script plotea la relacion entre el error y el spread para diferentes
%situaciones (segun hemisferio y estacion del anio).
addpath('../common_functions_vpo/');
EnsSizeThre=0.5;    %Ensambles mas chicos que este porcentaje del total no van a ser tenidos en cuenta en el calculo.

filein_kwbc='../RESULTS/kwbc/ERRORSPREAD/LATERRORINDEX_2007040112_2010040112.mat';
filein_ecmf='../RESULTS/ecmf/ERRORSPREAD/LATERRORINDEX_2007040112_2010040112.mat';

load(filein_kwbc)
IRSSLAT_TOTAL_KWBC=IRSSLAT_TOTAL;
SIRSSLAT_TOTAL_KWBC=SIRSSLAT_TOTAL;


load(filein_ecmf)
IRSSLAT_TOTAL_ECMF=IRSSLAT_TOTAL;
SIRSSLAT_TOTAL_ECMF=SIRSSLAT_TOTAL;



%FIGURA IRSS PARA EL TOTAL
figure
hold on
horas=((1:29)-1)*6;
plot(horas,IRSSLAT_TOTAL_KWBC','LineWidth',2)
plot(horas,IRSSLAT_TOTAL_ECMF','--','LineWidth',2)
legend('75% ERROR','50% ERROR','25% ERROR')
title('IRSS for the total sample')
set(gca,'XGrid','On','YGrid','On')
xlabel('Lead time')
ylabel('IRSS')



