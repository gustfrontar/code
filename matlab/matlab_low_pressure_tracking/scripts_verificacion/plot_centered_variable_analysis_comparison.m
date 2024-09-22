

clear all
close all

%EN ESTE SCRIPT VAMOS A HACER UN ANALYSIS DE LA RELACION SPREAD ERROR
%USANDO LA VARIABLE CENTRADA. PRIMERO EL ANALYSIS TRADICIONAL PARA TESTEAR
%LA RELIABILITY (QUE VA A INDICAR UN UNDERDISPERSION DEL ENSAMBLE). LUEGO
%UN ANALYSIS POR NIVELES, ES DECIR PARA DIFERENTES VALORES DEL SPREAD
%CALCULAR LA VARIABLE CENTRADA Y VER QUE PASA. SI EXISTE RELACION ENTRE EL
%SPREAD Y EL ERROR, LA VARIANZA Y LA MEDIA DE LA VARIABLE CENTRADA NO
%DEBERIAN SER FUNCION DEL SPREAD.

addpath('../common_functions_vpo/');

infile_ecmf='../RESULTS/ecmf/ERRORSPREAD/RCRV_2007040112_2010040112.mat';
infile_kwbc='../RESULTS/kwbc/ERRORSPREAD/RCRV_2007040112_2010040112.mat';
infile_egrr='../RESULTS/egrr/ERRORSPREAD/RCRV_2007040112_2010040112.mat';

load(infile_ecmf);
cent_sprdlat_total_ecmf=cent_sprdlat_total;
cent_sprdlon_total_ecmf=cent_sprdlon_total;
full_sprdlat_total_ecmf=full_sprdlat_total;
full_sprdlon_total_ecmf=full_sprdlon_total;
full_meanlat_total_ecmf=full_meanlat_total;
full_meanlon_total_ecmf=full_meanlon_total;


load(infile_kwbc);
cent_sprdlat_total_kwbc=cent_sprdlat_total;
cent_sprdlon_total_kwbc=cent_sprdlon_total;
full_sprdlat_total_kwbc=full_sprdlat_total;
full_sprdlon_total_kwbc=full_sprdlon_total;
full_meanlat_total_kwbc=full_meanlat_total;
full_meanlon_total_kwbc=full_meanlon_total;

load(infile_egrr);
cent_sprdlat_total_egrr=cent_sprdlat_total;
cent_sprdlon_total_egrr=cent_sprdlon_total;
full_sprdlat_total_egrr=full_sprdlat_total;
full_sprdlon_total_egrr=full_sprdlon_total;
full_meanlat_total_egrr=full_meanlat_total;
full_meanlon_total_egrr=full_meanlon_total;


group.minanom=[];
group.minlap=[];
group.uvel=[];
group.vvel=[];
group.trajid=[];
group.minlat=[];
group.minlon=[];
enssize=size(group.minlon,1);
flength=size(group.minlon,2);

%for ii=1:size(group.distlonspread,2)
%group.distlonspreadsmooth(:,ii)=smooth(group.distlonspread(:,ii),9);
%end
%==========================================================================
%PLOTS
%CENTERED VARIABLE MEAN AS A FUNCTION OF THE ENSEMBLE SPREAD.

%LAT
horas=((1:29)-1)*6;
figure
subplot(1,2,1)
hold on
plot(horas,full_sprdlat_total_ecmf,'r-','LineWidth',2)
plot(horas,full_sprdlat_total_kwbc,'b-','LineWidth',2)
plot(horas,full_sprdlat_total_egrr,'g-','LineWidth',2)
axis([0 168 0 7])
set(gca,'XGrid','On','YGrid','On','FontSize',15)
legend('ECMWF','NCEP','UKMET');
xlabel('Lead time')
ylabel('RCRV Spread')
title('(a)')
subplot(1,2,2)
hold on
plot(horas,full_meanlat_total_ecmf,'r-','LineWidth',2)
plot(horas,full_meanlat_total_kwbc,'b-','LineWidth',2)
plot(horas,full_meanlat_total_egrr,'g-','LineWidth',2)
axis([0 168 -1 1])
set(gca,'XGrid','On','YGrid','On','FontSize',15)
xlabel('Lead time')
ylabel('RCRV Mean')
title('(b)')

print('-dpng','./figures/RCRVLAT_COMPARISON.png')

%LAT
horas=((1:29)-1)*6;
figure
subplot(1,2,1)
hold on
plot(horas,full_sprdlon_total_ecmf,'r-','LineWidth',2)
plot(horas,full_sprdlon_total_kwbc,'b-','LineWidth',2)
plot(horas,full_sprdlon_total_egrr,'g-','LineWidth',2)
axis([0 168 0 7])
set(gca,'XGrid','On','YGrid','On','FontSize',15)
legend('ECMWF','NCEP','UKMET');
xlabel('Lead time')
ylabel('RCRV Spread')
title('(a)')
subplot(1,2,2)
hold on
plot(horas,full_meanlon_total_ecmf,'r-','LineWidth',2)
plot(horas,full_meanlon_total_kwbc,'b-','LineWidth',2)
plot(horas,full_meanlon_total_egrr,'g-','LineWidth',2)
axis([0 168 -1 1])
set(gca,'XGrid','On','YGrid','On','FontSize',15)
xlabel('Lead time')
ylabel('RCRV Mean')
title('(b)')

print('-dpng','./figures/RCRVLON_COMPARISON.png')

