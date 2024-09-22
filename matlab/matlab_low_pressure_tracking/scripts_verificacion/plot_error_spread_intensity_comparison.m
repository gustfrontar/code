clear all
close all
%situaciones (segun hemisferio y estacion del anio).

infile_ecmf='../RESULTS/ecmf/ERRORSPREAD/INTENSITYERROR_2007040112_2010040112.mat';
load(infile_ecmf);
meananomerror_total_ecmf=meananomerror_total;
ctrlanomerror_total_ecmf=ctrlanomerror_total;
meananombias_total_ecmf=meananombias_total;
anomsprd_total_ecmf=anomsprd_total;

infile_kwbc='../RESULTS/kwbc/ERRORSPREAD/INTENSITYERROR_2007040112_2010040112.mat';
load(infile_kwbc);
meananomerror_total_kwbc=meananomerror_total;
ctrlanomerror_total_kwbc=ctrlanomerror_total;
meananombias_total_kwbc=meananombias_total;
anomsprd_total_kwbc=anomsprd_total;

infile_kwbc='../RESULTS/egrr/ERRORSPREAD/INTENSITYERROR_2007040112_2010040112.mat';
load(infile_kwbc);
meananomerror_total_egrr=meananomerror_total;
ctrlanomerror_total_egrr=ctrlanomerror_total;
meananombias_total_egrr=meananombias_total;
anomsprd_total_egrr=anomsprd_total;

figure

hold on
horas=((1:29)-1)*6;
subplot(1,2,1)
hold on
plot(horas,meananomerror_total_ecmf,'r-','LineWidth',2);
plot(horas,meananomerror_total_kwbc,'b-','LineWidth',2);
plot(horas,meananomerror_total_egrr,'g-','LineWidth',2);
plot(horas,anomsprd_total_ecmf,'r--','LineWidth',2);
plot(horas,anomsprd_total_kwbc,'b--','LineWidth',2);
plot(horas,anomsprd_total_egrr,'g--','LineWidth',2);
set(gca,'XGrid','On','YGrid','On','FontSize',15)

legend('ECMWF','NCEP','UKMET')
xlabel('Lead time');
ylabel('Minimum geopotential anomaly error');

subplot(1,2,2)
hold on
plot(horas,meananombias_total_ecmf,'r-','LineWidth',2);
plot(horas,meananombias_total_kwbc,'b-','LineWidth',2);
plot(horas,meananombias_total_egrr,'g-','LineWidth',2);
set(gca,'XGrid','On','YGrid','On','FontSize',15)

xlabel('Lead time');
ylabel('Minimum geopotential anomaly bias');

print('-dpng','./figures/ERROR_SPREAD_ANOMALY_COMPARISON.png')
