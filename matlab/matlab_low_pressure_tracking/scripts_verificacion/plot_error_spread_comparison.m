%Este script plotea la relacion entre el error y el spread para diferentes
%situaciones (segun hemisferio y estacion del anio).

EnsSizeThre=0.5;    %Ensambles mas chicos que este porcentaje del total no van a ser tenidos en cuenta en el calculo.


infile_ecmf='../RESULTS/ecmf/ERRORSPREAD/DISTERROR_2007040112_2010040112.mat';
infile_kwbc='../RESULTS/kwbc/ERRORSPREAD/DISTERROR_2007040112_2010040112.mat';
infile_egrr='../RESULTS/egrr/ERRORSPREAD/DISTERROR_2007040112_2010040112.mat';

load(infile_ecmf);
meandist_total_ecmf=meandist_total;
ctrldist_total_ecmf=ctrldist_total;
distsprd_total_ecmf=distsprd_total;



load(infile_kwbc);
meandist_total_kwbc=meandist_total;
ctrldist_total_kwbc=ctrldist_total;
distsprd_total_kwbc=distsprd_total;


load(infile_egrr);
meandist_total_egrr=meandist_total;
ctrldist_total_egrr=ctrldist_total;
distsprd_total_egrr=distsprd_total;


figure
horas=((1:29)-1)*6;
subplot(1,2,1)
hold on
plot(horas,meandist_total_ecmf,'r-','LineWidth',2);
plot(horas,meandist_total_kwbc,'b-','LineWidth',2);
plot(horas,meandist_total_egrr,'g-','LineWidth',2);
plot(horas,ctrldist_total_ecmf,'r--','LineWidth',2);
plot(horas,ctrldist_total_kwbc,'b--','LineWidth',2);
plot(horas,ctrldist_total_egrr,'g--','LineWidth',2);
axis([0 168 0 9e5])
set(gca,'XGrid','On','YGrid','On','FontSize',15);
xlabel('Lead time');
ylabel('Total position error')
title('(b)')

subplot(1,2,2)
hold on
plot(horas,meandist_total_ecmf,'r-','LineWidth',2);
plot(horas,meandist_total_kwbc,'b-','LineWidth',2);
plot(horas,meandist_total_egrr,'g-','LineWidth',2);
plot(horas,distsprd_total_ecmf,'r-.','LineWidth',2);
plot(horas,distsprd_total_kwbc,'b-.','LineWidth',2);
plot(horas,distsprd_total_egrr,'g-.','LineWidth',2);
set(gca,'XGrid','On','YGrid','On','FontSize',15);
xlabel('Lead time');
title('(b)')
axis([0 168 0 9e5])

print('-dpng','./figures/ERROR_SPREAD_COMPARISON.png')










