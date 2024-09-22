%Este script plotea la relacion entre el error y el spread para diferentes
%situaciones (segun hemisferio y estacion del anio).
clear all
close all



EnsSizeThre=0.5;    %Ensambles mas chicos que este porcentaje del total no van a ser tenidos en cuenta en el calculo.


infile_ecmf='../RESULTS/ecmf/ERRORSPREAD/DISTERROR_2007040112_2010040112.mat';


load(infile_ecmf);
meandist_total_ecmf=meandist_total;
meandist_shemi_ecmf=meandist_shemi;
meandist_nhemi_ecmf=meandist_nhemi;

meandist_calida_ecmf=meandist_calida;
meandist_fria_ecmf=meandist_fria;

distsprd_total_ecmf=distsprd_total;
distsprd_shemi_ecmf=distsprd_shemi;
distsprd_nhemi_ecmf=distsprd_nhemi;

distsprd_calida_ecmf=distsprd_calida;
distsprd_fria_ecmf=distsprd_fria;

figure
horas=((1:29)-1)*6;
subplot(1,2,1)
hold on
plot(horas,meandist_shemi_ecmf,'r-','LineWidth',2);
plot(horas,meandist_nhemi_ecmf,'b-','LineWidth',2);

plot(horas,distsprd_shemi_ecmf,'r--','LineWidth',2);
plot(horas,distsprd_nhemi_ecmf,'b--','LineWidth',2);

axis([0 168 0 7e5])
set(gca,'XGrid','On','YGrid','On','FontSize',15);
xlabel('Lead time');
ylabel('Total position error')
legend('SH','NH')
title('(a)')

horas=((1:29)-1)*6;
subplot(1,2,2)
hold on
plot(horas,meandist_calida_ecmf,'r-','LineWidth',2);
plot(horas,meandist_fria_ecmf,'b-','LineWidth',2);

plot(horas,distsprd_calida_ecmf,'r--','LineWidth',2);
plot(horas,distsprd_fria_ecmf,'b--','LineWidth',2);
legend('Warm','Cold')
axis([0 168 0 7e5])
set(gca,'XGrid','On','YGrid','On','FontSize',15);
xlabel('Lead time');
ylabel('Total position error')
title('(b)')

print('-dpng','./figures/ERROR_SPREAD_SENSITIVITY_SHNH.png')










