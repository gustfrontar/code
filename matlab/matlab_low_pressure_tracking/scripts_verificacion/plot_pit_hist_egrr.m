clear all
close all

%Este script plotea la relacion entre el error y el spread para diferentes
%situaciones (segun hemisferio y estacion del anio).
%INCLUDE SPECIFIC FUNCTIONS DIRECTORY.
addpath('../common_functions_vpo/');

infile='../RESULTS/egrr/ERRORSPREAD/ERROR_SPREAD_2007040112_2010040112.mat';
outfile='../RESULTS/egrr/ERRORSPREAD/PITLONLAT_2007040112_2010040112.mat';

load(infile);

group.minlap=[];
group.minanom=[];
group.uvel=[];
group.vvel=[];
group.trajid=[];
group.minlat=[];
group.minlon=[];

enssize=size(group.distlatpert,1);
flength=size(group.distlonpert,2);


%CALCULO LOS HISTOGRAMAS DE RANGO PARA EL ERROR EN LATITUD Y EN LONGITUD.

for ii=1:flength
    ii
%TOTAL    
ensemble=(squeeze(group.distlonpert(:,ii,:)))';
obs=group.meandistlonerror(ii,:);
PitHistLon_total(:,ii)=compute_pit_histogram_fun(ensemble,obs);

ensemble=(squeeze(group.distlatpert(:,ii,:)))';
obs=group.meandistlaterror(ii,:);
PitHistLat_total(:,ii)=compute_pit_histogram_fun(ensemble,obs);

%HEMISFERIO NORTE
index=group.nhemi==1;
ensemble=(squeeze(group.distlonpert(:,ii,index)))';
obs=group.meandistlonerror(ii,index);
PitHistLon_nhemi(:,ii)=compute_pit_histogram_fun(ensemble,obs);

ensemble=(squeeze(group.distlatpert(:,ii,index)))';
obs=group.meandistlaterror(ii,index);
PitHistLat_nhemi(:,ii)=compute_pit_histogram_fun(ensemble,obs);

%HEMISFERIO SUR
index=group.shemi==1;
ensemble=(squeeze(group.distlonpert(:,ii,index)))';
obs=group.meandistlonerror(ii,index);
PitHistLon_shemi(:,ii)=compute_pit_histogram_fun(ensemble,obs);

ensemble=(squeeze(group.distlatpert(:,ii,index)))';
obs=group.meandistlaterror(ii,index);
PitHistLat_shemi(:,ii)=compute_pit_histogram_fun(ensemble,obs);

%ESTACION CALIDA
index=group.calida==1;
ensemble=(squeeze(group.distlonpert(:,ii,index)))';
obs=group.meandistlonerror(ii,index);
PitHistLon_calida(:,ii)=compute_pit_histogram_fun(ensemble,obs);

ensemble=(squeeze(group.distlatpert(:,ii,index)))';
obs=group.meandistlaterror(ii,index);
PitHistLat_calida(:,ii)=compute_pit_histogram_fun(ensemble,obs);

%ESTACION FRIA
index=group.fria==1;
ensemble=(squeeze(group.distlonpert(:,ii,index)))';
obs=group.meandistlonerror(ii,index);
PitHistLon_fria(:,ii)=compute_pit_histogram_fun(ensemble,obs);

ensemble=(squeeze(group.distlatpert(:,ii,index)))';
obs=group.meandistlaterror(ii,index);
PitHistLat_fria(:,ii)=compute_pit_histogram_fun(ensemble,obs);
end


%FIGURAS...

%TOTAL PARA DIFERENTES TIEMPOS TODOS EN UNA MISMA FIGURA.

figure
subplot(1,2,1)
hold on
plot(0.05:0.1:0.95,PitHistLon_total(:,1),'b','LineWidth',2)
plot(0.05:0.1:0.95,PitHistLon_total(:,9),'r','LineWidth',2)
plot(0.05:0.1:0.95,PitHistLon_total(:,17),'g','LineWidth',2)
plot(0.05:0.1:0.95,PitHistLon_total(:,29),'m','LineWidth',2)
axis([0 1 0 0.4])
legend('00hr','48hr','96hr','168hr')
set(gca,'YGrid','On')
xlabel('PIT')
ylabel('Relative frequency')
title('PIT histogram for the longitudes (All cases)')

subplot(1,2,2)
hold on
plot(0.05:0.1:0.95,PitHistLat_total(:,1),'b','LineWidth',2)
plot(0.05:0.1:0.95,PitHistLat_total(:,9),'r','LineWidth',2)
plot(0.05:0.1:0.95,PitHistLat_total(:,17),'g','LineWidth',2)
plot(0.05:0.1:0.95,PitHistLat_total(:,29),'m','LineWidth',2)
axis([0 1 0 0.4])
legend('00hr','48hr','96hr','168hr')
xlabel('PIT')
ylabel('Relative frequency')
set(gca,'YGrid','On')
title('PIT histogram for the latitudes (All cases)')

%HEMISFERIO SUR VS NORTE, 4 PANELES UNO PARA CADA TIEMPO.

%LONGITUDES
figure
subplot(2,2,1)
hold on
plot(0.05:0.1:0.95,PitHistLon_shemi(:,1),'b','LineWidth',2)
plot(0.05:0.1:0.95,PitHistLon_nhemi(:,1),'r','LineWidth',2)
axis([0 1 0 0.4])
legend('SH','NH')
xlabel('PIT')
ylabel('Relative frequency')
set(gca,'YGrid','On')
title('PIT histogram for the longitudes at 00hr lead time')

subplot(2,2,2)
hold on
plot(0.05:0.1:0.95,PitHistLon_shemi(:,9),'b','LineWidth',2)
plot(0.05:0.1:0.95,PitHistLon_nhemi(:,9),'r','LineWidth',2)
axis([0 1 0 0.4])
legend('SH','NH')
xlabel('PIT')
ylabel('Relative frequency')
set(gca,'YGrid','On')
title('PIT histogram for the longitudes at 48 hr lead time')

subplot(2,2,3)
hold on
plot(0.05:0.1:0.95,PitHistLon_shemi(:,17),'b','LineWidth',2)
plot(0.05:0.1:0.95,PitHistLon_nhemi(:,17),'r','LineWidth',2)
axis([0 1 0 0.4])
legend('SH','NH')
xlabel('PIT')
ylabel('Relative frequency')
set(gca,'YGrid','On')
title('PIT histogram for the longitudes at 96 hr lead time')

subplot(2,2,4)
hold on
plot(0.05:0.1:0.95,PitHistLon_shemi(:,29),'b','LineWidth',2)
plot(0.05:0.1:0.95,PitHistLon_nhemi(:,29),'r','LineWidth',2)
axis([0 1 0 0.4])
legend('SH','NH')
xlabel('PIT')
ylabel('Relative frequency')
set(gca,'YGrid','On')
title('PIT histogram for the longitudes at 168 hr lead time')

%LATITUDES
figure
subplot(2,2,1)
hold on
plot(0.05:0.1:0.95,PitHistLat_shemi(:,1),'b','LineWidth',2)
plot(0.05:0.1:0.95,PitHistLat_nhemi(:,1),'r','LineWidth',2)
axis([0 1 0 0.4])
legend('SH','NH')
xlabel('PIT')
ylabel('Relative frequency')
set(gca,'YGrid','On')
title('PIT histogram for the latitudes at 00hr lead time')

subplot(2,2,2)
hold on
plot(0.05:0.1:0.95,PitHistLat_shemi(:,9),'b','LineWidth',2)
plot(0.05:0.1:0.95,PitHistLat_nhemi(:,9),'r','LineWidth',2)
axis([0 1 0 0.4])
legend('SH','NH')
xlabel('PIT')
ylabel('Relative frequency')
set(gca,'YGrid','On')
title('PIT histogram for the latitudes at 48 hr lead time')

subplot(2,2,3)
hold on
plot(0.05:0.1:0.95,PitHistLat_shemi(:,17),'b','LineWidth',2)
plot(0.05:0.1:0.95,PitHistLat_nhemi(:,17),'r','LineWidth',2)
axis([0 1 0 0.4])
legend('SH','NH')
xlabel('PIT')
ylabel('Relative frequency')
set(gca,'YGrid','On')
title('PIT histogram for the latitudes at 96 hr lead time')

subplot(2,2,4)
hold on
plot(0.05:0.1:0.95,PitHistLat_shemi(:,29),'b','LineWidth',2)
plot(0.05:0.1:0.95,PitHistLat_nhemi(:,29),'r','LineWidth',2)
axis([0 1 0 0.4])
legend('SH','NH')
xlabel('PIT')
ylabel('Relative frequency')
set(gca,'YGrid','On')
title('PIT histogram for the latitudes at 168 hr lead time')


%ESTACION CALIDA VS ESTACION FRIA, 4 PANELES UNO PARA CADA TIEMPO.

figure
subplot(2,2,1)
hold on
plot(0.05:0.1:0.95,PitHistLon_fria(:,1),'b','LineWidth',2)
plot(0.05:0.1:0.95,PitHistLon_calida(:,1),'r','LineWidth',2)
axis([0 1 0 0.4])
legend('COLD','WARM')
xlabel('PIT')
ylabel('Relative frequency')
set(gca,'YGrid','On')
title('PIT histogram for the longitudes at 00hr lead time')

subplot(2,2,2)
hold on
plot(0.05:0.1:0.95,PitHistLon_fria(:,9),'b','LineWidth',2)
plot(0.05:0.1:0.95,PitHistLon_calida(:,9),'r','LineWidth',2)
axis([0 1 0 0.4])
legend('COLD','WARM')
xlabel('PIT')
ylabel('Relative frequency')
set(gca,'YGrid','On')
title('PIT histogram for the longitudes at 48 hr lead time')

subplot(2,2,3)
hold on
plot(0.05:0.1:0.95,PitHistLon_fria(:,17),'b','LineWidth',2)
plot(0.05:0.1:0.95,PitHistLon_calida(:,17),'r','LineWidth',2)
axis([0 1 0 0.4])
legend('COLD','WARM')
xlabel('PIT')
ylabel('Relative frequency')
set(gca,'YGrid','On')
title('PIT histogram for the longitudes at 96 hr lead time')

subplot(2,2,4)
hold on
plot(0.05:0.1:0.95,PitHistLon_fria(:,29),'b','LineWidth',2)
plot(0.05:0.1:0.95,PitHistLon_calida(:,29),'r','LineWidth',2)
axis([0 1 0 0.4])
legend('COLD','WARM')
xlabel('PIT')
ylabel('Relative frequency')
set(gca,'YGrid','On')
title('PIT histogram for the longitudes at 168 hr lead time')

clear group
clear ensemble obs
save(outfile)

 