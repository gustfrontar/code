clear all
close all
%Este script grafica las trajectorias obtenidas con los experimentos HFX,
%QFX y UST.


%==========================================================================
% GRAFICO COMPARATIVO DE LAS TRAYECTORIAS.
%==========================================================================

load TRAJECTORIES_HFX.mat
group_hfx=group;
gh_hfx=3; %Que grupo corresponde al huracan.

load TRAJECTORIES_QFX.mat
group_qfx=group;
gh_qfx=4;

load TRAJECTORIES_UST.mat
group_ust=group;
gh_ust=3;

load coast


%%%%%%%%%%%%%%%%%%% TRAJECTORY PLOT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

minlon=min([min(min(group_hfx.minlon(:,:,gh_hfx)))  min(min(group_qfx.minlon(:,:,gh_qfx))) min(min(group_ust.minlon(:,:,gh_ust))) ]);
minlat=min([min(min(group_hfx.minlat(:,:,gh_hfx)))  min(min(group_qfx.minlat(:,:,gh_qfx))) min(min(group_ust.minlat(:,:,gh_ust))) ]);
maxlon=max([max(max(group_hfx.minlon(:,:,gh_hfx)))  max(max(group_qfx.minlon(:,:,gh_qfx))) max(max(group_ust.minlon(:,:,gh_ust))) ]);
maxlat=max([max(max(group_hfx.minlat(:,:,gh_hfx)))  max(max(group_qfx.minlat(:,:,gh_qfx))) max(max(group_ust.minlat(:,:,gh_ust))) ]);
figure

subplot(1,3,1)
hold on
for ii=1:size(group_hfx.minlat,1) %Do over different parameter values.
    a=[0.07 0.07 0.07];
    b=[0 0 0];
    color=(ii-1)*a+b;
    plot(group_hfx.minlon(ii,:,gh_hfx),group_hfx.minlat(ii,:,gh_hfx),'-','Color',color,'LineWidth',2)    
end
plot(long,lat,'b','LineWidth',1.5)
axis([minlon-3 maxlon+3 minlat-3 maxlat+3])
title('Trayectories HFX perturbation')
xlabel('LON');ylabel('LAT')
legend('HFXF 0.5','0.6','0.7','0.8','0.9','1.0','1.1','1.2','1.3','1.4','1.5')

subplot(1,3,2)
hold on
for ii=1:size(group_qfx.minlat,1) %Do over different parameter values.
    a=[0.07 0.07 0.07];
    b=[0 0 0];
    color=(ii-1)*a+b;
    plot(group_qfx.minlon(ii,:,gh_qfx),group_qfx.minlat(ii,:,gh_qfx),'-','Color',color,'LineWidth',2)    
end
plot(long,lat,'b','LineWidth',1.5)
axis([minlon-3 maxlon+3 minlat-3 maxlat+3])
title('Trayectories QFX perturbation')
xlabel('LON');ylabel('LAT')
%legend('QFXF 0.5','0.6','0.7','0.8','0.9','1.0','1.1','1.2','1.3','1.4','1.5')

subplot(1,3,3)
hold on
for ii=1:size(group_ust.minlat,1) %Do over different parameter values.
    a=[0.07 0.07 0.07];
    b=[0 0 0];
    color=(ii-1)*a+b;
    plot(group_ust.minlon(ii,:,gh_ust),group_ust.minlat(ii,:,gh_ust),'-','Color',color,'LineWidth',2)    
end
plot(long,lat,'b','LineWidth',1.5)
axis([minlon-3 maxlon+3 minlat-3 maxlat+3])
title('Trayectories UST perturbation')
xlabel('LON');ylabel('LAT')
%legend('USTF 0.5','0.6','0.7','0.8','0.9','1.0','1.1','1.2','1.3','1.4','1.5')



%%%%%%%%%%%%%%%%%%% HURRICAN INTENSITY PLOT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


figure

subplot(1,3,1)
hold on
for ii=1:size(group_hfx.minlat,1) %Do over different parameter values.
    time=1:size(group_hfx.minlat,2);
    a=[0.07 0.07 0.07];
    b=[0 0 0];
    color=(ii-1)*a+b;
    plot(time,group_hfx.minanom(ii,:,gh_hfx),'-','Color',color,'LineWidth',2)    
end
title('Min pressure HFX perturbation')
ylabel('Pressure (hPa)');xlabel('Time (hours)')
legend('HFXF 0.5','0.6','0.7','0.8','0.9','1.0','1.1','1.2','1.3','1.4','1.5')
set(gca,'XGrid','On','YGrid','On')
axis([ 0 42 930 1000 ])

subplot(1,3,2)
hold on
for ii=1:size(group_qfx.minlat,1) %Do over different parameter values.
    a=[0.07 0.07 0.07];
    b=[0 0 0];
    color=(ii-1)*a+b;
    plot(time,group_qfx.minanom(ii,:,gh_qfx),'-','Color',color,'LineWidth',2)    
end
title('Min pressure QFX perturbation')
ylabel('Pressure (hPa)');xlabel('Time (hours)')
set(gca,'XGrid','On','YGrid','On')
axis([ 0 42 930 1000 ])
%legend('QFXF 0.5','0.6','0.7','0.8','0.9','1.0','1.1','1.2','1.3','1.4','1.5')

subplot(1,3,3)
hold on
for ii=1:size(group_ust.minlat,1) %Do over different parameter values.
    a=[0.07 0.07 0.07];
    b=[0 0 0];
    color=(ii-1)*a+b;
    plot(time,group_ust.minanom(ii,:,gh_ust),'-','Color',color,'LineWidth',2)    
end
title('Min pressure UST perturbation')
ylabel('Pressure (hPa)');xlabel('Time (hours)')
set(gca,'XGrid','On','YGrid','On')
axis([ 0 42 930 1000 ])
%legend('USTF 0.5','0.6','0.7','0.8','0.9','1.0','1.1','1.2','1.3','1.4','1.5')



%%%%%%%%%%%%%%%%%%% HURRICANE MEAN/MAX WINDS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


figure

subplot(1,3,1)
hold on
for ii=1:size(group_hfx.minlat,1) %Do over different parameter values.
    time=1:size(group_hfx.minlat,2);
    a=[0.07 0.07 0.07];
    b=[0 0 0];
    color=(ii-1)*a+b;
    plot(time,group_hfx.maxwind(ii,:,gh_hfx),'-','Color',color,'LineWidth',2)    
end
title('Max winds HFX perturbation')
ylabel('Wind (m/s)');xlabel('Time (hours)')
legend('HFXF 0.5','0.6','0.7','0.8','0.9','1.0','1.1','1.2','1.3','1.4','1.5')
set(gca,'XGrid','On','YGrid','On')

subplot(1,3,2)
hold on
for ii=1:size(group_qfx.minlat,1) %Do over different parameter values.
    a=[0.07 0.07 0.07];
    b=[0 0 0];
    color=(ii-1)*a+b
    plot(time,group_qfx.maxwind(ii,:,gh_qfx),'-','Color',color,'LineWidth',2)    
end
title('Max winds QFX perturbation')
ylabel('Wind (m/s)');xlabel('Time (hours)')
set(gca,'XGrid','On','YGrid','On')
%legend('QFXF 0.5','0.6','0.7','0.8','0.9','1.0','1.1','1.2','1.3','1.4','1.5')

subplot(1,3,3)
hold on
for ii=1:size(group_ust.minlat,1) %Do over different parameter values.
    a=[0.07 0.07 0.07];
    b=[0 0 0];
    color=(ii-1)*a+b
    plot(time,group_ust.maxwind(ii,:,gh_ust),'-','Color',color,'LineWidth',2)    
end
title('Max winds UST perturbation')
ylabel('Wind (m/s)');xlabel('Time (hours)')
set(gca,'XGrid','On','YGrid','On')
%legend('USTF 0.5','0.6','0.7','0.8','0.9','1.0','1.1','1.2','1.3','1.4','1.5')


figure

subplot(1,3,1)
hold on
for ii=1:size(group_hfx.minlat,1) %Do over different parameter values.
    time=1:size(group_hfx.minlat,2);
    a=[0.07 0.07 0.07];
    b=[0 0 0];
    color=(ii-1)*a+b;
    plot(time,group_hfx.meanwind(ii,:,gh_hfx),'-','Color',color,'LineWidth',2)    
end
title('Mean winds HFX perturbation')
ylabel('Wind (m/s)');xlabel('Time (hours)')
legend('HFXF 0.5','0.6','0.7','0.8','0.9','1.0','1.1','1.2','1.3','1.4','1.5')
set(gca,'XGrid','On','YGrid','On')

subplot(1,3,2)
hold on
for ii=1:size(group_qfx.minlat,1) %Do over different parameter values.
    a=[0.07 0.07 0.07];
    b=[0 0 0];
    color=(ii-1)*a+b
    plot(time,group_qfx.meanwind(ii,:,gh_qfx),'-','Color',color,'LineWidth',2)    
end
title('Mean winds QFX perturbation')
ylabel('Wind (m/s)');xlabel('Time (hours)')
set(gca,'XGrid','On','YGrid','On')
%legend('QFXF 0.5','0.6','0.7','0.8','0.9','1.0','1.1','1.2','1.3','1.4','1.5')

subplot(1,3,3)
hold on
for ii=1:size(group_ust.minlat,1) %Do over different parameter values.
    a=[0.07 0.07 0.07];
    b=[0 0 0];
    color=(ii-1)*a+b
    plot(time,group_ust.menawind(ii,:,gh_ust),'-','Color',color,'LineWidth',2)    
end
title('Mean winds UST perturbation')
ylabel('Wind (m/s)');xlabel('Time (hours)')
set(gca,'XGrid','On','YGrid','On')
%legend('USTF 0.5','0.6','0.7','0.8','0.9','1.0','1.1','1.2','1.3','1.4','1.5')
