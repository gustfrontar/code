clear all
close all

load ../EXPERIMENTOS/FORECAST_CONTROL40M_MEMNC/verification_cmorph/ets_bias.mat

BIASCTRL=bias;
ETSCTRL=ets;

load ../EXPERIMENTOS/FORECAST_QFX2DZLOC40M_MEMNC/verification_cmorph/ets_bias.mat

BIASPE=bias;
ETSPE=ets;


accum_times=[6 12 24];
umbral=[1 5 10 20 50];

figure
subplot(1,3,1)
hold on
set(gca,'FontSize',15)
title('24 hr forecast ')
plot(umbral,ETSCTRL{3}(:,1),'k-','LineWidth',3)
plot(umbral,ETSPE{3}(:,1),'b-','LineWidth',3)

axis([min(umbral) max(umbral) 0 0.3])
xlabel('Threshold (mm)');ylabel('ETS')
legend('CTRL','Parameter estimation')
grid on

subplot(1,3,2)
hold on
set(gca,'FontSize',15)
title('48 hr forecast ')
plot(umbral,ETSCTRL{3}(:,2),'k-','LineWidth',3)
plot(umbral,ETSPE{3}(:,2),'b-','LineWidth',3)

xlabel('Threshold (mm)');ylabel('ETS')
axis([min(umbral) max(umbral) 0 0.3])
grid on

subplot(1,3,3)
hold on
set(gca,'FontSize',15)
title('72 hr forecast')
plot(umbral,ETSCTRL{3}(:,3),'k-','LineWidth',3)
plot(umbral,ETSPE{3}(:,3),'b-','LineWidth',3)

xlabel('Threshold (mm)');ylabel('ETS')
axis([min(umbral) max(umbral) 0 0.3])
grid on

figure
subplot(1,3,1)
hold on
set(gca,'FontSize',15)
title('24 hr forecast ')
plot(umbral,BIASCTRL{3}(:,1),'k-','LineWidth',3)
plot(umbral,BIASPE{3}(:,1),'b-','LineWidth',3)
axis([min(umbral) max(umbral) 0.5 2.0])
xlabel('Threshold (mm)');ylabel('BIAS')
%legend('0D W. Vert. loc.','0D W/O Vert. Loc.','2D W. Vert. loc','0D W/O Vert. Loc.','CTRL')
grid on

subplot(1,3,2)
hold on
set(gca,'FontSize',15)
title('48 hr forecast ')
plot(umbral,BIASCTRL{3}(:,2),'k-','LineWidth',3)
plot(umbral,BIASPE{3}(:,2),'b-','LineWidth',3)

xlabel('Threshold (mm)');ylabel('BIAS')
axis([min(umbral) max(umbral) 0.5 2.0])
grid on

subplot(1,3,3)
hold on
set(gca,'FontSize',15)
title('72 hr forecast')
plot(umbral,BIASCTRL{3}(:,3),'k-','LineWidth',3)
plot(umbral,BIASPE{3}(:,3),'b-','LineWidth',3)

xlabel('Threshold (mm)');ylabel('BIAS')
axis([min(umbral) max(umbral) 0.5 2.0])
grid on
