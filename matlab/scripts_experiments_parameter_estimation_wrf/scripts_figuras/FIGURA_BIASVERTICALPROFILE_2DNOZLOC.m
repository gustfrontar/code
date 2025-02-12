clear all
close all


load ../EXPERIMENTOS/CONTROL40M_MEMNC/verification_fnl/rmse_bias.mat

BIASTCTRL=squeeze(nanmean(nanmean(abs(BIAST),2),1));
BIASUCTRL=squeeze(nanmean(nanmean(abs(BIASU),2),1));
BIASVCTRL=squeeze(nanmean(nanmean(abs(BIASV),2),1));
BIASQCTRL=squeeze(nanmean(nanmean(abs(BIASQ),2),1));
BIASHCTRL=squeeze(nanmean(nanmean(abs(BIASH),2),1));
BIASPCTRL=squeeze(nanmean(nanmean(abs(BIASP),2),1));

load ../EXPERIMENTOS/QFX2DNOZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

BIASTPE=squeeze(nanmean(nanmean(abs(BIAST),2),1));
BIASUPE=squeeze(nanmean(nanmean(abs(BIASU),2),1));
BIASVPE=squeeze(nanmean(nanmean(abs(BIASV),2),1));
BIASQPE=squeeze(nanmean(nanmean(abs(BIASQ),2),1));
BIASHPE=squeeze(nanmean(nanmean(abs(BIASH),2),1));
BIASPPE=squeeze(nanmean(nanmean(abs(BIASP),2),1));

zlevel=[1000 975 950 925 900 850 800 700 600 500 400 300 200 ];

figure

subplot(2,2,1)
hold on
set(gca,'FontSize',15)
plot(BIASTCTRL,-log(zlevel),'k-','LineWidth',3)
plot(BIASTPE,-log(zlevel),'b-','LineWidth',3)

set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('BIAS','FontSize',15)
title('Temperature','FontSize',15)
grid on
ylabel('Pressure','FontSize',15)

subplot(2,2,2)
hold on
set(gca,'FontSize',15)
plot(BIASQCTRL,-log(zlevel),'k-','LineWidth',3)
plot(BIASQPE,-log(zlevel),'b-','LineWidth',3)

set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
legend('CTRL','PARAMETER ESTIMATION','FontSize',15)
xlabel('BIAS')
title('Moisture','FontSize',15)
grid on
ylabel('Pressure','FontSize',15)

subplot(2,2,3)
hold on
set(gca,'FontSize',15)
plot(BIASUCTRL,-log(zlevel),'k-','LineWidth',3)
plot(BIASUPE,-log(zlevel),'b-','LineWidth',3)

set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('BIAS','FontSize',15)
title('U-Wind','FontSize',15)
grid on
ylabel('Pressure','FontSize',15)

subplot(2,2,4)
hold on
set(gca,'FontSize',15)
plot(BIASVCTRL,-log(zlevel),'k-','LineWidth',3)
plot(BIASVPE,-log(zlevel),'b-','LineWidth',3)

set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('BIAS','FontSize',15)
title('V-Wind','FontSize',15)
grid on
ylabel('Pressure','FontSize',15)



