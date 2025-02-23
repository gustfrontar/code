clear all
close all


load ../EXPERIMENTOS/CONTROL40M_MEMNC/verification_fnl/rmse_bias.mat

BIASTCTRL=squeeze(nanmean(nanmean(abs(BIAST),2),1));
BIASUCTRL=squeeze(nanmean(nanmean(abs(BIASU),2),1));
BIASVCTRL=squeeze(nanmean(nanmean(abs(BIASV),2),1));
BIASQCTRL=squeeze(nanmean(nanmean(abs(BIASQ),2),1));
BIASHCTRL=squeeze(nanmean(nanmean(abs(BIASH),2),1));
BIASPCTRL=squeeze(nanmean(nanmean(abs(BIASP),2),1));

load ../EXPERIMENTOS/QFX0DNOZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

BIAST0DNZ=squeeze(nanmean(nanmean(abs(BIAST),2),1));
BIASU0DNZ=squeeze(nanmean(nanmean(abs(BIASU),2),1));
BIASV0DNZ=squeeze(nanmean(nanmean(abs(BIASV),2),1));
BIASQ0DNZ=squeeze(nanmean(nanmean(abs(BIASQ),2),1));
BIASH0DNZ=squeeze(nanmean(nanmean(abs(BIASH),2),1));
BIASP0DNZ=squeeze(nanmean(nanmean(abs(BIASP),2),1));

load ../EXPERIMENTOS/QFX0DZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

BIAST0D=squeeze(nanmean(nanmean(abs(BIAST),2),1));
BIASU0D=squeeze(nanmean(nanmean(abs(BIASU),2),1));
BIASV0D=squeeze(nanmean(nanmean(abs(BIASV),2),1));
BIASQ0D=squeeze(nanmean(nanmean(abs(BIASQ),2),1));
BIASH0D=squeeze(nanmean(nanmean(abs(BIASH),2),1));
BIASP0D=squeeze(nanmean(nanmean(abs(BIASP),2),1));

load ../EXPERIMENTOS/QFX2DNOZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

BIAST2DNZ=squeeze(nanmean(nanmean(abs(BIAST),2),1));
BIASU2DNZ=squeeze(nanmean(nanmean(abs(BIASU),2),1));
BIASV2DNZ=squeeze(nanmean(nanmean(abs(BIASV),2),1));
BIASQ2DNZ=squeeze(nanmean(nanmean(abs(BIASQ),2),1));
BIASH2DNZ=squeeze(nanmean(nanmean(abs(BIASH),2),1));
BIASP2DNZ=squeeze(nanmean(nanmean(abs(BIASP),2),1));

load ../EXPERIMENTOS/QFX2DZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

BIAST2D=squeeze(nanmean(nanmean(abs(BIAST),2),1));
BIASU2D=squeeze(nanmean(nanmean(abs(BIASU),2),1));
BIASV2D=squeeze(nanmean(nanmean(abs(BIASV),2),1));
BIASQ2D=squeeze(nanmean(nanmean(abs(BIASQ),2),1));
BIASH2D=squeeze(nanmean(nanmean(abs(BIASH),2),1));
BIASP2D=squeeze(nanmean(nanmean(abs(BIASP),2),1));


zlevel=[1000 975 950 925 900 850 800 700 600 500 400 300 200];

figure
subplot(2,2,1)
hold on
plot(BIASTCTRL,-log(zlevel),'k-','LineWidth',3)
plot(BIAST0D,-log(zlevel),'b-','LineWidth',3)
plot(BIAST0DNZ,-log(zlevel),'b--','LineWidth',3)
plot(BIAST2D,-log(zlevel),'r-','LineWidth',3)
plot(BIAST2DNZ,-log(zlevel),'r--','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('BIAS')
title('Temperature','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,2)
hold on
plot(BIASQCTRL,-log(zlevel),'k-','LineWidth',3)
plot(BIASQ0D,-log(zlevel),'b-','LineWidth',3)
plot(BIASQ0DNZ,-log(zlevel),'b--','LineWidth',3)
plot(BIASQ2D,-log(zlevel),'r-','LineWidth',3)
plot(BIASQ2DNZ,-log(zlevel),'r--','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
legend('CTRL','0D W. Vert. loc.','0D W/O Vert. Loc.','2D W. Vert. loc','0D W/O Vert. Loc.')
xlabel('BIAS')
title('Moisture','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,3)
hold on
plot(BIASUCTRL,-log(zlevel),'k-','LineWidth',3)
plot(BIASU0D,-log(zlevel),'b-','LineWidth',3)
plot(BIASU0DNZ,-log(zlevel),'b--','LineWidth',3)
plot(BIASU2D,-log(zlevel),'r-','LineWidth',3)
plot(BIASU2DNZ,-log(zlevel),'r--','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('BIAS')
title('U-Wind','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,4)
hold on
plot(BIASVCTRL,-log(zlevel),'k-','LineWidth',3)
plot(BIASV0D,-log(zlevel),'b-','LineWidth',3)
plot(BIASV0DNZ,-log(zlevel),'b--','LineWidth',3)
plot(BIASV2D,-log(zlevel),'r-','LineWidth',3)
plot(BIASV2DNZ,-log(zlevel),'r--','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('BIAS')
title('V-Wind','FontSize',15)
grid on
ylabel('Pressure')



