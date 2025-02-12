clear all
close all


load ../EXPERIMENTOS/CONTROL40M_MEMNC/verification_fnl/rmse_bias.mat

RMSETCTRL=squeeze(nanmean(nanmean(RMSET,2),1));
RMSEUCTRL=squeeze(nanmean(nanmean(RMSEU,2),1));
RMSEVCTRL=squeeze(nanmean(nanmean(RMSEV,2),1));
RMSEQCTRL=squeeze(nanmean(nanmean(RMSEQ,2),1));
RMSEHCTRL=squeeze(nanmean(nanmean(RMSEH,2),1));
RMSEPCTRL=squeeze(nanmean(nanmean(RMSEP,2),1));

load ../EXPERIMENTOS/QFX0DNOZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

RMSET0DNZ=squeeze(nanmean(nanmean(RMSET,2),1));
RMSEU0DNZ=squeeze(nanmean(nanmean(RMSEU,2),1));
RMSEV0DNZ=squeeze(nanmean(nanmean(RMSEV,2),1));
RMSEQ0DNZ=squeeze(nanmean(nanmean(RMSEQ,2),1));
RMSEH0DNZ=squeeze(nanmean(nanmean(RMSEH,2),1));
RMSEP0DNZ=squeeze(nanmean(nanmean(RMSEP,2),1));

load ../EXPERIMENTOS/QFX0DZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

RMSET0D=squeeze(nanmean(nanmean(RMSET,2),1));
RMSEU0D=squeeze(nanmean(nanmean(RMSEU,2),1));
RMSEV0D=squeeze(nanmean(nanmean(RMSEV,2),1));
RMSEQ0D=squeeze(nanmean(nanmean(RMSEQ,2),1));
RMSEH0D=squeeze(nanmean(nanmean(RMSEH,2),1));
RMSEP0D=squeeze(nanmean(nanmean(RMSEP,2),1));

load ../EXPERIMENTOS/QFX2DNOZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

RMSET2DNZ=squeeze(nanmean(nanmean(RMSET,2),1));
RMSEU2DNZ=squeeze(nanmean(nanmean(RMSEU,2),1));
RMSEV2DNZ=squeeze(nanmean(nanmean(RMSEV,2),1));
RMSEQ2DNZ=squeeze(nanmean(nanmean(RMSEQ,2),1));
RMSEH2DNZ=squeeze(nanmean(nanmean(RMSEH,2),1));
RMSEP2DNZ=squeeze(nanmean(nanmean(RMSEP,2),1));

load ../EXPERIMENTOS/QFX2DZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

RMSET2D=squeeze(nanmean(nanmean(RMSET,2),1));
RMSEU2D=squeeze(nanmean(nanmean(RMSEU,2),1));
RMSEV2D=squeeze(nanmean(nanmean(RMSEV,2),1));
RMSEQ2D=squeeze(nanmean(nanmean(RMSEQ,2),1));
RMSEH2D=squeeze(nanmean(nanmean(RMSEH,2),1));
RMSEP2D=squeeze(nanmean(nanmean(RMSEP,2),1));


zlevel=[1000 975 950 925 900 850 800 700 600 500 400 300 200];

figure
subplot(2,2,1)
hold on
set(gca,'FontSize',15)
plot(RMSETCTRL,-log(zlevel),'k-','LineWidth',3)
plot(RMSET0D,-log(zlevel),'b-','LineWidth',3)
plot(RMSET0DNZ,-log(zlevel),'b--','LineWidth',3)
plot(RMSET2D,-log(zlevel),'r-','LineWidth',3)
plot(RMSET2DNZ,-log(zlevel),'r--','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE')
title('Temperature','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,2)
hold on
set(gca,'FontSize',15)
plot(RMSEQCTRL,-log(zlevel),'k-','LineWidth',3)
plot(RMSEQ0D,-log(zlevel),'b-','LineWidth',3)
plot(RMSEQ0DNZ,-log(zlevel),'b--','LineWidth',3)
plot(RMSEQ2D,-log(zlevel),'r-','LineWidth',3)
plot(RMSEQ2DNZ,-log(zlevel),'r--','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
legend('CTRL','0D W. Vert. loc.','0D W/O Vert. Loc.','2D W. Vert. loc','0D W/O Vert. Loc.')
xlabel('RMSE')
title('Moisture','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,3)
hold on
set(gca,'FontSize',15)
plot(RMSEUCTRL,-log(zlevel),'k-','LineWidth',3)
plot(RMSEU0D,-log(zlevel),'b-','LineWidth',3)
plot(RMSEU0DNZ,-log(zlevel),'b--','LineWidth',3)
plot(RMSEU2D,-log(zlevel),'r-','LineWidth',3)
plot(RMSEU2DNZ,-log(zlevel),'r--','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE')
title('U-Wind','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,4)
hold on
set(gca,'FontSize',15)
plot(RMSEVCTRL,-log(zlevel),'k-','LineWidth',3)
plot(RMSEV0D,-log(zlevel),'b-','LineWidth',3)
plot(RMSEV0DNZ,-log(zlevel),'b--','LineWidth',3)
plot(RMSEV2D,-log(zlevel),'r-','LineWidth',3)
plot(RMSEV2DNZ,-log(zlevel),'r--','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE')
title('V-Wind','FontSize',15)
grid on
ylabel('Pressure')



