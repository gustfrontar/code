clear all
close all

load ../EXPERIMENTOS/FORECAST_CONTROL40M_MEMNC/verification_fnl/rmse_bias.mat

RMSETCTRL=squeeze(nanmean(nanmean(RMSET,2),1));
RMSEUCTRL=squeeze(nanmean(nanmean(RMSEU,2),1));
RMSEVCTRL=squeeze(nanmean(nanmean(RMSEV,2),1));
RMSEQCTRL=squeeze(nanmean(nanmean(RMSEQ,2),1));
RMSEHCTRL=squeeze(nanmean(nanmean(RMSEH,2),1));
RMSEPCTRL=squeeze(nanmean(nanmean(RMSEP,2),1));

load ../EXPERIMENTOS/FORECAST_QFX0DNOZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

RMSET0DNZ=squeeze(nanmean(nanmean(RMSET,2),1));
RMSEU0DNZ=squeeze(nanmean(nanmean(RMSEU,2),1));
RMSEV0DNZ=squeeze(nanmean(nanmean(RMSEV,2),1));
RMSEQ0DNZ=squeeze(nanmean(nanmean(RMSEQ,2),1));
RMSEH0DNZ=squeeze(nanmean(nanmean(RMSEH,2),1));
RMSEP0DNZ=squeeze(nanmean(nanmean(RMSEP,2),1));

load ../EXPERIMENTOS/FORECAST_QFX0DZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

RMSET0D=squeeze(nanmean(nanmean(RMSET,2),1));
RMSEU0D=squeeze(nanmean(nanmean(RMSEU,2),1));
RMSEV0D=squeeze(nanmean(nanmean(RMSEV,2),1));
RMSEQ0D=squeeze(nanmean(nanmean(RMSEQ,2),1));
RMSEH0D=squeeze(nanmean(nanmean(RMSEH,2),1));
RMSEP0D=squeeze(nanmean(nanmean(RMSEP,2),1));

load ../EXPERIMENTOS/FORECAST_QFX2DNOZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

RMSET2DNZ=squeeze(nanmean(nanmean(RMSET,2),1));
RMSEU2DNZ=squeeze(nanmean(nanmean(RMSEU,2),1));
RMSEV2DNZ=squeeze(nanmean(nanmean(RMSEV,2),1));
RMSEQ2DNZ=squeeze(nanmean(nanmean(RMSEQ,2),1));
RMSEH2DNZ=squeeze(nanmean(nanmean(RMSEH,2),1));
RMSEP2DNZ=squeeze(nanmean(nanmean(RMSEP,2),1));

load ../EXPERIMENTOS/FORECAST_QFX2DZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

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
plot(RMSET0D(:,1),-log(zlevel),'b-','LineWidth',3)
plot(RMSET0DNZ(:,1),-log(zlevel),'b--','LineWidth',3)
plot(RMSET2D(:,1),-log(zlevel),'r-','LineWidth',3)
plot(RMSET2DNZ(:,1),-log(zlevel),'r--','LineWidth',3)
%plot(RMSETCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE')
title('Temperature','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,2)
hold on
set(gca,'FontSize',15)
plot(RMSEQ0D(:,1),-log(zlevel),'b-','LineWidth',3)
plot(RMSEQ0DNZ(:,1),-log(zlevel),'b--','LineWidth',3)
plot(RMSEQ2D(:,1),-log(zlevel),'r-','LineWidth',3)
plot(RMSEQ2DNZ(:,1),-log(zlevel),'r--','LineWidth',3)
%plot(RMSEQCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
%legend('0D W. Vert. loc.','0D W. Vert. Loc.','2D W/O Vert. loc')
xlabel('RMSE')
title('Moisture','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,3)
hold on
set(gca,'FontSize',15)
plot(RMSEU0D(:,1),-log(zlevel),'b-','LineWidth',3)
plot(RMSEU0DNZ(:,1),-log(zlevel),'b--','LineWidth',3)
plot(RMSEU2D(:,1),-log(zlevel),'r-','LineWidth',3)
plot(RMSEU2DNZ(:,1),-log(zlevel),'r--','LineWidth',3)
%plot(RMSEUCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE')
title('U-Wind','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,4)
hold on
set(gca,'FontSize',15)
plot(RMSEV0D(:,1),-log(zlevel),'b-','LineWidth',3)
plot(RMSEV0DNZ(:,1),-log(zlevel),'b--','LineWidth',3)
plot(RMSEV2D(:,1),-log(zlevel),'r-','LineWidth',3)
plot(RMSEV2DNZ(:,1),-log(zlevel),'r--','LineWidth',3)
%plot(RMSEVCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE')
title('V-Wind','FontSize',15)
grid on
ylabel('Pressure')



figure
subplot(2,2,1)
hold on
set(gca,'FontSize',15)
plot(RMSET0D(:,5),-log(zlevel),'b-','LineWidth',3)
plot(RMSET0DNZ(:,5),-log(zlevel),'b--','LineWidth',3)
plot(RMSET2D(:,5),-log(zlevel),'r-','LineWidth',3)
plot(RMSET2DNZ(:,5),-log(zlevel),'r--','LineWidth',3)
%plot(RMSETCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE')
title('Temperature','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,2)
hold on
set(gca,'FontSize',15)
plot(RMSEQ0D(:,5),-log(zlevel),'b-','LineWidth',3)
plot(RMSEQ0DNZ(:,5),-log(zlevel),'b--','LineWidth',3)
plot(RMSEQ2D(:,5),-log(zlevel),'r-','LineWidth',3)
plot(RMSEQ2DNZ(:,5),-log(zlevel),'r--','LineWidth',3)
%plot(RMSEQCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
%legend('0D W. Vert. loc.','0D W. Vert. Loc.','2D W/O Vert. loc')
xlabel('RMSE')
title('Moisture','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,3)
hold on
set(gca,'FontSize',15)
plot(RMSEU0D(:,5),-log(zlevel),'b-','LineWidth',3)
plot(RMSEU0DNZ(:,5),-log(zlevel),'b--','LineWidth',3)
plot(RMSEU2D(:,5),-log(zlevel),'r-','LineWidth',3)
plot(RMSEU2DNZ(:,5),-log(zlevel),'r--','LineWidth',3)
%plot(RMSEUCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE')
title('U-Wind','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,4)
hold on
set(gca,'FontSize',15)
plot(RMSEV0D(:,5),-log(zlevel),'b-','LineWidth',3)
plot(RMSEV0DNZ(:,5),-log(zlevel),'b--','LineWidth',3)
plot(RMSEV2D(:,5),-log(zlevel),'r-','LineWidth',3)
plot(RMSEV2DNZ(:,5),-log(zlevel),'r--','LineWidth',3)
%plot(RMSEVCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE')
title('V-Wind','FontSize',15)
grid on
ylabel('Pressure')

figure
subplot(2,2,1)
hold on
set(gca,'FontSize',15)
plot((RMSET0D(:,5)-RMSETCTRL(:,5))./RMSETCTRL(:,5),-log(zlevel),'b-','LineWidth',3)
plot((RMSET0DNZ(:,5)-RMSETCTRL(:,5))./RMSETCTRL(:,5),-log(zlevel),'b--','LineWidth',3)
plot((RMSET2D(:,5)-RMSETCTRL(:,5))./RMSETCTRL(:,5),-log(zlevel),'r-','LineWidth',3)
plot((RMSET2DNZ(:,5)-RMSETCTRL(:,5))./RMSETCTRL(:,5),-log(zlevel),'r--','LineWidth',3)
%plot(RMSETCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE')
title('Temperature','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,2)
hold on
set(gca,'FontSize',15)
%plot(RMSEQ0D(:,1),-log(zlevel),'b-','LineWidth',3)
plot((RMSEQ0DNZ(:,5)-RMSEQCTRL(:,5))./RMSEQCTRL(:,5),-log(zlevel),'b--','LineWidth',3)
plot((RMSEQ2D(:,5)-RMSEQCTRL(:,5))./RMSEQCTRL(:,5),-log(zlevel),'r-','LineWidth',3)
plot((RMSEQ2DNZ(:,5)-RMSEQCTRL(:,5))./RMSEQCTRL(:,5),-log(zlevel),'r--','LineWidth',3)
%plot(RMSEQCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
legend('0D W. Vert. loc.','0D W. Vert. Loc.','2D W/O Vert. loc')
xlabel('RMSE')
title('Moisture','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,3)
hold on
set(gca,'FontSize',15)
%plot(RMSEU0D(:,1),-log(zlevel),'b-','LineWidth',3)
plot((RMSEU0DNZ(:,5)-RMSEUCTRL(:,5))./RMSEUCTRL(:,5),-log(zlevel),'b--','LineWidth',3)
plot((RMSEU2D(:,5)-RMSEUCTRL(:,5))./RMSEUCTRL(:,5),-log(zlevel),'r-','LineWidth',3)
plot((RMSEU2DNZ(:,5)-RMSEUCTRL(:,5))./RMSEUCTRL(:,5),-log(zlevel),'r--','LineWidth',3)
%plot(RMSEUCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE')
title('U-Wind','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,4)
hold on
set(gca,'FontSize',15)
%plot(RMSEV0D(:,1),-log(zlevel),'b-','LineWidth',3)
plot((RMSEV0DNZ(:,5)-RMSEVCTRL(:,5))./RMSEVCTRL(:,5),-log(zlevel),'b--','LineWidth',3)
plot((RMSEV2D(:,5)-RMSEVCTRL(:,5))./RMSEVCTRL(:,5),-log(zlevel),'r-','LineWidth',3)
plot((RMSEV2DNZ(:,5)-RMSEVCTRL(:,5))./RMSEVCTRL(:,5),-log(zlevel),'r--','LineWidth',3)
%plot(RMSEVCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE')
title('V-Wind','FontSize',15)
grid on
ylabel('Pressure')



figure
subplot(2,2,1)
hold on
set(gca,'FontSize',15)
%plot(RMSET0D(:,1),-log(zlevel),'b-','LineWidth',3)
plot((RMSET0DNZ(:,9)-RMSETCTRL(:,9))./RMSETCTRL(:,9),-log(zlevel),'b--','LineWidth',3)
plot((RMSET2D(:,9)-RMSETCTRL(:,9))./RMSETCTRL(:,9),-log(zlevel),'r-','LineWidth',3)
plot((RMSET2DNZ(:,9)-RMSETCTRL(:,9))./RMSETCTRL(:,9),-log(zlevel),'r--','LineWidth',3)
%plot(RMSETCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE')
title('Temperature','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,2)
hold on
set(gca,'FontSize',15)
%plot(RMSEQ0D(:,1),-log(zlevel),'b-','LineWidth',3)
plot((RMSEQ0DNZ(:,9)-RMSEQCTRL(:,9))./RMSEQCTRL(:,9),-log(zlevel),'b--','LineWidth',3)
plot((RMSEQ2D(:,9)-RMSEQCTRL(:,9))./RMSEQCTRL(:,9),-log(zlevel),'r-','LineWidth',3)
plot((RMSEQ2DNZ(:,9)-RMSEQCTRL(:,9))./RMSEQCTRL(:,9),-log(zlevel),'r--','LineWidth',3)
%plot(RMSEQCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
legend('0D W. Vert. loc.','0D W. Vert. Loc.','2D W/O Vert. loc')
xlabel('RMSE')
title('Moisture','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,3)
hold on
set(gca,'FontSize',15)
%plot(RMSEU0D(:,1),-log(zlevel),'b-','LineWidth',3)
plot((RMSEU0DNZ(:,9)-RMSEUCTRL(:,9))./RMSEUCTRL(:,9),-log(zlevel),'b--','LineWidth',3)
plot((RMSEU2D(:,9)-RMSEUCTRL(:,9))./RMSEUCTRL(:,9),-log(zlevel),'r-','LineWidth',3)
plot((RMSEU2DNZ(:,9)-RMSEUCTRL(:,9))./RMSEUCTRL(:,9),-log(zlevel),'r--','LineWidth',3)
%plot(RMSEUCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE')
title('U-Wind','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,4)
hold on
set(gca,'FontSize',15)
%plot(RMSEV0D(:,1),-log(zlevel),'b-','LineWidth',3)
plot((RMSEV0DNZ(:,9)-RMSEVCTRL(:,9))./RMSEVCTRL(:,9),-log(zlevel),'b--','LineWidth',3)
plot((RMSEV2D(:,9)-RMSEVCTRL(:,9))./RMSEVCTRL(:,9),-log(zlevel),'r-','LineWidth',3)
plot((RMSEV2DNZ(:,9)-RMSEVCTRL(:,9))./RMSEVCTRL(:,9),-log(zlevel),'r--','LineWidth',3)
%plot(RMSEVCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE')
title('V-Wind','FontSize',15)
grid on
ylabel('Pressure')


figure
subplot(2,2,1)
hold on
set(gca,'FontSize',15)
%plot(RMSET0D(:,1),-log(zlevel),'b-','LineWidth',3)
plot((RMSET0DNZ(:,13)-RMSETCTRL(:,13))./RMSETCTRL(:,13),-log(zlevel),'b--','LineWidth',3)
plot((RMSET2D(:,13)-RMSETCTRL(:,13))./RMSETCTRL(:,13),-log(zlevel),'r-','LineWidth',3)
plot((RMSET2DNZ(:,13)-RMSETCTRL(:,13))./RMSETCTRL(:,13),-log(zlevel),'r--','LineWidth',3)
%plot(RMSETCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE')
title('Temperature','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,2)
hold on
set(gca,'FontSize',15)
%plot(RMSEQ0D(:,1),-log(zlevel),'b-','LineWidth',3)
plot((RMSEQ0DNZ(:,13)-RMSEQCTRL(:,13))./RMSEQCTRL(:,13),-log(zlevel),'b--','LineWidth',3)
plot((RMSEQ2D(:,13)-RMSEQCTRL(:,13))./RMSEQCTRL(:,13),-log(zlevel),'r-','LineWidth',3)
plot((RMSEQ2DNZ(:,13)-RMSEQCTRL(:,13))./RMSEQCTRL(:,13),-log(zlevel),'r--','LineWidth',3)
%plot(RMSEQCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
legend('0D W. Vert. loc.','0D W. Vert. Loc.','2D W/O Vert. loc')
xlabel('RMSE')
title('Moisture','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,3)
hold on
set(gca,'FontSize',15)
%plot(RMSEU0D(:,1),-log(zlevel),'b-','LineWidth',3)
plot((RMSEU0DNZ(:,13)-RMSEUCTRL(:,13))./RMSEUCTRL(:,13),-log(zlevel),'b--','LineWidth',3)
plot((RMSEU2D(:,13)-RMSEUCTRL(:,13))./RMSEUCTRL(:,13),-log(zlevel),'r-','LineWidth',3)
plot((RMSEU2DNZ(:,13)-RMSEUCTRL(:,13))./RMSEUCTRL(:,13),-log(zlevel),'r--','LineWidth',3)
%plot(RMSEUCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE')
title('U-Wind','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,4)
hold on
set(gca,'FontSize',15)
%plot(RMSEV0D(:,1),-log(zlevel),'b-','LineWidth',3)
plot((RMSEV0DNZ(:,13)-RMSEVCTRL(:,13))./RMSEVCTRL(:,13),-log(zlevel),'b--','LineWidth',3)
plot((RMSEV2D(:,13)-RMSEVCTRL(:,13))./RMSEVCTRL(:,13),-log(zlevel),'r-','LineWidth',3)
plot((RMSEV2DNZ(:,13)-RMSEVCTRL(:,13))./RMSEVCTRL(:,13),-log(zlevel),'r--','LineWidth',3)
%plot(RMSEVCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE')
title('V-Wind','FontSize',15)
grid on
ylabel('Pressure')

