clear all
close all


load ../EXPERIMENTOS/FORECAST_CONTROL40M_MEMNC/verification_fnl/rmse_bias.mat

SMTCTRL=squeeze(nanmean(nanmean(SMT,2),1));
SMUCTRL=squeeze(nanmean(nanmean(SMU,2),1));
SMVCTRL=squeeze(nanmean(nanmean(SMV,2),1));
SMQCTRL=squeeze(nanmean(nanmean(SMQ,2),1));
SMHCTRL=squeeze(nanmean(nanmean(SMH,2),1));
SMPCTRL=squeeze(nanmean(nanmean(SMP,2),1));

load ../EXPERIMENTOS/FORECAST_QFX0DNOZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

SMT0DNZ=squeeze(nanmean(nanmean(SMT,2),1));
SMU0DNZ=squeeze(nanmean(nanmean(SMU,2),1));
SMV0DNZ=squeeze(nanmean(nanmean(SMV,2),1));
SMQ0DNZ=squeeze(nanmean(nanmean(SMQ,2),1));
SMH0DNZ=squeeze(nanmean(nanmean(SMH,2),1));
SMP0DNZ=squeeze(nanmean(nanmean(SMP,2),1));

load ../EXPERIMENTOS/FORECAST_QFX0DZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

SMT0D=squeeze(nanmean(nanmean(SMT,2),1));
SMU0D=squeeze(nanmean(nanmean(SMU,2),1));
SMV0D=squeeze(nanmean(nanmean(SMV,2),1));
SMQ0D=squeeze(nanmean(nanmean(SMQ,2),1));
SMH0D=squeeze(nanmean(nanmean(SMH,2),1));
SMP0D=squeeze(nanmean(nanmean(SMP,2),1));

load ../EXPERIMENTOS/FORECAST_QFX2DNOZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

SMT2DNZ=squeeze(nanmean(nanmean(SMT,2),1));
SMU2DNZ=squeeze(nanmean(nanmean(SMU,2),1));
SMV2DNZ=squeeze(nanmean(nanmean(SMV,2),1));
SMQ2DNZ=squeeze(nanmean(nanmean(SMQ,2),1));
SMH2DNZ=squeeze(nanmean(nanmean(SMH,2),1));
SMP2DNZ=squeeze(nanmean(nanmean(SMP,2),1));

load ../EXPERIMENTOS/FORECAST_QFX2DZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

SMT2D=squeeze(nanmean(nanmean(SMT,2),1));
SMU2D=squeeze(nanmean(nanmean(SMU,2),1));
SMV2D=squeeze(nanmean(nanmean(SMV,2),1));
SMQ2D=squeeze(nanmean(nanmean(SMQ,2),1));
SMH2D=squeeze(nanmean(nanmean(SMH,2),1));
SMP2D=squeeze(nanmean(nanmean(SMP,2),1));


zlevel=[1000 975 950 925 900 850 800 700 600 500 400 300 200];

figure
subplot(2,2,1)
hold on

plot(SMT0D(:,1),-log(zlevel),'b-','LineWidth',3)
plot(SMT0DNZ(:,1),-log(zlevel),'b--','LineWidth',3)
plot(SMT2D(:,1),-log(zlevel),'r-','LineWidth',3)
plot(SMT2DNZ(:,1),-log(zlevel),'r--','LineWidth',3)
plot(SMTCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('SM')
title('Temperature','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,2)
hold on

plot(SMQ0D(:,1),-log(zlevel),'b-','LineWidth',3)
plot(SMQ0DNZ(:,1),-log(zlevel),'b--','LineWidth',3)
plot(SMQ2D(:,1),-log(zlevel),'r-','LineWidth',3)
plot(SMQ2DNZ(:,1),-log(zlevel),'r--','LineWidth',3)
plot(SMQCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
legend('0D W. Vert. loc.','0D W/O Vert. Loc.','2D W. Vert. loc','0D W/O Vert. Loc.','CTRL')
xlabel('SM')
title('Moisture','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,3)
hold on

plot(SMU0D(:,1),-log(zlevel),'b-','LineWidth',3)
plot(SMU0DNZ(:,1),-log(zlevel),'b--','LineWidth',3)
plot(SMU2D(:,1),-log(zlevel),'r-','LineWidth',3)
plot(SMU2DNZ(:,1),-log(zlevel),'r--','LineWidth',3)
plot(SMUCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('SM')
title('U-Wind','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,4)
hold on

plot(SMV0D(:,1),-log(zlevel),'b-','LineWidth',3)
plot(SMV0DNZ(:,1),-log(zlevel),'b--','LineWidth',3)
plot(SMV2D(:,1),-log(zlevel),'r-','LineWidth',3)
plot(SMV2DNZ(:,1),-log(zlevel),'r--','LineWidth',3)
plot(SMVCTRL(:,1),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('SM')
title('V-Wind','FontSize',15)
grid on
ylabel('Pressure')


figure
subplot(2,2,1)
hold on

plot(SMT0D(:,5),-log(zlevel),'b-','LineWidth',3)
plot(SMT0DNZ(:,5),-log(zlevel),'b--','LineWidth',3)
plot(SMT2D(:,5),-log(zlevel),'r-','LineWidth',3)
plot(SMT2DNZ(:,5),-log(zlevel),'r--','LineWidth',3)
plot(SMTCTRL(:,5),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('SM')
title('Temperature','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,2)
hold on

plot(SMQ0D(:,5),-log(zlevel),'b-','LineWidth',3)
plot(SMQ0DNZ(:,5),-log(zlevel),'b--','LineWidth',3)
plot(SMQ2D(:,5),-log(zlevel),'r-','LineWidth',3)
plot(SMQ2DNZ(:,5),-log(zlevel),'r--','LineWidth',3)
plot(SMQCTRL(:,5),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
legend('0D W. Vert. loc.','0D W/O Vert. Loc.','2D W. Vert. loc','0D W/O Vert. Loc.','CTRL')
xlabel('SM')
title('Moisture','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,3)
hold on

plot(SMU0D(:,5),-log(zlevel),'b-','LineWidth',3)
plot(SMU0DNZ(:,5),-log(zlevel),'b--','LineWidth',3)
plot(SMU2D(:,5),-log(zlevel),'r-','LineWidth',3)
plot(SMU2DNZ(:,5),-log(zlevel),'r--','LineWidth',3)
plot(SMUCTRL(:,5),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('SM')
title('U-Wind','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,4)
hold on

plot(SMV0D(:,5),-log(zlevel),'b-','LineWidth',3)
plot(SMV0DNZ(:,5),-log(zlevel),'b--','LineWidth',3)
plot(SMV2D(:,5),-log(zlevel),'r-','LineWidth',3)
plot(SMV2DNZ(:,5),-log(zlevel),'r--','LineWidth',3)
plot(SMVCTRL(:,5),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('SM')
title('V-Wind','FontSize',15)
grid on
ylabel('Pressure')


figure
subplot(2,2,1)
hold on

plot(SMT0D(:,9),-log(zlevel),'b-','LineWidth',3)
plot(SMT0DNZ(:,9),-log(zlevel),'b--','LineWidth',3)
plot(SMT2D(:,9),-log(zlevel),'r-','LineWidth',3)
plot(SMT2DNZ(:,9),-log(zlevel),'r--','LineWidth',3)
plot(SMTCTRL(:,9),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('SM')
title('Temperature','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,2)
hold on

plot(SMQ0D(:,9),-log(zlevel),'b-','LineWidth',3)
plot(SMQ0DNZ(:,9),-log(zlevel),'b--','LineWidth',3)
plot(SMQ2D(:,9),-log(zlevel),'r-','LineWidth',3)
plot(SMQ2DNZ(:,9),-log(zlevel),'r--','LineWidth',3)
plot(SMQCTRL(:,9),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
legend('0D W. Vert. loc.','0D W/O Vert. Loc.','2D W. Vert. loc','0D W/O Vert. Loc.','CTRL')
xlabel('SM')
title('Moisture','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,3)
hold on

plot(SMU0D(:,9),-log(zlevel),'b-','LineWidth',3)
plot(SMU0DNZ(:,9),-log(zlevel),'b--','LineWidth',3)
plot(SMU2D(:,9),-log(zlevel),'r-','LineWidth',3)
plot(SMU2DNZ(:,9),-log(zlevel),'r--','LineWidth',3)
plot(SMUCTRL(:,9),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('SM')
title('U-Wind','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,4)
hold on

plot(SMV0D(:,9),-log(zlevel),'b-','LineWidth',3)
plot(SMV0DNZ(:,9),-log(zlevel),'b--','LineWidth',3)
plot(SMV2D(:,9),-log(zlevel),'r-','LineWidth',3)
plot(SMV2DNZ(:,9),-log(zlevel),'r--','LineWidth',3)
plot(SMVCTRL(:,9),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('SM')
title('V-Wind','FontSize',15)
grid on
ylabel('Pressure')



figure
subplot(2,2,1)
hold on

plot(SMT0D(:,13),-log(zlevel),'b-','LineWidth',3)
plot(SMT0DNZ(:,13),-log(zlevel),'b--','LineWidth',3)
plot(SMT2D(:,13),-log(zlevel),'r-','LineWidth',3)
plot(SMT2DNZ(:,13),-log(zlevel),'r--','LineWidth',3)
plot(SMTCTRL(:,13),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('SM')
title('Temperature','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,2)
hold on

plot(SMQ0D(:,13),-log(zlevel),'b-','LineWidth',3)
plot(SMQ0DNZ(:,13),-log(zlevel),'b--','LineWidth',3)
plot(SMQ2D(:,13),-log(zlevel),'r-','LineWidth',3)
plot(SMQ2DNZ(:,13),-log(zlevel),'r--','LineWidth',3)
plot(SMQCTRL(:,13),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
legend('0D W. Vert. loc.','0D W/O Vert. Loc.','2D W. Vert. loc','0D W/O Vert. Loc.','CTRL')
xlabel('SM')
title('Moisture','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,3)
hold on

plot(SMU0D(:,13),-log(zlevel),'b-','LineWidth',3)
plot(SMU0DNZ(:,13),-log(zlevel),'b--','LineWidth',3)
plot(SMU2D(:,13),-log(zlevel),'r-','LineWidth',3)
plot(SMU2DNZ(:,13),-log(zlevel),'r--','LineWidth',3)
plot(SMUCTRL(:,13),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('SM')
title('U-Wind','FontSize',15)
grid on
ylabel('Pressure')

subplot(2,2,4)
hold on

plot(SMV0D(:,13),-log(zlevel),'b-','LineWidth',3)
plot(SMV0DNZ(:,13),-log(zlevel),'b--','LineWidth',3)
plot(SMV2D(:,13),-log(zlevel),'r-','LineWidth',3)
plot(SMV2DNZ(:,13),-log(zlevel),'r--','LineWidth',3)
plot(SMVCTRL(:,13),-log(zlevel),'k-','LineWidth',3)
set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('SM')
title('V-Wind','FontSize',15)
grid on
ylabel('Pressure')


