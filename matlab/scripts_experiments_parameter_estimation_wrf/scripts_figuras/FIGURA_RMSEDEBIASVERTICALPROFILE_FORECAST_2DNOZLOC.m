clear all
close all

load ../EXPERIMENTOS/FORECAST_CONTROL40M_MEMNC/verification_fnl/rmse_bias.mat

% RMSEDEBIAST=sqrt(RMSET.^2-BIAST.^2);
% RMSEDEBIASQ=sqrt(RMSEQ.^2-BIASQ.^2);
% RMSEDEBIASU=sqrt(RMSEU.^2-BIASU.^2);
% RMSEDEBIASV=sqrt(RMSEV.^2-BIASV.^2);

RMSETCTRL=squeeze(nanmean(nanmean(RMSEDEBIAST,2),1));
RMSEUCTRL=squeeze(nanmean(nanmean(RMSEDEBIASU,2),1));
RMSEVCTRL=squeeze(nanmean(nanmean(RMSEDEBIASV,2),1));
RMSEQCTRL=squeeze(nanmean(nanmean(RMSEDEBIASQ,2),1));
RMSEHCTRL=squeeze(nanmean(nanmean(RMSEDEBIASH,2),1));
RMSEPCTRL=squeeze(nanmean(nanmean(RMSEDEBIASP,2),1));

load ../EXPERIMENTOS/FORECAST_QFX2DNOZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

% RMSEDEBIAST=sqrt(RMSET.^2-BIAST.^2);
% RMSEDEBIASQ=sqrt(RMSEQ.^2-BIASQ.^2);
% RMSEDEBIASU=sqrt(RMSEU.^2-BIASU.^2);
% RMSEDEBIASV=sqrt(RMSEV.^2-BIASV.^2);

RMSETPE=squeeze(nanmean(nanmean(RMSEDEBIAST,2),1));
RMSEUPE=squeeze(nanmean(nanmean(RMSEDEBIASU,2),1));
RMSEVPE=squeeze(nanmean(nanmean(RMSEDEBIASV,2),1));
RMSEQPE=squeeze(nanmean(nanmean(RMSEDEBIASQ,2),1));
RMSEHPE=squeeze(nanmean(nanmean(RMSEDEBIASH,2),1));
RMSEPPE=squeeze(nanmean(nanmean(RMSEDEBIASP,2),1));

zlevel=[1000 975 950 925 900 850 800 700 600 500 400 300 200];

figure
hold on
set(gca,'FontSize',15)
plot( 100*(RMSETPE(:,1)-RMSETCTRL(:,1))./RMSETCTRL(:,1),-log(zlevel),'r-','LineWidth',3)
plot( 100*(RMSEQPE(:,1)-RMSEQCTRL(:,1))./RMSEQCTRL(:,1),-log(zlevel),'g-','LineWidth',3)
plot( 100*(RMSEUPE(:,1)-RMSEUCTRL(:,1))./RMSEUCTRL(:,1),-log(zlevel),'m-','LineWidth',3)
plot( 100*(RMSEVPE(:,1)-RMSEVCTRL(:,1))./RMSEVCTRL(:,1),-log(zlevel),'m--','LineWidth',3)

set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE','FontSize',15)
title('RMSE IMPROVEMENT (F00)','FontSize',15)
legend('T','Q','U','V','FontSize',15)
grid on
ylabel('Pressure','FontSize',15)

figure
hold on
set(gca,'FontSize',15)
plot( 100*(RMSETPE(:,5)-RMSETCTRL(:,5))./RMSETCTRL(:,5),-log(zlevel),'r-','LineWidth',3)
plot( 100*(RMSEQPE(:,5)-RMSEQCTRL(:,5))./RMSEQCTRL(:,5),-log(zlevel),'g-','LineWidth',3)
plot( 100*(RMSEUPE(:,5)-RMSEUCTRL(:,5))./RMSEUCTRL(:,5),-log(zlevel),'m-','LineWidth',3)
plot( 100*(RMSEVPE(:,5)-RMSEVCTRL(:,5))./RMSEVCTRL(:,5),-log(zlevel),'m--','LineWidth',3)

set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE','FontSize',15)
title('RMSE IMPROVEMENT (F24)','FontSize',15)
legend('T','Q','U','V','FontSize',15)
grid on
ylabel('Pressure','FontSize',15)

figure
hold on
set(gca,'FontSize',15)
plot( 100*(RMSETPE(:,9)-RMSETCTRL(:,9))./RMSETCTRL(:,9),-log(zlevel),'r-','LineWidth',3)
plot( 100*(RMSEQPE(:,9)-RMSEQCTRL(:,9))./RMSEQCTRL(:,9),-log(zlevel),'g-','LineWidth',3)
plot( 100*(RMSEUPE(:,9)-RMSEUCTRL(:,9))./RMSEUCTRL(:,9),-log(zlevel),'m-','LineWidth',3)
plot( 100*(RMSEVPE(:,9)-RMSEVCTRL(:,9))./RMSEVCTRL(:,9),-log(zlevel),'m--','LineWidth',3)

set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE','FontSize',15)
title('RMSE IMPROVEMENT (F48)','FontSize',15)
legend('T','Q','U','V','FontSize',15)
grid on
ylabel('Pressure','FontSize',15)

figure
hold on
set(gca,'FontSize',15)
plot( 100*(RMSETPE(:,13)-RMSETCTRL(:,13))./RMSETCTRL(:,13),-log(zlevel),'r-','LineWidth',3)
plot( 100*(RMSEQPE(:,13)-RMSEQCTRL(:,13))./RMSEQCTRL(:,13),-log(zlevel),'g-','LineWidth',3)
plot( 100*(RMSEUPE(:,13)-RMSEUCTRL(:,13))./RMSEUCTRL(:,13),-log(zlevel),'m-','LineWidth',3)
plot( 100*(RMSEVPE(:,13)-RMSEVCTRL(:,13))./RMSEVCTRL(:,13),-log(zlevel),'m--','LineWidth',3)

set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE','FontSize',15)
title('RMSE IMPROVEMENT (F72)','FontSize',15)
legend('T','Q','U','V','FontSize',15)
grid on
ylabel('Pressure','FontSize',15)




