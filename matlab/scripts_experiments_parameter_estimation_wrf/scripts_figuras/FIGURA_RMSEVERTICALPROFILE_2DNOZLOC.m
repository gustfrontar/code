clear all
close all


load ../EXPERIMENTOS/CONTROL40M_MEMNC/verification_fnl/rmse_bias.mat

RMSETCTRL=squeeze(nanmean(nanmean(RMSET,2),1));
RMSEUCTRL=squeeze(nanmean(nanmean(RMSEU,2),1));
RMSEVCTRL=squeeze(nanmean(nanmean(RMSEV,2),1));
RMSEQCTRL=squeeze(nanmean(nanmean(RMSEQ,2),1));
RMSEHCTRL=squeeze(nanmean(nanmean(RMSEH,2),1));
RMSEPCTRL=squeeze(nanmean(nanmean(RMSEP,2),1));

load ../EXPERIMENTOS/QFX2DNOZLOC40M_MEMNC/verification_fnl/rmse_bias.mat

RMSETPE=squeeze(nanmean(nanmean(RMSET,2),1));
RMSEUPE=squeeze(nanmean(nanmean(RMSEU,2),1));
RMSEVPE=squeeze(nanmean(nanmean(RMSEV,2),1));
RMSEQPE=squeeze(nanmean(nanmean(RMSEQ,2),1));
RMSEHPE=squeeze(nanmean(nanmean(RMSEH,2),1));
RMSEPPE=squeeze(nanmean(nanmean(RMSEP,2),1));

zlevel=[1000 975 950 925 900 850 800 700 600 500 400 300 200];

figure
%subplot(2,2,1)
set(gca,'FontSize',15)
hold on
set(gca,'FontSize',15)
plot(100*(RMSETPE-RMSETCTRL)./RMSETPE,-log(zlevel),'r-','LineWidth',3)
plot(100*(RMSEQPE-RMSEQCTRL)./RMSEQPE,-log(zlevel),'g-','LineWidth',3)
plot(100*(RMSEUPE-RMSEUCTRL)./RMSEUPE,-log(zlevel),'m-','LineWidth',3)
plot(100*(RMSEVPE-RMSEVCTRL)./RMSEVPE,-log(zlevel),'m--','LineWidth',3)
%plot(RMSETPE,-log(zlevel),'b-','LineWidth',3)
legend('T','Q','U','V')

set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
xlabel('RMSE IMPROVEMENT (%)','FontSize',15)
title('Temperature','FontSize',15)
grid on
ylabel('Pressure','FontSize',15)
% 
% subplot(2,2,2)
% hold on
% set(gca,'FontSize',15)
% set(gca,'FontSize',15)
% plot(100*(RMSEQPE-RMSEQCTRL)./RMSEQPE,-log(zlevel),'k-','LineWidth',3)
% %plot(RMSEQPE,-log(zlevel),'b-','LineWidth',3)
% 
% set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
% legend('CTRL','PARAMETER ESTIMATION','FontSize',15)
% xlabel('RMSE','FontSize',15)
% title('Moisture','FontSize',15)
% grid on
% ylabel('Pressure','FontSize',15)
% 
% subplot(2,2,3)
% hold on
% set(gca,'FontSize',15)
% set(gca,'FontSize',15)
% plot(100*(RMSEUPE-RMSEUCTRL)./RMSEUPE,-log(zlevel),'k-','LineWidth',3)
% %plot(RMSEUPE,-log(zlevel),'b-','LineWidth',3)
% 
% set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
% xlabel('RMSE')
% title('U-Wind','FontSize',15)
% grid on
% ylabel('Pressure','FontSize',15)
% 
% subplot(2,2,4)
% hold on
% set(gca,'FontSize',15)
% set(gca,'FontSize',15)
% plot(100*(RMSEVPE-RMSEVCTRL)./RMSEVPE,-log(zlevel),'k-','LineWidth',3)
% %plot(RMSEVPE,-log(zlevel),'b-','LineWidth',3)
% 
% set(gca,'YTick',[-log(1000) -log(850) -log(500) -log(300)],'YTickLabel',{'1000','850','500','300'})
% xlabel('RMSE','FontSize',15)
% title('V-Wind','FontSize',15)
% grid on
% ylabel('Pressure','FontSize',15)



