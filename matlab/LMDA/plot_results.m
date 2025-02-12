


function plot_results(state,statea,statef,stateaens,statefens,spreada,spreadf,yobs,iobs,R0) 


%==========================================================================
%   Compute the error with respect to the true evolution of the system.
%==========================================================================

 erra= sqrt(mean( (statea-state).^2, 2) );    %Analysis error
 errf= sqrt(mean( (statef-state).^2 , 2) );  %Forecast error

 stda = sqrt( mean( spreada , 2 ) );
 stdf = sqrt( mean( spreadf , 2 ) );
 
% mean RMS analysis/background error
rmse_a=mean(erra);
rmse_f=mean(errf);

numstep=length(erra);

fprintf(1,'Number of DA cycles= %g \n',numstep)
fprintf(1,'Mean background RMSE= %g \n',rmse_f)
fprintf(1,'Mean analysis RMSE= %g \n',rmse_a)


%==========================================================================
%   Plot the analysis error and spread and the forecast error and spread
%==========================================================================

hFig=figure(); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 30 15])

t1=1;
t2=length(erra);

subplot(1,2,1)
plot([t1,t2],[R0 R0],'-.k','linewidth',1.5);hold on
plot([t1,t2],[rmse_a rmse_a],'r','linewidth',2.)
plot(erra,'b');hold on
plot(stda,'r');hold on
axis([t1 t2 0. R0*3])
hleg=legend('Obs error','Mean RMS analysis error','Analysis error','Analysis spread');
set(hleg,'Fontsize',10.)
legend('boxoff')
xlabel('Time step')
ylabel('RMS error')
title(['Analysis Error'],'Fontsize',14,'Fontweight','bold')
set(gca,'FontSize',15);

subplot(1,2,2)
plot([t1,t2],[R0 R0],'-.k','linewidth',1.5);hold on
plot([t1,t2],[rmse_f rmse_f],'r','linewidth',2.)
plot(errf,'b');hold on
plot(stdf,'r');hold on
axis([t1 t2 0. R0*3])
hleg=legend('Obs error','Mean RMS forecast error','Forecast error','Forecast spread');
set(hleg,'Fontsize',10.)
legend('boxoff')
xlabel('Time step')
ylabel('RMS error')
title(['Forecast error'],'Fontsize',14,'Fontweight','bold')
set(gca,'FontSize',15);

%==========================================================================
%   Plot the analysis state, the true state and the observations for X, Y
%   and Z
%==========================================================================


% plot the states vs. obs
if (isempty(iobs) )
   yobs(:,1)=NaN;
end
tmp='XYZ';

yo=NaN(numstep,1);


hFig=figure(); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 39 30])
var=1;
subplot(2,1,1)
plot(state(:,var),'r','LineWidth',2);hold on
plot(statea(:,var),'b','LineWidth',2);
plot(squeeze(stateaens(:,var,:)),'Color',[0.5 0.5 0.5],'LineWidth',0.5);
plot(state(:,var),'r','LineWidth',2);
plot(statea(:,var),'b','LineWidth',2);
h=legend('Truth','Analysis mean','Analysis members');
if( isempty( find( iobs == var ) ) );
   yo(:)=NaN;
else
   yo(:)=yobs(:,find(iobs== var ));
end
plot(yo,'k*','MarkerSize',3);
set(gca,'Position',[0.075 0.6097 0.8 0.28])
set(h,'Position',[0.88 0.8 0.1 0.1])
legend('boxoff')
xlabel('Time')
ylabel(tmp(var),'Fontsize',12)
title('Truth vs. Analysis','Fontsize',12,'Fontweight','bold')

subplot(2,1,2)
plot(statea(:,var)-state(:,var),'b','Linewidth',1.5);hold on
%%plot([time(ind(1)) time(ind(end))],[0 0],'-k')
set(gca,'Ylim',[-2 2],'Xlim',[1 numstep])
set(gca,'Position',[0.075 0.135 0.8 0.28])
title('Analysis-Truth','Fontsize',12,'Fontweight','bold')
xlabel('Time')
ylabel(['Error in ' tmp(var)])


hFig=figure(); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 39 30])
var=2;
subplot(2,1,1)
plot(state(:,var),'r','LineWidth',2);hold on
plot(statea(:,var),'b','LineWidth',2);
plot(squeeze(stateaens(:,var,:)),'Color',[0.5 0.5 0.5],'LineWidth',0.5);
plot(state(:,var),'r','LineWidth',2);
plot(statea(:,var),'b','LineWidth',2);
h=legend('Tru','Ana');
if( isempty( find( iobs == var ) ) );
   yo(:)=NaN;
else
   yo(:)=yobs(:,find(iobs== var ));
end
plot(yo,'k*','MarkerSize',3);
set(gca,'Position',[0.075 0.6097 0.8 0.28])
set(h,'Position',[0.88 0.8 0.1 0.1])
legend('boxoff')
xlabel('Time')
ylabel(tmp(var),'Fontsize',12)
title('Truth vs. Analysis','Fontsize',12,'Fontweight','bold')

subplot(2,1,2)
plot(statea(:,var)-state(:,var),'b','Linewidth',1.5);hold on
%%plot([time(ind(1)) time(ind(end))],[0 0],'-k')
set(gca,'Ylim',[-2 2],'Xlim',[1 numstep])
set(gca,'Position',[0.075 0.135 0.8 0.28])
title('Analysis-Truth','Fontsize',12,'Fontweight','bold')
xlabel('Time')
ylabel(['Error in ' tmp(var)])

hFig=figure(); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 39 30])
var=3;
subplot(2,1,1)
plot(state(:,var),'r','LineWidth',2);hold on
plot(statea(:,var),'b','LineWidth',2);
plot(squeeze(stateaens(:,var,:)),'Color',[0.5 0.5 0.5],'LineWidth',0.5);
plot(state(:,var),'r','LineWidth',2);
plot(statea(:,var),'b','LineWidth',2);
h=legend('Tru','Ana');
if( isempty( find( iobs == var ) ) );
   yo(:)=NaN;
else
   yo(:)=yobs(:,find(iobs== var ));
end
plot(yo,'k*','MarkerSize',3);
set(gca,'Position',[0.075 0.6097 0.8 0.28])
set(h,'Position',[0.88 0.8 0.1 0.1])
legend('boxoff')
xlabel('Time')
ylabel(tmp(var),'Fontsize',12)
title('Truth vs. Analysis','Fontsize',12,'Fontweight','bold')

subplot(2,1,2)
plot(statea(:,var)-state(:,var),'b','Linewidth',1.5);hold on
%%plot([time(ind(1)) time(ind(end))],[0 0],'-k')
set(gca,'Ylim',[-2 2],'Xlim',[1 numstep])
set(gca,'Position',[0.075 0.135 0.8 0.28])
title('Analysis-Truth','Fontsize',12,'Fontweight','bold')
xlabel('Time')
ylabel(['Error in ' tmp(var)])