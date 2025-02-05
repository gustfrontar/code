function DAerrplt(t,err,spread,err_b,err_o,datype)
figure('Units','inches','Position',[1 1 9.5 6.5]);clf
% plot the errors from DA
%xyz='xyz';
%subtit=xyz(iobs(1));
%for ip=2:length(iobs)
%    subtit=strcat(strcat(subtit,','),xyz(iobs(ip)));
%end

t1=t(1);
t2=t(end);
n=length(t);
plot([t1,t2],[err_o err_o],'-.k','linewidth',1.5);hold on
plot([t1,t2],[err_b err_b],'r','linewidth',2.)
plot(t,err,'b');hold on
plot(t,spread,'r');hold on
axis([t1 t2 0. err_o*3])
hleg=legend('Obs error','Mean RMS analyerr error','Analysis error','Analysis spread');
set(hleg,'Fontsize',10.)
legend('boxoff')
xlabel('Time step')
ylabel('RMS error')
title(['RMS error from ',datype],'Fontsize',14,'Fontweight','bold')
return

