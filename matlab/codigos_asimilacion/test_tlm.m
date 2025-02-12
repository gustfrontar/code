clear all
close all


dt=0.01;

dx=[0.1 0.1 0.1];
for ii=1:15

    

x=[ 5.318217030478297  -1.382912086671714  31.364914753651490 ];

xf1=stepit(x,dt);
xf2=stepit(x+dx,dt);
xf3=stepit(x-dx,dt);

dxfnlmas  =xf2-xf1;
dxfnlmenos=xf3-xf1;

[tlm,adm] = stepit_tl_ad(x,dt);

xftmas=tlm*(dx');
xftmenos=tlm*(-dx');

%dxfnl
%dxfl'

dxfnlmas./xftmas'
dxfnlmenos./xftmenos'

dx=dx/2;

end


% dx=[0.1 -0.1 0.1];
% for ii=1:30
% 
%     
% 
% x=[ 5.318217030478297  -1.382912086671714  31.364914753651490 ];
% 
% % xf1=stepit(x,dt);
% % xf2=stepit(x+dx,dt);
% % xf3=stepit(x-dx,dt);
% 
% xf1=lorenz(x);
% xf2=lorenz(x+dx);
% xf3=lorenz(x-dx);
% 
% dxfnlmas  =xf2-xf1;
% dxfnlmenos=xf3-xf1;
% 
% tlm = lorenz_tl(x);
% 
% xftmas=tlm*(dx');
% xftmenos=tlm*(-dx');
% 
% %dxfnl
% %dxfl'
% 
% dxfnlmas./xftmas'
% dxfnlmenos./xftmenos'
% 
% dx=dx/2;
% 
% end
