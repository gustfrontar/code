% stepit.m


function [tlm,adm] = stepit_tl_ad(xnl,dt)

%xnl es la condicion inicial total (para la trayectoria no lineal)
%xi es la perturbacion 

   
%Calculo la trayectoria no lineal dentro del paso de tiempo alrededor de la
%cual voy a obtener el tangente lineal y el adjunto.

   f = lorenz(xnl);
   
   c1 = dt .* f;   
   xa = xnl + c1 /2.0d0;   
   f = lorenz(xa);
   
   c2 = dt .* f;   
   xb = xnl + c2 /2.0d0;   
   f = lorenz(xb);
   
   c3 = dt .* f;   
   xc = xnl + c3;   
   %f = lorenz(xc);
   
   %c4 = dt .* f;   
   %xend = xi + (c1 + 2.*c2 + 2.*c3 + c4)./6.0d0;   
   
   
   %Vamos a calcular el modelo tangente lineal.
   
   I=eye(3);
   Ji=lorenz_tl(xnl);
   Ja=lorenz_tl(xa);
   Jb=lorenz_tl(xb);
   Jc=lorenz_tl(xc);
   
   tlm= I+dt*(1/6)*( Ji + 2*Ja*( Ji*dt/2 + I ) + 2*Jb*( Ja*( Ji*dt/2 + I )*dt/2 + I  ) ...
        + Jc*(  dt*Jb*( Ja*( Ji*dt/2 + I )*dt/2 +I ) + I ) );
 
    
   %tlm=I +(1/6)*((I + (I + (I + dt*Jc/2 )*dt*Jb/2 )*dt*Ja )*dt*Ji...
   % + (2*I + (I + dt*Jc/2 )*dt*Jb )*dt*Ja + (2*I + dt*Jc )*dt*Jb + dt*Jc);

    %Vamos a calcular el adjunto del tangente lineal.
    
    adm=tlm';
   
   

 
   
   

