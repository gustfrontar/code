% stepit.m


function [tlm,adm] = stepit_tl_ad_parameters(xnl,dt)
%EN ESTE CASO LOS PARAMETROS ESTAN AUMENTADOS AL ESTADO.
%xnl es la condicion inicial total (para la trayectoria no lineal)
%xi es la perturbacion 

   
%Calculo la trayectoria no lineal dentro del paso de tiempo alrededor de la
%cual voy a obtener el tangente lineal y el adjunto.

   f = lorenz_parameters(xnl);
   
   c1 = dt .* f;   
   xa = xnl + c1 /2.0d0;   
   f = lorenz_parameters(xa);
   
   c2 = dt .* f;   
   xb = xnl + c2 /2.0d0;   
   f = lorenz_parameters(xb);
   
   c3 = dt .* f;   
   xc = xnl + c3;   
   %f = lorenz(xc);
   
   %c4 = dt .* f;   
   %xend = xi + (c1 + 2.*c2 + 2.*c3 + c4)./6.0d0;   
   
   
   %Vamos a calcular el modelo tangente lineal.
   
   I=eye(length(xnl));
   Ji=lorenz_tl_parameters(xnl);
   Ja=lorenz_tl_parameters(xa);
   Jb=lorenz_tl_parameters(xb);
   Jc=lorenz_tl_parameters(xc);
   
   tlm= I+dt*(1/6)*( Ji + 2*Ja*( Ji*dt/2 + I ) + 2*Jb*( Ja*( Ji*dt/2 + I )*dt/2 + I  ) ...
        + Jc*(  dt*Jb*( Ja*( Ji*dt/2 + I )*dt/2 +I ) + I ) );
 
    
   %tlm=I +(1/6)*((I + (I + (I + dt*Jc/2 )*dt*Jb/2 )*dt*Ja )*dt*Ji...
   % + (2*I + (I + dt*Jc/2 )*dt*Jb )*dt*Ja + (2*I + dt*Jc )*dt*Jb + dt*Jc);

    %Vamos a calcular el adjunto del tangente lineal.
    
    adm=tlm';
   
   

 
   
   

