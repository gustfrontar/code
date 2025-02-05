% stepit.m


function [xend] = stepit_parameters_trend(xi,dt)

%xnl es la condicion inicial total (para la trayectoria no lineal)
%xi es la perturbacion 

   
%Calculo la trayectoria no lineal dentro del paso de tiempo alrededor de la
%cual voy a obtener el tangente lineal y el adjunto.

   f = lorenz_parameters_trend(xi);
   
   c1 = dt .* f;   
   xa = xi + c1 /2.0d0;   
   f = lorenz_parameters_trend(xa);
   
   c2 = dt .* f;   
   xb = xi + c2 /2.0d0;   
   f = lorenz_parameters_trend(xb);
   
   c3 = dt .* f;   
   xc = xi + c3;   
   f = lorenz_parameters_trend(xc);
   
   c4 = dt .* f;   
   xend = xi + (c1 + 2.*c2 + 2.*c3 + c4)./6.0d0;
   
   
   

   
   

 
   
   

