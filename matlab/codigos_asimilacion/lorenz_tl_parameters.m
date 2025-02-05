% Two coupled Lorenz '63 systems

function [TL] = lorenz_tl_parameters(xnl)

%xnl es el estado de la trayectoria no lineal alrededor de la cual se
%calcula el modelo tangente lineal.

%------------------------------------------------------------
% specify coupled system parameters
%------------------------------------------------------------
a      = 10.0d0;	% standard L63 
r      = 28.0d0;	% standard L63
b      = 8.0d0/3.d0;	% standard L63
%-------------------------------------------------------------

TL=[-xnl(4) xnl(4) 0 xnl(2)-xnl(1) 0 0;-xnl(3)+xnl(6) -1 -xnl(1) 0 0 xnl(1);xnl(2) xnl(1) -xnl(5) 0 -xnl(3) 0; 0 0 0 0 0 0;0 0 0 0 0 0;0 0 0 0 0 0];




