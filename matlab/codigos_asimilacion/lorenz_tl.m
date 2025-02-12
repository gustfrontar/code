% Two coupled Lorenz '63 systems

function [TL] = lorenz_tl(xnl)

%xnl es el estado de la trayectoria no lineal alrededor de la cual se
%calcula el modelo tangente lineal.

%------------------------------------------------------------
% specify coupled system parameters
%------------------------------------------------------------
a      = 10.0d0;	% standard L63 
r      = 28.0d0;	% standard L63
b      = 8.0d0/3.d0;	% standard L63
%-------------------------------------------------------------

TL=[-a a 0;-xnl(3)+r -1 -xnl(1);xnl(2) xnl(1) -b];




