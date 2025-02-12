% Two coupled Lorenz '63 systems

function x_dot = lorenz(x)

%------------------------------------------------------------
% specify coupled system parameters
%------------------------------------------------------------
a      = 10.0d0;	% standard L63 
r      = 28.0d0;	% standard L63
b      = 8.0d0/3.d0;	% standard L63
%-------------------------------------------------------------

x_dot(1) = a*(x(2) - x(1));
x_dot(2) = -x(1)*x(3) + r*x(1) - x(2);
x_dot(3) = x(1)*x(2) - b*x(3);	


