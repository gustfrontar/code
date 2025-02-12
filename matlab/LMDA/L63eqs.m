% Two coupled Lorenz '63 systems

function lorenz = lor(x,par,n)

a      = par(1);
r      = par(2);
b      = par(3);

x_dot_1 = a*(x(2) - x(1));
y_dot_1 = -x(1)*x(3) + r*x(1) - x(2);
z_dot_1 = x(1)*x(2) - b*x(3);	

lorenz = [x_dot_1 y_dot_1 z_dot_1];

