% Two coupled Lorenz '63 systems
% IN THIS FUNCTION THE PARAMETERS ARE AUGMENTED TO THE STATE
% A=x(4), B=x(5) and R=x(6).

function x_dot = lorenz_parameter_trend(x)

x_dot(1) = x(4)*(x(2) - x(1));
x_dot(2) = -x(1)*x(3) + x(6)*x(1) - x(2);
x_dot(3) = x(1)*x(2) - x(5)*x(3);
x_dot(4) = x(7);    %Linear trend is assumed for the parameters within the assimilation window.
x_dot(5) = x(8);
x_dot(6) = x(9);
x_dot(7) = 0;
x_dot(8) = 0;
x_dot(9) = 0;



