% Two coupled Lorenz '63 systems

function xnplusone = logistic(xn)

%-------------------------------------------------------------

xnplusone(1) = xn(2)*xn(1)*(1-xn(1));
xnplusone(2)=xn(2);


