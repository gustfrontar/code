% stepit.m
%
% For use with a system of autonomous differential equations.
%
% Performs numerical integration of a function by Runge-Kutta
% method, over the range (start_x, start_x + stepsize). Therefore
% only one iteration is performed.
%
% Useage;
% stepit('func', funcarg, parameters, stepsize, dimension)
%
% where func is the name of the function to be integrated, funcarg is the 
% left hand limit of the integration, parameters contains any necessary
% parameters to pass to the derivs subroutine and stepsize is the 
% integration stepsize.
%
% You would normally include this in a loop.

function test = stepit(func, funcarg, parameters, stepsize, dim)

   x = funcarg;   
   p = parameters;
   f = feval(func, x, p, dim);
   
   c1 = stepsize .* f;   
   x = funcarg + c1 /2.0d0;   
   f = feval(func, x, p, dim);
   
   c2 = stepsize .* f;   
   x = funcarg + c2 /2.0d0;   
   f = feval(func, x, p, dim);
   
   c3 = stepsize .* f;   
   x = funcarg + c3;   
   f = feval(func, x, p, dim);
   
   c4 = stepsize .* f;   
   funcarg = funcarg + (c1 + 2.*c2 + 2.*c3 + c4)./6.0d0;   
   test = funcarg;

