function [state]=nature_run(x0,numtrans,par,h2,bst,numstep)

dim=length(x0);

%Integrate the model during the transient time.
%The solution converges to the atractor during this time.
x=x0;
for i=1:numtrans
    xout=stepit('L63eqs',x,par,h2,dim);
    x=xout;
end

% Redefine initial condition. x0 is now an initial condition in the model
% atractor.
x0 = x;

% 
% control run without assimilation
% state: true state 

x=x0;          % initial true state

% true state

for i=1:numstep
    for j=1:bst
        xout=stepit('L63eqs',x,par,h2,dim);
        x=xout;
    end
    state(i,:)=x;
end


end