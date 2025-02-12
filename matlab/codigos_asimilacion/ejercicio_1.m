%--------------------------------------------------------------

%--------------------------------------------------------------
clear all
close all
set(0,'format','long')
randn('state',0)

%------------------------------------------------------------
% Model and experienet setup
%------------------------------------------------------------
dim=3.0d0;            % specify dimension
dt=0.01d0;            % specify rk4 stepsize 
numtrans=600;         % specify transient time
numstep=1000;         % specify integration time
tskip=500;            % evaluate the error after tskip analysis cycles
x0=[8.0d0 0.0d0 30.0d0];      % specify initial conditions
bst=8;

% remove transient time
x=x0;
for i=1:numtrans
    xout=stepit(x,dt);
    x=xout;
end

% redefine initial conditions
x0 = x;

% 
% control run without assimilation
% traj: true trajectory
% state: true state 
% statep: perturbed state (no assimilation)
% errp: error variance

% true state
state(1,:)=x0;
state2(1,:)=x0;
pert=rand(1,3);

for i=2:numstep
    pert=1e-1*pert/(pert*pert');
    x2=state(i-1,:)+pert;
    x=state(i-1,:);
    for j=1:bst
        x=stepit(x,dt);
        x2=stepit(x2,dt);
    end
    state(i,:)=x;
    state2(i,:)=x2;
    pert=state2(i,:)-state(i,:);
    %pert=randn(1,3);
end



diffstate=sum((state2-state)*(state2-state)');

meandiffstate=mean(diffstate);
stddiffstate=std(diffstate);

figure
hold on
plot(state(diffstate > meandiffstate+stddiffstate/2,1),state(diffstate > meandiffstate+stddiffstate/2,2),'ro');
plot(state(diffstate < meandiffstate-stddiffstate/2,1),state(diffstate < meandiffstate-stddiffstate/2,2),'go');



