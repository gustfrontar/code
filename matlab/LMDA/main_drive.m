%--------------------------------------------------------------
%     Lorenz 3-variable system with data assimilation
%     DA scheme: Ensemble Kalman Filter
%     created by Shu-Chih Yang, 2005
%--------------------------------------------------------------
clear all
close all
set(0,'format','long')
randn('seed',0)

%------------------------------------------------------------
% Data assimilation setup
%------------------------------------------------------------
dx0=[0.1 0.1 0.1];        % initial perturbation from the true
R0=2.0;                   % the variance of observation error
iobs=[1];                 % observation index (location)
bst=40;                    % assimilation/observation interval (in number of model time steps)
ens_size=20;              % ensemble size
inflation=1.06;           % Multiplicative inflation parameter.
numstep=500;              % Number of data assimilation cycles to be performed.

%------------------------------------------------------------
% Lorenz model parameters setup.
%------------------------------------------------------------
par(1)=10.0;    % sigma parameter in standard L63
par(2)=28.0;    % rho parameter in standard L63
par(3)=8.0/3.0; % beta parameter in standard L63

%------------------------------------------------------------
% Integration and initialization setup.
%------------------------------------------------------------
h2=0.01;           % specify Runge-Kutta 4 time step
numtrans=600;      % specify transient time
x0=[8.0 0.0 30.0]; % specify initial conditions

%------------------------------------------------------------
% Start the experiment. Generate the "nature" run.
%------------------------------------------------------------

%Run the model without assimilating observations to generate the "nature
%run"
[state]=nature_run(x0,numtrans,par,h2,bst,numstep);

%state contains the nature run state every bst model time steps.

%------------------------------------------------------------
% Generate synthetic observations as well as R and H matrices
% (in this experiment h is linear so h(x) = H * x ;
%------------------------------------------------------------

% initialize the matrix operator for data assimilation
[R,H,yobs]=DA_init(state,iobs,R0);

%R is the observation error matrix
%H is the observation operator
%yobs contains the observations every bst model time step.

%------------------------------------------------------------
% Generate the ensemble for the first step. 
%------------------------------------------------------------

% create the initial ensemble perturbations
xrdn=1.0*randn(length(x0),ens_size);

for iens=1:ens_size
    xaens(:,iens)=state(1,:)'+dx0'+xrdn(:,iens); 
end

%------------------------------------------------------------
% Perform the data assimilation cycle experiment.
% This for controls the assimilation steps or cycles.
%------------------------------------------------------------

for i=1:numstep %Loop over time.
    
    % disp iteration
    i
    
    %Run the model to generate the ensemble of forecasts.
    [xfens]=run_ensemble_forecast(xaens,par,h2,bst); 
    
    % Get the data for the current time. 
    obs=yobs(i,:); 
    
    % Data assimilation.
    [xaens,xabar,xfens,xfbar,d,K,Pf,Pa]=RUN_EnKF(obs,xfens,H,R,inflation);  
    
    %saving the analysis and background state mean.
    statef(i,:)=xfbar;
    statea(i,:)=xabar;
    statefens(i,:,:)=xfens;
    stateaens(i,:,:)=xaens;
    %saving the analysis and background state spread.
    spreadf(i,:)=diag(Pf);
    spreada(i,:)=diag(Pa);
    
end

%------------------------------------------------------------
% Plot the results.
%------------------------------------------------------------

plot_results(state,statea,statef,stateaens,statefens,spreada,spreadf,yobs,iobs,R0);

