%--------------------------------------------------------------
%     Lorenz 3-variable system with data assimilation
%     DA scheme: Ensemble Transform Kalman Filter
%     created by Shu-Chih Yang, 2005
%--------------------------------------------------------------
clear all
close all
set(0,'format','long')
randn('state',0)

%------------------------------------------------------------
% specify coupled system parameters
% %------------------------------------------------------------
% a      = 10.0d0;	% standard L63 
% par(1) = a;
% r      = 28.0d0;	% standard L63
% par(2) = r;
% b      = 8.0d0/3.d0;	% standard L63
% par(3) = b;

%------------------------------------------------------------
% Model and experienet setup
%------------------------------------------------------------
dim=3.0d0;            % specify dimension
h2=0.01d0;            % specify rk4 stepsize 
numtrans=600;         % specify transient time
numstep=2000;         % specify integration time
tskip=500;            % evaluate the error after tskip analysis cycles
x0=[8.0d0 0.0d0 30.0d0];      % specify initial conditions


%------------------------------------------------------------
% Data assimilation setup
%------------------------------------------------------------
dx0 = [5. 5. 5.];      % initial perturbation from the true
R0=2.0d0;              % the variance of observation error
iobs=[1;2;3];          % observation index (location)
bst=8;                 % assimilation/observation interval
K=3;                   % ensemble size
% parameters for inflation : e(1) - multiplicative inflation
%                            e(2) - additive inflation

alfa_a=1.057d0; %Initial multiplicative inflation.
v_a=1.0;        %Initial value for kf estimation of the inflation.

% declare arrays
p=length(iobs);         % no. of observation for one assimilation cycle
H=zeros(p,dim);         % initialize observation operator
yobs=zeros(numstep,p);  % array to load observations
obsgen=1;               % obsgen (1: generate new obs, 0: load from default file, -1: load from given file)
xfens=zeros(dim,K);     % background ensemble perturbations
xaens=zeros(dim,K);     % analysis ensemble perturbations
xabar=zeros(dim,1);     % analysis ensemble mean


% remove transient time
x=x0;
for i=1:numtrans
    xout=stepit(x,h2);
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

x=x0;          % initial true state

% true state

for i=1:numstep
    for j=1:bst
        xout=stepit(x,h2);
        x=xout;
    end
    state(i,:)=x;
end

% initialize the matrix operator for data assimilation
[R,H,yobs]=DA_init(numstep,p,dim,iobs,R0,state,obsgen);

% Experiments with Ensemble Kalman Filter

% create the initial ensemble perturbations
randn('state',0);
xrdn=1.0d0*randn(dim,K);

for k1=1:K
    xaens(:,k1)=state(1,:)'+dx0'+xrdn(:,k1); 
end

for i=1:numstep
    %Integrate the ensemble from the analysis
    for k1=1:K
        x=xaens(:,k1)';
        for j=1:bst
            xout=stepit(x,h2);
            x=xout;    
        end
        xfens(:,k1)=x';
    end

    statef(i,:)=mean(xfens,2);

    time(i)=i*bst*h2;

    % saving forecast (background) state
    
    % Data assimilation 
     [xaens,xabar,alfa_a,v_a]=RUN_ETKF_estinf(yobs(i,:),xfens,p,dim,K,H,R,alfa_a,v_a);    

    %saving analysis state
    statea(i,:)=xabar;
    statefens(i,:,:)=xfens;
    stateaens(i,:,:)=xaens;
    inflation(i)=alfa_a;
    
    %calculate the root mean square error
    
    err(i)= sqrt(sum((statea(i,:)-state(i,:)).^2)/3.d0);
    errf(i)= sqrt(sum((statef(i,:)-state(i,:)).^2)/3.d0);

end

% mean RMS analysis/background error
rmse_a=mean(err(tskip+1:numstep));
rmse_f=mean(errf(tskip+1:numstep));

fprintf(1,'Obseveraion error= %g \n',sqrt(R0))
fprintf(1,'Number of DA cycles= %g \n',numstep)
fprintf(1,'Mean background RMSE= %g \n',rmse_f)
fprintf(1,'Mean analysis RMSE= %g \n',rmse_a)

ind=[1500:2000];

% plot the error
DAerrplt(time(ind),err(ind),rmse_a,sqrt(R0),iobs,'ETKF (K=3)');
%print -f1 -dpng ../output/rmsea.png;

% plot the states vs. obs
stateplt(time(ind),yobs(ind,1),state(ind,1),statea(ind,1),'X')
%print -f2 -dpng ../output/states.png;

stateo=yobs;

%save('../output/analysis.mat','statea','statef','state','statefens','stateaens','stateo');

