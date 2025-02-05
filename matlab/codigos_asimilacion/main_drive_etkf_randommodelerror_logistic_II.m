%--------------------------------------------------------------
%     La idea de este script es introducir un error random en la
%     condicion inicial y ver el efecto que esto produce sobre la 
%     estimacion de los parametros.
%--------------------------------------------------------------
clear all
close all
set(0,'format','long')
randn('state',0)

%EN ESTA VERSION SE ESTIMA UN PARAMETRO DE INFLACION PARA LAS VARIABLES DE
%ESTADO Y OTRO PARA LOS PARAMETROS.

%
%------------------------------------------------------------
% Model and experienet setup
%------------------------------------------------------------
dim=2.0d0;            % specify dimension
%h2=0.01d0;            % specify rk4 stepsize 
numtrans=600;         % specify transient time
numstep=10000;         % specify integration time
tskip=500;            % evaluate the error after tskip analysis cycles
x0=[0.5 3.6];      % specify initial conditions


%------------------------------------------------------------
% Data assimilation setup
%------------------------------------------------------------
dx0 = [0.1 0.2];      % initial perturbation from the true
R0=0.001d0;              % the variance of observation error
iobs=[1];          % observation index (location)
bst=2;                 % assimilation/observation interval
K=10;                   % ensemble size
% parameters for inflation : e(1) - multiplicative inflation
%                            e(2) - additive inflation
% optimal setup for K=3 e=[1.057d0,0.015];

alfa_a=1.057d0;
v_a=1.0d0;

% declare arrays
p=length(iobs);         % no. of observation for one assimilation cycle
H=1;         % initialize observation operator
yobs=zeros(numstep,p);  % array to load observations
obsgen=1;               % obsgen (1: generate new obs, 0: load from default file, -1: load from given file)
xfens=zeros(dim,K);     % background ensemble perturbations
xaens=zeros(dim,K);     % analysis ensemble perturbations
xabar=zeros(dim,1);     % analysis ensemble mean
sigmapar=[0.6 0.2 0.6]; %Parameter ensemble standar deviation.
amppar=[0.6 0.2 0.6];
%sigmapar=[1.0 0.1 1.0]

% remove transient time
x=x0;
for i=1:numtrans
    x=logistic(x);
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

% true state with time varying parameters.

for i=1:numstep
    for j=1:bst
        x=logistic(x);
    end
    state(i,:)=x;
end

% initialize the matrix operator for data assimilation
[R,H,yobs]=DA_init(numstep,p,dim,iobs,R0,state,obsgen);

% Experiments with Ensemble Kalman Filter


for ierr=1:10
    tic
    error(ierr)=0.005*(ierr-1);
      
    randn('state',0);
    xrdn=0.1d0*randn(2,K);

    for k1=1:K
    xaens(:,k1)=state(1,:)'+dx0'+xrdn(:,k1); 
    end
    
for i=1:numstep
    %Integrate the ensemble from the analysis
    xaens(1,:)=xaens(1,:)+error(ierr)*randn;
    for k1=1:K
        x=xaens(:,k1)';
        for j=1:bst
            x=logistic(x);    
        end
        xfens(:,k1)=x';
       
    end
    %error=randn(3,1);
    %errorens=repmat(error,[1 K]);
    %xfens(1,:)=xfens(1,:)+error(ierr)*randn;

    statef(i,:)=mean(xfens,2);

    time(i)=i*bst;

    % saving forecast (background) state
    
    % Data assimilation 
    [xaens,xabar,alfa_a,v_a]=RUN_ETKF_estinf(yobs(i,:),xfens,p,dim,K,H,R,alfa_a,v_a);
    
    %saving analysis state
    statea(i,:)=xabar;
    statefens(i,:,:)=xfens;
    stateaens(i,:,:)=xaens;
    inflation(i)=alfa_a;
    
    %calculate the root mean square error
    err(i)= sqrt(sum((statea(i,1)-state(i,1)).^2)/3.d0);
    errf(i)= sqrt(sum((statef(i,1)-state(i,1)).^2)/3.d0);

end

toc

parametro(ierr)=mean(statea(:,2));
rmse(ierr)=sqrt(mean((statea(:,1)-state(:,1)).^2));
bias(ierr)=mean((statea(:,1)-state(:,1));

end

figure
plot(error,parametro,'LineWidth',2)
xlabel('Error')
ylabel('Parametro estimado')
figure
plot(error,rmse,'LineWidth',2)
figure
plot(error,bias,'LineWidth',2)


