%--------------------------------------------------------------
%     Lorenz 3-variable system with data assimilation
%     DA scheme: Extended Kalman Filter
%     Juan Ruiz, 2011
%     based on the code by Shu-Chi-Yang
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
dim=6.0d0;            % specify dimension (3 new dimensions for the parameter)
dt=0.01d0;            % specify rk4 stepsize 
numtrans=600;         % specify transient time
numstep=2000;         % specify integration time
tskip=500;            % evaluate the error after tskip analysis cycles
x0=[8.0d0 0.0d0 30.0d0 10.0d0 8.0d0/3.0d0 28.0d0];      % specify initial conditions (including parameters)


%------------------------------------------------------------
% Data assimilation setup
%------------------------------------------------------------
dx0 = [5. 5. 5. 1.0 1/3 1.0];      % initial perturbation from the true
R0=2.0d0;              % the variance of observation error
iobs=[1;2;3];          % observation index (location)
bst=8;                 % assimilation/observation interval
ro=0.005d0;             % covariance inflation.
%ro=0;

% declare arrays
p=length(iobs);         % no. of observation for one assimilation cycle
H=zeros(p,dim);         % initialize observation operator
yobs=zeros(numstep,p);  % array to load observations
obsgen=1;               % obsgen (1: generate new obs, 0: load from default file, -1: load from given file)

% remove transient time
x=x0;
for i=1:numtrans
    xout=stepit_parameters(x,dt);
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
        xout=stepit_parameters(x,dt);
        x=xout;
    end
    state(i,:)=x;
end

% initialize the matrix operator for data assimilation
[R,H,yobs]=DA_init(numstep,p,dim,iobs,R0,state,obsgen);

P0f=eye(dim);


% Experiments with EKF

statea(1,:)=state(1,:)'+P0f*randn(dim,1);
%statea(1,:)=[0.1 0.1 0.1]
Pa=P0f;

for i=2:numstep
    
    %Vamos a hacer el pronostico de x con el modelo no lineal y el
    %pronostico de la covarianza con el tangente lineal y el adjunto.
    x=statea(i-1,:);
    P=Pa;
    TLM=eye(dim);
    for j=1:bst
        [tlm,adm] = stepit_tl_ad_parameters(x,dt);
        TLM=tlm*TLM;
        x=stepit_parameters(x,dt);  
    end
    statef(i,:)=x;
    Pf=TLM*Pa*TLM';
    
    Pf(1:3,1:3)=Pf(1:3,1:3)+ro*diag(abs(randn(1,3)));
    Pf(4:6,4:6)=Pf(4:6,4:6)+0.01*ro*diag(abs(randn(1,3)));
    %Pf=Pf+ro*diag(abs(randn(1,dim)));
    %Only perform data assimilation if we have observations for all the
    %times within the data assimilation window.
    
    % Data assimilation 
     [xa,Pa]=RUN_EKF(yobs(i,:),statef(i,:),Pf,H,R); 
     

     
     statea(i,:)=xa;
     

    %calculate the root mean square error
    
    err(i)= sqrt(sum((statea(i,:)-state(i,:)).^2)/3.d0);
    errf(i)=sqrt(sum((statef(i,:)-state(i,:)).^2)/3.d0);
    
    Pft(:,:,i)=Pf;
    time(i)=i*bst*dt;
end

% mean RMS analysis/background error
rmse_a=mean(err(tskip+1:numstep));


fprintf(1,'Obseveraion error= %g \n',sqrt(R0))
fprintf(1,'Number of DA cycles= %g \n',numstep)
fprintf(1,'Mean analysis RMSE= %g \n',rmse_a)

ind=[1500:2000];

% plot the error
DAerrplt(time(ind),err(ind),rmse_a,sqrt(R0),iobs,'ETKF (K=3)');
%print -f1 -dpng ../output/rmsea.png;

% plot the states vs. obs
stateplt(time(ind),yobs(ind,1),state(ind,1),statea(ind,1),'X')
%print -f2 -dpng ../output/states.png;

stateo=yobs;


