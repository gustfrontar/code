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
totaltime=2000*8;     % Total time (timesteps)
tskip=500;            % evaluate the error after tskip analysis cycles
x0=[8.0d0 0.0d0 30.0d0];      % specify initial conditions
obsfrec=1;            % observation frequency in time steps.

%------------------------------------------------------------
% Data assimilation setup
%------------------------------------------------------------
dx0 = [5. 5. 5.];      % initial perturbation from the true
R0=2.0d0;              % the variance of observation error
iobs=[1 2 3];          % observation index (location)
bst=8;                 % assimilation/observation interval
K=3;                   % ensemble size
numstep=totaltime/bst;         % specify integration time
% parameters for inflation : e(1) - multiplicative inflation
%                            e(2) - additive inflation
% optimal setup for K=3 e=[1.057d0,0.015];

e(1)=1.057d0;
e(2)=0.0150;

% declare arrays
p=length(iobs);         % no. of observation for one assimilation cycle
xfens=zeros(dim,K);     % background ensemble perturbations
xaens=zeros(dim,K);     % analysis ensemble perturbations
xabar=zeros(dim,1);     % analysis ensemble mean

nslots=ceil(1.5*bst);   %Total number of time steps in the forecast 
anaslot=bst;            %Time step corresponding to the analysis time.

Rdiag=R0*ones(p,1);

%Generate H
H=zeros(p,dim);
for i=1:p
    H(i,iobs(i))=1.d0;
end

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
eostd=sqrt(R0);

for i=1:numstep
    for j=1:bst
        xout=stepit(x,h2);
        x=xout;
        erro=eostd*randn(1,p);
        yobs(:,(i-1)*bst+j)=xout+erro;
    end
    state(i,:)=x;
 
end

%Keep only the observations that will be assimilated.
time=1:size(yobs,2);
yobs=yobs(iobs,obsfrec:obsfrec:max(time));
timeobs=time(obsfrec:obsfrec:max(time));



% Experiments with Ensemble Kalman Filter

% create the initial ensemble perturbations
randn('state',0);
xrdn=1.0d0*randn(dim,K);

for k1=1:K
    xaens(:,k1)=state(1,:)'+dx0'+xrdn(:,k1); 
end

numstep=numstep-1;
for i=1:numstep
    %Integrate the ensemble from the analysis
    for k1=1:K
        x=xaens(:,k1)';
        for j=1:bst+ceil(bst/2);
            xout=stepit(x,h2);
            x=xout;   
            xfens_4d(:,k1,j)=x'; %4D Ensemble.
            enstime(j)=(i-1)*bst+j;
            if( j == bst)
                xfens(:,k1)=x';
            end
        end
    end

    statef(i,:)=mean(xfens,2);
    
    %Define the time window.
    time_start=i*bst - floor(bst/2) + 1;
    time_end=  i*bst + ceil(bst/2)     ;
    
    observation_index=( timeobs > time_start & timeobs < time_end );
    ensemble_index=( enstime > time_start & enstime < time_end );
    tmpobs=yobs(:,observation_index);
    
    %Keep only the ensemble times at the observation times.
    xfens_4d=xfens_4d(:,:,ensemble_index);
    
    % saving forecast (background) state
    
    % Data assimilation 
     
     [xaens,xabar]=RUN_4DETKF(tmpobs,xfens,xfens_4d,dim,K,H,Rdiag,e);    

    
    %saving analysis state
    statea(i,:)=xabar;
    statefens(i,:,:)=xfens;
    stateaens(i,:,:)=xaens;
    
    stateasprd(i,:)=std(xaens,[],2);
    statefsprd(i,:)=std(xfens,[],2);
    
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


