%--------------------------------------------------------------
%     La idea de este script es introducir un error random en la
%     condicion inicial y ver el efecto que esto produce sobre la 
%     estimacion de los parametros.
%--------------------------------------------------------------
clear all
close all
set(0,'format','long')
randn('state',0)
maxNumCompThreads(2)
%EN ESTA VERSION SE ESTIMA UN PARAMETRO DE INFLACION PARA LAS VARIABLES DE
%ESTADO Y OTRO PARA LOS PARAMETROS.

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
dim=6.0d0;            % specify dimension
h2=0.01d0;            % specify rk4 stepsize 
numtrans=600;         % specify transient time
numstep=5000;         % specify integration time
tskip=500;            % evaluate the error after tskip analysis cycles
x0=[8.0d0 0.0d0 30.0d0 10.0d0 8.0/3.0 28.0];      % specify initial conditions


%------------------------------------------------------------
% Data assimilation setup
%------------------------------------------------------------
dx0 = [5. 5. 5. 3.0 1/3 3.0];      % initial perturbation from the true
R0=1.0d0;              % the variance of observation error
iobs=[1;2;3];          % observation index (location)
bst=8;                 % assimilation/observation interval
K=30;                   % ensemble size
% parameters for inflation : e(1) - multiplicative inflation
%                            e(2) - additive inflation
% optimal setup for K=3 e=[1.057d0,0.015];

alfa_a=1.057d0;
v_a=1.0d0;

% declare arrays
p=length(iobs);         % no. of observation for one assimilation cycle
H=zeros(p,dim);         % initialize observation operator
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
    xout=stepit_parameters(x,h2);
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

% true state with time varying parameters.

for i=1:numstep
    for j=1:bst
        xout=stepit_parameters(x,h2);
        x=xout;
        %x(4:6)=x0(4:6)+dx0(4:6)*cos(2*pi*((i-1)*bst+j)/(500*bst));
    end
    state(i,:)=x;
end

% initialize the matrix operator for data assimilation
[R,H,yobs]=DA_init(numstep,p,dim,iobs,R0,state,obsgen);

% Experiments with Ensemble Kalman Filter

% create the initial ensemble perturbations
randn('state',0);
xrdn(1:3,:)=1.0d0*randn(3,K);
xrdn(4,:)=0; %sigmapar(1)*randn(1,K);
xrdn(5,:)=sigmapar(2)*randn(1,K);
xrdn(6,:)=0; %sigmapar(3)*randn(1,K);

%xrdn(4:6,:)=xrdn(4:6,:);

for k1=1:K
    xaens(:,k1)=state(1,:)'+dx0'+xrdn(:,k1); 
end

for i=1:numstep
    %Integrate the ensemble from the analysis
    for k1=1:K
        x=xaens(:,k1)';
        for j=1:bst
            xout=stepit_parameters(x,h2);
            x=xout;    
        end
        xfens(:,k1)=x';
       
    end
    error=1*randn(3,1);
    errorens=repmat(error,[1 K]);
    xfens(1:3,:)=xfens(1:3,:)+errorens;

    statef(i,:)=mean(xfens,2);

    time(i)=i*bst*h2;

    % saving forecast (background) state
    
    % Data assimilation 
    [xaens,xabar,alfa_a,v_a]=RUN_ETKF_estinf(yobs(i,:),xfens,p,dim,K,H,R,alfa_a,v_a);
   
    
    P=((xfens-repmat(mean(xfens,2),[1 K]))*(xfens-repmat(mean(xfens,2),[1 K]))')/(K-1);
    Hprima=[0 0 1 0 0 0];
    tmp=inv(Hprima*P*Hprima'+1.0);
    update(i,:)=P*Hprima'*tmp*(yobs(i,3)'-Hprima*mean(xfens,2));
    %P=(eye(6)-P*Hprima'*tmp*Hprima)*P;
    std_par=0.5;
    mean_par=mean(xaens(5,:),2);
    tmp=randn(1,K)*std_par;
    tmp=tmp-mean(tmp);
    xaens(5,:)=mean_par+tmp;
    xabar(5,:)=mean_par;
    %xabar(5:6)=x0(5:6);
    %xaens(5,:)=x0(5);
    %xaens(6,:)=x0(6);

    %saving analysis state
    statea(i,:)=xabar;
    statefens(i,:,:)=xfens;
    stateaens(i,:,:)=xaens;
    inflation(i)=alfa_a;
    
    %calculate the root mean square error
    err(i)= sqrt(sum((statea(i,:)-state(i,:)).^2)/3.d0);
    errf(i)= sqrt(sum((statef(i,:)-state(i,:)).^2)/3.d0);

end




