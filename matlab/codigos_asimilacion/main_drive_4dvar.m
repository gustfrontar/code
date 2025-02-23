%--------------------------------------------------------------
%     Lorenz 3-variable system with data assimilation
%     DA scheme: strong constrain 4DVAR
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
dim=3.0d0;            % specify dimension
dt=0.01d0;            % specify rk4 stepsize 
numtrans=600;         % specify transient time
numstep=500;         % specify integration time
tskip=500;            % evaluate the error after tskip analysis cycles
x0=[8.0d0 0.0d0 30.0d0];      % specify initial conditions


%------------------------------------------------------------
% Data assimilation setup
%------------------------------------------------------------
dx0 = [5. 5. 5.];      % initial perturbation from the true
R0=2.0d0;              % the variance of observation error
iobs=[1;2;3];          % observation index (location)
bst=8;                 % assimilation/observation interval
lengthwin=4;           % assimilation window (number of bst).


% declare arrays
p=length(iobs);         % no. of observation for one assimilation cycle
H=zeros(p,dim);         % initialize observation operator
yobs=zeros(numstep,p);  % array to load observations
obsgen=1;               % obsgen (1: generate new obs, 0: load from default file, -1: load from given file)

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

x=x0;          % initial true state

% true state

for i=1:numstep
    for j=1:bst
        xout=stepit(x,dt);
        x=xout;
    end
    state(i,:)=x;
end

% initialize the matrix operator for data assimilation
[R,H,yobs]=DA_init(numstep,p,dim,iobs,R0,state,obsgen);


B0=[0.065756210718056   0.067234686711634   0.005607020427899;...
   0.067234686711634   0.149885331553173   0.008407504326656;...
   0.005607020427899   0.008407504326656   0.344431817511781];


% Experiments with 4DVAR

stateasmooth(1,:)=state(1,:)'+dx0'.*rand(3,1);

for i=1:numstep

    
    if( i > lengthwin)
    %Only perform data assimilation if we have observations for all the
    %times within the data assimilation window.
    
    % Data assimilation 
     [xa,xasmooth]=RUN_4DVAR(yobs(i-lengthwin:i,:),stateasmooth(i-lengthwin,:),H,R,B0,lengthwin,dt,bst);    

     stateasmooth(i-lengthwin:i,:)=xasmooth; %Save the smoothed solution.
     
     statea(i,:)=xa;

    %calculate the root mean square error
    
    err(i)= sqrt(sum((statea(i,:)-state(i,:)).^2)/3.d0);
    errs(i)=sqrt(sum((stateasmooth(i-lengthwin,:)-state(i-lengthwin,:)).^2)/3.d0);
    
    
    
    else
        
    statea(i,:)= state(1,:)'+dx0'.*randn(3,1);  
    err(i)=NaN;
    errs(i)=NaN;
    end

end

stateo=yobs;


