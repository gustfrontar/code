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
dim=6.0d0;            % specify dimension
dt=0.01d0;            % specify rk4 stepsize 
numtrans=600;         % specify transient time
numstep=500;         % specify integration time
tskip=500;            % evaluate the error after tskip analysis cycles
x0=[8.0d0 0.0d0 30.0d0 10.0d0 8/3 28.0d0];      % specify initial conditions
sigmapar=[0.6 0.2 0.6];         %Parameter ensemble standar deviation.

%------------------------------------------------------------
% Data assimilation setup
%------------------------------------------------------------
dx0 = [5. 5. 5. 1.0 1/3 1.0];      % initial perturbation from the true
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


B0=[0.065756210718056   0.067234686711634   0.005607020427899 0.0d0 0.0d0 0.0d0;...
   0.067234686711634   0.149885331553173   0.008407504326656  0.0d0 0.0d0 0.0d0;...
   0.005607020427899   0.008407504326656   0.344431817511781  0.0d0 0.0d0 0.0d0
   0.0d0               0.0d0               0.0d0              sigmapar(1) 0.0d0 0.0d0;
   0.0d0               0.0d0               0.0d0              0.0d0 sigmapar(2) 0.0d0;
   0.0d0               0.0d0               0.0d0              0.0d0 0.0d0 sigmapar(3)];

% Experiments with 4DVAR

stateasmooth(1,:)=state(1,:)'+dx0'.*rand(dim,1);

for i=1:numstep

    
    if( i > lengthwin)
    %Only perform data assimilation if we have observations for all the
    %times within the data assimilation window.
    
    % Data assimilation 
     [xa,xasmooth]=RUN_4DVAR_parameters(yobs(i-lengthwin:i,:),stateasmooth(i-lengthwin,:),H,R,B0,lengthwin,dt,bst);    

     stateasmooth(i-lengthwin:i,:)=xasmooth; %Save the smoothed solution.
     
     statea(i,:)=xa;

    %calculate the root mean square error
    
    err(i)= sqrt(sum((statea(i,:)-state(i,:)).^2)/3.d0);
    errs(i)=sqrt(sum((stateasmooth(i-lengthwin,:)-state(i-lengthwin,:)).^2)/3.d0);
%    errf(i)=sqrt(sum((statef(i,:)-state(i,:)).^2)/3.d0);
    
    
    else
        
    statea(i,:)= state(1,:)'+dx0'.*randn(dim,1);  
    err(i)=NaN;
    errs(i)=NaN;
    errf(i)=NaN;
    end

end

stateo=yobs;
% mean RMS analysis/background error
rmse_a=mean(err(tskip+1:numstep));
rmse_f=mean(err(tskip+1:numstep));
%rmse_f=mean(errf(tskip+1:numstep));

fprintf(1,'Obseveraion error= %g \n',sqrt(R0))
fprintf(1,'Number of DA cycles= %g \n',numstep)
%fprintf(1,'Mean background RMSE= %g \n',rmse_f)
fprintf(1,'Mean analysis RMSE= %g \n',rmse_a)

ind=[1500:2000];

% plot the error
DAerrplt(time(ind),err(ind),rmse_a,sqrt(R0),iobs,'ETKF (K=3)');
%print -f1 -dpng ../output/rmsea.png;

% plot the states vs. obs
stateplt(time(ind),yobs(ind,1),state(ind,1),statea(ind,1),'X')
%print -f2 -dpng ../output/states.png;
figure
plot(std(stateaens(:,4:6,:),[],3))
figure
plot(statea(:,4:6))
stateo=yobs;

%save('../output/analysis.mat','statea','statef','state','statefens','stateaens','stateo');

