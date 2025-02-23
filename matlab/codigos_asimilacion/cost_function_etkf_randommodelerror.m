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
R0=2.0d0;              % the variance of observation error
iobs=[1;2;3];          % observation index (location)
bst=8;                 % assimilation/observation interval
K=30;                   % ensemble size

error_factor=0.0
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
xrdn(4,:)=sigmapar(1)*randn(1,K);
xrdn(5,:)=sigmapar(2)*randn(1,K);
xrdn(6,:)=sigmapar(3)*randn(1,K);

xrdn(4:6,:)=xrdn(4:6,:);

for k1=1:K
    xaens(:,k1)=state(1,:)'+dx0'+xrdn(:,k1); 
end

%Calculo el analisis. Luego a partir de este analisis voy a calcular la
%funcion de costo perfecta e imperfecta para alguno de los parametros del
%modelo.

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
    %error=randn(3,1);
    %errorens=repmat(error,[1 K]);
    %xfens(1:3,:)=xfens(1:3,:)+error_factor*errorens;

    statef(i,:)=mean(xfens,2);

    time(i)=i*bst*h2;

    % saving forecast (background) state
    
    % Data assimilation 
    [xaens,xabar,alfa_a,v_a]=RUN_ETKF_estinf(yobs(i,:),xfens,p,dim,K,H,R,alfa_a,v_a);
    
    xabar(5:6)=x0(5:6);
    xaens(5,:)=x0(5);
    xaens(6,:)=x0(6);

    %saving analysis state
    statea(i,:)=xabar;
    statefens(i,:,:)=xfens;
    stateaens(i,:,:)=xaens;
    inflation(i)=alfa_a;
    
    %calculate the root mean square error
    err(i)= sqrt(sum((statea(i,:)-state(i,:)).^2)/3.d0);
    errf(i)= sqrt(sum((statef(i,:)-state(i,:)).^2)/3.d0);

end


%Calculo la funcion de costo.
npar=100;
minpar=0;
maxpar=20;

Jimperfect=NaN(3,npar,numstep-500);
Jperfect=NaN(3,npar,numstep-500);

for i=500:numstep-1
    %Integrate the ensemble from the analysis
    error=randn(1,3);
    for ipar=1:npar
        parameter(ipar)=(ipar-1)*(maxpar-minpar)/(npar-1)+minpar;
        x=statea(i,:);
        x(1:3)=x(1:3)+error_factor*error;
        x(4)=parameter(ipar);
        for j=1:bst
            xout=stepit_parameters(x,h2);
            x=xout;    
        end
        Jimperfect(:,ipar,i-500+1)=(x(1:3)-state(i+1,1:3)).^2;
        
        x=state(i,:);
        x(4)=parameter(ipar);
        for j=1:bst
            xout=stepit_parameters(x,h2);
            x=xout;    
        end    
        Jperfect(:,ipar,i-500+1)=(x(1:3)-state(i+1,1:3)).^2;
        
    end
end

figure
hold on
meanJp=nanmean(nanmean(Jperfect,3),1);
meanJi=nanmean(nanmean(Jimperfect,3),1);
plot(parameter,meanJp-min(meanJp),'b','LineWidth',3)
plot(parameter,meanJi-min(meanJi),'r','LineWidth',3)
grid on

meanJpt=squeeze(nanmean(Jperfect,1));
meanJit=squeeze(nanmean(Jimperfect,1));
for ii=1:size(Jperfect,3)
    
    [minj imin]=min(squeeze(meanJpt(:,ii)));
MinParp(ii)=parameter(imin);
    [minj imin]=min(squeeze(meanJit(:,ii)));
MinPari(ii)=parameter(imin);
    
end



% mean RMS analysis/background error
rmse_a=mean(err(tskip+1:numstep));
rmse_f=mean(errf(tskip+1:numstep));

fprintf(1,'Obseveraion error= %g \n',sqrt(R0))
fprintf(1,'Number of DA cycles= %g \n',numstep)
fprintf(1,'Mean background RMSE= %g \n',rmse_f)
fprintf(1,'Mean analysis RMSE= %g \n',rmse_a)

ind=[1500:2000];

%FIGURES
%Analysis state vs obs and true state.
range=1:200;
figure
hold on

plot(state(range,1),'k-','LineWidth',2);
plot(mean(statefens(range,1,:),3),'b-','LineWidth',2);
plot(yobs(range,1),'o','MarkerFaceColor','r');
legend('Reconstructed trayectory','True Trayectory')

%FIGURES
%Three dimensional trajectory.
figure
plot3(state(range,1),state(range,2),state(range,3));


%Analysis state vs obs and true state.
range=1:200;
figure
subplot(2,1,1)
hold on
plot(state(range,1),'k-','LineWidth',2);
plot(mean(statefens(range,1,:),3),'r-','LineWidth',2);
plot(yobs(range,1),'o','MarkerFaceColor','r','MarkerEdgeColor','r');
legend('True trayectory','Reconstructed Trayectory','Individual ensemble members')
title('X variable as a function of time')
subplot(2,1,2)

hold on
plot(mean(statefens(range,1,:),3),'r-','LineWidth',2);
plot(squeeze(statefens(range,1,:)),'b-','LineWidth',0.5);
plot(mean(statefens(range,1,:),3),'r-','LineWidth',2);
%plot(state(range,1),'k-','LineWidth',2);
%plot(yobs(range,1),'o','MarkerFaceColor','r');
legend('Reconstructed trayectory','Individual ensemble members')
title('X variable as a function of time')

%Evolucion temporal del segundo momento de la PDF para el error de X y
%comparacion con el error de X.

figure
subplot(2,1,1)
hold on
plot((statea(range,2)-state(range,2)).^2,'k-','LineWidth',2);
plot(squeeze(covarens(2,2,range)),'r-','LineWidth',2);
legend('Error','Ensemble spread')
title('X variable')

subplot(2,1,2)
hold on
plot((statea(range,3)-state(range,3)).^2,'k-','LineWidth',2);
plot(squeeze(covarens(3,3,range)),'r-','LineWidth',2);
legend('Error','Ensemble spread')
title('Z variable')


%Evolucion temporal de las covarianzas.

figure
hold on
plot(squeeze(covarens(2,1,range)),'b-','LineWidth',2);
plot(squeeze(covarens(3,1,range)),'r-','LineWidth',2);
legend('COV X-Y','COV Z-X')
set(gca,'XGrid','On','YGrid','On')


%Evolucion parametros estimados
%Evolucion parametros estimados
range=1:500;
figure
subplot(2,1,1)
hold on
plot(state(range,4),'k-','LineWidth',3);
plot(mean(statefens(range,4,:),3),'r-','LineWidth',3);

legend('True parameter','Estimated parameter','Individual ensemble members')
title('X variable as a function of time')
subplot(2,1,2)
hold on
plot(mean(statefens(range,4,:),3),'r-','LineWidth',2.5);
plot(state(range,4),'k--','LineWidth',2);
plot(squeeze(statefens(range,4,:)),'-','Color',[0.7 0.7 0.7],'LineWidth',0.5);
plot(state(range,4),'k--','LineWidth',2);
plot(mean(statefens(range,4,:),3),'r-','LineWidth',2.5);
%plot(state(range,1),'k-','LineWidth',2);
%plot(yobs(range,1),'o','MarkerFaceColor','r');
axis([0 500 5 15])
legend('True Parameter','Estimated parameter mean','Individual members')
title('X variable as a function of time')




%Evolucion temporal de las covarianzas entre los parametros y 
figure
range=300:500;
hold on
plot(squeeze(covarens(4,1,range)),'b-','LineWidth',2);
plot(squeeze(covarens(4,3,range)),'r-','LineWidth',2);
legend('COV a-X','COV a-Z')
set(gca,'XGrid','On','YGrid','On')

%Evolucion temporal de las covarianzas entre los parametros y 
figure
range=300:500;
hold on
plot(squeeze(covarens(4,5,range)),'b-','LineWidth',2);
plot(squeeze(covarens(4,6,range)),'r-','LineWidth',2);
legend('COV a-r','COV a-b')
set(gca,'XGrid','On','YGrid','On')






