function [xaens,xabar,Wa,coeficiente]=RUN_4DETKF(yo,xbens,xbens4d,dim,K,H,diagR,e)

%---------------------------------------------
%  apply ETKF to update analysis state
%
%  input:
%       yo(times,p) - observations
%       xbens(dim,K) - background ensemble state
%       xbens(dim,K,times) - 4D background ensemble state
%       K - ensemble size
%       H - observation operator
%       diagR - observation error covariance diagonal
%       e(2) - inflation parameters
%  output:
%       xaens(dim,K) - analysis ensemble state
%       xabar(dim,K) - analysis mean state
%
%  Shu-Chih Yang, 2006
%---------------------------------------------
ntimes=size(xbens4d,3);

% ensemble mean
xbbar=mean(xbens,2);
xbbar4d=squeeze(mean(xbens4d,2));

% background ensemble perturbations

for i=1:K
    xbens(:,i)=xbens(:,i)-xbbar;
    for ii=1:ntimes
       xbens4d(:,i,ii)=xbens4d(:,i,ii)-xbbar4d(:,ii); 
    end
end

% background ensemble perturbation in observation space
yb=[];
d=[];
totaldiagR=[];
%Compute innovation vector

for ii=1:ntimes

yb=[yb ; H*squeeze(xbens4d(:,:,ii))];
d=[d ; yo(:,ii)-H*squeeze(xbbar4d(:,ii))];
totaldiagR=[totaldiagR ; diagR ];
end



Rinv=diag(1./totaldiagR);
Rinvd=Rinv*d;


% analysis error covariance in ensemble space
dummy=yb'*(Rinv*yb)+double(K-1)*eye(K)/sqrt(e(1));

[U,S,VT]=svd(dummy);


Pahat=U*(inv(S))*U';

% weight to update ensemble mean
wabar=Pahat*(yb'*Rinvd);

clear dummy;

% weight to update ensemble perturbations
dummy=(K-1)*Pahat;
[U,S,VT]= svd(dummy);

clear dummy;
for i=1:K
   S(i,i)=sqrt(S(i,i));
end

coeficiente=K/sum(diag(S));


%for symmetric square root matrix
Wa=U*S*U';


% update analysis ensemble mean and perturbations

xabar=xbbar+xbens*wabar;
xaens=xbens*Wa;


% finalize the analysis ensemble
for i=1:K
    xaens(:,i)=xabar+xaens(:,i);
end
return

