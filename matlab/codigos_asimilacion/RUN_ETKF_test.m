function [xaens,xabar,Wa,wabar]=RUN_ETKF_test(yo,xbens,p,dim,K,H,R)

%---------------------------------------------
%  apply ETKF to update analysis state
%
%  input:
%       p - total observations
%       yo(1,p) - observations
%       xbens(dim,K) - background ensemble state
%       K - ensemble size
%       H - observation operator
%       R - observation error covariance
%       alfa_f - multiplicative inflation
%  output:
%       xaens(dim,K) - analysis ensemble state
%       xabar(dim,K) - analysis mean state
%
%  Shu-Chih Yang, 2006
%---------------------------------------------


% ensemble mean
xbbar=mean(xbens,2);

% background ensemble perturbations
for i=1:K
    xbens(:,i)=xbens(:,i)-xbbar;
end

% apply multiplicative inflation
xbens(1:3,:)=xbens(1:3,:);    %STATE MULTIPLICATIVE INFLATION WILL BE APPLIED TO STATE VARIABLES ONLY.

% background ensemble perturbation in observation space
yb=H*xbens;

% innovation vector (obs-background)
d=yo(1,1:p)'-H*xbbar(1:dim,1);

Rinv=diag(1./diag(R));

Rinvd=Rinv*d;

% analysis error covariance in ensemble space
tmp=eye(K);
tmp2=mean(tmp,1);
Wf=tmp-tmp2(1);

traceWf=sum(diag(Wf).^2);

dummy=yb'*(Rinv*yb)+double(K-1)*tmp;

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

%for symmetric square root matrix
Wa=U*S*U';

tmp2=mean(Wa,1);
tmp=Wa-tmp2(1);

traceWa=sum(diag(tmp).^2);

inf=(0.4*traceWf+0.6*traceWa)/traceWa
Wa=inf*Wa;

% update analysis ensemble mean and perturbations

xabar=xbbar+xbens*wabar;
xaens=xbens*Wa;


      
% finalize the analysis ensemble
for i=1:K
    xaens(:,i)=xabar+xaens(:,i);
end
%DONE!

return

