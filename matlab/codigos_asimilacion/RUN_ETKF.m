function [xaens,xabar,Wa,coeficiente]=RUN_ETKF(yo,xbens,p,dim,K,H,R,e)

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
%       e(2) - inflation parameters
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
xbens=e(1)*xbens;

% background ensemble perturbation in observation space
yb=H*xbens;

% innovation vector (obs-background)
d=yo(1,1:p)'-H*xbbar(1:dim,1);

Rinv=diag(1./diag(R));

Rinvd=Rinv*d;

% analysis error covariance in ensemble space
dummy=yb'*(Rinv*yb)+double(K-1)*eye(K);

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

% use additive inflation to perturb the analysis perturbations

% Mu=e(2)*randn(dim,K);
% Mu2=Mu*Mu';
% [U,S,VT]=svd(Mu2);
% xrdn=U*sqrt(S)*U';
% 
% xrdn=zeros(dim,K);
% if (e(2) > 0) 
%    xrdn=e(2)*randn(dim,K);
%    dummy=mean(xrdn,2);
%    for i=1:K
%        xrdn(:,i)=xrdn(:,i)-dummy;
%    end
% end
% xaens=xaens+xrdn;

% finalize the analysis ensemble
for i=1:K
    xaens(:,i)=xabar+xaens(:,i);
end
return

