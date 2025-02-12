function [xaens,xabar,xbens,xbbar,d,K,Pf,Pa]=RUN_ETKF(yo,xbens,H,R,inflation)

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
%  Based on the algorithm presented by Hunt et al. 2007
%  Physica D.
%---------------------------------------------
K=NaN;
Pf=NaN;
Pa=NaN;


[dim ens_size]=size(xbens);
p=length(yo);


% ensemble mean
xbbar=mean(xbens,2);

% background ensemble perturbations
for i=1:ens_size
    xbens(:,i)=xbens(:,i)-xbbar;
end

% apply multiplicative inflation
xbens=inflation*xbens;

% background ensemble perturbation in observation space
yb=H*xbens;

% innovation vector (obs-background)
d=yo(1,1:p)'-H*xbbar(1:dim,1);

Rinv=diag(1./diag(R));

Rinvd=Rinv*d;

% analysis error covariance in ensemble space
dummy=yb'*(Rinv*yb)+double(ens_size-1)*eye(ens_size);
Pahat=inv(dummy);

% weight to update ensemble mean
wabar=Pahat*(yb'*Rinvd);

clear dummy;

% weight to update ensemble perturbations
dummy=(ens_size-1)*Pahat;
[U,S,VT]= svd(dummy);
clear dummy;
for i=1:ens_size
    S(i,i)=sqrt(S(i,i));
end

% for symmetric square root matrix
Wa=U*S*U';

% update analysis ensemble mean and perturbations
xabar=xbbar+xbens*wabar;
xaens=xbens*Wa;

xrdn=zeros(dim,ens_size);

xaens=xaens+xrdn;

% finalize the analysis ensemble
for i=1:ens_size
    xaens(:,i)=xabar+xaens(:,i);
end
return

