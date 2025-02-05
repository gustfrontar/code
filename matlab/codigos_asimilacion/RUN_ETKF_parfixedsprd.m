function [xaens,xabar,alfa_a,v_a]=RUN_ETKF_estinf(yo,xbens,p,dim,K,H,R,alfa_f,v_a)

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
%AN INFLATION FACTOR FOR THE STATE AND THE PARAMETERS WILL BE ESTIMATED.


% ensemble mean
xbbar=mean(xbens,2);

% background ensemble perturbations
for i=1:K
    xbens(:,i)=xbens(:,i)-xbbar;
end

% apply multiplicative inflation
xbens(1:3,:)=alfa_f*xbens(1:3,:);    %STATE MULTIPLICATIVE INFLATION WILL BE APPLIED TO STATE VARIABLES ONLY.

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

%for symmetric square root matrix
Wa=U*S*U';


% update analysis ensemble mean and perturbations

xabar=xbbar+xbens*wabar;
xaens=xbens*Wa;

%Estimate inflation
%Based on Li et al. 2009.
maxinf=1.2;
mininf=0.9;
fp=1.03;    %Forgetting parameter.

v_o=1;
v_f=fp*v_a;

alfa_o=(( d )'*( d )-trace(R))/(trace(yb'*yb)/(K-1));
      
    if(alfa_o > maxinf);alfa_o=maxinf;end
    if(alfa_o < mininf);alfa_o=mininf;end

    alfa_a=(v_o*alfa_f+v_f*alfa_o)/(v_o+v_f);
    v_a=(1-v_f/(v_f+v_o))*v_f;
    
    if(alfa_a < 1.0);alfa_a=1.01;end
    
%End of inflation estimation.



%Parameter inflation estimation
      %apply inflation to the parameters.
      for ii=4:6
          tmp=sum(xaens(ii,:).^2)/(K-1);
          tmp2=sum(xbens(ii,:).^2)/(K-1);
          %sqrt(tmp2/tmp)
          xaens(ii,:)=sqrt(tmp2/tmp)*xaens(ii,:);
      end
          

      
% finalize the analysis ensemble
for i=1:K
    xaens(:,i)=xabar+xaens(:,i);
end
%DONE!

return

