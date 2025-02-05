function [xaens,xabar,alfa_a,v_a]=RUN_4DETKF_estinf(yo,xbens,xbens4d,dim,K,H,diagR,alfa_f,v_a)

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
dummy=yb'*(Rinv*yb)+double(K-1)*eye(K)/sqrt(alfa_f);

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


%Estimate inflation
%Based on Li et al. 2009.
maxinf=1.1;
mininf=1.0;
fp=1.03;    %Forgetting parameter.

v_o=4;
v_f=fp*v_a;

alfa_o=(sum( d.*d)-sum(totaldiagR))/(trace(yb'*yb)/(K-1));
    trace(yb'*yb)/(K-1)
    sum( d.*d)-sum(totaldiagR)
    %if(alfa_o > maxinf);alfa_o=maxinf;end
    %if(alfa_o < mininf);alfa_o=mininf;end

    alfa_a=(v_o*alfa_f+v_f*alfa_o)/(v_o+v_f);
    v_a=(1-v_f/(v_f+v_o))*v_f;
    
    if(alfa_a < mininf);alfa_a=mininf;end
    if(alfa_a > maxinf);alfa_a=maxinf;end
    
%End of inflation estimation.


% finalize the analysis ensemble
for i=1:K
    xaens(:,i)=xabar+xaens(:,i);
end
return

