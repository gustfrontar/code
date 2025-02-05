function [xaens,xabar,xfens,xfbar,d,K,Pf,Pa]=RUN_EnKF(yo,xfens,H,R,inflation)

%---------------------------------------------
%  apply EnKF to update analysis state
%
%  input:
%       yo - observations
%       xbens(dim,K) - background ensemble state
%       H - observation operator
%       R - observation error covariance
%       inflation - multiplicative inflation parameter
%  output:
%       xaens(dim,K) - analysis ensemble state
%       xabar(dim,K) - analysis mean state
%       d            - innovation vector
%       Pf           - background error covariance matrix
%       K            - Kalman Gain
%
% Ensemble Data Assimilation without Perturbed Observations
% JEFFREY S. WHITAKER AND THOMAS M. HAMILL
% NOAA/CIRES Climate Diagnostics Center, Boulder, Colorado
% 2003 MWR
%---------------------------------------------

%Get information about ensemble size and state dimension.

[dim , ens_size]=size(xfens);

[p]=length(yo);

% compute background ensemble mean
xfbar=mean(xfens,2);

% compute background ensemble perturbations

for iens=1:ens_size
    xfpert(:,iens)=xfens(:,iens)-xfbar;
end

% apply multiplicative inflation
xfpert=inflation*xfpert;

% compute background error covariance matrix
Pf= xfpert*xfpert' * (1/(ens_size-1));

%If there are no observations to be assimilated we will return at this
%point. No correction is done to the ensemble.
if( p == 0 )
    xaens=xfens;
    xabar=xfbar;
    Pa=Pf;
    K=NaN;
    d=NaN;
    return
end

%Compute Kalman Gain
[U,S,VT]= svd(H*Pf*H'+R);
for i=1:p
    S(i,i)=sqrt(S(i,i));
end
% for symmetric square root matrix
SQRTHPHTR=U*S*U';

K=(Pf*H')*( ( SQRTHPHTR ) )'*( ( SQRTHPHTR + R^(0.5) )^(-1) );

xaens=zeros(size(xfens));

%Use perturbed observations method to obtain the analysis ensemble.
for iens=1:ens_size
    
   % innovation vector (y - h(x) + epsilon )
   d = yo' - H*xfens(:,iens);
  
   %Update ensemble member iens
   xaens(:,iens)=xfens(:,iens) + K*d ;
    
end

% compute analysis ensemble mean
xabar=mean(xaens,2);

% compute analysis ensemble perturbations.
for iens=1:ens_size
    xapert(:,iens)=xaens(:,iens)-xabar;
end

%Compute analysis error covariance matrix (just for fun).
Pa= xapert*xapert' * (1/(ens_size-1));

return

