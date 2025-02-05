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
%  Based on the script by Shu-Chih Yang, 2006
%  This EnKF implementation follows:
%  Analysis Scheme in the Ensemble Kalman Filter
%  GERRIT BURGERS Royal Netherlands Meteorological Institute
%  PETER JAN VAN LEEUWEN Institute for Marine and Atmospheric Research 
%  GEIR EVENSEN Nansen Environmental and Remote Sensing Center
%  1998 Monthly Weather Review.
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
K= (Pf*H') *( (H*Pf*H' + R )^-1);

xaens=xfens;

%Use perturbed observations method to obtain the analysis ensemble.
for iens=1:ens_size
    
   %random perturbation for the observations (Burgers et al. 1998 MWR)
   epsilon = sqrt(diag(R)).*randn(p,1);
   % innovation vector (y - h(x) + epsilon )
   d = yo' - H*xfens(:,iens) + epsilon ;
  
   %Update ensemble member iens
   xaens(:,iens)=xaens(:,iens) + K*d ;
    
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

