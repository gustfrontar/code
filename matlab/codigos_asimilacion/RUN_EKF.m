function [xa,Pa]=RUN_EKF(yo,xf,Pf,H,R)

%---------------------------------------------
%  apply EKF to update analysis state
%
%  input:
%       yo      - observations
%       xb(dim) - background state
%       H - observation operator
%       R - observation error covariance
%  output:
%       xa(dim) - analysis
%
%---------------------------------------------

tmp=inv(H*Pf*H'+R);
K=Pf*H'*tmp;

xa=xf'+K*(yo'-H*(xf'));

I=eye(size(Pf));
Pa=(I-K*H)*Pf;

return

