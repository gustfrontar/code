function [R,H,yobs]=DA_init(state,iobs,R0)
%
%  initialize the data assimilation
%  input : 
%  R0  - observation error variance (diagnal elements of observation error covariance)
%  state - true state
%  Based on the script by Shu-Chih Yang 2005
%  

[numstep , dim]=size(state);
%Get the number of observations that we will assimilate in each
%assimilation cycle.
p=length(iobs);

%   create the observation error covariance
R=R0*eye(p);

%   create the observation operator 
%   (in this case, model variables are directly observed)
H=zeros(p,dim);
for i=1:p
    H(i,iobs(i))=1.d0;
end

% generate observations
yobs=zeros(numstep,p);

obs_error=sqrt(R0)*randn(numstep,p);

for j=1:p
    yobs(:,j)=state(:,iobs(j))+obs_error(:,j);
end
     
     
end
