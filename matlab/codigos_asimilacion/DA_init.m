function [R,H,yobs]=DA_init(numstep,p,n,iobs,R0,state,obsgen,obsfile)
%
%  initialize the data assimilation
%  input : 
%  numstep - number of analysis cycle
%  p   - observation number
%  n   -
%  ios - observation location
%  R0  - observation error variance (diagnal elements of observation error covariance)
%  state - true state
%  obsgen -generate the observation  
%  obsfile - if not generating new observation, load from file  
%  Shu-Chih Yang 2005
%  
%
%   create the observation error covariance
R=R0*eye(p);


%   create the observation operator 
%   (in this case, obsevation locations are on the model grid points)
H=zeros(p,n);
for i=1:p
    H(i,iobs(i))=1.d0;
end

yobs=zeros(numstep,p);
% generate observations
     eostd=sqrt(R0);
     erro=eostd*randn(numstep,p);
     for i=1:numstep
        for j=1:p
            yobs(i,j)=state(i,iobs(j))+erro(i,j);
        end
     end

