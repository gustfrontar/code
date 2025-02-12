clear all;
close all;

conf.nanalogs=64;
conf.dt=1;
conf.method=1;  %1=weigthed analog, 2=best analog.
conf.ibox=1;
conf.jbox=1;
conf.var_good=[1];
conf.loc=false;
conf.inf_factor=0.1;
conf.add_inf_factor=0.01;
conf.R=100;
conf.var_good_obs=1;
nyears_obs=5;



load('d:/RHBL_matlab/010/local_patch_xloc=60_yloc=15');
catalog(1).data=data(5:7,5:7,29,2000:end);
clear data;

load('d:/RHBL_matlab/007/local_patch_xloc=60_yloc=15');
catalog(2).data=data(5:7,5:7,29,2000:end);
clear data;
% 
load('d:/RHBL_matlab/004/local_patch_xloc=60_yloc=15');
catalog(3).data=data(5:7,5:7,29,2000:end);
clear data

load('d:/RHBL_matlab/011/local_patch_xloc=60_yloc=15');
%[data]=perturb_data(data,conf);
obs.data=data(5:7,5:7,29,end-365*4*nyears_obs:end);
clear data

%Compute the forecast for all the dates in the sample.
for icatalog=1:size(catalog,2)
[forecast(icatalog).data forecast(icatalog).index_wknn forecast(icatalog).dist_wknn]=analog_model(conf,catalog(icatalog).data,obs.data);
end

%Now we can update the probability for each model
P(1).Otsuka=ones(size(catalog,2),1);
P(1).Otsuka=P.Otsuka/sum(P.Otsuka);  %Initial probability for each model.

P(1).Gaussian=P.Otsuka;
P(1).Constant_prior=P.Otsuka;

for itime = 1:size(obs.data,4)-conf.dt;
    
    for icatalog=1:size(catalog,2)
       current_forecast(:,:,:,icatalog)=forecast(icatalog).data(:,:,conf.var_good_obs,itime);
    end
    current_obs=obs.data(:,:,conf.var_good_obs,itime+conf.dt);
     
    [P(itime+1) error(:,itime+1)]=update_p(current_forecast,current_obs,P(itime),conf);
    
end

for itime= 1:size(obs.data,4)-conf.dt;
   
  P_Otsuka(itime,:)=P(itime).Otsuka;
  P_Gaussian(itime,:)=P(itime).Gaussian;
  P_Constant_prior(itime,:)=P(itime).Constant_prior;
    
end



figure
subplot(1,3,1)
bar(mean(P_Gaussian));
title('Mean(P) Gaussian')
subplot(1,3,2)
bar(mean(P_Otsuka));
title('Mean(P) Inverse RMSE')
subplot(1,3,3)
bar(mean(P_Constant_prior));
title('Mean(P) constant prior')

figure
subplot(1,3,1)
plot((P_Gaussian));
title('Mean(P) Gaussian')
subplot(1,3,2)
plot((P_Otsuka));
title('Mean(P) Inverse RMSE')
subplot(1,3,3)
plot((P_Constant_prior));
title('Mean(P) constant prior')

figure
subplot(1,2,1)
bar(mean(error,2));
title('Mean RMSE')

subplot(1,2,2)
for ii=1:1:size(catalog,2)
  tmp(ii,:)=smooth(error(ii,:),500);  
end
plot(tmp');
title('RMSE')






