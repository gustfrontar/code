clear all;
close all;

%In this case we use CFSR and GFDL data

conf.nanalogs=100;
conf.dt=1;
conf.method=3;  %1=weigthed analog centered, 2=best analog centered, 3=weigthed analog uncentered, 4=best analog uncentered.
conf.ibox=2;
conf.jbox=2;
conf.var_good=[1 2];
conf.var_good_obs=[1];
conf.loc=false;
conf.inf_factor=0.1;
conf.add_inf_factor=0.01;
%conf.normfields=false;
conf.model_type=2;    %1= linear model , 2= analog regression
conf.R=100;           %Observation error standard deviation (in Pa);

nyears_obs=5;

load('d:/CFSR_DATA/CFSR_slp_WEurope');
%Observations are taken from the first 5 years of reanalysis data.
data(:,:,:)=( data(:,:,:)-repmat(nanmean(data,3),[1 1 size(data,3)]) )./repmat(nanstd(data,[],3),[1 1 size(data,3)]);
obs.data(:,:,1,:)=data(13:17,13:17,2:nyears_obs*4*365);
obs.data(:,:,2,:)=data(13:17,13:17,2:nyears_obs*4*365)-data(13:17,13:17,1:nyears_obs*4*365-1);

%The rest of the years are used to bild the first catalog.
catalog(1).data(:,:,1,:)=data(13:17,13:17,nyears_obs*4*365+1:end-1500);
catalog(1).data(:,:,2,:)=data(13:17,13:17,nyears_obs*4*365+1:end-1500)-data(13:17,13:17,nyears_obs*4*365:end-1501);
clear data;

catalog_size=size(catalog(1).data,4);
%We use data from GFDL nature run with GHG as a second catalog (the size of
%the catalog is the same as the one generated from the reanalysis data).
load('d:/nomads.gfdl.noaa.gov/GFSL_XXGHG_slp_WEurope.mat')%data(:,:,:)=( data(:,:,:)-repmat(nanmean(data,3),[1 1 size(data,3)]) )./repmat(nanstd(data,[],3),[1 1 size(data,3)]);
catalog(2).data(:,:,1,:)=data(13:17,13:17,2:catalog_size+1);
catalog(2).data(:,:,2,:)=data(13:17,13:17,2:catalog_size+1)-data(13:17,13:17,1:catalog_size);
clear data;

catalog_size=size(catalog(1).data,4);
%We use data from GFDL nature run with GHG as a second catalog (the size of
%the catalog is the same as the one generated from the reanalysis data).
load('d:/nomads.gfdl.noaa.gov/GFSL_PICONTROL_slp_WEurope.mat')
data(:,:,:)=( data(:,:,:)-repmat(nanmean(data,3),[1 1 size(data,3)]) )./repmat(nanstd(data,[],3),[1 1 size(data,3)]);
catalog(3).data(:,:,1,:)=data(13:17,13:17,2:catalog_size+1);
catalog(3).data(:,:,2,:)=data(13:17,13:17,2:catalog_size+1)-data(13:17,13:17,1:catalog_size);
clear data;

%Compute the forecast for all the dates in the sample.
for icatalog=1:size(catalog,2)
   if( conf.model_type == 1)
    [forecast(icatalog).data]=linear_model(conf,catalog(icatalog).data,obs.data);   
   end
   if( conf.model_type == 2)  
    [forecast(icatalog).data forecast(icatalog).index_wknn forecast(icatalog).dist_wknn]=analog_model(conf,catalog(icatalog).data,obs.data);
   end
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

