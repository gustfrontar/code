clear all
close all

%Evaluation of bias in the forecast.


load model_data.mat
load lluvia_obs_hires.mat
load lat_lon.mat


nforecast=size(lluvia_obs,3);
ndays=size(lluvia_obs,4);

%Compare PDF.

bins=[1 20];  %Thresholds to compute FSS.

nbins=length(bins);


%Compute FSS.
areas=[1 5 10 20 40];
for ii=1:nforecast %No verificamos todo el pronostico porque es muy larga la cuenta sino.

    [fss(:,:,ii)]=fss_fun(squeeze(lluvia_modelo(:,:,ii,:)),squeeze(lluvia_obs(:,:,ii,:)),bins,areas);
    
end
save('fss.mat','fss','areas','bins')











