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

%Compute PROBABILITY AND ITS VERIFICATION SCORES.
parea=40;
for ii=1:8 %No hago todos los pronosticos por cuestiones de tiempo.
   tic
   %tmp_for=real(squeeze(lluvia_modelo(:,:,ii,:)) > bins(i_umb));
   tmp_for=squeeze(lluvia_modelo(:,:,ii,:));
   tmp_obs=squeeze(lluvia_obs(:,:,ii,:));
   [prob_for rank_for enssize]=prob_fun(tmp_for,tmp_obs,bins,parea);

   for i_umb=1:length(bins)
   tmp_obs=reshape(lluvia_obs(:,:,ii,:),[size(lluvia_obs,1)*size(lluvia_obs,2)*size(lluvia_obs,4) 1]);
   tmp_prob=reshape(prob_for(:,:,:,i_umb),[size(lluvia_obs,1)*size(lluvia_obs,2)*size(lluvia_obs,4) 1]);
   [brier(ii,i_umb) breliability(ii,i_umb) bresolution(ii,i_umb) buncertainty(ii,i_umb) brier_clim(ii,i_umb)] = brier_fun(tmp_obs,tmp_prob,bins(i_umb),0.1);
   [reliability(:,ii,i_umb) n_forecast(:,ii,i_umb) prob_ref(:,ii,i_umb)] = reliability_fun(tmp_obs,tmp_prob,bins(i_umb),0.1);
   [hit(:,ii,i_umb) far(:,ii,i_umb) area(ii,i_umb) ets bias area_2] = roc_fun(tmp_obs,tmp_prob,bins(i_umb),0.1);
   end
   %Compute rank histogram.
   for jj=0:enssize 
      rank_hist(ii,jj+1)=sum(sum(sum(rank_for(parea:end-parea,parea:end-parea,:)==jj)));
   end
   toc
end
   
clear lluvia_modelo lluvia_obs LAT LON 
save('prob_40p.mat');


