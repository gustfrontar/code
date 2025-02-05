clear all;
close all;
% 
% dir_file=dir('/Users/pierretandeo/Desktop/SPEEDY_analog/data/007/*.mat');
% 
% load(['/Users/pierretandeo/Desktop/SPEEDY_analog/data/007/',dir_file(1).name]);
load('d:/speedy_data.mat','data'); 
%load data_speedy_2.mat %tropical pacifique
%load data_speedy_3.mat %tropical pacifique

MAX_NANALOGS=1000;
MIN_NANALOGS=50;
MAX_EXP=5;
order=1;
skip=1;   
ibox=1;

%We include pressure and meridional wind variables to find the analogs.
data=data(:,:,[29 8],1461:end);

%save('d:/test_data.mat','data');
%load('d:/test_data.mat');




for iana=1:MAX_EXP
    
NANALOGS=(iana-1)*(MAX_NANALOGS-MIN_NANALOGS)/10 + MIN_NANALOGS;
iana
%Filter data
i_good_analogs=[1 2];
i_good=[6-ibox:6+ibox];
T=size(data,4);
analog_data=data(i_good,i_good,i_good_analogs,:);

%Final 
nvariables=size(data,3);
indice=nvariables;
indice2=skip;

analog_data=repmat(analog_data,[1 1 order 1]);

for jj=1:order-1
    
analog_data(:,:,indice+1:indice+nvariables,indice2+1:end)=analog_data(:,:,1:nvariables,1:end-indice2);
indice=indice+nvariables;
indice2=indice2+skip;

end

% train and test

percmin=9;
percmax=9;
for perc=percmin:percmax
    perc
    tic
dt=1;
%These are subsamples of the data that will be used to find the analogs.
analog_data_training=analog_data(:,:,:,1:ceil(perc*T/10));
analog_data_training_r=reshape(analog_data_training,size(analog_data_training,1)*size(analog_data_training,2)*size(analog_data_training,3),size(analog_data_training,4));
analog_data_training_x_r=analog_data_training_r(:,1:end-dt);
analog_data_training_y_r=analog_data_training_r(:,dt+1:end);

%clear analog_data_training analog_data_training_r 

analog_data_testing=analog_data(:,:,:,ceil(1+9*T/10):end);
analog_data_testing_r=reshape(analog_data_testing,size(analog_data_testing,1)*size(analog_data_testing,2)*size(analog_data_testing,3),size(analog_data_testing,4));

analog_data_testing_x=analog_data_testing(:,:,:,dt+1:end);
analog_data_testing_y=analog_data_testing(:,:,:,1:end-dt);

analog_data_testing_r=reshape(analog_data_testing,size(analog_data_testing,1)*size(analog_data_testing,2)*size(analog_data_testing,3),size(analog_data_testing,4));
analog_data_testing_x_r=analog_data_testing_r(:,1:end-dt);
analog_data_testing_y_r=analog_data_testing_r(:,dt+1:end);


%clear analog_data_testing analog_data_testing_r 

%These are subsamples of the data that we want to forecast.
data_training=data(:,:,:,1:ceil(perc*T/10));
data_testing =data(:,:,:,ceil(1+9*T/10):end);

data_training_x=data_training(:,:,:,1:end-dt);
data_training_y=data_training(:,:,:,2:end);

data_testing_x=data_testing(:,:,:,1:end-dt);
data_testing_y=data_testing(:,:,:,2:end);

% linear regressions will use full data for linear regression.
display('computing global linear regression')
data_training_x_r=reshape(data_training_x,size(data_training_x,1)*size(data_training_x,2)*size(data_training_x,3),size(data_training_x,4));
data_training_y_r=reshape(data_training_y,size(data_training_y,1)*size(data_training_y,2)*size(data_training_y,3),size(data_training_y,4));

data_testing_x_r=reshape(data_testing_x,size(data_testing_x,1)*size(data_testing_x,2)*size(data_testing_x,3),size(data_testing_x,4));
data_testing_y_r=reshape(data_testing_y,size(data_testing_y,1)*size(data_testing_y,2)*size(data_testing_y,3),size(data_testing_y,4));

beta=lscov(analog_data_training_x_r',(data_training_y_r-data_training_x_r)');
data_testing_yhat_r_lm=(analog_data_testing_x_r'*beta)'+data_testing_x_r;

data_testing_yhat_lm=reshape(data_testing_yhat_r_lm,size(data_testing_y,1),size(data_testing_y,2),size(data_testing_y,3),size(data_testing_y,4));

% wknn regressions
w=ones(1,size(analog_data_testing_x_r,1));


display('computing analogues')
[index_wknn,dist_wknn]=find_analog(analog_data_training_x_r',analog_data_testing_x_r',w,NANALOGS);

lambda=median(dist_wknn(:));
pds=exp(-(dist_wknn/lambda).^2);
pds=pds./repmat(sum(pds,2),[1 size(pds,2)]);

%Y=data_training_y_r';
data_testing_yhat_mean_r=NaN(size(data_testing_y_r));
data_testing_yhat_best_r=NaN(size(data_testing_y_r));
data_testing_yhat_tla_r=NaN(size(data_testing_y_r));

warning off


display('Applying non-linear regression')
for j=1:size(data_testing_y_r,2)
    
    beta_tla=lscov(analog_data_training_x_r(:,index_wknn(j,:))',(data_training_y_r(:,index_wknn(j,:))-data_training_x_r(:,index_wknn(j,:)))',pds(j,:));
    
    data_testing_yhat_tla_r(:,j)=(analog_data_testing_x_r(:,j)'*beta_tla)'+ data_testing_x_r(:,j);
    
    data_testing_yhat_mean_r(:,j)=sum( ( data_training_y_r(:,index_wknn(j,:))-data_training_x_r(:,index_wknn(j,:)) ).*repmat(pds(j,:),size(data_testing_yhat_mean_r,1),1),2) ...
        + data_testing_x_r(:,j);
    data_testing_yhat_best_r(:,j)=data_training_y_r(:,index_wknn(j,1))-data_training_x_r(:,index_wknn(j,1)) + data_testing_x_r(:,j);
end % end for

data_testing_yhat_mean=reshape(data_testing_yhat_mean_r,size(data_testing_y,1),size(data_testing_y,2),size(data_testing_y,3),size(data_testing_y,4));
data_testing_yhat_best=reshape(data_testing_yhat_best_r,size(data_testing_y,1),size(data_testing_y,2),size(data_testing_y,3),size(data_testing_y,4));
data_testing_yhat_tla=reshape(data_testing_yhat_tla_r,size(data_testing_y,1),size(data_testing_y,2),size(data_testing_y,3),size(data_testing_y,4));


RMSE_best(:,:,:,iana,perc)=squeeze(sqrt( nanmean( ( data_testing_yhat_best - data_testing_y ).^2 ,  4 ) ));
RMSE_mean(:,:,:,iana,perc)=squeeze(sqrt( nanmean( ( data_testing_yhat_mean - data_testing_y ).^2 ,  4 ) ));
RMSE_lm(:,:,:,iana,perc)=squeeze(sqrt( nanmean( ( data_testing_yhat_lm - data_testing_y ).^2 ,  4 ) ));
RMSE_tla(:,:,:,iana,perc)=squeeze(sqrt( nanmean( ( data_testing_yhat_tla - data_testing_y ).^2 ,  4 ) ));


toc
end


end

figure

%Error for the analog model
subplot(3,6,1)
contourf(RMSE_mean(:,:,1,1,9),[0:50:500]);caxis([0 500]);colorbar
subplot(3,6,2)
contourf(RMSE_mean(:,:,1,2,9),[0:50:500]);caxis([0 500]);colorbar
subplot(3,6,3)
contourf(RMSE_mean(:,:,1,3,9),[0:50:500]);caxis([0 500]);colorbar
subplot(3,6,4)
contourf(RMSE_mean(:,:,1,4,9),[0:50:500]);caxis([0 500]);colorbar
subplot(3,6,5)
contourf(RMSE_mean(:,:,1,5,9),[0:50:500]);caxis([0 500]);colorbar
% subplot(3,6,6)
% contourf(RMSE_mean(:,:,1,6,9));caxis([0 500]);colorbar

%Error for the linear model
subplot(3,6,7)
contourf(RMSE_tla(:,:,1,1,9),[0:50:500]);caxis([0 500]);colorbar
subplot(3,6,8)
contourf(RMSE_tla(:,:,1,2,9),[0:50:500]);caxis([0 500]);colorbar
subplot(3,6,9)
contourf(RMSE_tla(:,:,1,3,9),[0:50:500]);caxis([0 500]);colorbar
subplot(3,6,10)
contourf(RMSE_tla(:,:,1,4,9),[0:50:500]);caxis([0 500]);colorbar
subplot(3,6,11)
contourf(RMSE_tla(:,:,1,5,9),[0:50:500]);caxis([0 500]);colorbar
% subplot(3,6,12)
% contourf(RMSE_lm(:,:,1,6,9));caxis([0 500]);colorbar

%Error for the linear model
subplot(3,6,13)
contourf(RMSE_lm(:,:,1,1,9),[0:50:500]);caxis([0 500]);colorbar
subplot(3,6,14)
contourf(RMSE_lm(:,:,1,2,9),[0:50:500]);caxis([0 500]);colorbar
subplot(3,6,15)
contourf(RMSE_lm(:,:,1,3,9),[0:50:500]);caxis([0 500]);colorbar
subplot(3,6,16)
contourf(RMSE_lm(:,:,1,4,9),[0:50:500]);caxis([0 500]);colorbar
subplot(3,6,17)
contourf(RMSE_lm(:,:,1,5,9),[0:50:500]);caxis([0 500]);colorbar
% subplot(3,6,18)
% contourf(RMSE_lm(:,:,1,6,9));caxis([0 500]);colorbar



figure
plot(squeeze(RMSE_lm(6,6,1,:,9)),'r');
hold on
plot(squeeze(RMSE_mean(6,6,1,:,9)),'b');
plot(squeeze(RMSE_best(6,6,1,:,9)),'g');
plot(squeeze(RMSE_tla(6,6,1,:,9)),'k');
legend('Linear','Analog-mean','Analog-best','Analog-tla')

% 
% figure
% plot(squeeze(RMSE_best(3,3,29,:)),'r');
% hold on
% plot(squeeze(RMSE_mean(3,3,29,:)),'g');
% plot(squeeze(RMSE_lm(3,3,29,:)),'b');
% 
% figure
% plot(squeeze(data_testing_y(3,3,29,1000:1500)));
% hold on
% plot(squeeze(data_testing_yhat_lm(3,3,29,1000:1500)),'r');
% plot(squeeze(data_testing_yhat_mean(3,3,29,1000:1500)),'g');
% 
% % subplot(1,2,1);pcolor(data_hat_Y_tmp(:,:,end,1));colorbar
% % subplot(1,2,2);pcolor(data_test_Y_tmp(:,:,end,1));colorbar



