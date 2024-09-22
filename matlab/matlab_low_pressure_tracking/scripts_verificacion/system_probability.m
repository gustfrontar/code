close all
clear all

%INCLUDE SPECIFIC FUNCTIONS DIRECTORY.
addpath('../common_functions_vpo/');

config.date_ini='2007040112';          
config.date_end='2007123112';

config.model='kwbc';
config.dataprefix='REA2NCEP_HGT_';
config.datadateformat='yyyymmddHH';
config.resultfrec=24; 
config.grouppath=['../RESULTS/' config.model '/GROUP/'];
config.forecastlength=7;

%Will read all the groups comming from the selected model during the
%selected period.

tic
[group]=read_groups_fun(config);
tiempo=toc;
fprintf('Time to read the data : %f ',tiempo);
%Compute probability.


sistemprobability=squeeze(sum(~isnan(group.minlat),1))/size(group.minlat,1);
sistemexistance=squeeze(~isnan(group.minlatanalysis));

%RELIABILITY DIAGRAM
for ii=1:size(sistemprobability,1)

[reliability(ii,:),n_forecast(ii,:),prob_ref] = reliability_fun(sistemexistance(ii,:),sistemprobability(ii,:),0.1);

end

a=jet(size(reliability,1));
icolor=1; 

figure
for ii=1:4:size(reliability,1)
hold on
plot(prob_ref,reliability(ii,:)','LineWidth',2,'Color',a(ii,:))

end
legend('00','24','48','72','96','120','144','168');
plot([0 1],[0 1],'k--','LineWidth',2);

%BSS

for ii=1:size(sistemprobability,1)

[brier(ii) breli(ii) bresol(ii) bun(ii)]=brier_fun(sistemexistance(ii,:)',sistemprobability(ii,:)',0.5,0.1);

end

figure
subplot(1,3,1)
plot(1-brier./bun);
title('BRIER SKILL SCORE')

subplot(1,3,2)
plot(breli)
title('RELIABILITY')
subplot(1,3,3)
plot(bresol)
title('RESOLUTION')



