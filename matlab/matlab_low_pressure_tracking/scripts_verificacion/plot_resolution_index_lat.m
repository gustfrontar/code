clear all
close all


%Este script plotea la relacion entre el error y el spread para diferentes
%situaciones (segun hemisferio y estacion del anio).
addpath('../common_functions_vpo/');
EnsSizeThre=0.5;    %Ensambles mas chicos que este porcentaje del total no van a ser tenidos en cuenta en el calculo.

load '../RESULTS/kwbc/ERRORSPREAD/ERROR_SPREAD_2007040112_2010040112.mat'

enssize=size(group.minlat,1);
flength=size(group.minlat,2);

mask=( isnan(group.meandistlaterror) | isnan(group.distlatspread) | group.nummembers < enssize*EnsSizeThre );
group.meandistlaterrror(mask)=NaN;
group.distlatspread(mask)=NaN;

%Calculo para todos los grupos.
meanerror_total=abs(group.meandistlaterror);
ctrlerror_total=abs(group.ctrldistlaterror);
spread_total=group.distlatspread;
%Genero un error syntetico que proviene de una distribucion normal,
%unbiased y en donde cada dia el spread de dicha distribucion es igual al
%spread del ensemble.
synerror_total=abs(spread_total.*randn(size(spread_total)));

%Para el hemisferio sur
index=group.shemi==1;
meanerror_shemi=abs(group.meandistlaterror(:,index));
ctrlerror_shemi=abs(group.ctrldistlaterror(:,index));
spread_shemi=group.distlatspread(:,index);
synerror_shemi=abs(spread_shemi.*randn(size(spread_shemi)));

%Para el hemisferio norte
index=group.nhemi==1;
meanerror_nhemi=abs(group.meandistlaterror(:,index));
ctrlerror_nhemi=abs(group.ctrldistlaterror(:,index));
spread_nhemi=group.distlatspread(:,index);
synerror_nhemi=abs(spread_nhemi.*randn(size(spread_nhemi)));

%Para la estacion calida
index=group.calida==1;
meanerror_calida=abs(group.meandistlaterror(:,index));
ctrlerror_calida=abs(group.ctrldistlaterror(:,index));
spread_calida=group.distlatspread(:,index);
synerror_calida=abs(spread_calida.*randn(size(spread_calida)));

%Para la estacion fria
index=group.fria==1;
meanerror_fria=abs(group.meandistlaterror(:,index));
ctrlerror_fria=abs(group.ctrldistlaterror(:,index));
spread_fria=group.distlatspread(:,index);
synerror_fria=abs(spread_fria.*randn(size(spread_fria)));

prob_res=25;
%COMPUTE THE RESOLUTION INDEX 
for ii=1:flength
[IRLAT_TOTAL(:,ii) IRSSLAT_TOTAL(:,ii) IP PDFLAT_TOTAL(:,:,ii)]=resolution_index_fun(spread_total(ii,:),meanerror_total(ii,:),prob_res,prob_res);
[SIRLAT_TOTAL(:,ii) SIRSSLAT_TOTAL(:,ii) SIP SPDFLAT_TOTAL(:,:,ii)]=resolution_index_fun(spread_total(ii,:),synerror_total(ii,:),prob_res,prob_res);
[IRLAT_SHEMI(:,ii) IRSSLAT_SHEMI(:,ii) IP PDFLAT_SHEMI(:,:,ii)]=resolution_index_fun(spread_shemi(ii,:),meanerror_shemi(ii,:),prob_res,prob_res);
[SIRLAT_SHEMI(:,ii) SIRSSLAT_SHEMI(:,ii) SIP SPDFLAT_SHEMI(:,:,ii)]=resolution_index_fun(spread_shemi(ii,:),synerror_shemi(ii,:),prob_res,prob_res);
[IRLAT_NHEMI(:,ii) IRSSLAT_NHEMI(:,ii) IP PDFLAT_NHEMI(:,:,ii)]=resolution_index_fun(spread_nhemi(ii,:),meanerror_nhemi(ii,:),prob_res,prob_res);
[SIRLAT_NHEMI(:,ii) SIRSSLAT_NHEMI(:,ii) SIP SPDFLAT_NHEMI(:,:,ii)]=resolution_index_fun(spread_nhemi(ii,:),synerror_nhemi(ii,:),prob_res,prob_res);
[IRLAT_CALIDA(:,ii) IRSSLAT_CALIDA(:,ii) IP PDFLAT_CALIDA(:,:,ii)]=resolution_index_fun(spread_calida(ii,:),meanerror_calida(ii,:),prob_res,prob_res);
[SIRLAT_CALIDA(:,ii) SIRSSLAT_CALIDA(:,ii) SIP SPDFLAT_CALIDA(:,:,ii)]=resolution_index_fun(spread_calida(ii,:),synerror_calida(ii,:),prob_res,prob_res);
[IRLAT_FRIA(:,ii) IRSSLAT_FRIA(:,ii) IP PDFLAT_FRIA(:,:,ii)]=resolution_index_fun(spread_fria(ii,:),meanerror_fria(ii,:),prob_res,prob_res); 
[SIRLAT_FRIA(:,ii) SIRSSLAT_FRIA(:,ii) SIP SPDFLAT_FRIA(:,:,ii)]=resolution_index_fun(spread_fria(ii,:),synerror_fria(ii,:),prob_res,prob_res); 
end


%FIGURA IRSS PARA EL TOTAL
figure
hold on
plot(IRSSLAT_TOTAL','LineWidth',2)
plot(SIRSSLAT_TOTAL','--','LineWidth',2)
legend('75% ERROR','50% ERROR','25% ERROR')
title('IRSS for the total sample')

%FIGURAS HEMISFERIO NORTE VS HEMISFERIO SUR.
figure
hold on
plot(IRSSLAT_SHEMI','LineWidth',2)
plot(IRSSLAT_NHEMI','--','LineWidth',2)
legend('75% ERROR SH','50% ERROR SH','25% ERROR SH','75% ERROR NH','50% ERROR NH','25% ERROR NH')
title('IRSS by hemisphere')

%FIGURAS ESTACION CALIDA VS ESTACION FRIA.
figure
hold on
plot(IRSSLAT_CALIDA','LineWidth',2)
plot(IRSSLAT_FRIA','--','LineWidth',2)
legend('75% ERROR W','50% ERROR W','25% ERROR W','75% ERROR C','50% ERROR C','25% ERROR C')
title('IRSS by hemisphere')

%FIGURAS PDF EJEMPLOS (3 TIEMPOS). MUESTRA TOTAL
probs=( [prob_res:prob_res:100] + [0:prob_res:100-prob_res] )/2;
figure
subplot(1,3,1)
hold on
plot(probs,PDFLAT_TOTAL(:,:,1)','LineWidth',2)
plot(probs,SPDFLAT_TOTAL(:,:,1)','--','LineWidth',2)
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total sample. 00 hr. lead time.')
subplot(1,3,2)
hold on
plot(probs,PDFLAT_TOTAL(:,:,16)','LineWidth',2)
plot(probs,SPDFLAT_TOTAL(:,:,16)','--','LineWidth',2)
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total sample. 96 hr. lead time.')
subplot(1,3,3)
hold on
plot(probs,PDFLAT_TOTAL(:,:,29)','LineWidth',2)
plot(probs,SPDFLAT_TOTAL(:,:,29)','--','LineWidth',2)
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total. 168 hr. lead time.')


%FIGURAS PDF COMPARISON SH VS NH.
probs=( [prob_res:prob_res:100] + [0:prob_res:100-prob_res] )/2;
figure
subplot(1,3,1)
hold on
plot(probs,PDFLAT_SHEMI(:,:,1)','LineWidth',2)
plot(probs,PDFLAT_NHEMI(:,:,1)','--','LineWidth',2)
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total sample. 00 hr. lead time.')
subplot(1,3,2)
hold on
plot(probs,PDFLAT_SHEMI(:,:,16)','LineWidth',2)
plot(probs,PDFLAT_NHEMI(:,:,16)','--','LineWidth',2)
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total sample. 96 hr. lead time.')
subplot(1,3,3)
hold on
plot(probs,PDFLAT_SHEMI(:,:,29)','LineWidth',2)
plot(probs,PDFLAT_NHEMI(:,:,29)','--','LineWidth',2)
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total. 168 hr. lead time.')

%FIGURAS PDF COMPARISON SH VS NH.
probs=( [prob_res:prob_res:100] + [0:prob_res:100-prob_res] )/2;
figure
subplot(1,3,1)
hold on
plot(probs,PDFLAT_CALIDA(:,:,1)','LineWidth',2)
plot(probs,PDFLAT_FRIA(:,:,1)','--','LineWidth',2)
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total sample. 00 hr. lead time.')
subplot(1,3,2)
hold on
plot(probs,PDFLAT_CALIDA(:,:,16)','LineWidth',2)
plot(probs,PDFLAT_FRIA(:,:,16)','--','LineWidth',2)
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total sample. 96 hr. lead time.')
subplot(1,3,3)
hold on
plot(probs,PDFLAT_CALIDA(:,:,29)','LineWidth',2)
plot(probs,PDFLAT_FRIA(:,:,29)','--','LineWidth',2)
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total. 168 hr. lead time.')




