clear all
close all


%Este script plotea la relacion entre el error y el spread para diferentes
%situaciones (segun hemisferio y estacion del anio).
addpath('../common_functions_vpo/');
EnsSizeThre=0.5;    %Ensambles mas chicos que este porcentaje del total no van a ser tenidos en cuenta en el calculo.

filein='../RESULTS/kwbc/ERRORSPREAD/ERROR_SPREAD_2007040112_2010040112.mat';
outfile='../RESULTS/kwbc/ERRORSPREAD/LONERRORINDEX_2007040112_2010040112.mat';

load(filein)

group.minanom=[];
group.minlap=[];
group.uvel=[];
group.vvel=[];
group.trajid=[];
group.minlat=[];
group.minlon=[];

enssize=size(group.distlonpert,1);
flength=size(group.distlonpert,2);

mask=( isnan(group.meandistlonerror) | isnan(group.distlonspread) | group.nummembers < enssize*EnsSizeThre );
group.meandistlonerrror(mask)=NaN;
group.distlonspread(mask)=NaN;

%Calculo para todos los grupos.
meanerror_total=abs(group.meandistlonerror);
ctrlerror_total=abs(group.ctrldistlonerror);
spread_total=group.distlonspread;
%Genero un error syntetico que proviene de una distribucion normal,
%unbiased y en donde cada dia el spread de dicha distribucion es igual al
%spread del ensemble.
synerror_total=abs(spread_total.*randn(size(spread_total)));

%Para el hemisferio sur
index=group.shemi==1;
meanerror_shemi=abs(group.meandistlonerror(:,index));
ctrlerror_shemi=abs(group.ctrldistlonerror(:,index));
spread_shemi=group.distlonspread(:,index);
synerror_shemi=abs(spread_shemi.*randn(size(spread_shemi)));

%Para el hemisferio norte
index=group.nhemi==1;
meanerror_nhemi=abs(group.meandistlonerror(:,index));
ctrlerror_nhemi=abs(group.ctrldistlonerror(:,index));
spread_nhemi=group.distlonspread(:,index);
synerror_nhemi=abs(spread_nhemi.*randn(size(spread_nhemi)));

%Para la estacion calida
index=group.calida==1;
meanerror_calida=abs(group.meandistlonerror(:,index));
ctrlerror_calida=abs(group.ctrldistlonerror(:,index));
spread_calida=group.distlonspread(:,index);
synerror_calida=abs(spread_calida.*randn(size(spread_calida)));

%Para la estacion fria
index=group.fria==1;
meanerror_fria=abs(group.meandistlonerror(:,index));
ctrlerror_fria=abs(group.ctrldistlonerror(:,index));
spread_fria=group.distlonspread(:,index);
synerror_fria=abs(spread_fria.*randn(size(spread_fria)));

prob_res=10;
error_prob_res=25;
%COMPUTE THE RESOLUTION INDEX 
for ii=1:flength
[IRLON_TOTAL(:,ii) IRSSLON_TOTAL(:,ii) IP PDFLON_TOTAL(:,:,ii)]=resolution_index_fun(spread_total(ii,:),meanerror_total(ii,:),prob_res,error_prob_res);
[SIRLON_TOTAL(:,ii) SIRSSLON_TOTAL(:,ii) SIP SPDFLON_TOTAL(:,:,ii)]=resolution_index_fun(spread_total(ii,:),synerror_total(ii,:),prob_res,error_prob_res);
[IRLON_SHEMI(:,ii) IRSSLON_SHEMI(:,ii) IP PDFLON_SHEMI(:,:,ii)]=resolution_index_fun(spread_shemi(ii,:),meanerror_shemi(ii,:),prob_res,error_prob_res);
[SIRLON_SHEMI(:,ii) SIRSSLON_SHEMI(:,ii) SIP SPDFLON_SHEMI(:,:,ii)]=resolution_index_fun(spread_shemi(ii,:),synerror_shemi(ii,:),prob_res,error_prob_res);
[IRLON_NHEMI(:,ii) IRSSLON_NHEMI(:,ii) IP PDFLON_NHEMI(:,:,ii)]=resolution_index_fun(spread_nhemi(ii,:),meanerror_nhemi(ii,:),prob_res,error_prob_res);
[SIRLON_NHEMI(:,ii) SIRSSLON_NHEMI(:,ii) SIP SPDFLON_NHEMI(:,:,ii)]=resolution_index_fun(spread_nhemi(ii,:),synerror_nhemi(ii,:),prob_res,error_prob_res);
[IRLON_CALIDA(:,ii) IRSSLON_CALIDA(:,ii) IP PDFLON_CALIDA(:,:,ii)]=resolution_index_fun(spread_calida(ii,:),meanerror_calida(ii,:),prob_res,error_prob_res);
[SIRLON_CALIDA(:,ii) SIRSSLON_CALIDA(:,ii) SIP SPDFLON_CALIDA(:,:,ii)]=resolution_index_fun(spread_calida(ii,:),synerror_calida(ii,:),prob_res,error_prob_res);
[IRLON_FRIA(:,ii) IRSSLON_FRIA(:,ii) IP PDFLON_FRIA(:,:,ii)]=resolution_index_fun(spread_fria(ii,:),meanerror_fria(ii,:),prob_res,error_prob_res); 
[SIRLON_FRIA(:,ii) SIRSSLON_FRIA(:,ii) SIP SPDFLON_FRIA(:,:,ii)]=resolution_index_fun(spread_fria(ii,:),synerror_fria(ii,:),prob_res,error_prob_res); 

[ERRORV_TOTAL(:,ii) SPREADV_TOTAL(:,ii) R_TOTAL(ii) nada nada SLOPE_TOTAL(ii)]=spread_consistency_fun(spread_total(ii,:),meanerror_total(ii,:),20);
[ERRORV_SHEMI(:,ii) SPREADV_SHEMI(:,ii) R_SHEMI(ii) nada nada SLOPE_SHEMI(ii)]=spread_consistency_fun(spread_shemi(ii,:),meanerror_shemi(ii,:),20);
[ERRORV_NHEMI(:,ii) SPREADV_NHEMI(:,ii) R_NHEMI(ii) nada nada SLOPE_NHEMI(ii)]=spread_consistency_fun(spread_nhemi(ii,:),meanerror_nhemi(ii,:),20);
[ERRORV_CALIDA(:,ii) SPREADV_CALIDA(:,ii) R_CALIDA(ii) nada nada SLOPE_CALIDA(ii)]=spread_consistency_fun(spread_calida(ii,:),meanerror_calida(ii,:),20);
[ERRORV_FRIA(:,ii) SPREADV_FRIA(:,ii) R_FRIA(ii) nada nada SLOPE_FRIA(ii)]=spread_consistency_fun(spread_fria(ii,:),meanerror_fria(ii,:),20);


end
horas=((1:29)-1)*6;

%FIGURA BETA COMO FUNCION DEL TIEMPO DERIVADA DEL ENSAMBLE.
tmp=spread_total;
tmp(tmp==0)=NaN;
BETA_TOTAL=nanstd(log(tmp),[],2);
MEANS_TOTAL=nanmean(log(tmp),2);

tmp=spread_shemi;
tmp(tmp==0)=NaN;
BETA_SHEMI=nanstd(log(tmp),[],2);
MEANS_SHEMI=nanmean(log(tmp),2);

tmp=spread_nhemi;
tmp(tmp==0)=NaN;
BETA_NHEMI=nanstd(log(tmp),[],2);
MEANS_NHEMI=nanmean(log(tmp),2);

tmp=spread_calida;
tmp(tmp==0)=NaN;
BETA_CALIDA=nanstd(log(tmp),[],2);
MEANS_CALIDA=nanmean(log(tmp),2);

tmp=spread_fria;
tmp(tmp==0)=NaN;
BETA_FRIA=nanstd(log(tmp),[],2);
MEANS_FRIA=nanmean(log(tmp),2);

figure
plot(BETA_TOTAL,'LineWidth',2);
hold on
plot(MEANS_TOTAL/10,'--','LineWidth',2);
set(gca,'XGrid','On','YGrid','On')
title('Beta as a function of time')
xlabel('Lead time')
ylabel('Beta')

figure
hold on
plot(BETA_CALIDA,'b-','LineWidth',2);
plot(BETA_FRIA,'r-','LineWidth',2);
plot(BETA_TOTAL,'k-','LineWidth',2);
plot(MEANS_CALIDA/10,'b--','LineWidth',2);
plot(MEANS_FRIA/10,'r--','LineWidth',2);
plot(MEANS_TOTAL/10,'k--','LineWidth',2);
set(gca,'XGrid','On','YGrid','On')
title('Beta as a function of time')
xlabel('Lead time')
ylabel('Beta')

%FIGURA IRSS PARA EL TOTAL

figure
hold on
plot(horas,IRSSLON_TOTAL','LineWidth',2)
plot(horas,SIRSSLON_TOTAL','--','LineWidth',2)
legend('Small error','Medium error','Large error')
title('IRSS for the total sample')
xlabel('Lead time')
ylabel('IRSS')
set(gca,'XGrid','On','YGrid','On')


figure
hold on
subplot(1,2,1)
plot(horas,R_TOTAL,'LineWidth',2)
xlabel('Lead time')
ylabel('Correlation coefficient')
set(gca,'XGrid','On','YGrid','On')
axis([0 168 0.5 1])
subplot(1,2,2)
plot(horas,SLOPE_TOTAL,'LineWidth',2)
xlabel('Lead time')
ylabel('Slope')
set(gca,'XGrid','On','YGrid','On')
axis([0 168 0.0 1])

figure
hold on
subplot(1,2,1)
hold on
plot(horas,R_SHEMI,'LineWidth',2)
plot(horas,R_NHEMI,'--','LineWidth',2)
xlabel('Lead time')
ylabel('Correlation coefficient')
set(gca,'XGrid','On','YGrid','On')
legend('SH','NH')
axis([0 168 0.5 1])
subplot(1,2,2)
hold on
plot(horas,SLOPE_SHEMI,'LineWidth',2)
plot(horas,SLOPE_NHEMI,'--','LineWidth',2)
xlabel('Lead time')
ylabel('Slope')
set(gca,'XGrid','On','YGrid','On')
axis([0 168 0.0 1])

%FIGURAS HEMISFERIO NORTE VS HEMISFERIO SUR.
figure
hold on
plot(horas,IRSSLON_SHEMI','LineWidth',2)
plot(horas,IRSSLON_NHEMI','--','LineWidth',2)
xlabel('Lead time')
ylabel('IRSS')
legend('Small E. SH','Medium E. SH','Large E. SH','Small E. NH','Medium E. NH','Large E. NH')
title('IRSS by hemisphere')
set(gca,'XGrid','On','YGrid','On')

%FIGURAS ESTACION CALIDA VS ESTACION FRIA.
figure
hold on
plot(IRSSLON_CALIDA','LineWidth',2)
plot(IRSSLON_FRIA','--','LineWidth',2)
legend('75% ERROR W','50% ERROR W','25% ERROR W','75% ERROR C','50% ERROR C','25% ERROR C')
title('IRSS by hemisphere')
set(gca,'XGrid','On','YGrid','On')

%FIGURAS PDF EJEMPLOS (3 TIEMPOS). MUESTRA TOTAL
probs=( [prob_res:prob_res:100] + [0:prob_res:100-prob_res] )/2;
figure
subplot(1,3,1)
hold on
plot(probs,PDFLON_TOTAL(:,:,1)','LineWidth',2)
plot(probs,SPDFLON_TOTAL(:,:,1)','--','LineWidth',2)
legend('SMALL ERROR','MEDIUM ERROR','LARGE ERROR')
xlabel('Spread probability')
ylabel('Conditional error probability')
title('00 hr. lead time.')
set(gca,'XGrid','On','YGrid','On')
subplot(1,3,2)
hold on
plot(probs,PDFLON_TOTAL(:,:,16)','LineWidth',2)
plot(probs,SPDFLON_TOTAL(:,:,16)','--','LineWidth',2)
%legend('SMALL ERROR','MEDIUM ERROR','LARGE ERROR')
title('96 hr. lead time.')
xlabel('Spread probability')
ylabel('Conditional error probability')
set(gca,'XGrid','On','YGrid','On')
subplot(1,3,3)
hold on
plot(probs,PDFLON_TOTAL(:,:,29)','LineWidth',2)
plot(probs,SPDFLON_TOTAL(:,:,29)','--','LineWidth',2)
%legend('SMALL ERROR','MEDIUM ERROR','LARGE ERROR')
title('168 hr. lead time.')
xlabel('Spread probability')
ylabel('Conditional error probability')
set(gca,'XGrid','On','YGrid','On')


%FIGURAS PDF COMPARISON SH VS NH.
probs=( [prob_res:prob_res:100] + [0:prob_res:100-prob_res] )/2;
figure
subplot(1,3,1)
hold on
plot(probs,PDFLON_SHEMI(:,:,1)','LineWidth',2)
plot(probs,PDFLON_NHEMI(:,:,1)','--','LineWidth',2)
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total sample. 00 hr. lead time.')
set(gca,'XGrid','On','YGrid','On')
subplot(1,3,2)
hold on
plot(probs,PDFLON_SHEMI(:,:,16)','LineWidth',2)
plot(probs,PDFLON_NHEMI(:,:,16)','--','LineWidth',2)
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total sample. 96 hr. lead time.')
set(gca,'XGrid','On','YGrid','On')
subplot(1,3,3)
hold on
plot(probs,PDFLON_SHEMI(:,:,29)','LineWidth',2)
plot(probs,PDFLON_NHEMI(:,:,29)','--','LineWidth',2)
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total. 168 hr. lead time.')
set(gca,'XGrid','On','YGrid','On')

%FIGURAS PDF COMPARISON SH VS NH.
probs=( [prob_res:prob_res:100] + [0:prob_res:100-prob_res] )/2;
figure
subplot(1,3,1)
hold on
plot(probs,PDFLON_CALIDA(:,:,1)','LineWidth',2)
plot(probs,PDFLON_FRIA(:,:,1)','--','LineWidth',2)
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total sample. 00 hr. lead time.')
set(gca,'XGrid','On','YGrid','On')
subplot(1,3,2)
hold on
plot(probs,PDFLON_CALIDA(:,:,16)','LineWidth',2)
plot(probs,PDFLON_FRIA(:,:,16)','--','LineWidth',2)
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total sample. 96 hr. lead time.')
set(gca,'XGrid','On','YGrid','On')
subplot(1,3,3)
hold on
plot(probs,PDFLON_CALIDA(:,:,29)','LineWidth',2)
plot(probs,PDFLON_FRIA(:,:,29)','--','LineWidth',2)
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total. 168 hr. lead time.')
set(gca,'XGrid','On','YGrid','On')

clear group
clear tmpmean tmpctrl tmpmemb tmpsprd
save(outfile)


