clear all
close all

%Este script plotea la relacion entre el error y el spread para diferentes
%situaciones (segun hemisferio y estacion del anio).
addpath('../common_functions_vpo/');
EnsSizeThre=0.5;    %Ensambles mas chicos que este porcentaje del total no van a ser tenidos en cuenta en el calculo.

load '../RESULTS/kwbc/ERRORSPREAD/ERROR_SPREAD_2007040112_2010040112.mat'

%for ii=1:size(group.distlonspread,2)
%group.distlonspreadsmooth(:,ii)=smooth(group.distlonspread(:,ii),9);
%end

enssize=size(group.minlon,1);
flength=size(group.minlon,2);

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
%smoothspread_total=group.distlonspreadsmooth;

%Para el hemisferio sur
index=group.shemi==1;
meanerror_shemi=abs(group.meandistlonerror(:,index));
ctrlerror_shemi=abs(group.ctrldistlonerror(:,index));
spread_shemi=group.distlonspread(:,index);
synerror_shemi=abs(spread_shemi.*randn(size(spread_shemi)));
%smoothspread_shemi=group.distlonspreadsmooth(:,index);

%Para el hemisferio norte
index=group.nhemi==1;
meanerror_nhemi=abs(group.meandistlonerror(:,index));
ctrlerror_nhemi=abs(group.ctrldistlonerror(:,index));
spread_nhemi=group.distlonspread(:,index);
synerror_nhemi=abs(spread_nhemi.*randn(size(spread_nhemi)));
%smoothspread_nhemi=group.distlonspreadsmooth(:,index);

%Para la estacion calida
index=group.calida==1;
meanerror_calida=abs(group.meandistlonerror(:,index));
ctrlerror_calida=abs(group.ctrldistlonerror(:,index));
spread_calida=group.distlonspread(:,index);
synerror_calida=abs(spread_calida.*randn(size(spread_calida)));
%smoothspread_calida=group.distlonspreadsmooth(:,index);

%Para la estacion fria
index=group.fria==1;
meanerror_fria=abs(group.meandistlonerror(:,index));
ctrlerror_fria=abs(group.ctrldistlonerror(:,index));
spread_fria=group.distlonspread(:,index);
synerror_fria=abs(spread_fria.*randn(size(spread_fria)));
%smoothspread_fria=group.distlonspreadsmooth(:,index);

prob_res=25;
%COMPUTE THE RESOLUTION INDEX 
for ii=1:flength
[IRLON_TOTAL(:,ii) IRSSLON_TOTAL(:,ii) IP PDFLON_TOTAL(:,:,ii)]=resolution_index_fun(spread_total(ii,:),meanerror_total(ii,:),prob_res,prob_res);
%[IRLON_TOTALSMOOTH(:,ii) IRSSLON_TOTALSMOOTH(:,ii) IP PDFLON_TOTALSMOOTH(:,:,ii)]=resolution_index_fun(smoothspread_total(ii,:),meanerror_total(ii,:),prob_res,prob_res);
[SIRLON_TOTAL(:,ii) SIRSSLON_TOTAL(:,ii) SIP SPDFLON_TOTAL(:,:,ii)]=resolution_index_fun(spread_total(ii,:),synerror_total(ii,:),prob_res,prob_res);
[IRLON_SHEMI(:,ii) IRSSLON_SHEMI(:,ii) IP PDFLON_SHEMI(:,:,ii)]=resolution_index_fun(spread_shemi(ii,:),meanerror_shemi(ii,:),prob_res,prob_res);
[SIRLON_SHEMI(:,ii) SIRSSLON_SHEMI(:,ii) SIP SPDFLON_SHEMI(:,:,ii)]=resolution_index_fun(spread_shemi(ii,:),synerror_shemi(ii,:),prob_res,prob_res);
[IRLON_NHEMI(:,ii) IRSSLON_NHEMI(:,ii) IP PDFLON_NHEMI(:,:,ii)]=resolution_index_fun(spread_nhemi(ii,:),meanerror_nhemi(ii,:),prob_res,prob_res);
[SIRLON_NHEMI(:,ii) SIRSSLON_NHEMI(:,ii) SIP SPDFLON_NHEMI(:,:,ii)]=resolution_index_fun(spread_nhemi(ii,:),synerror_nhemi(ii,:),prob_res,prob_res);
[IRLON_CALIDA(:,ii) IRSSLON_CALIDA(:,ii) IP PDFLON_CALIDA(:,:,ii)]=resolution_index_fun(spread_calida(ii,:),meanerror_calida(ii,:),prob_res,prob_res);
[SIRLON_CALIDA(:,ii) SIRSSLON_CALIDA(:,ii) SIP SPDFLON_CALIDA(:,:,ii)]=resolution_index_fun(spread_calida(ii,:),synerror_calida(ii,:),prob_res,prob_res);
[IRLON_FRIA(:,ii) IRSSLON_FRIA(:,ii) IP PDFLON_FRIA(:,:,ii)]=resolution_index_fun(spread_fria(ii,:),meanerror_fria(ii,:),prob_res,prob_res); 
[SIRLON_FRIA(:,ii) SIRSSLON_FRIA(:,ii) SIP SPDFLON_FRIA(:,:,ii)]=resolution_index_fun(spread_fria(ii,:),synerror_fria(ii,:),prob_res,prob_res); 
end

%FIGURA IRSS PARA EL TOTAL
horas=((1:29)-1)*6;
figure
hold on
plot(horas,IRSSLON_TOTAL','LineWidth',2)
plot(horas,SIRSSLON_TOTAL','--','LineWidth',2)
%plot(IRSSLON_TOTALSMOOTH','-.','LineWidth',2)
xlabel('Lead time')
ylabel('IRSS')
legend('75% ERROR','50% ERROR','25% ERROR')
title('IRSS for the total sample')
set(gca,'XGrid','On','YGrid','On')

%FIGURAS HEMISFERIO NORTE VS HEMISFERIO SUR.
figure
hold on
plot(horas,IRSSLON_SHEMI','LineWidth',2)
plot(horas,IRSSLON_NHEMI','--','LineWidth',2)
xlabel('Lead time')
ylabel('IRSS')
legend('75% ERROR SH','50% ERROR SH','25% ERROR SH','75% ERROR NH','50% ERROR NH','25% ERROR NH')
title('IRSS by hemisphere')
set(gca,'XGrid','On','YGrid','On')

%FIGURAS ESTACION CALIDA VS ESTACION FRIA.
figure
hold on
plot(horas,IRSSLON_CALIDA','LineWidth',2)
plot(horas,IRSSLON_FRIA','--','LineWidth',2)
xlabel('Lead time')
ylabel('IRSS')
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
xlabel('Spread quantile')
ylabel('Error conditional probability')
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total sample. 00 hr. lead time.')
set(gca,'XGrid','On','YGrid','On')
subplot(1,3,2)
hold on
plot(probs,PDFLON_TOTAL(:,:,16)','LineWidth',2)
plot(probs,SPDFLON_TOTAL(:,:,16)','--','LineWidth',2)
xlabel('Spread quantile')
ylabel('Error conditional probability')
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total sample. 96 hr. lead time.')
set(gca,'XGrid','On','YGrid','On')
subplot(1,3,3)
hold on
plot(probs,PDFLON_TOTAL(:,:,29)','LineWidth',2)
plot(probs,SPDFLON_TOTAL(:,:,29)','--','LineWidth',2)
xlabel('Spread quantile')
ylabel('Error conditional probability')
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total. 168 hr. lead time.')
set(gca,'XGrid','On','YGrid','On')

%FIGURAS PDF COMPARISON SH VS NH.
probs=( [prob_res:prob_res:100] + [0:prob_res:100-prob_res] )/2;
figure
subplot(1,3,1)
hold on
plot(probs,PDFLON_SHEMI(:,:,1)','LineWidth',2)
plot(probs,PDFLON_NHEMI(:,:,1)','--','LineWidth',2)
xlabel('Spread quantile')
ylabel('Error conditional probability')
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total sample. 00 hr. lead time.')
set(gca,'XGrid','On','YGrid','On')
subplot(1,3,2)
hold on
plot(probs,PDFLON_SHEMI(:,:,16)','LineWidth',2)
plot(probs,PDFLON_NHEMI(:,:,16)','--','LineWidth',2)
xlabel('Spread quantile')
ylabel('Error conditional probability')
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total sample. 96 hr. lead time.')
set(gca,'XGrid','On','YGrid','On')
subplot(1,3,3)
hold on
plot(probs,PDFLON_SHEMI(:,:,29)','LineWidth',2)
plot(probs,PDFLON_NHEMI(:,:,29)','--','LineWidth',2)
xlabel('Spread quantile')
ylabel('Error conditional probability')
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
xlabel('Spread quantile')
ylabel('Error conditional probability')
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total sample. 00 hr. lead time.')
set(gca,'XGrid','On','YGrid','On')
subplot(1,3,2)
hold on
plot(probs,PDFLON_CALIDA(:,:,16)','LineWidth',2)
plot(probs,PDFLON_FRIA(:,:,16)','--','LineWidth',2)
xlabel('Spread quantile')
ylabel('Error conditional probability')
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total sample. 96 hr. lead time.')
set(gca,'XGrid','On','YGrid','On')
subplot(1,3,3)
hold on
plot(probs,PDFLON_CALIDA(:,:,29)','LineWidth',2)
plot(probs,PDFLON_FRIA(:,:,29)','--','LineWidth',2)
xlabel('Spread quantile')
ylabel('Error conditional probability')
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total. 168 hr. lead time.')
set(gca,'XGrid','On','YGrid','On')




