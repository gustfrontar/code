clear all
close all


%Este script plotea la relacion entre el error y el spread para diferentes
%situaciones (segun hemisferio y estacion del anio).
addpath('../common_functions_vpo/');
EnsSizeThre=0.5;    %Ensambles mas chicos que este porcentaje del total no van a ser tenidos en cuenta en el calculo.

load '../RESULTS/kwbc/ERRORSPREAD/ERROR_SPREAD_2007040112_2010040112.mat'

enssize=size(group.minanom,1);
flength=size(group.minanom,2);

%Calculos previos
tmpmeananomerror=squeeze(nanmean(group.minanom,1))-group.minanomanalysis;
tmpctrlanomerror=squeeze(group.minanom(1,:,:))-group.minanomanalysis;
tmpmeanlaperror=squeeze(nanmean(group.minlap,1))-group.minlapanalysis;
tmpctrllaperror=squeeze(group.minlap(1,:,:))-group.minlapanalysis;

tmpsprdanom=squeeze(nanstd(group.minanom,1));
tmpsprdlap=squeeze(nanstd(group.minlap,1));


tmpmemb=group.nummembers;
mask=( isnan(tmpmeananomerror) | isnan(tmpctrlanomerror) | tmpmemb < enssize*EnsSizeThre );  %Esta mascara considera solo los puntos donde la media y el control estan presentes.

tmpmeananomerror(mask)=NaN;
tmpctrlanomerror(mask)=NaN;
tmpmeanlaperror(mask)=NaN;
tmpsprdanom(mask)=NaN;
tmpctrllaperror(mask)=NaN;

%Calculo para todos los grupos.
meanerror_total=abs(tmpmeananomerror);
ctrlerror_total=abs(tmpctrlanomerror);
spread_total=tmpsprdanom;
%Genero un error syntetico que proviene de una distribucion normal,
%unbiased y en donde cada dia el spread de dicha distribucion es igual al
%spread del ensemble.
synerror_total=abs(spread_total.*randn(size(spread_total)));

%Para el hemisferio sur
index=group.shemi==1;
meanerror_shemi=abs(tmpmeananomerror(:,index));
ctrlerror_shemi=abs(tmpctrlanomerror(:,index));
spread_shemi=tmpsprdanom(:,index);
synerror_shemi=abs(spread_shemi.*randn(size(spread_shemi)));

%Para el hemisferio norte
index=group.nhemi==1;
meanerror_nhemi=abs(tmpmeananomerror(:,index));
ctrlerror_nhemi=abs(tmpctrlanomerror(:,index));
spread_nhemi=tmpsprdanom(:,index);
synerror_nhemi=abs(spread_nhemi.*randn(size(spread_nhemi)));

%Para la estacion calida
index=group.calida==1;
meanerror_calida=abs(tmpmeananomerror(:,index));
ctrlerror_calida=abs(tmpctrlanomerror(:,index));
spread_calida=tmpsprdanom(:,index);
synerror_calida=abs(spread_calida.*randn(size(spread_calida)));

%Para la estacion fria
index=group.fria==1;
meanerror_fria=abs(tmpmeananomerror(:,index));
ctrlerror_fria=abs(tmpctrlanomerror(:,index));
spread_fria=tmpsprdanom(:,index);
synerror_fria=abs(spread_fria.*randn(size(spread_fria)));

prob_res=25;
%COMPUTE THE RESOLUTION INDEX 
for ii=1:flength
[IRANOM_TOTAL(:,ii) IRSSANOM_TOTAL(:,ii) IP PDFANOM_TOTAL(:,:,ii)]=resolution_index_fun(spread_total(ii,:),meanerror_total(ii,:),prob_res,prob_res);
[SIRANOM_TOTAL(:,ii) SIRSSANOM_TOTAL(:,ii) SIP SPDFANOM_TOTAL(:,:,ii)]=resolution_index_fun(spread_total(ii,:),synerror_total(ii,:),prob_res,prob_res);
[IRANOM_SHEMI(:,ii) IRSSANOM_SHEMI(:,ii) IP PDFANOM_SHEMI(:,:,ii)]=resolution_index_fun(spread_shemi(ii,:),meanerror_shemi(ii,:),prob_res,prob_res);
[SIRANOM_SHEMI(:,ii) SIRSSANOM_SHEMI(:,ii) SIP SPDFANOM_SHEMI(:,:,ii)]=resolution_index_fun(spread_shemi(ii,:),synerror_shemi(ii,:),prob_res,prob_res);
[IRANOM_NHEMI(:,ii) IRSSANOM_NHEMI(:,ii) IP PDFANOM_NHEMI(:,:,ii)]=resolution_index_fun(spread_nhemi(ii,:),meanerror_nhemi(ii,:),prob_res,prob_res);
[SIRANOM_NHEMI(:,ii) SIRSSANOM_NHEMI(:,ii) SIP SPDFANOM_NHEMI(:,:,ii)]=resolution_index_fun(spread_nhemi(ii,:),synerror_nhemi(ii,:),prob_res,prob_res);
[IRANOM_CALIDA(:,ii) IRSSANOM_CALIDA(:,ii) IP PDFANOM_CALIDA(:,:,ii)]=resolution_index_fun(spread_calida(ii,:),meanerror_calida(ii,:),prob_res,prob_res);
[SIRANOM_CALIDA(:,ii) SIRSSANOM_CALIDA(:,ii) SIP SPDFANOM_CALIDA(:,:,ii)]=resolution_index_fun(spread_calida(ii,:),synerror_calida(ii,:),prob_res,prob_res);
[IRANOM_FRIA(:,ii) IRSSANOM_FRIA(:,ii) IP PDFANOM_FRIA(:,:,ii)]=resolution_index_fun(spread_fria(ii,:),meanerror_fria(ii,:),prob_res,prob_res); 
[SIRANOM_FRIA(:,ii) SIRSSANOM_FRIA(:,ii) SIP SPDFANOM_FRIA(:,:,ii)]=resolution_index_fun(spread_fria(ii,:),synerror_fria(ii,:),prob_res,prob_res); 
end

horas=((1:29)-1)*6;
%FIGURA IRSS PARA EL TOTAL
figure
hold on
plot(horas,IRSSANOM_TOTAL','LineWidth',2)
plot(horas,SIRSSANOM_TOTAL','--','LineWidth',2)
xlabel('Lead time')
ylabel('IRSS')
set(gca,'XGrid','On','YGrid','On')
legend('75% ERROR','50% ERROR','25% ERROR')
title('IRSS for the total sample')

%FIGURAS HEMISFERIO NORTE VS HEMISFERIO SUR.
figure
hold on
plot(horas,IRSSANOM_SHEMI','LineWidth',2)
plot(horas,IRSSANOM_NHEMI','--','LineWidth',2)
xlabel('Lead time')
ylabel('IRSS')
set(gca,'XGrid','On','YGrid','On')
legend('75% ERROR SH','50% ERROR SH','25% ERROR SH','75% ERROR NH','50% ERROR NH','25% ERROR NH')
title('IRSS by hemisphere')

%FIGURAS ESTACION CALIDA VS ESTACION FRIA.
figure
hold on
plot(horas,IRSSANOM_CALIDA','LineWidth',2)
plot(horas,IRSSANOM_FRIA','--','LineWidth',2)
xlabel('Lead time')
ylabel('IRSS')
set(gca,'XGrid','On','YGrid','On')
legend('75% ERROR W','50% ERROR W','25% ERROR W','75% ERROR C','50% ERROR C','25% ERROR C')
title('IRSS by hemisphere')

%FIGURAS PDF EJEMPLOS (3 TIEMPOS). MUESTRA TOTAL
probs=( [prob_res:prob_res:100] + [0:prob_res:100-prob_res] )/2;
figure
subplot(1,3,1)
hold on
plot(probs,PDFANOM_TOTAL(:,:,1)','LineWidth',2)
plot(probs,SPDFANOM_TOTAL(:,:,1)','--','LineWidth',2)
xlabel('Spread')
ylabel('Error probability')
set(gca,'XGrid','On','YGrid','On')
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total sample. 00 hr. lead time.')
subplot(1,3,2)
hold on
plot(probs,PDFANOM_TOTAL(:,:,16)','LineWidth',2)
plot(probs,SPDFANOM_TOTAL(:,:,16)','--','LineWidth',2)
xlabel('Spread')
ylabel('Error probability')
set(gca,'XGrid','On','YGrid','On')
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total sample. 96 hr. lead time.')
subplot(1,3,3)
hold on
plot(probs,PDFANOM_TOTAL(:,:,29)','LineWidth',2)
plot(probs,SPDFANOM_TOTAL(:,:,29)','--','LineWidth',2)
xlabel('Spread')
ylabel('Error probability')
set(gca,'XGrid','On','YGrid','On')
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total. 168 hr. lead time.')


%FIGURAS PDF COMPARISON SH VS NH.
probs=( [prob_res:prob_res:100] + [0:prob_res:100-prob_res] )/2;
figure
subplot(1,3,1)
hold on
plot(probs,PDFANOM_SHEMI(:,:,1)','LineWidth',2)
plot(probs,PDFANOM_NHEMI(:,:,1)','--','LineWidth',2)
xlabel('Spread')
ylabel('Error probability')
set(gca,'XGrid','On','YGrid','On')
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total sample. 00 hr. lead time.')
subplot(1,3,2)
hold on
plot(probs,PDFANOM_SHEMI(:,:,16)','LineWidth',2)
plot(probs,PDFANOM_NHEMI(:,:,16)','--','LineWidth',2)
xlabel('Spread')
ylabel('Error probability')
set(gca,'XGrid','On','YGrid','On')
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total sample. 96 hr. lead time.')
subplot(1,3,3)
hold on
plot(probs,PDFANOM_SHEMI(:,:,29)','LineWidth',2)
plot(probs,PDFANOM_NHEMI(:,:,29)','--','LineWidth',2)
xlabel('Spread')
ylabel('Error probability')
set(gca,'XGrid','On','YGrid','On')
legend('75% ERROR','50% ERROR','25% ERROR')
title('Error conditional probability, total. 168 hr. lead time.')

%FIGURAS PDF COMPARISON SH VS NH.
probs=( [prob_res:prob_res:100] + [0:prob_res:100-prob_res] )/2;
figure
subplot(1,3,1)
hold on
plot(probs,PDFANOM_CALIDA(:,:,1)','LineWidth',2)
plot(probs,PDFANOM_FRIA(:,:,1)','--','LineWidth',2)
xlabel('Spread')
ylabel('Error probability')
legend('75% ERROR','50% ERROR','25% ERROR')
set(gca,'XGrid','On','YGrid','On')
title('Error conditional probability, total sample. 00 hr. lead time.')
subplot(1,3,2)
hold on
plot(probs,PDFANOM_CALIDA(:,:,16)','LineWidth',2)
plot(probs,PDFANOM_FRIA(:,:,16)','--','LineWidth',2)
xlabel('Spread')
ylabel('Error probability')
legend('75% ERROR','50% ERROR','25% ERROR')
set(gca,'XGrid','On','YGrid','On')
title('Error conditional probability, total sample. 96 hr. lead time.')
subplot(1,3,3)
hold on
plot(probs,PDFANOM_CALIDA(:,:,29)','LineWidth',2)
plot(probs,PDFANOM_FRIA(:,:,29)','--','LineWidth',2)
xlabel('Spread')
ylabel('Error probability')
legend('75% ERROR','50% ERROR','25% ERROR')
set(gca,'XGrid','On','YGrid','On')
title('Error conditional probability, total. 168 hr. lead time.')




