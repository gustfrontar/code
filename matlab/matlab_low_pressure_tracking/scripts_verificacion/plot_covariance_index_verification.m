clear all
close all

%En este script la idea es verificar las covarianzas de los errores latitud
%y longitud, lo que vamos a hacer es utilizar el indice de resolucion, la
%covarianza de las perturbaciones derivada del ensamble y la covarianza del
%error "estimada" como el producto de las componentes del error en latitud
%y longitud. Luego vamos a aplicar el indice de resolucion sobre dichas
%variables para medir la intensidad de la relacion.

addpath('../common_functions_vpo/');
EnsSizeThre=0.5;    %Ensambles mas chicos que este porcentaje del total no van a ser tenidos en cuenta en el calculo.

filein='../RESULTS/ecmf/ERRORSPREAD/ERROR_SPREAD_2007040112_2010040112.mat';
outfile='../RESULTS/ecmf/ERRORSPREAD/COVERRORINDEX_2007040112_2010040112.mat';

load(filein)

group.minanom=[];
group.minlap=[];
group.uvel=[];
group.vvel=[];
group.trajid=[];
group.minlat=[];
group.minlon=[];

enssize=size(group.minlon,1);
flength=size(group.minlon,2);

%Calculo para todos los grupos.
pertlat_total=group.distlatpert;
pertlon_total=group.distlonpert;
meanerrorlon_total=group.meandistlonerror;
meanerrorlat_total=group.meandistlaterror;

coverrorideal_total=NaN(size(meanerrorlon_total));
coverror_total=NaN(size(meanerrorlon_total));
for ii=1:flength
   for jj=1:size(pertlat_total,3)
    tmplon=pertlon_total(:,ii,jj);
    tmplat=pertlat_total(:,ii,jj);
    tmplon=tmplon(~isnan(tmplon));
    tmplat=tmplat(~isnan(tmplat));
    if(numel(tmplon) >= enssize*EnsSizeThre)   
    errorcovmatrix_total(:,:,ii,jj)=cov(tmplon,tmplat);
    tmp=mvnrnd([0 0],squeeze(errorcovmatrix_total(:,:,ii,jj)),1);
    coverror_total(ii,jj)=meanerrorlon_total(ii,jj)*meanerrorlat_total(ii,jj);
    coverrorideal_total(ii,jj)=tmp(2)*tmp(1);
    else
    errorcovmatrix_total(:,:,ii,jj)=NaN(2,2);
    end
   end
end

%Calculo para hemisferios norte y sur.
index=group.shemi==1;
pertlat_shemi=group.distlatpert(:,:,index);
pertlon_shemi=group.distlonpert(:,:,index);
meanerrorlon_shemi=group.meandistlonerror(:,index);
meanerrorlat_shemi=group.meandistlaterror(:,index);

meanerrorlonideal_shemi=NaN(size(meanerrorlon_shemi));
meanerrorlatideal_shemi=NaN(size(meanerrorlat_shemi));
coverror_shemi=NaN(size(meanerrorlon_shemi));
for ii=1:flength
   for jj=1:size(pertlat_shemi,3)
    tmplon=pertlon_shemi(:,ii,jj);
    tmplat=pertlat_shemi(:,ii,jj);
    tmplon=tmplon(~isnan(tmplon));
    tmplat=tmplat(~isnan(tmplat));
    if(numel(tmplon) >= enssize*EnsSizeThre)   
    errorcovmatrix_shemi(:,:,ii,jj)=cov(tmplon,tmplat);
    tmp=mvnrnd([0 0],squeeze(errorcovmatrix_shemi(:,:,ii,jj)),1);
    coverror_shemi(ii,jj)=meanerrorlon_shemi(ii,jj)*meanerrorlat_shemi(ii,jj);
    coverrorideal_shemi(ii,jj)=tmp(2)*tmp(1);
    else
    errorcovmatrix_shemi(:,:,ii,jj)=NaN(2,2);
    end
   end
end

index=group.nhemi==1;
pertlat_nhemi=group.distlatpert(:,:,index);
pertlon_nhemi=group.distlonpert(:,:,index);
meanerrorlon_nhemi=group.meandistlonerror(:,index);
meanerrorlat_nhemi=group.meandistlaterror(:,index);

meanerrorlonideal_nhemi=NaN(size(meanerrorlon_nhemi));
meanerrorlatideal_nhemi=NaN(size(meanerrorlat_nhemi));
coverror_nhemi=NaN(size(meanerrorlon_nhemi));
for ii=1:flength
   for jj=1:size(pertlat_nhemi,3)
    tmplon=pertlon_nhemi(:,ii,jj);
    tmplat=pertlat_nhemi(:,ii,jj);
    tmplon=tmplon(~isnan(tmplon));
    tmplat=tmplat(~isnan(tmplat));
    if(numel(tmplon) >= enssize*EnsSizeThre)   
    errorcovmatrix_nhemi(:,:,ii,jj)=cov(tmplon,tmplat);
    tmp=mvnrnd([0 0],squeeze(errorcovmatrix_nhemi(:,:,ii,jj)),1);
    coverror_nhemi(ii,jj)=meanerrorlon_nhemi(ii,jj)*meanerrorlat_nhemi(ii,jj);
    coverrorideal_nhemi(ii,jj)=tmp(2)*tmp(1);
    else
    errorcovmatrix_nhemi(:,:,ii,jj)=NaN(2,2);
    end
   end
end

%Calculo para estacion calida y fria.
% index=group.calida==1;
% pertlat_calida=group.distlatpert(:,:,index);
% pertlon_calida=group.distlonpert(:,:,index);
% meanerrorlon_calida=group.meandistlonerror(:,index);
% meanerrorlat_calida=group.meandistlaterror(:,index);
% 
% meanerrorlonideal_calida=NaN(size(meanerrorlon_calida));
% meanerrorlatideal_calida=NaN(size(meanerrorlat_calida));
% coverror_calida=NaN(size(meanerrorlon_calida));
% for ii=1:flength
%    for jj=1:size(pertlat_calida,3)
%     tmplon=pertlon_calida(:,ii,jj);
%     tmplat=pertlat_calida(:,ii,jj);
%     tmplon=tmplon(~isnan(tmplon));
%     tmplat=tmplat(~isnan(tmplat));
%     if(numel(tmplon) >= enssize*EnsSizeThre)   
%     errorcovmatrix_calida(:,:,ii,jj)=cov(tmplon,tmplat);
%     tmp=mvnrnd([0 0],squeeze(errorcovmatrix_calida(:,:,ii,jj)),1);
%     coverror_calida(ii,jj)=meanerrorlon_calida(ii,jj)*meanerrorlat_calida(ii,jj);
%     coverrorideal_calida(ii,jj)=tmp(2)*tmp(1);
%     else
%     errorcovmatrix_calida(:,:,ii,jj)=NaN(2,2);
%     end
%    end
% end
% 
% index=group.fria==1;
% pertlat_fria=group.distlatpert(:,:,index);
% pertlon_fria=group.distlonpert(:,:,index);
% meanerrorlon_fria=group.meandistlonerror(:,index);
% meanerrorlat_fria=group.meandistlaterror(:,index);
% 
% meanerrorlonideal_fria=NaN(size(meanerrorlon_fria));
% meanerrorlatideal_fria=NaN(size(meanerrorlat_fria));
% coverror_fria=NaN(size(meanerrorlon_fria));
% for ii=1:flength
%    for jj=1:size(pertlat_fria,3)
%     tmplon=pertlon_fria(:,ii,jj);
%     tmplat=pertlat_fria(:,ii,jj);
%     tmplon=tmplon(~isnan(tmplon));
%     tmplat=tmplat(~isnan(tmplat));
%     if(numel(tmplon) >= enssize*EnsSizeThre)   
%     errorcovmatrix_fria(:,:,ii,jj)=cov(tmplon,tmplat);
%     tmp=mvnrnd([0 0],squeeze(errorcovmatrix_fria(:,:,ii,jj)),1);
%     coverror_fria(ii,jj)=meanerrorlon_fria(ii,jj)*meanerrorlat_fria(ii,jj);
%     coverrorideal_fria(ii,jj)=tmp(2)*tmp(1);
%     else
%     errorcovmatrix_fria(:,:,ii,jj)=NaN(2,2);
%     end
%    end
% end


prob_res=25;
%COMPUTE THE RESOLUTION INDEX 
for ii=2:flength
[IR_TOTAL(:,ii) IRSS_TOTAL(:,ii) IP PDF_TOTAL(:,:,ii)]=resolution_index_fun(squeeze(errorcovmatrix_total(1,2,ii,:))',coverror_total(ii,:),prob_res,prob_res);
[SIR_TOTAL(:,ii) SIRSS_TOTAL(:,ii) SIP SPDF_TOTAL(:,:,ii)]=resolution_index_fun(squeeze(errorcovmatrix_total(1,2,ii,:))',coverrorideal_total(ii,:),prob_res,prob_res);
[IR_SHEMI(:,ii) IRSS_SHEMI(:,ii) IP PDF_SHEMI(:,:,ii)]=resolution_index_fun(squeeze(errorcovmatrix_shemi(1,2,ii,:))',coverror_shemi(ii,:),prob_res,prob_res);
[SIR_SHEMI(:,ii) SIRSS_SHEMI(:,ii) SIP SPDF_SHEMI(:,:,ii)]=resolution_index_fun(squeeze(errorcovmatrix_shemi(1,2,ii,:))',coverrorideal_shemi(ii,:),prob_res,prob_res);
[IR_NHEMI(:,ii) IRSS_NHEMI(:,ii) IP PDF_NHEMI(:,:,ii)]=resolution_index_fun(squeeze(errorcovmatrix_nhemi(1,2,ii,:))',coverror_nhemi(ii,:),prob_res,prob_res);
[SIR_NHEMI(:,ii) SIRSS_NHEMI(:,ii) SIP SPDF_NHEMI(:,:,ii)]=resolution_index_fun(squeeze(errorcovmatrix_nhemi(1,2,ii,:))',coverrorideal_nhemi(ii,:),prob_res,prob_res);
end


%VERIFICAR PORQUE EN DETERMINADAS CATEGORIAS NO TENGO VALORES, ENMASCARAR
%LOS CASOS DONDE EL NUMERO DE MIEMBROS EN EL ENSAMBLE ES MENOR QUE UN
%DETERMINADO UMBRAL PARA SER CONSISTENTES CON LOS OTROS EXPERIMENTOS Y
%ANALISIS.


%FIGURA IRSS PARA EL TOTAL
horas=((1:29)-1)*6;
figure
hold on
plot(horas,IRSS_TOTAL','LineWidth',2)
plot(horas,SIRSS_TOTAL','--','LineWidth',2)
legend('75% PROD.','50% PROD.','25% PROD.')
xlabel('Lead time')
ylabel('IRSS')
title('IRSS for the total sample')

%FIGURAS HEMISFERIO NORTE VS HEMISFERIO SUR.
figure
hold on
plot(horas,IRSS_SHEMI','LineWidth',2)
plot(horas,IRSS_NHEMI','--','LineWidth',2)
xlabel('Lead time')
ylabel('IRSS')
legend('75% PROD. SH','50% PROD. SH','25% PROD. SH','75% PROD. NH','50% PROD. NH','25% PROD. NH')
title('IRSS by hemisphere')


%FIGURAS PDF EJEMPLOS (3 TIEMPOS). MUESTRA TOTAL
probs=( [prob_res:prob_res:100] + [0:prob_res:100-prob_res] )/2;
figure
subplot(1,3,1)
hold on
plot(probs,PDF_TOTAL(:,:,1)','LineWidth',2)
plot(probs,SPDF_TOTAL(:,:,1)','--','LineWidth',2)
legend('75% PROD.','50% PROD.','25% PROD.')
set(gca,'XGrid','On','YGrid','On')
xlabel('Spread')
ylabel('Error product probability')
title('Error conditional probability, total sample. 00 hr. lead time.')
subplot(1,3,2)
hold on
plot(probs,PDF_TOTAL(:,:,16)','LineWidth',2)
plot(probs,SPDF_TOTAL(:,:,16)','--','LineWidth',2)
set(gca,'XGrid','On','YGrid','On')
xlabel('Spread')
ylabel('Error product probability')
legend('75% PROD.','50% PROD.','25% PROD.')
title('Error conditional probability, total sample. 96 hr. lead time.')
subplot(1,3,3)
hold on
plot(probs,PDF_TOTAL(:,:,29)','LineWidth',2)
plot(probs,SPDF_TOTAL(:,:,29)','--','LineWidth',2)
set(gca,'XGrid','On','YGrid','On')
xlabel('Spread')
ylabel('Error product probability')
legend('75% PROD.','50% PROD.','25% PROD.')
title('Error conditional probability, total. 168 hr. lead time.')


%FIGURAS PDF COMPARISON SH VS NH.
probs=( [prob_res:prob_res:100] + [0:prob_res:100-prob_res] )/2;
figure
subplot(1,3,1)
hold on
plot(probs,PDF_SHEMI(:,:,1)','LineWidth',2)
plot(probs,PDF_NHEMI(:,:,1)','--','LineWidth',2)
set(gca,'XGrid','On','YGrid','On')
xlabel('Spread')
ylabel('Error product probability')
legend('75% PROD.','50% PROD.','25% PROD.')
title('Error conditional probability, total sample. 00 hr. lead time.')
subplot(1,3,2)
hold on
plot(probs,PDF_SHEMI(:,:,16)','LineWidth',2)
plot(probs,PDF_NHEMI(:,:,16)','--','LineWidth',2)
set(gca,'XGrid','On','YGrid','On')
xlabel('Spread')
ylabel('Error product probability')
legend('75% PROD.','50% PROD.','25% PROD.')
title('Error conditional probability, total sample. 96 hr. lead time.')
subplot(1,3,3)
hold on
plot(probs,PDF_SHEMI(:,:,29)','LineWidth',2)
plot(probs,PDF_NHEMI(:,:,29)','--','LineWidth',2)
set(gca,'XGrid','On','YGrid','On')
xlabel('Spread')
ylabel('Error product probability')
legend('75% PROD.','50% PROD.','25% PROD.')
title('Error conditional probability, total. 168 hr. lead time.')

clear group
clear tmpmean tmpctrl tmpmemb tmpsprd
save(outfile)


