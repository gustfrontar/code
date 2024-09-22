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

filein='../RESULTS/egrr/ERRORSPREAD/ERROR_SPREAD_2007040112_2010040112.mat';
outfile='../RESULTS/egrr/ERRORSPREAD/COVERRORINDEX_2007040112_2010040112.mat';

load(filein)

group.minanom=[];
group.minlap=[];
group.uvel=[];
group.vvel=[];
group.trajid=[];
group.minlat=[];
group.minlon=[];

enssize=size(group.distlatpert,1);
flength=size(group.distlatpert,2);

%Calculo para todos los grupos.
pertlat_total=group.distlatpert;
pertlon_total=group.distlonpert;
meanerrorlon_total=group.meandistlonerror;
meanerrorlat_total=group.meandistlaterror;

coverrorideal_total=NaN(size(meanerrorlon_total));
coverror_total=NaN(size(meanerrorlon_total));
errorcovmatrix_total=NaN(2,2,flength,size(pertlat_total,3));

for ii=2:flength
   for jj=1:size(pertlat_total,3)
    tmplon=pertlon_total(:,ii,jj);
    tmplat=pertlat_total(:,ii,jj);
    tmplon=tmplon(~isnan(tmplon));
    tmplat=tmplat(~isnan(tmplat));
    if(numel(tmplon) >= enssize*EnsSizeThre ) 
        
    errorcovmatrix_total(:,:,ii,jj)=cov(tmplon,tmplat);
    [T,err] = cholcov(errorcovmatrix_total(:,:,ii,jj));
     if( err == 0)
     tmp=mvnrnd([0 0],squeeze(errorcovmatrix_total(:,:,ii,jj)),1);
     coverrorideal_total(ii,jj)=tmp(2)*tmp(1);
     end
     coverror_total(ii,jj)=meanerrorlon_total(ii,jj)*meanerrorlat_total(ii,jj);
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
coverrorideal_shemi=NaN(size(meanerrorlon_shemi));
errorcovmatrix_shemi=NaN(2,2,flength,size(pertlat_shemi,3));

for ii=2:flength
   for jj=1:size(pertlat_shemi,3)
    tmplon=pertlon_shemi(:,ii,jj);
    tmplat=pertlat_shemi(:,ii,jj);
    tmplon=tmplon(~isnan(tmplon));
    tmplat=tmplat(~isnan(tmplat));
    if(numel(tmplon) >= enssize*EnsSizeThre ) 
        
    errorcovmatrix_shemi(:,:,ii,jj)=cov(tmplon,tmplat);
    [T,err] = cholcov(errorcovmatrix_shemi(:,:,ii,jj));
     if( err == 0)
     tmp=mvnrnd([0 0],squeeze(errorcovmatrix_shemi(:,:,ii,jj)),1);
     coverrorideal_shemi(ii,jj)=tmp(2)*tmp(1);
     end
     coverror_shemi(ii,jj)=meanerrorlon_shemi(ii,jj)*meanerrorlat_shemi(ii,jj);
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
coverrorideal_nhemi=NaN(size(meanerrorlon_nhemi));
errorcovmatrix_nhemi=NaN(2,2,flength,size(pertlat_nhemi,3));

for ii=2:flength
   for jj=1:size(pertlat_nhemi,3)
    tmplon=pertlon_nhemi(:,ii,jj);
    tmplat=pertlat_nhemi(:,ii,jj);
    tmplon=tmplon(~isnan(tmplon));
    tmplat=tmplat(~isnan(tmplat));
    if(numel(tmplon) >= enssize*EnsSizeThre ) 
        
    errorcovmatrix_nhemi(:,:,ii,jj)=cov(tmplon,tmplat);
    [T,err] = cholcov(errorcovmatrix_nhemi(:,:,ii,jj));
     if( err == 0)
     tmp=mvnrnd([0 0],squeeze(errorcovmatrix_nhemi(:,:,ii,jj)),1);
     coverrorideal_nhemi(ii,jj)=tmp(2)*tmp(1);
     end
     coverror_nhemi(ii,jj)=meanerrorlon_nhemi(ii,jj)*meanerrorlat_nhemi(ii,jj);
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

[ERRORCV_TOTAL(:,ii) COVV_TOTAL(:,ii) R_TOTAL(ii)  SLOPE_TOTAL(ii)]=covariance_consistency_fun(squeeze(errorcovmatrix_total(1,2,ii,:))',meanerrorlon_total(ii,:),meanerrorlat_total(ii,:),20);
[ERRORCV_SHEMI(:,ii) COVV_SHEMI(:,ii) R_SHEMI(ii)  SLOPE_SHEMI(ii)]=covariance_consistency_fun(squeeze(errorcovmatrix_shemi(1,2,ii,:))',meanerrorlon_shemi(ii,:),meanerrorlat_shemi(ii,:),20);
[ERRORCV_NHEMI(:,ii) COVV_NHEMI(:,ii) R_NHEMI(ii)  SLOPE_NHEMI(ii)]=covariance_consistency_fun(squeeze(errorcovmatrix_nhemi(1,2,ii,:))',meanerrorlon_nhemi(ii,:),meanerrorlat_nhemi(ii,:),20);

end


%VERIFICAR PORQUE EN DETERMINADAS CATEGORIAS NO TENGO VALORES, ENMASCARAR
%LOS CASOS DONDE EL NUMERO DE MIEMBROS EN EL ENSAMBLE ES MENOR QUE UN
%DETERMINADO UMBRAL PARA SER CONSISTENTES CON LOS OTROS EXPERIMENTOS Y
%ANALISIS.

horas=((1:29)-1)*6;


%FIGURA BETA COMO FUNCION DEL TIEMPO DERIVADA DEL ENSAMBLE.
tmp=squeeze((errorcovmatrix_total(1,2,:,:)));
tmp(tmp==0)=NaN;
BETA_TOTAL=nanstd(tmp,[],2);
MEANS_TOTAL=nanmean(tmp,2);

figure
plot(BETA_TOTAL,'LineWidth',2);
hold on
plot(MEANS_TOTAL,'--','LineWidth',2);
set(gca,'XGrid','On','YGrid','On')
title('Beta as a function of time')
xlabel('Lead time')
ylabel('Beta')


%FIGURA IRSS PARA EL TOTAL
figure
hold on
plot(horas(2:end),IRSS_TOTAL(:,2:end)','LineWidth',2)
plot(horas(2:end),SIRSS_TOTAL(:,2:end)','--','LineWidth',2)
legend('Small cov.','Medium cov.','Large cov.')
xlabel('Lead time')
ylabel('IRSS')
title('IRSS for the total sample')
set(gca,'XGrid','On','YGrid','On')

%FIGURAS HEMISFERIO NORTE VS HEMISFERIO SUR.
figure
hold on
plot(horas(2:end),IRSS_SHEMI(:,2:end)','LineWidth',2)
plot(horas(2:end),IRSS_NHEMI(:,2:end)','--','LineWidth',2)
xlabel('Lead time')
ylabel('IRSS')
legend('75% PROD. SH','50% PROD. SH','25% PROD. SH','75% PROD. NH','50% PROD. NH','25% PROD. NH')
title('IRSS by hemisphere')


%FIGURAS PDF EJEMPLOS (3 TIEMPOS). MUESTRA TOTAL
probs=( [prob_res:prob_res:100] + [0:prob_res:100-prob_res] )/2;
figure
subplot(1,3,1)
hold on
plot(probs,PDF_TOTAL(:,:,2)','LineWidth',2)
plot(probs,SPDF_TOTAL(:,:,2)','--','LineWidth',2)
legend('Small PROD.','Medium PROD.','Large PROD.')
set(gca,'XGrid','On','YGrid','On')
xlabel('Spread')
ylabel('Error product conditional probability')
title('Error conditional probability, total sample. 00 hr. lead time.')
subplot(1,3,2)
hold on
plot(probs,PDF_TOTAL(:,:,16)','LineWidth',2)
plot(probs,SPDF_TOTAL(:,:,16)','--','LineWidth',2)
set(gca,'XGrid','On','YGrid','On')
xlabel('Spread')
ylabel('Error product conditional probability')
%legend('75% PROD.','50% PROD.','25% PROD.')
title('Error conditional probability, total sample. 96 hr. lead time.')
subplot(1,3,3)
hold on
plot(probs,PDF_TOTAL(:,:,29)','LineWidth',2)
plot(probs,SPDF_TOTAL(:,:,29)','--','LineWidth',2)
set(gca,'XGrid','On','YGrid','On')
xlabel('Spread')
ylabel('Error product conditional probability')
%legend('75% PROD.','50% PROD.','25% PROD.')
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
clear pertlat_total pertlat_shemi pertlat_nhemi 
clear pertlon_total pertlon_shemi pertlon_nhemi
clear meanerrorlon_total meanerrorlat_total meanerrorlon_shemi meanerrorlat_shemi
clear meanerrorlat_nhemi meanerrorlon_nhemi
clear errorcovmatrix_total errorcovmatrix_nhemi errorcovmatrix_shemi
clear meanerrorlonideal_nhemi meanerrorlatideal_nhemi 
clear meanerrorlonideal_shemi meanerrorlatideal_shemi
clear coverror_nhemi coverror_shemi coverror_total
clear coverrorideal_nhemi coverrorideal_shemi coverrorideal_total

save(outfile)


