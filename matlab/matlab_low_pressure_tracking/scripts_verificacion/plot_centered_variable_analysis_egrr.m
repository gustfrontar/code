clear all
close all

%EN ESTE SCRIPT VAMOS A HACER UN ANALYSIS DE LA RELACION SPREAD ERROR
%USANDO LA VARIABLE CENTRADA. PRIMERO EL ANALYSIS TRADICIONAL PARA TESTEAR
%LA RELIABILITY (QUE VA A INDICAR UN UNDERDISPERSION DEL ENSAMBLE). LUEGO
%UN ANALYSIS POR NIVELES, ES DECIR PARA DIFERENTES VALORES DEL SPREAD
%CALCULAR LA VARIABLE CENTRADA Y VER QUE PASA. SI EXISTE RELACION ENTRE EL
%SPREAD Y EL ERROR, LA VARIANZA Y LA MEDIA DE LA VARIABLE CENTRADA NO
%DEBERIAN SER FUNCION DEL SPREAD.

addpath('../common_functions_vpo/');
EnsSizeThre=0.5;    %Ensambles mas chicos que este porcentaje del total no van a ser tenidos en cuenta en el calculo.

filein='../RESULTS/egrr/ERRORSPREAD/ERROR_SPREAD_2007040112_2010040112.mat';
outfile='../RESULTS/egrr/ERRORSPREAD/RCRV_2007040112_2010040112.mat';

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

%for ii=1:size(group.distlonspread,2)
%group.distlonspreadsmooth(:,ii)=smooth(group.distlonspread(:,ii),9);
%end


%Calculo para todos los grupos.

distlatspread_total=group.distlatspread;
distlonspread_total=group.distlonspread;
meanerrorlon_total=group.meandistlonerror;
meanerrorlat_total=group.meandistlaterror;
nummember_total=group.nummembers;

%Calculo para hemisferios norte y sur.
index=group.shemi==1;
distlatspread_shemi=group.distlatspread(:,index);
distlonspread_shemi=group.distlonspread(:,index);
meanerrorlon_shemi=group.meandistlonerror(:,index);
meanerrorlat_shemi=group.meandistlaterror(:,index);
nummember_shemi=group.nummembers(:,index);

index=group.nhemi==1;
distlatspread_nhemi=group.distlatspread(:,index);
distlonspread_nhemi=group.distlonspread(:,index);
meanerrorlon_nhemi=group.meandistlonerror(:,index);
meanerrorlat_nhemi=group.meandistlaterror(:,index);
nummember_nhemi=group.nummembers(:,index);

%Calculo para estacion calida y fria.
index=group.calida==1;
distlatspread_calida=group.distlatspread(:,index);
distlonspread_calida=group.distlonspread(:,index);
meanerrorlon_calida=group.meandistlonerror(:,index);
meanerrorlat_calida=group.meandistlaterror(:,index);
nummember_calida=group.nummembers(:,index);

index=group.fria==1;
distlatspread_fria=group.distlatspread(:,index);
distlonspread_fria=group.distlonspread(:,index);
meanerrorlon_fria=group.meandistlonerror(:,index);
meanerrorlat_fria=group.meandistlaterror(:,index);
nummember_fria=group.nummembers(:,index);

for ii=1:size(group.distlatspread,1)
[cent_meanlat_total(ii,:) cent_sprdlat_total(ii,:) full_meanlat_total(ii) full_sprdlat_total(ii)]=centered_variable_analysis_fun(distlatspread_total(ii,:),meanerrorlat_total(ii,:),nummember_total(ii,:));
[cent_meanlat_shemi(ii,:) cent_sprdlat_shemi(ii,:) full_meanlat_shemi(ii) full_sprdlat_shemi(ii)]=centered_variable_analysis_fun(distlatspread_shemi(ii,:),meanerrorlat_shemi(ii,:),nummember_shemi(ii,:));
[cent_meanlat_nhemi(ii,:) cent_sprdlat_nhemi(ii,:) full_meanlat_nhemi(ii) full_sprdlat_nhmei(ii)]=centered_variable_analysis_fun(distlatspread_nhemi(ii,:),meanerrorlat_nhemi(ii,:),nummember_nhemi(ii,:));
[cent_meanlat_calida(ii,:) cent_sprdlat_calida(ii,:) full_meanlat_calida(ii) full_sprdlat_calida(ii)]=centered_variable_analysis_fun(distlatspread_calida(ii,:),meanerrorlat_calida(ii,:),nummember_calida(ii,:));
[cent_meanlat_fria(ii,:) cent_sprdlat_fria(ii,:) full_meanlat_fria(ii) full_sprdlat_fria(ii)]=centered_variable_analysis_fun(distlatspread_fria(ii,:),meanerrorlat_fria(ii,:),nummember_fria(ii,:));

[cent_meanlon_total(ii,:) cent_sprdlon_total(ii,:) full_meanlon_total(ii) full_sprdlon_total(ii)]=centered_variable_analysis_fun(distlonspread_total(ii,:),meanerrorlon_total(ii,:),nummember_total(ii,:));
[cent_meanlon_shemi(ii,:) cent_sprdlon_shemi(ii,:) full_meanlon_shemi(ii) full_sprdlon_shemi(ii)]=centered_variable_analysis_fun(distlonspread_shemi(ii,:),meanerrorlon_shemi(ii,:),nummember_shemi(ii,:));
[cent_meanlon_nhemi(ii,:) cent_sprdlon_nhemi(ii,:) full_meanlon_nhemi(ii) full_sprdlon_nhmei(ii)]=centered_variable_analysis_fun(distlonspread_nhemi(ii,:),meanerrorlon_nhemi(ii,:),nummember_nhemi(ii,:));
[cent_meanlon_calida(ii,:) cent_sprdlon_calida(ii,:) full_meanlon_calida(ii) full_sprdlon_calida(ii)]=centered_variable_analysis_fun(distlonspread_calida(ii,:),meanerrorlon_calida(ii,:),nummember_calida(ii,:));
[cent_meanlon_fria(ii,:) cent_sprdlon_fria(ii,:) full_meanlon_fria(ii) full_sprdlon_fria(ii)]=centered_variable_analysis_fun(distlonspread_fria(ii,:),meanerrorlon_fria(ii,:),nummember_fria(ii,:));

end

%==========================================================================
%PLOTS

%CENTERED VARIABLE MEAN AS A FUNCTION OF THE ENSEMBLE SPREAD.
%LAT
horas=((1:29)-1)*6;
figure
subplot(1,2,1)
plot(horas,cent_meanlat_total,'LineWidth',2)
hold on
plot(horas,full_meanlat_total,'k--','LineWidth',2)
axis([0 29 -1.5 1.5])
set(gca,'XGrid','On','YGrid','On')
legend('Low Spread','Medium Spread','High Spread');
xlabel('Lead time')
ylabel('Mean')
title('Latitude RCRV mean by spread categories (total sample)')
subplot(1,2,2)
plot(horas,cent_meanlon_total,'LineWidth',2)
hold on
plot(horas,full_meanlon_total,'k--','LineWidth',2)
axis([0 29 -1.5 1.5])
set(gca,'XGrid','On','YGrid','On')
legend('Low Spread','Medium Spread','High Spread');
xlabel('Lead time')
ylabel('Mean')
title('Longitude RCRV mean by spread categories (total sample)')

figure
subplot(1,2,1)
plot(horas,cent_meanlat_shemi,'LineWidth',2)
hold on
plot(horas,full_meanlat_nhemi,'k--','LineWidth',2)
axis([0 29 -1.5 1.5])
set(gca,'XGrid','On','YGrid','On')
legend('SH Low Spread','SH Medium Spread','SH High Spread','NH Low Spread','NH Medium Spread','NH High Spread');
xlabel('Lead time')
ylabel('Mean')
title('Latitude RCRV mean by spread categories')
subplot(1,2,2)
plot(horas,cent_meanlon_shemi,'LineWidth',2)
hold on
plot(horas,full_meanlon_nhemi,'k--','LineWidth',2)
axis([0 29 -1.5 1.5])
set(gca,'XGrid','On','YGrid','On')
xlabel('Lead time')
ylabel('Mean')
legend('SH Low Spread','SH Medium Spread','SH High Spread','NH Low Spread','NH Medium Spread','NH High Spread');
title('Longitude RCRV mean by spread categories')


%CENTERED VARIABLE MEAN AS A FUNCTION OF THE ENSEMBLE SPREAD.

%LAT
figure
subplot(1,2,1)
plot(cent_sprdlat_total,'LineWidth',2)
hold on
plot(full_sprdlat_total,'k--','LineWidth',2)
axis([0 29 0 6])
set(gca,'XGrid','On','YGrid','On')
legend('Low Spread','Medium Spread','High Spread');
xlabel('Lead time')
ylabel('Spread')
title('Latitude Centered variable spread by spread categories (total sample)')
subplot(1,2,2)
plot(cent_sprdlon_total,'LineWidth',2)
hold on
plot(full_sprdlon_total,'k--','LineWidth',2)
axis([0 29 0 6])
set(gca,'XGrid','On','YGrid','On')
xlabel('Lead time')
ylabel('Spread')
legend('Low Spread','Medium Spread','High Spread');
title('Longitude Centered variable mean by spread categories (total sample)')

figure
subplot(1,2,1)
plot(cent_sprdlat_shemi,'LineWidth',2)
hold on
plot(cent_sprdlat_nhemi,'--','LineWidth',2)
axis([0 29 0 6])
set(gca,'XGrid','On','YGrid','On')
xlabel('Lead time')
ylabel('Spread')
legend('SH Low Spread','SH Medium Spread','SH High Spread','NH Low Spread','NH Medium Spread','NH High Spread');
title('Latitude Centered variable spread by spread categories')
subplot(1,2,2)
plot(cent_sprdlon_shemi,'LineWidth',2)
hold on
plot(cent_sprdlon_nhemi,'--','LineWidth',2)
axis([0 29 0 6])
set(gca,'XGrid','On','YGrid','On')
xlabel('Lead time')
ylabel('Spread')
legend('SH Low Spread','SH Medium Spread','SH High Spread','NH Low Spread','NH Medium Spread','NH High Spread');
title('Longitude Centered variable mean by spread categories')

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


