clear all
close all

%En este script la idea es verificar las covarianzas de los errores en
%latitud y longitud. En algunos casos, existiran correlaciones entre dichos
%errores en los diferentes miembros del ensamble indicando tal vez una
%tendencia a que exista una direccion preferencial en la incertidumbre de
%la posicion de un dado sistema. La idea es intentar ver si en la practica
%se verifica dicha correlacion.
%Para eso vamos a considerar casos en donde la correlacion entre los
%errores es alta (muy positiva) y casos en donde es baja (muy negativa) y
%para cada una de esas situaciones vamos a calcular las correlaciones en
%los errores para ver si tambien resultan positivas o negativas. Para
%determinar que es muy positivo y que es muy negativo vamos a tomar la
%distribucion de la correlacion entre los errores.

addpath('../common_functions_vpo/');
EnsSizeThre=0.5;    %Ensambles mas chicos que este porcentaje del total no van a ser tenidos en cuenta en el calculo.

load '../RESULTS/kwbc/ERRORSPREAD/ERROR_SPREAD_2007040112_2010040112.mat'


enssize=size(group.minlon,1);
flength=size(group.minlon,2);

%for ii=1:size(group.distlonspread,2)
%group.distlonspreadsmooth(:,ii)=smooth(group.distlonspread(:,ii),9);
%end


%Calculo para todos los grupos.
pertlat_total=group.distlatpert;
pertlon_total=group.distlonpert;
meanerrorlon_total=group.meandistlonerror;
meanerrorlat_total=group.meandistlaterror;

%Calculo para hemisferios norte y sur.
index=group.shemi==1;
pertlat_shemi=group.distlatpert(:,:,index);
pertlon_shemi=group.distlonpert(:,:,index);
meanerrorlon_shemi=group.meandistlonerror(:,index);
meanerrorlat_shemi=group.meandistlaterror(:,index);

index=group.nhemi==1;
pertlat_nhemi=group.distlatpert(:,:,index);
pertlon_nhemi=group.distlonpert(:,:,index);
meanerrorlon_nhemi=group.meandistlonerror(:,index);
meanerrorlat_nhemi=group.meandistlaterror(:,index);

%Calculo para estacion calida y fria.
index=group.calida==1;
pertlat_calida=group.distlatpert(:,:,index);
pertlon_calida=group.distlonpert(:,:,index);
meanerrorlon_calida=group.meandistlonerror(:,index);
meanerrorlat_calida=group.meandistlaterror(:,index);

index=group.fria==1;
pertlat_fria=group.distlatpert(:,:,index);
pertlon_fria=group.distlonpert(:,:,index);
meanerrorlon_fria=group.meandistlonerror(:,index);
meanerrorlat_fria=group.meandistlaterror(:,index);

[rerr_total rerrideal_total tercilesr_total]=covariance_verif_fun(pertlon_total,pertlat_total,meanerrorlon_total,meanerrorlat_total);
[rerr_shemi rerrideal_shemi tercilesr_shemi]=covariance_verif_fun(pertlon_shemi,pertlat_shemi,meanerrorlon_shemi,meanerrorlat_shemi);
[rerr_nhemi rerrideal_nhemi tercilesr_nhemi]=covariance_verif_fun(pertlon_nhemi,pertlat_nhemi,meanerrorlon_nhemi,meanerrorlat_nhemi);
[rerr_calida rerrideal_calida tercilesr_calida]=covariance_verif_fun(pertlon_calida,pertlat_calida,meanerrorlon_calida,meanerrorlat_calida);
[rerr_fria rerrideal_fria tercilesr_fria]=covariance_verif_fun(pertlon_fria,pertlat_fria,meanerrorlon_fria,meanerrorlat_fria);

%PLOTS.
horas=((1:29)-1)*6;

figure
hold on
plot(horas,rerr_total,'LineWidth',2)
plot(horas,rerrideal_total,'--','LineWidth',2)
title('Error correlation (lon/lat) as a function of the ensemble covariance (Full sample)')
set(gca,'YGrid','On')
legend('1st tercile','second tercile','third tercile')

figure
hold on
plot(horas,rerr_shemi,'LineWidth',2)
plot(horas,rerr_nhemi,'--','LineWidth',2)
title('Error correlation (lon/lat) as a function of the ensemble covariance (HN and HS)')
set(gca,'YGrid','On')
legend('1st tercile','second tercile','third tercile')


