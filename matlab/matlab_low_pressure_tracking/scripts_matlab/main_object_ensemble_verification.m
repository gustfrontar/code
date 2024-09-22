
clear all
close all

%La idea de este script es aplicar verificacion orientada a objetos a los
%pronosticos por ensambles. Definir spread no en el punto de reticula sino
%en la posicion e intensidad de los sistemas. 
%Calcular direcciones principales para el spread y verificar si se cumplen,
%etc.


NATURE_PATH='../DATA/nature/'      %Path to climatology data.
INI_DATE_CLIM='1982010112';        %Ini date for climatology.
END_DATE_CLIM='1982033100';        %End date for climatology.

ENSEMBLE_PATH='../DATA/test/ensemble/';  
INI_DATE_EXP='1982011512';         %Ini date for experiment.
END_DATE_EXP='1982033118';         %End date for experiment.
INTERVAL=6;    
NMEMBERS=20;
RANGE='F48';                       %Forecast range.
geo_treshold=-1.5;
lap_treshold=0.1;

nvars=29; 
nx_s=96;
ny_s=48;
nz_s=7;

%SPEEDY GRID.
lat_s=[-87.159 -83.479 -79.777 -76.070 -72.362 -68.652 -64.942 -61.232 -57.521 ...
      -53.810 -50.099 -46.389 -42.678 -38.967 -35.256 -31.545 -27.833 -24.122  ...
      -20.411 -16.700 -12.989  -9.278  -5.567  -1.856   1.856   5.567   9.278  ...
      12.989  16.700  20.411  24.122  27.833  31.545  35.256  38.967  42.678   ...
      46.389  50.099  53.810  57.521  61.232  64.942  68.652  72.362  76.070   ...
      79.777  83.479  87.159];
  
lon_s=0:3.75:360-3.75;

%Calculamos la climatologia del periodo para poder calcular anomalias y en
%base a eso obtener los objetos.
[clim_mean clim_std]=compute_climatology_fun(INI_DATE_CLIM,END_DATE_CLIM,NATURE_PATH);


%Vamos a comentar el ciclo sobre los dias del experimento.

info_systems.size=[];
info_systems.lat=[];
info_systems.lon=[];
info_systems.mingeo=[];
info_systems.meangeo=[];
info_systems.ref_lat=[];
info_systems.ref_lon=[];
info_systems.ref_size=[];
info_systems.isfor=[];
info_systems.isobs=[];



C_DATE=datenum(INI_DATE_EXP,'yyyymmddHH');
E_DATE=datenum(END_DATE_EXP,'yyyymmddHH');

while ( C_DATE <= E_DATE )
% 1) Read the ensemble and compute standarized anomaly.
%    Store the ensemble in a temporary array tmpensemble.

    TODAY=datestr(C_DATE,'yyyymmddHH');
    
    [tmpensemble]=read_ensemble(ENSEMBLE_PATH,NMEMBERS,TODAY,RANGE);
    [tmpobs]=read_single(NATURE_PATH,TODAY);
    
    
%  2) Compute anomalies for surface pressure and compute vorticity.
%     Smooth both fields.

    obs_geo=(tmpobs(:,:,32)-clim_mean(:,:,32))./clim_std(:,:,32);
    [auxx auxy]=gradient(obs_geo);
    obs_lap=divergence(auxx,auxy);
    
    for iens=1:NMEMBERS
       ens_geo(:,:,iens)=(tmpensemble(:,:,32,iens)-clim_mean(:,:,32))./clim_std(:,:,32); 
       [auxx auxy]=gradient(ens_geo(:,:,iens));
       ens_lap(:,:,iens)=divergence(auxx,auxy);
       
    end
    ens_geo=fast_filter_fun(ens_geo,5);
    ens_lap=fast_filter_fun(ens_lap,7);
    obs_lap=fast_filter_fun(obs_lap,7);
    obs_geo=fast_filter_fun(obs_geo,5);
    
    
%   3) Compute the mask using surface pressure and vorticity.

    mask=zeros(ny_s,nx_s,NMEMBERS);
    masko=zeros(ny_s,nx_s);
    mask( ens_geo < geo_treshold & ens_lap > lap_treshold)=1;
    masko( obs_geo < geo_treshold & obs_lap > lap_treshold)=1;
    %mask( ens_geo.*(abs( lap_treshold )) < geo_treshold )=1;
    
%   4) Identificamos los sistemas presentes en cada miembro del ensamble.    

    mask_2=NaN(size(mask));
    masko_2=NaN(size(masko));
    for iens=1:NMEMBERS
    [mask_2(:,:,iens)]=identifica_sistema_fun(mask(:,:,iens));
    end
    masko_2=identifica_sistema_fun(masko);
    
    [today_systems]=caracteriza_sistema_fun(mask_2,masko_2,lat_s,lon_s,ens_geo,obs_geo);
    
    info_systems.size=[info_systems.size;today_systems.size];
    info_systems.lat=[info_systems.lat;today_systems.lat];
    info_systems.lon=[info_systems.lon;today_systems.lon];
    info_systems.mingeo=[info_systems.mingeo;today_systems.mingeo];
    info_systems.meangeo=[info_systems.meangeo;today_systems.meangeo];
    info_systems.ref_lat=[info_systems.ref_lat;today_systems.ref_lat'];
    info_systems.ref_lon=[info_systems.ref_lon;today_systems.ref_lon'];
    info_systems.ref_size=[info_systems.ref_size;today_systems.ref_size'];
    info_systems.isfor=[info_systems.isfor;today_systems.isfor];
    info_systems.isobs=[info_systems.isobs;today_systems.isobs];

    C_DATE=C_DATE+INTERVAL/24;
end

save('sistemas.mat','info_systems');



%Compute reliability for test.

prob=mean(info_systems.isfor,2);

[reliability,n_forecast,prob_int] = reliability_fun(info_systems.isobs,prob,0.5,0.1);
aux=length(reliability);

hold on
plot(0.05:0.1:1,reliability(2:end),'o')
plot([0 1 ],[0 1],'k--')


%Compute relationship between error and spread for sistem lat.

aux_lat=info_systems.lat(prob > 0.5,:);
aux_ref_lat=info_systems.ref_lat(prob> 0.5,:);
mean_lat=nanmean(aux_lat,2);
sprd_lat=nanstd(aux_lat,[],2);
error_lat=abs(mean_lat-aux_ref_lat);
corrcoef(sprd_lat,error_lat)

error=(error_lat-mean(error_lat))/std(error_lat);

spread=(sprd_lat-mean(sprd_lat))/std(sprd_lat);
    
k=25; %Porcentaje para el spread,
kerror=25;  %Porcentaje para determinar los umbrales de error (los cuartiles de la distribucion).
          
clear rango_spread    

s_error=sort(spread(isnan(spread)==0));
n1=length(s_error);
jk=1;
for ik=k:k:100-k;
   naux=round(ik*n1/(100));
   rango_spread(jk)=s_error(naux);
   jk=jk+1;
end
length_spread=jk;
rango_spread=[-1e10 rango_spread 1e10];
clear s_error;

s_error=sort(error(isnan(error)==0));
n1=length(s_error);
jk=1;
for ik=kerror:kerror:100-kerror;
 perror(jk,:)=ik/100*ones(1,length_spread); %Construyo la matriz que tiene la probabilidad climatologica de los errores.
 naux=round(ik*n1/(100));
 umbral_error(jk)=s_error(naux);
 jk=jk+1;
end
clear s_error;
perror=flipdim(perror,1);


pu=[k:k:100]'/100;
pi=[0:k:100-k]'/100;
pe=[kerror:kerror:100-kerror]'/100; 
nk=length(pu);
ne=length(pe);
for ii=1:ne
    for jj=1:nk 
      if( pu(jj) <= pe(ii)) 
      pperfect(ii,jj)=0;
      end
      if( pi(jj) >= pe(ii))
      pperfect(ii,jj)=1;
      end
      if( pi(jj) < pe(ii) & pu(jj) > pe(ii))
      pperfect(ii,jj)=(pe(ii)-pu(jj))/(pu(jj)-pi(jj)) 
      end
      
    end
end
index_p=sum((k/100)*(perror-pperfect).^2,2); %Este es el valor de indice para el error climatologico


[p n nada] = pre_cal_fun3(error,spread,umbral_error,0,rango_spread);
index_independiente=sum((k/100)*(p-pperfect).^2,2);
%index_norm_independiente=1-squeeze(index_independiente)./index_p';   





