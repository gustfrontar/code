
clear all
close all

%La idea de este script es calcular trayectorias de objetos, esto despues
%se puede usar para hacer una verificacion 4D.


NATURE_PATH='../DATA/nature/';     %Path to climatology data.
INI_DATE_CLIM='1982010112';        %Ini date for climatology.
END_DATE_CLIM='1982033100';        %End date for climatology.

EXP_PATH='../DATA/nature/';        %Path to forecast data.
INI_DATE_EXP='1982010100';         %Ini date for experiment.
END_DATE_EXP='1982013000';         %End date for experiment.
INTERVAL=6; 

RANGE=[];

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

[lon lat]=meshgrid(lon_s,lat_s);

%Calculamos la climatologia del periodo para poder calcular anomalias y en
%base a eso obtener los objetos.
[clim_mean clim_std]=compute_climatology_fun(INI_DATE_CLIM,END_DATE_CLIM,NATURE_PATH);

%Vamos a leer el periodo experimental sobre el cual se calcularan las
%trayectorias.

C_DATE=datenum(INI_DATE_EXP,'yyyymmddHH');
I_DATE=C_DATE;
E_DATE=datenum(END_DATE_EXP,'yyyymmddHH');

ntimes=(E_DATE-I_DATE)*24/INTERVAL + 1;

g500_unfilt=NaN(size(lat,1),size(lat,2),ntimes);
g500_filt=NaN(size(lat,1),size(lat,2),ntimes);

%Este loop es solo para leer los datos de prueba
while ( C_DATE <= E_DATE )
% 1) Read the ensemble and compute standarized anomaly.
%    Store the ensemble in a temporary array tmpensemble.
    
    itime=(C_DATE-I_DATE)*24/INTERVAL+1;
    
    times(itime)=C_DATE;
    TODAY=datestr(C_DATE,'yyyymmddHH');
    
    [tmpfield]=read_speedy(EXP_PATH,TODAY);

   
%  2) Compute anomalies for surface pressure and compute vorticity.
%     Smooth both fields.

    anom=(tmpfield(:,:,32)-clim_mean(:,:,32))./clim_std(:,:,32);

    g500_unfilt(:,:,itime)=anom;
    %Habria que buscar un filtro mas sofisticado que filtrar en funcion de
    %distancias y no de puntos de reticula, porque de esa manera nos
    %quedariamos con las mismas escalas espaciales, sobre todo para
    %dominios globales. (buscar filtros gaussianos... tal vez recursivos)
    g500_filt(:,:,itime)=fast_filter_fun(anom,5);

    
    C_DATE=C_DATE+INTERVAL/24;
end

% Elementos que deben estar presentes en la estructura input...
% Data: un array tridimensional con anomalias estandarizadas y filtradas.
% Data_unfilt: un array cuatridimensional (lat,lon,tiempo,variables) donde 
% esta el valor de las variables que van a ser promediadas y donde se
% buscaran los extremos para cada sistema. Aca irian los campos no
% filtrados de las variables.
% lat: Un array 2D con las latitudes.
% lon: Un array 2D con las longitudes.
% times: Un vector con las fechas (en formato numero de matlab).

input.Data=g500_filt;
input.Data_unfilt=g500_unfilt;
input.lat=lat;
input.lon=lon;
input.times=times;

config=[]; %Uso la configuracion por defecto.

[trayectorias]=calc_trayectory_fun(input,config);

figure
for i=1:length(trayectorias)
   
   hold on
   plot(trayectorias(i).lon,trayectorias(i).lat,'o-')
   axis([0 360 -90 90])
end


