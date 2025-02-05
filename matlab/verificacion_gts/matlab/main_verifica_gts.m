%Script para realizar la verificacion del WRF con GTS.

%Estos parametros que estan comentados deberian definirse antes de ejecutar este script.
%ini_date='25-Oct-2009';
%end_date='02-Nov-2009';
%data_path='/home/wrf/datos/synop/'
%wrf_path='/home/wrf/archivo/surface/'
%fig_path='../png/'
%dominio_path='/home/wrf/datos/dominio/dominio.dat'
%nx_w=149;
%ny_w=299;
%===========================================================================================
% El resto del script.

%Obtengo los datos del dominio WRF.
display 'Leyendo el dominio del WRF'
tic
[lat_wrf lon_wrf hgt_wrf mask_wrf]=get_wrf_domain_fun(dominio_path,nx_w,ny_w,'b');
toc

%Leo las estaciones, las ordeno y las guardo en un array con las siguientes
%dimensiones: OBS(estacion,fecha,hora del dia,variable)
display 'Leyendo datos GTS OGIMET'
tic
[OBS ESTACIONES FECHAS]=get_obs_fun(ini_date,end_date,data_path);
toc

%Corrigo la posicion de la estacion Ezeiza.
aux=find(ESTACIONES(:,1)==87576);
if(~isempty(aux))
ESTACIONES(aux,2)=-34.8;
ESTACIONES(aux,3)=-58.5;
end


%Para las estaciones que lei obtengo el  punto mas cercano de la reticula
%del wrf (y un array logico que me dice de todas las estaciones que le pase
%cuales estan dentro del dominio del wrf).
display 'Leyendo datos del WRF'
tic
[FORECAST ESTACIONES_UTILES]=wrf_to_station_fun(ESTACIONES,FECHAS,wrf_path,lat_wrf,lon_wrf,mask_wrf);
toc

%Filtro del total de estaciones solo las que quedaron dentro del dominio
%del WRF. Filtro tambien en array ESTACIONES que tiene la info de cada
%estacion.
OBS=OBS(ESTACIONES_UTILES,:,:,:); 
ESTACIONES=ESTACIONES(ESTACIONES_UTILES,:); 

%Calculamos el RMSE y BIAS.
display 'Calculando RMSE y BIAS'
tic
[BIAS RMSE RMSEDB MSENUL MSESS]=rmse_gts_fun(OBS,FORECAST);
toc

%Graficamos las marchas para todas las estaciones.
tic
plot_all_stations_fun(OBS,FORECAST,RMSE,RMSEDB,MSESS,BIAS,ESTACIONES,FECHAS,fig_path);
toc


% %Ploteo la posicion de las estaciones
% pcolor(lon_wrf,lat_wrf,hgt_wrf)
% shading flat
% hold on
% plot(lon_est,lat_est,'ko')
% plot(lon_est(estaciones_utiles),lat_est(estaciones_utiles),'ro')

