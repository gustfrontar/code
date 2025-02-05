function []=plot_all_stations(OBS,FORECAST,RMSE,RMSEDB,MSESS,BIAS,ESTACIONES,FECHAS,PNG_PATH)


%Generamos las legendas para los graficos.
LHORA{1}='00UTC';LHORA{2}='06UTC';LHORA{3}='12UTC';LHORA{4}='18UTC';

LFORECAST{1,1}='F00';LFORECAST{1,2}='F12';LFORECAST{1,3}='F24';LFORECAST{1,4}='F36';LFORECAST{1,5}='F48';
LFORECAST{2,1}='F06';LFORECAST{2,2}='F18';LFORECAST{2,3}='F30';LFORECAST{2,4}='F42';LFORECAST{2,5}='';
LFORECAST{3,1}='F00';LFORECAST{3,2}='F12';LFORECAST{3,3}='F24';LFORECAST{3,4}='F36';LFORECAST{3,5}='F48';
LFORECAST{4,1}='F06';LFORECAST{4,2}='F18';LFORECAST{4,3}='F30';LFORECAST{4,4}='F42';LFORECAST{4,5}='';

var{1}='Temperatura';var{2}='Td';var{3}='U';var{4}='V';var{5}='PNM';var{6}='VEL';var{7}='DIR';

[nest ntimes nhoras nvariables]=size(OBS);

 for ivar=1:7 %Por ahora grafico solo hasta Td mas adelante hay que ampliar a otras variables.
 for est=1:nest
     
 %Recorto las variables para quedarme solo con la de la estacion
 %correspondiente.
 plot_obs=squeeze(OBS(est,:,:,ivar));
 plot_for=squeeze(FORECAST(est,:,:,:,ivar));
 plot_estacion=num2str(ESTACIONES(est,1));
 plot_rmse=squeeze(RMSE(est,:,:,ivar));
 plot_bias=squeeze(BIAS(est,:,:,ivar));
 plot_rmsedb=squeeze(RMSEDB(est,:,:,ivar));
 plot_msess=squeeze(MSESS(est,:,:,ivar));
 
% %Llamo a las funciones que hacen los graficos.
 plot_estation_fun(plot_obs,plot_for,FECHAS,PNG_PATH,var{ivar},plot_estacion,LFORECAST,LHORA);
 plot_rmsebias_fun(plot_rmse,plot_rmsedb,plot_msess,plot_bias,FECHAS,PNG_PATH,var{ivar},plot_estacion,LFORECAST,LHORA);
% 
 end
 end

for est=1:nest
    
%Recorto las variables para quedarme solo con la de la estacion
%correspondiente para los graficos de las series de diferentes pronosticos.
plot_obs=squeeze(OBS(est,:,:,:));
plot_for=squeeze(FORECAST(est,:,:,:,:));
plot_estacion=num2str(ESTACIONES(est,1));

%Llamo a las funciones que hacen los graficos.
plot_forecasts_fun(plot_obs,plot_for,FECHAS,PNG_PATH,plot_estacion)

end
