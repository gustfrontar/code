function [BIAS RMSE RMSEDB MSENUL MSESS]=rmse_gts_fun(OBS,FORECAST)
%*************************************************************************
% A esta funcion llegan 3 arrays con las siguientes estructuras:
% OBS(estaciones, dia, hora del dia, variables)
% FORECAST(estaciones,dia,hora del dia,plazo de pronostico,variables)
% ESTACIONES(estaciones,data) n estacion, lat, lon, altura estacion.
%*************************************************************************
% Juan Ruiz - 2009


[nest ntiempos nhoras nforecast nvar]=size(FORECAST);

%Calculo cuantos datos tengo para cada estacion y hora del dia en el
%periodo completo.
NOBS=squeeze(sum(isnan(OBS)==0,2));
STDOBS=squeeze(nanstd(OBS,[],2));

%Vamos a calcular el RMSE y el bias para todas las estaciones, plazos de
%pronostico y horas del dia en forma independiente.
  
STDFORECAST=squeeze(nanstd(FORECAST,[],2)); 

%Pre allocate de variables.
RMSE=NaN(nest,nhoras,nforecast,nvar);
BIAS=NaN(nest,nhoras,nforecast,nvar);
RMSEDB=NaN(nest,nhoras,nforecast,nvar);
FORECASTDB=NaN(size(FORECAST));

   for ihourf=1:nforecast
       
       RMSE(:,:,ihourf,:)=squeeze(nanmean((squeeze(FORECAST(:,:,:,ihourf,:))-OBS).^2,2).^0.5);
       BIAS(:,:,ihourf,:)=squeeze(nanmean((squeeze(FORECAST(:,:,:,ihourf,:))-OBS),2));

       %Recalculo el caso particular de la direccion del viento (variable 7).
       tmp=squeeze(FORECAST(:,:,:,ihourf,7)-OBS(:,:,:,7));
       tmp(tmp>180)=tmp(tmp>180)-360;
       tmp(tmp<-180)=tmp(tmp<-180)+360;

       RMSE(:,:,ihourf,7)=squeeze(nanmean(tmp.^2,2).^0.5);
       BIAS(:,:,ihourf,7)=squeeze(nanmean(tmp,2));

       %Calculamos el pronostico Debias.
       for itime=3:ntiempos
       FORECASTDB(:,itime,:,ihourf,:)=squeeze(FORECAST(:,itime,:,ihourf,:))-squeeze(BIAS(:,:,ihourf,:));
       end
       %Calculamos el rmse sin el bias.
       RMSEDB(:,:,ihourf,:,:)=squeeze(nanmean((squeeze(FORECASTDB(:,:,:,ihourf,:))-OBS).^2,2).^0.5);

       %Recalculo el caso particular de la direccion del viento (variable 7).
       for itime=3:ntiempos
       tmp=squeeze(FORECAST(:,itime,:,ihourf,7))-squeeze(BIAS(:,:,ihourf,7))-squeeze(OBS(:,itime,:,7));
       tmp(tmp>180)=tmp(tmp>180)-360;
       tmp(tmp<-180)=tmp(tmp<-180)+360;
       FORECASTDB(:,itime,:,ihourf,7)=tmp;
       end

       RMSEDB(:,:,ihourf,7)=squeeze(nanmean(FORECASTDB(:,:,:,ihourf,7).^2,2).^0.5);
       
       
       %Calculo el rmse de un pronostico sin correlacion.
       MSENUL(:,:,ihourf,:)=squeeze(STDFORECAST(:,:,ihourf,:).^2)+STDOBS.^2;
       MSENUL(:,:,ihourf,7)=NaN; %No es trivial definir el MSENUL para la variable direccion del viento.
   end
   
   MSESS=(MSENUL-RMSEDB.^2)./MSENUL;


 

