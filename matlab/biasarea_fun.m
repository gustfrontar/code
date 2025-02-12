%**************************************************************************
%       ESTA FUNCION CALCULA EL BIAS AREAL.
%**************************************************************************

function [bias_area] = biasarea_fun(obs,forecast,umbral)
%**************************************************************************
n_umb=length(umbral);
% obs es un vector columna con las observaciones.
% forecast es una matriz. Cada columna es un pronóstico distinto (un
% miembro del ensemble).
a=size(forecast);
ens=a(2); 

for iumb=1:n_umb
    for iens=1:ens
        %Primero busco que no sean NaN los puntos.
        inonnan=find(isnan(obs)==0 & isnan(squeeze(forecast(:,iens)))==0);
        %Calculo los puntos donde llovio mas que el umbral
        iarea_obs=find(obs(inonnan) >= umbral(iumb));
        %Calculo los puntos donde al prono le llovio mas que el umbral
        iarea_for=find(forecast(inonnan,iens) >= umbral(iumb));
        bias(iumb,iens)=length(iarea_for)/length(iarea_obs);
        clear iarea_obs iarea_for bias_area inonnan
    end
end

bias_area=bias;

%**************************************************************************






