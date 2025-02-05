%**************************************************************************
%       ESTA FUNCION CALCULA EL PRONOSTICO PROBABILISTICO SOLO TENIENDO EN
%       CUENTA LOS MIEMBROS DEL ENSEMBLE (PRONOSTICO NO CALIBRADO)
%**************************************************************************
function [probabilidad] = prob_cal_fun(forecast,umbral)
%**************************************************************************
% forecast es una matriz. Cada columna es un pronóstico distinto (un
% miembro del ensemble).
% umbral es un vector con los umbrales de precipitacion para los cuales
% vamos a calcular la probabilidad.
%**************************************************************************

a=size(forecast);
n_umb=length(umbral);



for i_umb=1:n_umb
    aux=zeros(a(1),a(2));
    aux(find(forecast >= umbral(i_umb)))=1;
    probabilidad(:,i_umb)=squeeze(sum(aux,2)./a(2));
    clear aux
end

i_nan=find(isnan(forecast(:,1))==1);
probabilidad(i_nan,:)=NaN;

    








