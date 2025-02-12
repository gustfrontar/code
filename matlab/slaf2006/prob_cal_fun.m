%**************************************************************************
%       ESTA FUNCION CALCULA EL PRONOSTICO PROBABILISTICO CALIBRADO POR EL 
%       METODO DEL RANK HISTOGRAM  (HAMILL Y COLLUCCI).
%**************************************************************************
function [probabilidad] = prob_cal_fun(forecast_old,forecast,rank_hist,umb_sigma,umbral,flag,flag2)
%**************************************************************************
% forecast_old es una matriz donde cada columna es un pronóstico y
% representa pronósticos pasados. Estos pronósticos pasados nos sirven para
% calcular la distribución de probabilidades de la lluvia asociada al
% ensemble.
% forecast es una matriz. Cada columna es un pronóstico distinto (un
% miembro del ensemble).
% rank es una matriz en cada fila esta el rank histogram (probabilidades) correspondiente a
% un rango de varianzas de los miembros del ensemble. Por ejemplo la fila 1
% corresponde al rank histogram asociado a los casos de poca dispersion.
% umb_sigma marca los umbrales de sigma a partir de los cuales usamos un
% rank histogram o el otro.
% umbral es un vector con los umbrales de precipitacion para los cuales
% vamos a calcular la probabilidad.
%Flag1 determina si vamos a considerar o no los 0 en la distribución de
%precipitación y si vamos pronosticos u observaciones para determinar la
%distribución de probabilidades de un evento.
%flag=1 no considero los 0.
%flag=2 considero los 0.
%Flag2 determina si usamos distribución lineal o la distribución estimada
%con los datos para estimar la probabilidad de los umbrales que están por
%debajo del mínimo valor del ensemble.
%flag2=1 usamos la distribución de P estimada.
%flag2=2 usamos una distribución lineal como en Hamill y Colucci 1997.
%**************************************************************************

a=size(forecast);
ens=a(2); 
ndatos=a(1);

nsigmas=length(umb_sigma)+1;
n_umb=length(umbral);

%**************************************************************************
%Tomo todos los pronósticos de todos los miembros de una muestra pasada y
%los reacomodo en un vector columna que me va a permitir estimar la
%probabilidad de los distintos umbrales y de los valores extremos del rank
%histogram para realizar la calibración.
%Eventualmente forecast_old puede ser una distribución de datos observados
%en los últimos n dias en lugar de pronósticos. De acuerdo al valor de flag
%considero o no los 0 en la distribución.

if(flag==1 | flag==2)
b=size(forecast_old);
aux=reshape(forecast_old,b(1)*b(2),1);
if(flag==1)
prob_est=sort(aux(find(aux > 0)));
clear aux
else
prob_est=sort(aux(find(aux >= 0)));
clear aux
end
end


%**************************************************************************

%queremos reducir al máximo la longitud de los loops para eso usamos la
%función find

%Primero que pasa si todos los miembros del ensamble no dan lluvia... en
%ese caso todas las probabilidades son 0.

sort_forecast=sort(forecast,2,'ascend'); %ordeno los miembros del ensemble de menor a mayor.
std_forecast=std(forecast,1,2);          %calculo el sigma de cada día.
n_total=length(prob_est);
probabilidad=NaN(ndatos,n_umb);

%Calculo algunas probabilidades que aceleran el cálculo

for i_umb=1:n_umb
    
p_umb(i_umb)=length(find(prob_est < umbral(i_umb)))/n_total;
end

for i_forecast=1:ndatos
    
    if(sum(forecast(i_forecast,:))==0)
        %Si todos los miembros pronostican 0 entonces la probabilidad es 0
        %independientemente del umbral.
        probabilidad(i_forecast,:)=zeros(1,n_umb);

    else

    %Si no son todos 0 entonces comienzo la estimación de la probabilidad
    %para cada umbral
    
    for i_umb=1:n_umb
       %Primer paso, busco a que rank corresponde el umbral.
       i_aux=find(sort_forecast(i_forecast,:) < umbral(i_umb));
       local_rank=length(i_aux)+1;
       %Local rank puede ser o 1 o ENS+1. Si es 1 o ENS+1 entonces tenemos
       %que estimar la probabilidad usando la distribución. Sinó la cuenta
       %es más facil.
       %Determinamos que rank vamos a usar en funcion de la dispersion.
       i_sigma=length(find(umb_sigma < std_forecast(i_forecast)))+1; %Determino que rank hist voy a usar en función del sigma

       if(local_rank < ens+1 & local_rank > 1)
           %El umbral cae en un rank mayor que 1 y menor que ens+1
           %En este caso directamente estimo la probabilidad del rank
           %histogram.
           %Interpolo linealmente entre la probabilidad del mayor y el menor
           %valor del intervalo en donde cae mi umbral.
           
           aux_prob=rank_hist(i_sigma,local_rank)*(umbral(i_umb)-sort_forecast(i_forecast,local_rank-1))/(sort_forecast(i_forecast,local_rank)-sort_forecast(i_forecast,local_rank-1));
           probabilidad(i_forecast,i_umb)=1-(sum(rank_hist(i_sigma,1:local_rank-1))+aux_prob);
           clear aux_prob
       end
       
       if(local_rank == 1)
           %El umbral está por debajo del mínimo valor pronosticado por el
           %ensemble... uso la distribucion de eventos de lluvia pasados
           %para estimar que porción de probabilidad le asigno a este
           %valor.
           %Tengo que estimar la probabilidad del umbral y del mínimo
           %miembro del ensemble en la distribución de los pronósticos de
           %lluvia.

           %Calculo la probabilidad del mínimo miembro del ensemble
           if( isnan(sort_forecast(i_forecast,1))==0) %En esta categoria vienen a parar los que son NaN.

             probabilidad(i_forecast,i_umb)=1-rank_hist(i_sigma,1)*(umbral(i_umb)/sort_forecast(i_forecast,1));

           end
       end
       
       if(local_rank == ens+1)
           %El umbral está por encima del máximo miembro del ensemble...
           %uso la distribucion de eventos de lluvia pasaods para estimar
           %la porcion de probabilidad que le asigno a la ocurrencia de
           %precipitaciones por encima del umbral.
           %Para eso necesito la probabilidad del máximo miembro del
           %ensemble y del umbral. 
           p_max=length(find(prob_est < sort_forecast(i_forecast,ens)))/n_total;
           probabilidad(i_forecast,i_umb)=rank_hist(i_sigma,ens+1)*(1-p_umb(i_umb))/(1-p_max);
           
       end
       
    end
    
       
        
    end
    
end

    








