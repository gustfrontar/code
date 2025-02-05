%**************************************************************************
%       CALCULA LA CURVA DE CONFIANZA PARA EL PRONOSTICO PROBABILISTICO
%       ESTE CALCULO PERMITE OBTENER EL RELIABILITY DIAGRAM
%**************************************************************************
function [reliability,n_forecast,prob_ref,clim_prob] = reliability_fun(obs,prob,umbral,prob_int)
%**************************************************************************
% Obs es un vector con observaciones de precipitacion.
% prob es una matriz con tantas columnas como umbrales tenemos. Las filas
% se corresponden con las observaciones.
% prob_int es un vector con los límites de los intervalos de clase para las
% probabilidades por ejemplo: [0 0.1 0.2 0.3 ... 1] consideraría en una
% primera categoria todas las probabilidades entre 0 y 0.1, luego las entre
%  0.1 y 0.2 ... y así.
%**************************************************************************


a=size(prob);
n_umb=length(umbral);
n_prob=length(prob_int)-1;

for i_umb=1:n_umb
    
           %Calculo la frecuencia climatológica de cada umbral
           i_aux=length(find(obs >= umbral(i_umb)));
           i_aux2=length(find(obs >= 0)); %Con esto filtro los NaN.
           clim_prob(i_umb)=i_aux/i_aux2; %Calculo la probabilidad de tener
           %precipitaciones por encima de un umbral independientemente del
           %pronóstico.
           clear i_aux i_aux2
           
       for i_prob=1:n_prob
           
               prob_min=prob_int(i_prob);
               prob_max=prob_int(i_prob+1);
               prob_ref(i_prob)=(prob_min+prob_max)/2;

           %calculo el total de veces en que la probabilidad esta dentro
           %del rango definido por prob_max y prob_min y que 
           %ademas llovio por encima del umbral

           i_1=find(obs >= umbral(i_umb) & prob(:,i_umb) >= prob_min & prob(:,i_umb) <= prob_max);
           %calculo el total de veces en que la probabilidad esta dentro
           %del rango definido por prob_max y prob_min.
           i_2=find(prob(:,i_umb) >= prob_min & prob(:,i_umb) <= prob_max); 

           if(length(i_2)>10)
           reliability(i_umb,i_prob)=length(i_1)/length(i_2);
           n_forecast(i_umb,i_prob)=length(i_2);
           else
           reliability(i_umb,i_prob)=NaN;
           n_forecast(i_umb,i_prob)=0;
           end
           clear i_1 i_2
                    
       end     
   end

    








