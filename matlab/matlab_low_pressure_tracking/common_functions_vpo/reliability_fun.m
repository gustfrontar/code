%**************************************************************************
%       CALCULA LA CURVA DE CONFIANZA PARA EL PRONOSTICO PROBABILISTICO
%       ESTE CALCULO PERMITE OBTENER EL RELIABILITY DIAGRAM
%**************************************************************************
function [reliability,n_forecast,prob_ref] = reliability_fun(obs,prob,prob_res)
%**************************************************************************
% RELABILITY DIAGRAM ADAPTADO PARA APLICARSE A LA EXISTENCIA DE LOS
% SISTEMAS.
% OBS es 0 o 1 dependiendo de si el sistema existe o no.
%**************************************************************************

prob_int=(0:prob_res:1); %
n_prob=length(prob_int);

reliability=NaN(length(prob_int),1);
n_forecast =NaN(length(prob_int),1);
prob_ref=NaN(length(prob_int),1);

            
       %Primero trato el caso especial de 0. Que pasa con la probabilidad
       %cuando la probabilidad pronosticada vale 0.
              i_1=(obs == 1 & prob==0);
              i_2=(prob==0  & obs >= 0);
              a1=sum(i_1);
              a2=sum(i_2);
              if(a2>2)
              reliability(1)=a1/a2;
              n_forecast(1)=a2;
              else
              reliability(1)=NaN;
              n_forecast(1)=0;
              end
              prob_ref(1)=0;
              
       %Ahora me ocupo del resto de los intervalos... 
       for i_prob=2:n_prob
           
               prob_min=prob_int(i_prob-1);
               prob_max=prob_int(i_prob);
               prob_ref(i_prob)=(prob_min+prob_max)/2;

           %calculo el total de veces en que la probabilidad esta dentro
           %del rango definido por prob_max y prob_min y que 
           %ademas llovio por encima del umbral
           i_1=(obs == 1 & prob > prob_min & prob <= prob_max);
           %calculo el total de veces en que la probabilidad esta dentro
           %del rango definido por prob_max y prob_min.
           i_2=(prob > prob_min & prob <= prob_max & obs >=0 ); 
           a1=sum(i_1);
           a2=sum(i_2);
           if(a2>2)
           reliability(i_prob)=a1/a2;
           n_forecast(i_prob)=a2;
           else
           reliability(i_prob)=NaN;
           n_forecast(i_prob)=0;
           end
           clear i_1 i_2
                    
       end     


    








