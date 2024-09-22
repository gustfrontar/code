%**************************************************************************
%       CALCULA LA CURVA DE CONFIANZA PARA EL PRONOSTICO PROBABILISTICO
%       ESTE CALCULO PERMITE OBTENER EL RELIABILITY DIAGRAM
%**************************************************************************
function [reliability,n_forecast,prob_int] = reliability_fun(obs,prob,umbral,prob_res)
%**************************************************************************
% Obs es un vector con observaciones de precipitacion.
% prob es una matriz con tantas columnas como umbrales tenemos. Las filas
% se corresponden con las observaciones.
% prob_int es un vector con los l??mites de los intervalos de clase para las
% probabilidades por ejemplo: [0 0.1 0.2 0.3 ... 1] considerar??a en una
% primera categoria todas las probabilidades entre 0 y 0.1, luego las entre
%  0.1 y 0.2 ... y as??. Modificado en Febrero 2008.
% prob_res define la resolucion con la que vamos a trabajar (cual es la
% longitud de los intervalos en los que dividimos el espacio de la
% probabilidad de 0 a 1.
%**************************************************************************

prob_int=(0:prob_res:1); %
a=size(prob);
n_umb=length(umbral);
n_prob=length(prob_int);

for i_umb=1:n_umb
            
       %Primero trato el caso especial de 0. Que pasa con la probabilidad
       %cuando la probabilidad pronosticada vale 0.
              i_1=(obs >= umbral(i_umb) & prob(:,i_umb)==0);
              i_2=(prob(:,i_umb)==0 & obs >= 0);
              a1=sum(i_1);
              a2=sum(i_2);
              if(a2>50)
              reliability(i_umb,1)=a1/a2;
              n_forecast(i_umb,1)=a2;
              else
              reliability(i_umb,1)=NaN;
              n_forecast(i_umb,1)=0;
              end
              
       %Ahora me ocupo del resto de los intervalos... 
       for i_prob=2:n_prob
           
               prob_min=prob_int(i_prob-1);
               prob_max=prob_int(i_prob);
               prob_ref(i_prob)=(prob_min+prob_max)/2;

           %calculo el total de veces en que la probabilidad esta dentro
           %del rango definido por prob_max y prob_min y que 
           %ademas llovio por encima del umbral

           i_1=(obs >= umbral(i_umb) & prob(:,i_umb) > prob_min & prob(:,i_umb) <= prob_max);
           %calculo el total de veces en que la probabilidad esta dentro
           %del rango definido por prob_max y prob_min.
           i_2=(prob(:,i_umb) > prob_min & prob(:,i_umb) <= prob_max & obs >=0 ); 
           a1=sum(i_1);
           a2=sum(i_2);
           if(a2>50)
           reliability(i_umb,i_prob)=a1/a2;
           n_forecast(i_umb,i_prob)=a2;
           else
           reliability(i_umb,i_prob)=NaN;
           n_forecast(i_umb,i_prob)=0;
           end
           clear i_1 i_2
                    
       end     
   end

    








