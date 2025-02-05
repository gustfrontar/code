%**************************************************************************
%    FUNCION DEL CALCULO DEL VALOR DE UN PRONÓSTICO (PARA PRONOSTICOS PROBABILISTICOS). 
%**************************************************************************
function [realv_index potv_index v_index max_prob max_prob_offset v_forecast v_climate v_perfect]=value_fun(obs,prob,umbral,prob_umb)

%**************************************************************************
%           obs= contiene las observaciones (mm)
%           prob= contiene las probabilidades pronosticadas para cada
%           umbral.
%           umb= vector con los umbrales para los cuales vamos a calcular
%           el valor
%           El calculo del valor se hace siguiendo a Zhu et. al. The
%           Economic Value of Ensemble-Based Weather Forecast (BAMS 2002).
%           
%           Utilizamos las siguientes definiciones:
%           c= cost, es el costo de mitigacion (costo asociado a suspender
%           la tarea o actividad y a tomar proteccion contra el fenomeno)
%           lu= unprotectable loss, perdida residual que se produce en caso
%           de que el fenómeno es correctamente pronosticado (no toda la
%           pérdida puede evitarse)
%           lp= perdida asociada a la ocurrencia no prevista del fenómeno.
%           Cuando el fenómeno ocurre y no fue previsto la pérdida total L
%           es igual a la suma de lp+lu.
%           n=ganancia asociada al caso en el que no se produce el fenomeno
%           y la actividad se desarrollo normalmente. (puede suponerse como
%           0 en terminos)
%
%           Los hits tienen asociado un costo : c+lu (costo de inactividad+
%           mitigacion+ unprotectable loss.
%           Las falsas alarmas tienen asociado un costo: c solo
%           inactividad+ mitigacion.
%           Las miss tienen asociado una perdida que es lp+lu.
%           Los pronosticos que correctamente pronostican la no ocurrencia
%           del fenomeno tienen asociado un valor n (este valor puede
%           elegirse arbitrariamente como 0 dado que no nos interesa
%           considerar la viabilidad economica de la actividad sino el
%           impacto del pronostico en reducir la perdida asociada a la
%           ocurrencia de un determinado fenomeno).
%
%           En esta formulacion vamos a considerar todos los costos como
%           valores positivos en forma analoga a Zhu et. al. esto
%           implicaria que lo que estamos calculando es la perdida total
%           asociada a la ocurrencia de un evento que perjudica la
%           actividad y estamos viendo como el pronostico impacta en la
%           reduccion de ese costo.
%
%           V_INDEX es un indice que mide la mejora relativa de un
%           pronostico sobre el pronostico climatologico con respecto a la
%           mejora asociada a un pronostico perfecto.
%           v_index= (v_climate - v_forecast) / (v_climate - v_perfect)
%           Superiormente esta acotada por 1. Inferiormente no tiene cota.
%
%           Este script calcula el realv_index que obtiene el umbral de
%           probabilidad utilizado en la toma de decision en forma teorica
%           como el cociente c/lp. Este es el umbral de probabilidad que
%           maximiza el valor si la calibracion es correcta. Por otro lado
%           calcula el potv_index que calcula el maximo valor sobre todos
%           los umbrales de probabilidad considerados, es que si el
%           pronostico no esta calibrado el maximo valor puede on ocurrir
%           necesariamente en la probabilidad optima. Esta medida da una
%           idea de cuanto podria mejorar el valor del pronostico con una
%           mejora de la calibracion.
%           max_prob es la probabilidad donde se alcanza el maximo valor que
%           que para un pronostico correctamente calibrado deberia
%           coincidir con la probabilidad teorica de toma de decision
%           (c/lp)
%           La diferencia entre la probabilidad del maximo valor y la
%           probabilidad teorica puede ser utilizado como una medida de la
%           calibracion.
%           De hecho, se puede pensar en una calibracion orientada
%           totalmente a maximizar el valor economico de la informacion, es
%           decir orientada que max_prob coincida con c/lp para los
%           diferentes valores de c/lp y los distintos umbrales.

            lp=1;
            lu=0.1;
            n=0;
%                    
%**************************************************************************

%***********************************************************************
%Vamos a calcular para distintos umbrales, como varÃ­a el coeficiente de
%acierto y el de falsa alarma en funcion del umbral de probabilidad
%utilizado para la toma de decision en la detecciÃ³n del fenomeno.
   

% Primero eliminamos los que son NaN.
%**************************************************************************

    i=find(isnan(obs)==0 & isnan(prob(:,1))==0 );
    prono=prob(i,:);
    aux_obs=obs(i);
    clear i
    
%**************************************************************************

for ir=1:99
    c=lp*ir/100; %Vamos variando el valor de LP de forma tal que crezca como
    %un porcentaje del valor de C, desde 1% a 100%.
    
   for i_umb=1:length(umbral)               %Loop sobre los umbrales
       
       %Para cada umbral vamos a calcular el valor del pronostico
       %climatologico y el valor del pronostico perfecto.
       %*******************************************************************
       
       %Calculo la probabilidad de ocurrencia del fenomeno.
       %*******************************************************************
       
       p_lluvia(i_umb)=length(find(aux_obs >= umbral(i_umb)))/length(aux_obs);
       
       %Calculo el valor del pronostico climatico (dependiendo de la
       %probabilidad puede ser mas ventajoso tomar accion preventiva
       %siempre o no tomarla nunca. Como los costos estan expresados como
       %valores positivos, al tomar el minimo de la funcion, estoy tomando
       %de las dos opcines la que mayor reduccion produce en los costos.
       %Como el valor del pronostico al azar es una funcion lineal de la
       %proporcion de dias en los que se pronostico lluvia, el mayor valor
       %siempre va a estar en uno de los dos extremos.
       %*******************************************************************
       
       v_climate(i_umb,ir)=p_lluvia(i_umb)*lu+ min([ p_lluvia(i_umb)*lp ; (c+(1-p_lluvia(i_umb))*n) ]) ;
       
       %Calculo el valor del pronostico perfecto:
       %*******************************************************************
       
       v_perfect(i_umb,ir)=p_lluvia(i_umb)*(c+lu)+(1-p_lluvia(i_umb))*n;
       
       for iprob=1:length(prob_umb)         %Loop sobre las probabilidades.
%Para cada umbral de probabilidad calculamos los aciertos y las falsas
%alarmas.
           
    i_hits=find(aux_obs >= umbral(i_umb) & prono(:,i_umb) >= prob_umb(iprob));
    i_hits2=find(aux_obs < umbral(i_umb) & prono(:,i_umb) < prob_umb(iprob));
    i_false=find(aux_obs < umbral(i_umb) & prono(:,i_umb) >= prob_umb(iprob));
    i_miss=find(aux_obs >= umbral(i_umb) & prono(:,i_umb) < prob_umb(iprob));
    
    total=length(aux_obs);
    hit=length(i_hits)/total;
    hit2=length(i_hits2)/total;   
    false=length(i_false)/total;
    misses=length(i_miss)/total;
    
    v_forecast(iprob,i_umb,ir)=hit*(c+lu)+false*c+misses*(lp+lu)+hit2*n;
    
    v_index(iprob,i_umb,ir)= (v_climate(i_umb,ir)-v_forecast(iprob,i_umb,ir))/(v_climate(i_umb,ir)-v_perfect(i_umb,ir));
    clear i_hits i_false i_hits2 i_miss
                       
       end   
   end
   
   %Calculamos un v_index para cada umbral de probabilidad y para cada
   %umbral de lluvia.
   %En una aplicacion real, el usuario debe seleccionar un umbral de
   %probabilidad y tomar la decision siempre basandose en dicho umbral. Si
   %la calibracion del pronostico es buena este umbral puede ser
   %seleccionado teoricamente sabiendo que el umbral que maximiza el valor
   %del pronostico es c/lp . Esto nos daria el valor del pronostico
   %asumiendo que su reliability es perfecta.
   %Por otra parte como sabemos que la reliability no es perfecta, podemos
   %calcular el valor potencial como el maximo valor obtenido para algun
   %umbral que no coincida con c/lp por otro lado podemos medir la
   %diferencia entre este umbral y el umbral teorico y obtener una medida
   %de cuanto se aparta nuestra calibracion de lo ideal para dicho umbral
   %de lluvia y dicho usuario. 

   %Calculo el valor de la probabilidad teorica.
   %*******************************************************************
   opt_prob=c/lp;
   
   %El valor real asociado a cada pronostico sera el valor asumiendo que el
   %usuario uso el umbral de probabilidad teorica.
   
   %encuentro de todos los valores calculados en mas cercano al umbral
   %teórico.
   %*******************************************************************
   
   aux=abs(prob_umb - opt_prob);
   i_teo=find(aux==min(aux));
   
   %Para cada umbral tomo el valor del pronóstico usando como umbral de
   %probabilidad el umbral teorico.
   %*******************************************************************
   
   realv_index(:,ir)=v_index(i_teo,:,ir);
   
   %Calculo el valor potencial (sujeto a mejoras en la calibracion).
   %Para cada umbral de precipitacion calculamos el valor asociado a
   %distintos valores de umbral de probabilidad, en el calculo del valor
   %potencial vamos a considerar el maximo valor obtenido y el valor de
   %probabilidad al cual corresponde.
   
   %Calculo el valor potencial
   potv_index(:,ir)=squeeze(max(v_index(:,:,ir),[],1));
   
   %Busco a que valor de probabilidad corresponde.
   
   for i_umb=1:length(umbral)
   i_prob=find(v_index(:,i_umb,ir)==potv_index(i_umb,ir));
   max_prob(i_umb,ir)=mean(prob_umb(i_prob)); %Calculo el promedio por las dudas de que sean varios los valores de umbral
                                           %que coincidan con el criterio
                                           %de maximo valor.   
                                           
   max_prob_offset(i_umb,ir)=max_prob(i_umb,ir)-opt_prob;
   
   end
   
end %End del do sobre los valores de C.
   
   
   
   
   
   
   
   
   
   
   
   
   
   