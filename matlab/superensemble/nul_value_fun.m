%**************************************************************************
%    FUNCION DEL CALCULO DEL VALOR DEL PRONOSTICO NULO 
% El valor del pronóstico, depende de a, b, c y d y ademas depnende del
% umbral seleccinado para este modelo sencillo de la payoff table. Por eso,
% es necesario calcular el valor del pronóstico nulo, porque en muchos
% casos, proteger siempre o no proteger nunca puede ser la mejor opción y
% este procedimiento permite detectar eso, cuan cerca estamos de la
% situacion en la que de acuerdo a la probabilidad de ocurrencia del
% fenómeno la elección de proteger siempre o no proteger nunca son las más
% eficientes.

%En general lo que se observa es que los extremos conducen a las mayores
%ganancias en el caso del pronóstico puramente estocastico. El proteger
%siempre o no proteger nunca son las opciones mas favorables (como es
%logico) frente a un pronostico estocastico. De todas formas los valores
%obtenidos de este analisis permiten relativizar el valor economioco de la
%informacion provista por un pronostico.
%**************************************************************************
function [nul_value]=nul_value_fun(obs,umbral,a,b,c,d)

%**************************************************************************
%           obs= contiene las observaciones (mm)
%           umb= vector con los umbrales para los cuales vamos a calcular
%           el valor nulo
%           Nan Ready (R).
%**************************************************************************

%Definimos un rango de alfas (alfa es el coeficiente que controla que
%porcentaje del total de dias vamos a pronosticar lluvia o no lluvia).

alfa=0:0.1:1;

%***********************************************************************
%Vamos a calcular para distintos umbrales, como varÃ­a el coeficiente de
%acierto y el de falsa alarma en funcion del umbral de probabilidad
%utilizado para la toma de decisiÃ³n en la detecciÃ³n del fenomeno.
   
i_obs=find(isnan(obs)==0);
aux=obs(i_obs);
clear obs
obs=aux;
clear aux;


   for i_umb=1:length(umbral)               %Loop sobre los umbrales
    %Primero calculo la P de lluvia para el umbral i_umb.
    p_lluvia=length(find(obs >= umbral(i_umb)))/length(obs);
    n_total=length(obs);
    %Calculo el numero de pronosticos de lluvia en función de alfa.
    n_prono_lluvia=alfa*length(obs);
    
           
    hit(:,i_umb)=p_lluvia.*n_prono_lluvia;
    hit2(:,i_umb)=(1-p_lluvia)*(n_total-n_prono_lluvia);
    false(:,i_umb)=(1-p_lluvia)*n_prono_lluvia;
    miss(:,i_umb)=length(find(obs >= umbral(i_umb)))-p_lluvia.*n_prono_lluvia;                   

   end
   %Valor del pronóstico nulo en u$s por dia.
   aux=(a*miss+b*hit2+c*hit+d*false)./length(obs); %Calculamos el valor para cada valor de alfa y para cada umbral.
   
   nul_value=max(aux); %Defino el valor nulo como el maximo valor sobre todos los posibles alfas.
   opt_prob=(b-d)/((b-d)+(c-a));