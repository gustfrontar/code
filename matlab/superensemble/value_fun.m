%**************************************************************************
%    FUNCION DEL CALCULO DEL VALOR DE UN PRON�STICO (PARA PRONOSTICOS PROBABILISTICOS). 
%**************************************************************************
function [value opt_prob]=ets_fun(obs,prob,umbral,prob_umb,a,b,c,d)

%**************************************************************************
%           obs= contiene las observaciones (mm)
%           prob= contiene las probabilidades pronosticadas para cada
%           umbral.
%           umb= vector con los umbrales para los cuales vamos a calcular
%           el ets
%          
%**************************************************************************

%***********************************************************************
%Vamos a calcular para distintos umbrales, como varía el coeficiente de
%acierto y el de falsa alarma en funcion del umbral de probabilidad
%utilizado para la toma de decisión en la detección del fenomeno.
   
   for i_umb=1:length(umbral)               %Loop sobre los umbrales
       for iprob=1:length(prob_umb)     %Loop sobre las probabilidades.
%Para cada umbral de probabilidad calculamos los aciertos y las falsas
%alarmas, eso es lo que podemos plotear en un diagrama ROC.

    i=find(isnan(obs)==0 & isnan(prob(:,i_umb))==0 );
    prono=prob(i,i_umb);
    aux_obs=obs(i);
           
    i_hits=find(aux_obs >= umbral(i_umb) & prono >= prob_umb(iprob));
    i_hits2=find(aux_obs < umbral(i_umb) & prono < prob_umb(iprob));
    i_false=find(aux_obs < umbral(i_umb) & prono >= prob_umb(iprob));
    i_miss=find(aux_obs >= umbral(i_umb) & prono < prob_umb(iprob));
    
    hit(iprob,i_umb)=length(i_hits);
    hit2(iprob,i_umb)=length(i_hits2);   
    false(iprob,i_umb)=length(i_false);
    misses(iprob,i_umb)=length(i_miss);
    total(iprob,i_umb)=length(aux_obs);

    clear i_hits i_false i_hits2 i_miss i aux_obs prono
                       
       end
   end

   
   %Valor del pronostico diario en u$s por dia.
   value=(a*misses+b*hit2+c*hit+d*false)./total;
   opt_prob=(b-d)/((b-d)+(c-a));