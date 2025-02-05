%**************************************************************************
%       CALCULA EL BRIER SCORE A PARTIR DE UNA DADA DISTRIBUCION DE
%       PROBABILIDADES, UMBRALES Y UN SET DE VERIFICACION
%**************************************************************************

function [brier] = biasarea_fun(obs,prob,umbral)
%**************************************************************************
%Calculo el BRIER score para verificar el pronóstico de probabilidad.
%***********************************************************************
   n_umb=length(umbral);
   ndatos=length(find(obs>=0)); %Busco los que no son NAN.
   i_obs=find(obs>=0);
   for i_umb=1:n_umb
       aux=zeros(ndatos,1);
       aux(find(obs(i_obs) >= umbral(i_umb)))=1; %Aux representa la observacion que es 0 o 1.
       brier(i_umb)=nanmean((prob(i_obs,i_umb)-aux).^2);
       clear aux  
   end
   
   %Fin del calculo del BRIER SCORE.
   %Aca faltaría agregar el cálculo de las componentes en las que se puede
   %descomponer el BRIER...
%**************************************************************************






