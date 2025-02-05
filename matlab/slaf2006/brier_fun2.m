%**************************************************************************
%       CALCULA EL BRIER SCORE DE LA CLIMATOLOGIA Y DE PRONOSTICOS
%       DETERMINISTICOS.
%**************************************************************************

function [brier_clim brier_ens] = brier_fun2(obs,ens,umbral)
%**************************************************************************
%Calculo el BRIER para la climatologia
%***********************************************************************


   n_umb=length(umbral);
   ndatos=length(find(obs>=0)); %Busco los que no son NAN.
   i_obs=find(obs>=0);
 
 %Brier de la climatologia
   
   for i_umb=1:n_umb
       %Para cada umbral primero calculo la probabilidad climatologica.
       
       aux=zeros(ndatos,1);
       aux(find(obs(i_obs) >= umbral(i_umb)))=1; %Aux representa la observacion que es 0 o 1.
       clim_prob=sum(aux)/ndatos;
       brier_clim(i_umb)=nanmean((clim_prob-aux).^2);
       clear aux  
   end
   
   %Fin del calculo del BRIER SCORE para la climatologia.
   
  
  %Brier de los miembros del ensemble
  [filas columnas]=size(ens);
  
  for i_ens=1:columnas
  for i_umb=1:n_umb

   ndatos=length(find(ens(:,i_ens)>=0 & obs>=0) ); %Busco los que no son NAN.
   i_obs=find(ens(:,i_ens)>=0 & obs>=0);
        
       aux=zeros(ndatos,1);
       aux2=zeros(ndatos,1);
       aux(find(ens(i_obs,i_ens) >= umbral(i_umb)))=1; %Aux represnta el pronostico deterministico que es 0 o 1.
       aux2(find(obs(i_obs) >= umbral(i_umb)))=1;
       brier_ens(i_umb,i_ens)=nanmean((aux-aux2).^2);
       clear aux   
      
      
  end
  end
  
  %Fin del calculo del brier para los miembros del ensemble.
  
  
%**************************************************************************






