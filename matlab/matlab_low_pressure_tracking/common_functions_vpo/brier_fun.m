%**************************************************************************
%       CALCULA EL BRIER SCORE (BRIER 1950) A PARTIR DE UNA DADA DISTRIBUCION DE
%       PROBABILIDADES, UMBRALES Y UN SET DE VERIFICACION
%       TAMBIEN CALCULA LA DESCOMPOSICION DEL BRIER EN RELIABILITY
%       RESOLUTION Y UNCERTAINTY COMO EN MURPHY (1973)
%**************************************************************************

function [brier reliability resolution uncertainty brier_clim] = brier_fun(obs,prob,umbral,prob_res)
%**************************************************************************
%Calculo el BRIER score para verificar el pron??stico de probabilidad.
%obs es un vector con las observaciones.
%prob es una matriz en donde cada columna es el pron?stico probabilistico
%para un determinado umbral. Las filas deben estar ordenadas de la misma
%manera que lo estan en el vector obs.
%prob_res indica con que resolucion vamos a barrer las probabilidades entre
%0 y 1 para calcular el brier y sus componentes.
%***********************************************************************

   categoria=0:prob_res:1; %Como particione el espacio de la probabilidad para calcular las componentes del BRIER.
   n_umb=length(umbral);
   
   no_nan=(obs >=0 & sum(prob(:,:),2) >= 0);
   
   obs=obs(no_nan);
   prob=prob(no_nan,:);
   
   brier=NaN(1,n_umb);
   reliability=NaN(1,n_umb);
   uncertainty=NaN(1,n_umb);
   brier_clim=NaN(1,n_umb);
   resolution=NaN(1,n_umb);
   
   for i_umb=1:n_umb
       pobs=zeros(length(obs),1);
       pobs(obs >= umbral(i_umb))=1; %Aux representa la observacion que es 0 o 1.
       brier(i_umb)=mean((prob(:,i_umb)-pobs).^2);
 
       
       %Vamos a calcular la partici?n del BRIER en componentes de
       %reliability, resolution and uncertainty.
       
       %La componente de uncertainty solo depende de la probabilidad de
       %ocurrencia (p) del fen?meno considerado en la muestra de
       %verificacion. U=p(1-p). Este valor U tambi?n se puede interpretar
       %como el BRIER de la climatologia.
       
       N=length(obs);
       if(N>0) %Solo hago los calculos si efectivamente tengo observaciones.
       p=mean(pobs);

       uncertainty(i_umb)=p*(1-p);
       brier_clim(i_umb)=p*(1-p);
       
       %Vamos a particionar el espacio de la probabilidad en una serie de
       %categorias definidas por la variable categorias.
       
       reliability(i_umb)=0;
       resolution(i_umb)=0;
       for icat=1:length(categoria)-1;
          %Para cada categor?a busco los pron?sticos que est?n en dicha
          %categor?a.
          clear indice_cat
          if (icat+1 == length(categoria)) %Si estoy en el ?ltimo intervalo incluyo tambi?n el valor del l?mite superior.
          indice_cat=( prob(:,i_umb) >= categoria(icat) & prob(:,i_umb) <= categoria(icat+1) ); 
          else
          indice_cat=( prob(:,i_umb) >= categoria(icat) & prob(:,i_umb) < categoria(icat+1) );
          end
          %Una vez que conozco quienes est?n en cada categoria tengo que
          %calcular la frecuencia relativa observada en cada categoria.
          nk=sum(indice_cat); %Numero de eventos en cada categoria.
          %fk=(categoria(icat)+categoria(icat+1))/2; %Valor central del intervalo.
          
          if(nk>0) %solo calculo el incremento en reliability y resolution si nk es mayor que 0, sino no tiene sentido!!
              okj=pobs(indice_cat);
              ok=mean(okj);
              fkj=squeeze(prob(indice_cat,i_umb));
              
              fk=mean(fkj);
              varfkj=sum((fkj-fk).^2);
              covarfkj=sum((fkj-fk).*(okj-ok));

              reliability(i_umb)=reliability(i_umb)+(nk*(fk-ok)^2)/N;
              resolution(i_umb)=resolution(i_umb)-(nk/N)*((ok-p).^2)+(1/N)*varfkj-(1/N)*2*covarfkj;
          end    
       end

       end
       
   end
   
   
   %Fin del calculo del BRIER SCORE y sus componentes.
%**************************************************************************






