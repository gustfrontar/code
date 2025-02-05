%**************************************************************************
%       ESTA FUNCION CALCULA LAS CURVAS NECESARIAS PARA CONSTRUIR UN
%       DIAGRAMA ROC (BASICAMENTE ACIERTOS Y FALSAS ALARMAS EN FUNCION DEL
%       UMBRAL DE PROBABILIDAD SELECCIONADO.
%       TAMBIEN REALIZA LOS CALCULOS PARA CONFECCIONAR UN DIAGRAMA ETS-BIAS
%**************************************************************************
function [hit far area ets bias area_2] = roc_fun(obs,prob,umbral,prob_res)
%**************************************************************************
% Obs es un vector con observaciones de precipitacion.
% prob es una matriz con tantas columnas como umbrales tenemos. Las filas
% se corresponden con las observaciones.
% prob_int es un vector con los l??mites de los intervalos de clase para las
% probabilidades por ejemplo: [0.1 0.2 0.3 ...0.9] considerar??a en una
% 
% Las categorias con menos de 10 pron??sticos se ponen como NaN.
% El area calculada del ROC asume que los extremos de la curva est??n en 1,1
% 0,0 y se le resta ademas el area de la curva de no-skill que es 0.5.
% Los aciertos y falsas alarmas del pronostico nulo se construyen en base a
% un pron??stico puramente por azar.
% Hit null y far null son los aciertos y falsas alarmas de un pron??stico al
% azar.
% Area 2 es el area del diagrama ETS-BIAS normalizada. Esto es el area bajo
% la curva de ETS entre el valor BIAS=0 y BIAS=BIASMAX donde BIASMAX es el
% maximo BIAS que puede alcanzar un pron?stico y que es 1/P(de ocurrencia).
% El area se pesa por el BIASMAX de forma que sea comparable entre
% distintos umbrales y diferentes reg?menes de precipitaci?n.
% CORREGIDO FEBRERO DE 2008 BUG EN EL CALCULO DEL BIAS EN EL CASO DE QUE
% HUBIERA NAN EN LAS OBSERVACIONES. OPTIMIZACION DEL CODIGO PARA QUE CORRA
% UN POCO MAS RAPIDO.
%**************************************************************************

prob_umb=0.1:prob_res:1;

%***********************************************************************
%Vamos a calcular para distintos umbrales, como var??a el coeficiente de
%acierto y el de falsa alarma en funcion del umbral de probabilidad
%utilizado para la toma de decisi??n en la detecci??n del fenomeno.
   
   for i_umb=1:length(umbral)               %Loop sobre los umbrales
       for iprob=1:length(prob_umb)     %Loop sobre las probabilidades.
%Para cada umbral de probabilidad calculamos los aciertos y las falsas
%alarmas, eso es lo que podemos plotear en un diagrama ROC.
           
    i_hits=(obs >= umbral(i_umb) & prob(:,i_umb) >= prob_umb(iprob)); 
    i_total=(obs >= umbral(i_umb) & prob(:,i_umb) >= 0);
    i_total_total=(obs >= 0 & prob(:,i_umb) >= 0);
    if(i_total_total >0)
    clim_prob(i_umb)=sum(i_total)/sum(i_total_total);
    else
    clim_prob(i_umb)=NaN;
    end
    prob_lluvia(i_umb)=clim_prob(i_umb);
    hit_num(iprob,i_umb)=sum(i_hits);
    tot=sum(i_total);
    tottot=sum(i_total_total);
    if(tot > 0)
    hit(iprob,i_umb)=hit_num(iprob,i_umb)/tot;
    else
    hit(iprob,i_umb)=NaN;
    end
    i_false=( obs < umbral(i_umb) & prob(:,i_umb) >= prob_umb(iprob));
    false_num(iprob,i_umb)=sum(i_false);
    misses_num(iprob,i_umb)=tot-hit_num(iprob,i_umb);
    si_lluvia=hit_num(iprob,i_umb)+false_num(iprob,i_umb);
    if(si_lluvia > 0)
        %FAR falsa_alarma/(numero total de veces que no llovio).
        %Cual es la probabilidad de que pronostique lluvia dado que no
        %llueve.
    far(iprob,i_umb)=false_num(iprob,i_umb)/(tottot-tot);
    else
    far(iprob,i_umb)=NaN;
    end
    num(iprob,i_umb)=si_lluvia;
    clear i_hits i_total i_false 
    
    %ETS equitable threat score 
    %ETS=( hits - hitsrandom )/ (hits + misses + false alarms - hitsrandom)
    %hitsrandom = (hits+misses)*(hits+false alarms)/(total)
    
    hits_random(iprob,i_umb)=(hit_num(iprob,i_umb) + misses_num(iprob,i_umb))*( hit_num(iprob,i_umb) + false_num(iprob,i_umb) )./tottot;
    ets(iprob,i_umb)=(hit_num(iprob,i_umb)-hits_random(iprob,i_umb))/(hit_num(iprob,i_umb) +  misses_num(iprob,i_umb) + false_num(iprob,i_umb) - hits_random(iprob,i_umb) ); 
 
    %BIAS
    %Calculo los puntos donde llovio mas que el umbral
    iarea_obs=sum(obs >= umbral(i_umb) & prob(:,i_umb) >= 0); %me aseguro que no sean NaN.
    %Calculo los puntos donde al prono le llovio mas que el umbral
    iarea_for=sum(prob(:,i_umb) >= prob_umb(iprob) & obs >=0 );
    bias(iprob,i_umb)=iarea_for/iarea_obs;
    clear iarea_obs iarea_for bias_area
           
                      
       end
   end

        
%**************************************************************************   
%Defino una medida global del desempe??o que ser??a la suma de las
%diferencias entre la curva asociada a mi umbral de precipitaci??n y la
%curva de no-skill.
%**************************************************************************
 
%Si hay menos de 10 pron??sticos para una categoria los pongo como NaN.
%La idea de esto es calcular el area con valores m??s o menos estables.

hit(find(num <= 10))=NaN;
far(find(num <= 10))=NaN;

%Calculo el ?rea asociada al ROC.
for i_umb=1:length(umbral)
       %para cada umbral me genero un vector de hit y far sacando los NaN.
       aux=squeeze(hit(:,i_umb));
       aux2=squeeze(far(:,i_umb));
       hit_aux=aux( ~isnan(aux) & ~isnan(aux2) );
       far_aux=aux2( ~isnan(aux) & ~isnan(aux2) );
       %Les agrego los extremos 0,0 y 1,1.
       hit_aux=[1 hit_aux' 0];
       far_aux=[1 far_aux' 0];
       %Ahora integro el area bajo la curva.
       area(i_umb)=0;
       for i=1:length(hit_aux)-1
           area(i_umb)=area(i_umb)+(far_aux(i)-far_aux(i+1))*(hit_aux(i+1)+hit_aux(i))/2;    
       end
       %Le resto 0.5 que es el area del no-skill forecast.
       area(i_umb)=area(i_umb);
       clear hit_aux far_aux aux aux2
end
   
%**************************************************************************

%Busco los casos en los que tengo pocos datos y para esos casos pongo el
%ETS y el BIAS como NAN antes de calcular el ?rea y hacer los graficos.

ets(find(num <= 10))=NaN;
bias(find(num <= 10))=NaN;


%Calculo el area asociada al BIAS-ETS.
for i_umb=1:length(umbral)
       bias_max(i_umb)=1/clim_prob(i_umb);
       %para cada umbral me genero un vector de hit y far sacando los NaN.
       aux=squeeze(ets(:,i_umb));
       aux2=squeeze(bias(:,i_umb));
       ets_aux=aux( find( isnan(aux) == 0 ) );
       bias_aux=aux2( find( isnan(aux2) == 0 ) );
       %Les agrego los extremos 0,0 y 1,1.
       ets_aux=[0 ets_aux' 0];
       bias_aux=[bias_max(i_umb) bias_aux' 0];
       %Ahora integro el area bajo la curva.
       area_2(i_umb)=0;
       for i=1:length(ets_aux)-1
           area_2(i_umb)=area_2(i_umb)+(bias_aux(i)-bias_aux(i+1))*(ets_aux(i+1)+ets_aux(i))/2;    
       end
       area_2(i_umb)=area_2(i_umb)/bias_max(i_umb); %Normalizo por el BIAS maximo.
       clear bias_aux ets_aux aux aux2 
end





