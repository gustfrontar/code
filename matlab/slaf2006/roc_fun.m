%**************************************************************************
%       ESTA FUNCION CALCULA LAS CURVAS NECESARIAS PARA CONSTRUIR UN
%       DIAGRAMA ROC (BASICAMENTE ACIERTOS Y FALSAS ALARMAS EN FUNCION DEL
%       UMBRAL DE PROBABILIDAD SELECCIONADO.
%**************************************************************************
function [hit far area ets bias area_2] = roc_fun(obs,prob,umbral,prob_umb)
%**************************************************************************
% Obs es un vector con observaciones de precipitacion.
% prob es una matriz con tantas columnas como umbrales tenemos. Las filas
% se corresponden con las observaciones.
% prob_int es un vector con los l铆mites de los intervalos de clase para las
% probabilidades por ejemplo: [0.1 0.2 0.3 ...0.9] considerar铆a en una
% 
% Las categorias con menos de 10 pron贸sticos se ponen como NaN.
% El area calculada del ROC asume que los extremos de la curva est谩n en 1,1
% 0,0 y se le resta ademas el area de la curva de no-skill que es 0.5.
% Los aciertos y falsas alarmas del pronostico nulo se construyen en base a
% un pron贸stico puramente por azar.
% Hit null y far null son los aciertos y falsas alarmas de un pron贸stico al
% azar.
% Area 2 es el area del diagrama ETS-BIAS normalizada. Esto es el area bajo
% la curva de ETS entre el valor BIAS=0 y BIAS=BIASMAX donde BIASMAX es el
% maximo BIAS que puede alcanzar un pron髎tico y que es 1/P(de ocurrencia).
% El area se pesa por el BIASMAX de forma que sea comparable entre
% distintos umbrales y diferentes reg韒enes de precipitaci髇.
%**************************************************************************


%***********************************************************************
%Vamos a calcular para distintos umbrales, como var铆a el coeficiente de
%acierto y el de falsa alarma en funcion del umbral de probabilidad
%utilizado para la toma de decisi贸n en la detecci贸n del fenomeno.
   
   for i_umb=1:length(umbral)               %Loop sobre los umbrales
       for iprob=1:length(prob_umb)     %Loop sobre las probabilidades.
%Para cada umbral de probabilidad calculamos los aciertos y las falsas
%alarmas, eso es lo que podemos plotear en un diagrama ROC.
           
    i_hits=find(obs >= umbral(i_umb) & prob(:,i_umb) >= prob_umb(iprob)); 
    i_total=find(obs >= umbral(i_umb) & prob(:,i_umb) >= 0);
    i_total_total=find(obs >= 0 & prob(:,i_umb) >= 0);
    if(i_total_total >0)
    clim_prob(i_umb)=length(i_total)/length(i_total_total);
    else
    clim_prob(i_umb)=NaN;
    end
    prob_lluvia(i_umb)=length(i_total)/length(i_total_total);
    hit_num(iprob,i_umb)=length(i_hits);
    if(length(i_total) > 0)
    hit(iprob,i_umb)=length(i_hits)/length(i_total);
    else
    hit(iprob,i_umb)=NaN;
    end
    i_false=find( obs < umbral(i_umb) & prob(:,i_umb) >= prob_umb(iprob));
    false_num(iprob,i_umb)=length(i_false);
    misses_num(iprob,i_umb)=length(i_total)-length(i_hits);
    if(length(i_hits)+length(i_false) > 0)
        %FAR falsa_alarma/(numero total de veces que no llovio).
        %Cual es la probabilidad de que pronostique lluvia dado que no
        %llueve.
    far(iprob,i_umb)=length(i_false)/(length(i_total_total)-length(i_total));
    else
    far(iprob,i_umb)=NaN;
    end
    num(iprob,i_umb)=(length(i_hits)+length(i_false));
    clear i_hits i_total i_false 
           
                      
       end
   end

        
%**************************************************************************   
%Defino una medida global del desempe帽o que ser铆a la suma de las
%diferencias entre la curva asociada a mi umbral de precipitaci贸n y la
%curva de no-skill.
%**************************************************************************
 
%Si hay menos de 10 pron贸sticos para una categoria los pongo como NaN.
%La idea de esto es calcular el area con valores m谩s o menos estables.

hit(find(num <= 10))=NaN;
far(find(num <= 10))=NaN;

%Calculo el 醨ea asociada al ROC.
for i_umb=1:length(umbral)
       %para cada umbral me genero un vector de hit y far sacando los NaN.
       aux=squeeze(hit(:,i_umb));
       aux2=squeeze(far(:,i_umb));
       hit_aux=aux( find( isnan(aux) == 0 ) );
       far_aux=aux2( find( isnan(aux2) == 0 ) );
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
%Calculo tambien otros par谩metros como el BIAS y el ETS en funci贸n del
%umbral de probabilidad seleccionado.
%**************************************************************************


for i_umb=1:length(umbral)
    
    for iprob=1:length(prob_umb) 
    
    %ETS equitable threat score 
    %ETS=( hits - hitsrandom )/ (hits + misses + false alarms - hitsrandom)
    %hitsrandom = (hits+misses)*(hits+false alarms)/(total)
    
    hits_random(iprob,i_umb)=(hit_num(iprob,i_umb) + misses_num(iprob,i_umb))*( hit_num(iprob,i_umb) + false_num(iprob,i_umb) )./length(i_total_total);
    ets(iprob,i_umb)=(hit_num(iprob,i_umb)-hits_random(iprob,i_umb))/(hit_num(iprob,i_umb) +  misses_num(iprob,i_umb) + false_num(iprob,i_umb) - hits_random(iprob,i_umb) ); 
    
    end
end

% obs es un vector columna con las observaciones.
% forecast es una matriz. Cada columna es un pron贸stico distinto (un
% miembro del ensemble).

for i_umb=1:length(umbral)
    for iprob=1:length(prob_umb)

%Calculo los puntos donde llovio mas que el umbral
iarea_obs=find(obs >= umbral(i_umb) & prob(:,i_umb) >= 0); %me aseguro que no sean NaN.
%Calculo los puntos donde al prono le llovio mas que el umbral
iarea_for=find(prob(:,i_umb) >= prob_umb(iprob));
bias(iprob,i_umb)=length(iarea_for)/length(iarea_obs);
clear iarea_obs iarea_for bias_area
    end
end
%**************************************************************************

%Busco los casos en los que tengo pocos datos y para esos casos pongo el
%ETS y el BIAS como NAN antes de calcular el 醨ea y hacer los graficos.

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





