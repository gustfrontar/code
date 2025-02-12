%**************************************************************************
%       ESTA FUNCION CALCULA LAS CURVAS NECESARIAS PARA CONSTRUIR UN
%       DIAGRAMA ROC (BASICAMENTE ACIERTOS Y FALSAS ALARMAS EN FUNCION DEL
%       UMBRAL DE PROBABILIDAD SELECCIONADO.
%**************************************************************************
function [hit far area ets bias area_2] = roc_fun2(obs,ens,umbral)
%**************************************************************************
% Obs es un vector con observaciones de precipitacion.
% ens es una matriz en donde cada columna representa un pronÛstico de un
% miembro del ensemble. Las filas de ens se deben corresponder con las
% filas de obs (o con cada elemento de obs).
% 
% Las categorias con menos de 10 pron√≥sticos se ponen como NaN.
% El area calculada del ROC asume que los extremos de la curva est√°n en 1,1
% 0,0.
% La idea de este script va a ser tomar el pronÛstico determinÌstico y
% variar el BIAS del mismo para cada umbral (esto se hace para cada miembro
% del ensemble). Esto se logra, variando el umbral del pronÛstico que vamos
% a usar para predecir un umbral de la observaciÛn. Por ejemplo si queremos
% pronosticar lluvias por encima de 2mm vamos a usar los pronÛsticos de
% lluvia por encima de 1, 2, 5, 10, etc como predictores de la lluvia por
% encima de 2 mm y a cada uno de esos elementos se le asignar· una hit rate
%, una falsa alarma, un ETS y un BIAS. De esta forma obtenemos una curva
%HIT vs FAR, ETS vs BIAS de manera an·loga a lo que se obtiene con el
%pronÛstico probabilistico, pero solo que en este caso solo "calibrando" el
% BIAS de un pronÛstico determinÌstico. El pronÛstico probabilÌstico para
% ser realmente ˙til deberia al menos lograr superar este procedimiento que
% no requiere de un costo computacional mayor a una sola realizacion del
% ensemble. Otra forma de pensar este procedimiento es como un pronÛstico
% probabilÌstico en donde la probabilidad de ocurrencia, es proporcional a
% cuanto exede la precipitaciÛn pronosticada al umbral seleccionado.
% Area 2 es el area del diagrama ETS-BIAS normalizada. Esto es el area bajo
% la curva de ETS entre el valor BIAS=0 y BIAS=BIASMAX donde BIASMAX es el
% maximo BIAS que puede alcanzar un pronÛstico y que es 1/P(de ocurrencia).
% El area se pesa por el BIASMAX de forma que sea comparable entre
% distintos umbrales y diferentes regÌmenes de precipitaciÛn.

% hit es una matriz en donde la primera dimension varÌa sobre los distintos
% BIAS, la segunda los dinstintos umbrales de pp observada y la tercera
% sobre los distintos miembros del ensemble. Es decir que para cada k (de
% la tercera dimensiÛn) es una matriz donde cada fila nos da el acierto
% para los distintos valores de BIAS. Las demas variables se ordenan de la
% misma forma.
%**************************************************************************


%***********************************************************************
%A diferencia de la versiÛn probabilistica del ROC, aca el do no se hace
%sobre
[filas n_ens]=size(ens)  %Obtengo el numero de miembros del ensemble.

umbral2=[0.01 0.05 0.10 0.15 0.20 0.25 0.30 0.4 0.5 0.6 0.7 0.8 0.9 1 1.5 2]*25.4;; %Estos son los umbrales que voy a usar para modificar el BIAS del pronostico deterministico.
 
for i_ens=1:n_ens
   for i_umb=1:length(umbral)               %Loop sobre los umbrales
       for i_umb2=1:length(umbral2)         %Loop sobre la calibracion del BIAS.
%Para cada umbral de probabilidad calculamos los aciertos y las falsas
%alarmas, eso es lo que podemos plotear en un diagrama ROC.
           
    i_hits=find(obs >= umbral(i_umb) & ens(:,i_ens) >= umbral2(i_umb2)); 
    i_total=find(obs >= umbral(i_umb)& ens(:,i_ens) >= 0);
    i_total_total=find(obs >= 0 & ens(:,i_ens) >= 0);
    n_total(i_ens)=length(i_total_total);
    hit_num(i_umb2,i_umb,i_ens)=length(i_hits);
    
    if(i_total_total >0)
    clim_prob(i_umb)=length(i_total)/length(i_total_total);
    else
    clim_prob(i_umb)=NaN;
    end
    
    if(length(i_total) > 0)
    hit(i_umb2,i_umb,i_ens)=length(i_hits)/length(i_total);
    else
    hit(i_umb2,i_umb,i_ens)=NaN;
    end
    i_false=find( obs < umbral(i_umb) & ens(:,i_ens) >= umbral2(i_umb2));
    false_num(i_umb2,i_umb,i_ens)=length(i_false);
    
    misses_num(i_umb2,i_umb,i_ens)=length(i_total)-length(i_hits);
    
    if(n_total(i_ens)-length(i_total))
        %FAR falsa_alarma/(numero total de veces que no llovio).
        %Cual es la probabilidad de que pronostique lluvia dado que no
        %llueve
    far(i_umb2,i_umb,i_ens)=length(i_false)/(n_total(i_ens)-length(i_total));
    else
    far(i_umb2,i_umb,i_ens)=NaN;
    end
    num(i_umb2,i_umb,i_ens)=(length(i_hits)+length(i_false));
    clear i_hits i_total i_false i_total_total
           
                      
       end
   end
end

        
%**************************************************************************   
%Defino una medida global del desempe√±o que ser√≠a la suma de las
%diferencias entre la curva asociada a mi umbral de precipitaci√≥n y la
%curva de no-skill.
%**************************************************************************
 
%Si hay menos de 10 pron√≥sticos para una categoria los pongo como NaN.
%La idea de esto es calcular el area con valores m√°s o menos estables.

hit(find(num <= 10))=NaN;
far(find(num <= 10))=NaN;

%Calculo el ·rea asociada al ROC.
for i_ens=1:n_ens
for i_umb=1:length(umbral)
       %para cada umbral me genero un vector de hit y far sacando los NaN.
       aux=squeeze(hit(:,i_umb,i_ens));
       aux2=squeeze(far(:,i_umb,i_ens));
       hit_aux=aux( find( isnan(aux) == 0 ) );
       far_aux=aux2( find( isnan(aux2) == 0 ) );
       %Les agrego los extremos 0,0 y 1,1.
       hit_aux=[1 hit_aux' 0];
       far_aux=[1 far_aux' 0];
       %Ahora integro el area bajo la curva.
       area(i_umb,i_ens)=0;
       for i=1:length(hit_aux)-1
           area(i_umb,i_ens)=area(i_umb,i_ens)+(far_aux(i)-far_aux(i+1))*(hit_aux(i+1)+hit_aux(i))/2;    
       end
       %El area ahora es una matrÌz donde cada fila representa el area para
       %el umbral i para los distintos miembros del ensemble.
       %area(i_umb,i_ens)=area(i_umb,i_ens);
       if(area(i_umb,i_ens)==0)
           area(i_umb,i_ens)=NaN;
       end
       clear hit_aux far_aux aux aux2
end
end
   
%**************************************************************************
%Calculo tambien otros par√°metros como el BIAS y el ETS en funci√≥n de la
%modificaciÛn del BIAS.
%**************************************************************************

for i_ens=1:n_ens
for i_umb=1:length(umbral) 
    for i_umb2=1:length(umbral2) 
    
    %ETS equitable threat score 
    %ETS=( hits - hitsrandom )/ (hits + misses + false alarms - hitsrandom)
    %hitsrandom = (hits+misses)*(hits+false alarms)/(total)
    if(n_total(i_ens)>0)
    hits_random(i_umb2,i_umb,i_ens)=(hit_num(i_umb2,i_umb,i_ens) + misses_num(i_umb2,i_umb,i_ens))*( hit_num(i_umb2,i_umb,i_ens) + false_num(i_umb2,i_umb,i_ens) )./n_total(i_ens);

    ets(i_umb2,i_umb,i_ens)=(hit_num(i_umb2,i_umb,i_ens)-hits_random(i_umb2,i_umb,i_ens))/(hit_num(i_umb2,i_umb,i_ens) +  misses_num(i_umb2,i_umb,i_ens) + false_num(i_umb2,i_umb,i_ens) - hits_random(i_umb2,i_umb,i_ens) ); 
    else 
    ets(i_umb2,i_umb,i_ens)=NaN;
    end
    
    end
end
end

for i_ens=1:n_ens
for i_umb=1:length(umbral)
    for i_umb2=1:length(umbral2)

%Calculo los puntos donde llovio mas que el umbral
iarea_obs=find(obs >= umbral(i_umb) & ens(:,i_ens) >= 0); %me aseguro que no sean NaN.
%Calculo los puntos donde al prono le llovio mas que el umbral
iarea_for=find(ens(:,i_ens) >= umbral2(i_umb2) & obs >= 0);
if(length(iarea_obs) > 0)
    bias(i_umb2,i_umb,i_ens)=length(iarea_for)/length(iarea_obs);
else
    bias(i_umb2,i_umb,i_ens)=NaN;
end
clear iarea_obs iarea_for bias_area
    end
end
end
%**************************************************************************

%Busco los casos en los que tengo pocos datos y para esos casos pongo el
%ETS y el BIAS como NAN antes de calcular el ·rea y hacer los graficos.

ets(find(num <= 10))=NaN;
bias(find(num <= 10))=NaN;


%Calculo el area asociada al BIAS-ETS.
for i_ens=1:n_ens
for i_umb=1:length(umbral)
       bias_max(i_umb)=1/clim_prob(i_umb);
       %para cada umbral me genero un vector de ETS y BIAS sacando los NaN.
       aux=squeeze(ets(:,i_umb,i_ens));
       aux2=squeeze(bias(:,i_umb,i_ens));
       ets_aux=aux( find( isnan(aux) == 0 ) );
       bias_aux=aux2( find( isnan(aux2) == 0 ) );
       %Les agrego los extremos 0,0 y 1,1.
       ets_aux=[0 ets_aux' 0];
       bias_aux=[bias_max(i_umb) bias_aux' 0];
       %Ahora integro el area bajo la curva.
       area_2(i_umb,i_ens)=0;
       for i=1:length(ets_aux)-1
           area_2(i_umb,i_ens)=area_2(i_umb,i_ens)+(bias_aux(i)-bias_aux(i+1))*(ets_aux(i+1)+ets_aux(i))/2;    
       end
       area_2(i_umb,i_ens)=area_2(i_umb,i_ens)/bias_max(i_umb); %Normalizo por el BIAS maximo.
       if(area_2(i_umb,i_ens)==0)
           area_2(i_umb,i_ens)=NaN
       end
       clear bias_aux ets_aux aux aux2 
end
end





