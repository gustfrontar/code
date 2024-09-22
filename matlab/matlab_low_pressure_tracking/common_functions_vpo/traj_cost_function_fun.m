
function [cost_function time_cost_function cost_function_gsuperposition cost_function_tsuperposition]=...
traj_cost_function_fun(lon1,lat1,times1,length1,lon2,lat2,times2,initime)
%Esta funcion calcula la distancia media entre 2 trayectorias.
%Datas dos trayectorias (1) y (2), primero se verifica si hay superposicion
%temporal (aunque sea un punto). Siempre y cuando la coincidencia sea de
%al menos el 50% del largo de la trayectoria mas corta. (para evitar
%asociar trayectorias largas que solo se tocan en un punto o cosas por el
%estilo).

%Luego se calcula la distancia media de los primeros 4 puntos de
%superposicion temporal (o si alguna de las trayectorias es mas corta...se
%toman menos puntos, no hay limite en cuanto a cuan corta pueda ser una de
%las trayectorias en tanto y en cuanto cumpla con el criterio de cercania.

%Los resultados son los siguientes.

%cost_function: funcion de costo de la distancia cuan lejos o cerca estan
%los puntos de la trayectoria y del grupo tomando solamente los primeros 4
%puntos del subconjunto de superposicion entre ambos. Cuanto mas me alejo
%del tiempo inicial initime, esta cost_function se achica (a igual
%distancia, si el tiempo esta muy lejos del tiempo inicial la funcion de
%costo resultara menor dando mayor chance de que los sistemas lejanos se
%asocien entre si a medida que avanza el plazo de pronostico).

%time_cost_function: cuan lejos o cerca (en horas al cuadrado) estan el
%origen del grupo y el origen de la treyectoria.

%cost_function_gsuperposition: que porcentaje de los puntos del grupo
%coinciden con la trayectoria.

%cost_function_tsuperposition: que porcentaje de los puntos de la
%trayectoria coinciden con los puntos del grupo. (es similar al anterior
%pero desde el punto de vista de la trayectoria).

mean_length=6;         %Sobre cuantos puntos vamos a calcular la distancia media.
MaxDistance=4.0e6;       %Si la distancia entre los primeros 2 puntos de las trayectorias es mayor que este valor no se calculan los demas.

%Inicialmente asumimos que la trayectoria esta infinitamente lejos del
%grupo. (es un grupo muy ortiva...)
time_cost_function=Inf;
cost_function=Inf; 
cost_function_gsuperposition=0;
cost_function_tsuperposition=0;
%Usamos la funcion intersec para evaluar la superposicion temporal.

%Calculamos la media del grupo y en funcion de eso calculamos la
%superposicion temporal.

if(isempty(lon2) | isempty(lat2) | isempty(lon1) | isempty(lat1))
    %Si alguno de los inputs esta vacio (estoy puede pasar con trayectorias
    %que se han "fusionado" con otras trayectorias, en ese caso salgo de la
    %funcion con los valores por defecto para las funciones de costo.
    return
end


[intersection, index1,index2]=intersect(times1,times2);

%CRITERIO DE NO INTERSECCION TEMPORAL.
if(isempty(intersection))
    %Si no hay interseccion temporal hago una salida rapida.
    return
end

%CRITERIO DE INTERSECCION TEMPORAL DE AL MENOS 50% PARA LA TRAYECTORIA MAS
%CORTA.
%length1=length1;  %Para el grupo uso la longitud media y no la longitud maxima.
%Length1 viene dada de afuera asi puedo usar la longitud media del grupo.
%La longitud de lat trayectoria 2 (la que aplica para pertenecer al grupo)
%la calculo directamente.
length2=length(lon2);
lengthint=length(intersection);

%Esta parte comentada ya esta cubierta por los criterios que agregue
%afuera.
%min_length=min([length1 length2]);

%if( lengthint < 0.5*min_length)
%    %Si la trayectoria mas corta, solo coincide en un 50 % con la mas larga
%    %entonces salimos de la funcion, no hay suficiente coincidencia.
%    return
%end

%CALCULO LAS FUNCIONES DE COSTO DE SUPERPOSICION.

cost_function_gsuperposition=lengthint/length1;
cost_function_tsuperposition=lengthint/length2;


%CALCULO LAS DISTANCIAS ENTRE LOS PRIMEROS 4 PUNTOS.
if(lengthint  < mean_length)
    
   mean_length=lengthint;
    
end


tmp_dist=distll_fun(lon1(index1(1)),lat1(index1(1)),lon2(index2(1)),lat2(index2(1))); %*1.5;
%tmp_dist=sqrt(((lat1(index1(1))-lat2(index2(1))*111000)^2+(diff_lon_fun(lon1(index1(1)),lon2(index2(1)))*cosd(0.5*(lat2(index1(1)+lat2(index2(1))))*111000)^2);
           

if(tmp_dist > MaxDistance)
    %Si la distancia entre los primeros 2 puntos es muy grande no calculo
    %el resto. Esto ahorra computos.
    return
else
    
cost_function=tmp_dist;

for itime=2:mean_length;

%Calculo las distancias sobre los primeros puntos en los que ambas trayectorias coinciden.    
%La funcion de costo tiene un coeficiente de correccion que busca castigar
%mas las distancias en los tiempos mas tempranos y menos a medida que el
%tiempo pasa.

tmp_dist=distll_fun(lon1(index1(itime)),lat1(index1(itime)),lon2(index2(itime)),lat2(index2(itime)));
%tmp_dist=sqrt(((lat1(index1(itime))-lat2(index2(itime))*111000)^2+(diff_lon_fun(lon1(index1(itime)),lon2(index2(itime)))*cosd(0.5*(lat2(index1(itime)+lat2(index2(itime))))*111000)^2);
   

cost_function=cost_function+tmp_dist; %*(1.25-0.25*(itime-1)/(mean_length-1));

end
cost_function=cost_function/mean_length;

end

time_correct_coef=1-0.2*(intersection(1)-initime)/7; %A los 7 dias de pronostico se incrementa un 20%... aprox 1100 KM. (lo que equivale llevar el umbral al doble)

%Aplico la correccion por tiempo a la funcion de costo.
cost_function=cost_function*time_correct_coef;

%CALCULO LA FUNCION DE COSTO TEMPORAL.
%Cuanto difieren entre si en tiempo el inicio del grupo y de las
%trayectorias.

%DIVIDO LA DISTANCIA EN TIEMPO POR LA MAXIMA DISTANCIA EN TIEMPO QUE PUEDO
%TENER Y LE CALCULO LA RAIZ PARA ATENUAR UN POCO EL EFECTO. 
%time_cost_function=(0.5+0.5*sqrt(abs(times1(1)-times2(1))/29)); 

%Incorporo el tiempo en la funcion de costo para dar mas ventaja a las
%trayectorias que estan mas cerca del origen del grupo. 
%cost_function=cost_function*time_cost_function;

end







