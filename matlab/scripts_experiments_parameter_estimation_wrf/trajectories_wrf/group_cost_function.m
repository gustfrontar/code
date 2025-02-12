function [cost_function]...
         =group_cost_function(lon1,lat1,times1,lon2,lat2,times2,maxdist)

%Inicialmente asumimos que la trayectoria esta infinitamente lejos del
%grupo. (es un grupo muy ortiva...)
cost_function=1e10;
initialindex=0;

index1=~isnan(lon1);
index2=~isnan(lon2);
lon1=lon1(index1);
lon2=lon2(index2);
lat1=lat1(index1);
lat2=lat2(index2);
times1=times1(index1);
times2=times2(index2);


%Usamos la funcion intersec para evaluar la superposicion temporal.

%Calculamos la media del grupo y en funcion de eso calculamos la
%superposicion temporal.

if(isempty(lon2) || isempty(lat2) || isempty(lon1) || isempty(lat1))
    %Si alguno de los inputs esta vacio (estoy puede pasar con trayectorias
    %que se han "fusionado" con otras trayectorias, en ese caso salgo de la
    %funcion con los valores por defecto para las funciones de costo.
    return
end

%Busco la interseccion entre ambas trayectorias.
[intersection,index1,index2]=intersect(times1,times2);

%CRITERIO DE NO INTERSECCION TEMPORAL.
if(isempty(intersection))
    %Si no hay interseccion temporal hago una salida rapida.
    return
end

%CALCULO LAS FUNCIONES DE COSTO DE SUPERPOSICION.

lon1=lon1(index1);
lat1=lat1(index1);
lon2=lon2(index2);
lat2=lat2(index2);


if(abs(mean(lat1)-mean(lat2)) > 20)
   %Test rapido para saber si las trayectorias estan cerca o no.
  return 
end


%Sobre los puntos de la interseccion vamos a calcular el umbral de
%distancia. Quiero saber cuantos puntos de la trayectoria estan a una
%distancia menor que el umbral. 

dist=NaN(1,length(lon1));
cercano=false;
for ii=1:length(lon1)
    lona=lon1(ii);
    lata=lat1(ii);
    lonb=lon2(ii);
    latb=lat2(ii);
    dist(ii)=distll_fun(lona,lata,lonb,latb); 
end


npuntos=sum(dist<maxdist);

if(npuntos == 0)
    return
end

%Tomo la distancia media.

cost_function=(mean(dist(dist<maxdist))/maxdist);


                                                 
