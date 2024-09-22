function [cost_function initialindex]...
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


[intersection,index1,index2]=intersect(times1,times2);

%CRITERIO DE NO INTERSECCION TEMPORAL.
if(isempty(intersection))
    %Si no hay interseccion temporal hago una salida rapida.
    return
end

%CALCULO LAS FUNCIONES DE COSTO DE SUPERPOSICION.

total_length1=length(lon1);
total_length2=length(lon2);
inter_length=length(intersection);


lon1=lon1(index1);
lat1=lat1(index1);
lon2=lon2(index2);
lat2=lat2(index2);



if(abs(mean(lat1)-mean(lat2)) > 20)
   %Test rapido para saber si las trayectorias estan cerca o no.
  return 
end

if(inter_length/total_length2 < 0.4);
    %Menos del 40 % de la trayectoria esta contenida en el grupo. Entonces
    %por ahora no la asociamos.
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
    %Me fijo si este es el primer punto de la trayectoria que es cercano.
    %Si asocio la trayectoria lo voy a hacer de este punto en adelante.
    %Todos los puntos que no sean cercanos en tiempos previos no los voy a
    %tener en cuenta.
    if(~cercano && dist(ii) < maxdist)
        if(ii > 1)
        initialindex=index2(ii); 
        cercano=true;
        else
        %Si el punto que se asocia mejor esta al principio de la
        %interseccion entre la trayectoria y el grupo entonces asocio la
        %trayectoria desde el principio. (sino los grupos nunca pueden
        %crecer hacia "atras")
        initialindex=1;
        cercano=true;
        end
    end
    
end


npuntos=sum(dist<maxdist);

if(npuntos == 0)
    return
end

%El primer termino mide la distancia, mientras que el segundo cuanto tiempo
%de la trayectoria esta cerca del grupo con respecto al total del tiempo en
%el que ambos se superponen.
%Por ahora la distancia maxima es independiente del tiempo.

cost_function=(mean(dist(dist<maxdist))/maxdist)*sqrt(inter_length/npuntos)*sqrt(0.5*(total_length1+total_length2)/(2*(inter_length-initialindex+1)));


                                                 
