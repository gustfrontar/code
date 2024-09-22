
function [cost_function time_cost_function cost_function_gsuperposition cost_function_tsuperposition]=...
traj_cost_function_fun(lon1,lat1,times1,length1,lon2,lat2,times2,initime)



cost_function=1e10;
deg2km=111000;

index_intersection=( ~isnan(lon1) & ~isnan(lon2));

if(abs(mean(lat2-lat1)) > 20)
    %Test rapido para saber si las trayectorias estan cerca o no.
   return 
end



total_length1=sum(~isnan(lon1));
total_length2=sum(~isnan(lon2));
inter_length=sum(index_intersection);

times=1:length(lon1);

lon1=lon1(index_intersection);
lat1=lat1(index_intersection);
lon2=lon2(index_intersection);
lat2=lat2(index_intersection);

if(sum(index_intersection)/total_length2 < 0.4);
    %Menos del 40 % de la trayectoria esta contenida en el grupo. Entonces
    %por ahora no la asociamos.
    return   
end

%Sobre los puntos de la interseccion vamos a calcular el umbral de
%distancia. Quiero saber cuantos puntos de la trayectoria estan a una
%distancia menor que el umbral. 

dist=NaN(1,length(lon1));
for ii=1:length(lon1)
    dist(ii)=distll_fun(lon1,lat1,lon2,lat2);  
end

npuntos=sum(dist<maxdist);

if(npuntos == 0)
    return
end

%El primer termino mide la distancia, mientras que el segundo cuanto tiempo
%de la trayectoria esta cerca del grupo con respecto al total del tiempo en
%el que ambos se superponen.
%Por ahora la distancia maxima es independiente del tiempo.
cost_function=(mean(dist(dist<maxdist))/maxdist)*sqrt(inter_length/npuntos);




end







