function [cost_function]=traj_cost_function_2(lon1,lat1,times1,lon2,lat2,times2)

%VAMOS A IMPLEMENTAR LA FUNCION DE COSTO DE BLENDER AND SCHUBERT 2000.
cost_function=1e10;

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
if(lat1(index1(1))-lat2(index2(2)) > 20)
    %En el primer punto temporal en el que coinciden las trayectorias estan
    %muy lejos.
    return
end


beta=100;  %Valores de alfa y beta para la combinacion de la distancia en tiempo y espacio.
alfa=1;  

A1=(times1(end)-times1(1))*86400;   %Calculo el largo de la trayectoria 1 en segundos.
A2=(times2(end)-times2(1))*86400;

tmp=0;
for ii=1:length(lon1)
    for jj=1:length(lon1)
        if( ii ~= jj)
        tmp=alfa*distll_fun(lon1(ii),lat1(ii),lon1(jj),lat1(jj))+beta*(times1(ii)-times1(jj))*86400+tmp;
        end
    end
end

SIGMA1SQ=(1/(A1^2))*(tmp);

tmp=0;
for ii=1:length(lon2)
    for jj=1:length(lon2)
        if( ii ~= jj)
        tmp=alfa*distll_fun(lon2(ii),lat2(ii),lon2(jj),lat2(jj))+beta*(times2(ii)-times2(jj))*86400+tmp;
        end
    end
end

SIGMA2SQ=(1/(A1^2))*(tmp);

tmp=0;
for ii=1:length(lon1)
    for jj=1:length(lon2)
        
        tmp=alfa*distll_fun(lon1(ii),lat1(ii),lon2(jj),lat2(jj))+beta*(times1(ii)-times2(jj))*86400+tmp;
        
    end
end

SIGMA12SQ=(1/(A2*A1))*(tmp);

cost_function=(1/(A1*A2))*(SIGMA12SQ-0.5*(SIGMA1SQ+SIGMA2SQ));


                                                 