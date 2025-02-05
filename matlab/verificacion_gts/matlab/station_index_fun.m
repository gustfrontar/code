function [INDICE ESTACIONES_UTILES]=station_index_fun(ESTACIONES,lat_wrf,lon_wrf,mask_wrf,umbral_distancia)

%Estaa funcion ingresa una array ESTACIONES que tiene 4 columnas Numero de
%estacion, lat_est, lon_est y altura de la estacion y 3 matrices una con
%las latitudes de la reticula del WRF, otra con las longitudes y otra con
%la mascara que indica tierra y mar (mar=2, tierra=1).

%La idea es que la funcion devuelve los indices de la matriz de lat y lon
%del wrf que corresponden a los puntos mas cercanos de las estaciones
%listadas en estaciones. Tambie devuelve un subconjunto del array
%ESTACIONES con las estaciones que estan cerca del dominio WRF y las que
%estan en el "continente" de acuerdo a la costa del modelo.
%Umbral distancia marca la maxima distancia en grados hasta donde una
%estacion se va a considerar cercana a un punto de reticula dado. Esto
%sirve nada mas que para las estaciones que estan fuera del dominio pero
%cerca del borde.
%Estaciones utiles es un array logico que permite filtrar las observaciones
%y el array estaciones quedandonos solamente con las estaciones que estan
%dentro del continente del modelo.

nest=length(ESTACIONES(:,1)); %Cantidad de estaciones que tengo.

lat_est=ESTACIONES(:,2);
lon_est=ESTACIONES(:,3);

%Voy a encontrar el I y el J de la matriz del WRF correspondiente al punto
%mas cercano a cada estacion.
for iest=1:nest
    
    distancia=((lat_wrf-lat_est(iest)).^2+(lon_wrf-lon_est(iest)).^2).^0.5;
    mindist=min(min(distancia));
    
   if(mindist < umbral_distancia)  
   %Indice me da el indice en la matriz del WRF del punto mas cercano a
   %cada estacion.
   INDICE(iest)=find(distancia==mindist);
   if(mask_wrf(INDICE(iest))==0)
       %Rechazo la estacion porque el punto del modelo mas cercano esta en
       %el mar.
       %[ESTACIONES(iest,2:3) lat_wrf(INDICE(iest)) lon_wrf(INDICE(iest))]

       INDICE(iest)=NaN;
   end
   else
   INDICE(iest)=NaN; %Voy a descartar esta estacion.
   end   
end

%Recalculo el numero de estaciones teniendo en cuenta que algunas estan
%fuera del dominio y otras estan muy cerca de la costa.
ESTACIONES_UTILES=~isnan(INDICE);
INDICE=INDICE(ESTACIONES_UTILES);
