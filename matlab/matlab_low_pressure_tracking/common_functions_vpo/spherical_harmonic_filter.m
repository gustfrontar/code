
function [filtered_grid]=spherical_harmonic_filter(gridtmp,lat,lon,truncation,lat_filt,lon_filt)

%Funcion que realiza un filtrado global de un campo utilizando armonicos
%esfericos. 
%El campo debe estar en una reticula regular.
% grid : es el campo a filtrar.
% lat  : es un vector conteniendo las latitudes.
% lon  : es un vector conteniendo las longitudes.
% truncation: es el nivel de truncado triangular deseado (T62, T35 etc).
% lat_filt : es un vector conteniendo las latitudes en donde se generara el
% campo filtrado.
% lon_filt : es un vector conteniendo las longitudes donde se generara el
% campo filtrado. Si no se desea cambiar de reticula, lat_filt y lon_filt
% deben setearse iguales a lat y lon.


if(size(lon,2)==1)
    lon=lon';
end
if(size(lat,2)==1)
    lat=lat';
end
if(size(lon_filt,2)==1)
    lon_filt=lon_filt';
end
if(size(lat_filt,2)==1)
    lat_filt=lat_filt';
end


%Primero, interpolamos el campo original a una reticula gaussiana para que
%la exactitud de la conversion sea mayor.

if(mod(length(lat),2)==0)
    nlatg=length(lat);
else
    nlatg=length(lat)+1;
end

 [latg]=gauss2lats(nlatg);
 latg=[-latg latg(end:-1:1)];
 
  gridtmp=interp2(lon',lat,gridtmp,lon',latg);
  
  
%calculo el n y m de los armonicos de acuerdo con el truncado solicitado.
nar=1;  
for in=0:truncation
     for im=0:in
         m(nar)=im;
         n(nar)=in;
         nar=nar+1;
     end 
end
 
%Calculamos la transformada y anti transformada en la reticula original.
[spec]=grid_to_spec(gridtmp,m,n,lon,latg);
[filtered_grid]=spec_to_grid(spec,m,n,lon_filt,lat_filt);

return





