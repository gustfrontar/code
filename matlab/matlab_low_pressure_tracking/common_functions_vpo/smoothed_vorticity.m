
function [vorticity]=smoothed_vorticity(geopotential,lat,lon,tlow,tmax,lat_filt,lon_filt)

%Funcion que realiza un filtrado global de un campo utilizando armonicos
%esfericos. 
%El campo debe estar en una reticula regular.
% grid : es el campo a filtrar.
% lat  : es un vector conteniendo las latitudes.
% lon  : es un vector conteniendo las longitudes.
% lat_filt : es un vector conteniendo las latitudes en donde se generara el
% campo filtrado.
% lon_filt : es un vector conteniendo las longitudes donde se generara el
% campo filtrado. Si no se desea cambiar de reticula, lat_filt y lon_filt
% deben setearse iguales a lat y lon.
% tlow, tmax son los niveles de truncado que se van a retener en la
% solucion final. Por ejemplo de T5 a T42 retiene todas las ondas entre T5
% y T42 (es como un filtro pasabanda).


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

  geopotential=interp2(lon',lat,geopotential,lon',latg);
  
  
%calculo el n y m de los armonicos de acuerdo con el truncado solicitado.
nar=1;  
for in=0:tmax
     for im=0:in
         if( in + im > tlow)
         m(nar)=im;
         n(nar)=in;
         nar=nar+1;
         end
     end 
end
 


%Calculamos la transformada y anti transformada en la reticula original.
[spec_hgt]=grid_to_spec(geopotential,m,n,lon,latg);

spec_vort=NaN(size(spec_hgt));
for ii=1:length(spec_hgt)
 spec_vort(ii)=spec_hgt(ii)*(-n(ii)*(n(ii)+1))/6357000^2; 
end


[vorticity]=spec_to_grid(spec_vort,m,n,lon_filt,lat_filt);

return





