%==================================================
% ESTA FUNCION PASA DE LOS COEFICIENTES DE UN CAMPO
% EN ARMONICOS ESFERICOS A OBTENER LOS VALORES EN UNA
% RETICULA DE LAT Y LON REGULAR.
% INPUTS
% SPEC= SON LOS COEFICIENTES ESPECTRALES COMPLEJOS.
% M = SON LOS VALORES DE M CORRESPONDIENTES A CADA 
% COMPONENTE ESPECTRAL.
% N = SON LOS VALORES DE N (GRADO) CORRESPONDIENTES 
% A CADA POMPONENTE ESPECTRAL.
% LON = ES UN VECTOR CON LAS LONGITUDES DE LA RETICULA.
% LAT = ES UN VECTOR CON LAS LATITUDES DE LA RETICULA.
%==================================================

function [field]=spec_to_grid(spec,m,n,lon,lat)

if(size(lon,2)==1)
    lon=lon';
end
if(size(lat,2)==1)
    lat=lat';
end

%Compute phy and theta.
phy=(lon-180)*2*pi/360;
theta=(lat)*2*pi/360;
sintheta=sin(theta);


max_n=max(n);

field=zeros(length(lat),length(lon));

for in=0:max_n;
    tmp=legendre(in,sintheta,'norm');
    for im=0:in
        tmp2=tmp(im+1,:)*((-1)^im)*sqrt(1/4); %La normalizacion que usa el NCEP no es ningua de las tradicionales.;
        ymn=repmat(tmp2',[1 length(phy)]).*repmat(exp(1i*im*phy),[length(theta) 1]); 
        index=find(n==in & m==im);
        if(~isempty(index))
        if(im~=0)     
        field=field+spec(index)*ymn+conj(spec(index))*conj(ymn); 
        else
        field=field+spec(index)*ymn;   
        end
        end
    end   
end

return