%==================================================
% ESTA FUNCION PASA DE UN CAMPO
% A ARMONICOS ESFERICOS.
% INPUTS
% GRID= ES EL VALOR DEL CAMPO EN LA RETICULA REGULAR DEFINIDA
% POR LOS VECTORES LON Y LAT.

% LON = ES UN VECTOR CON LAS LONGITUDES DE LA RETICULA.
% LAT = ES UN VECTOR CON LAS LATITUDES DE LA RETICULA.
%==================================================

function [spec]=grid_to_spec(grid,m,n,lon,lat)

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

[phy_grid theta_grid]=meshgrid(phy,theta);
deltaphy=abs(phy(2)-phy(1));
deltatheta=abs(diff(theta));
deltatheta=interp1(0.5*(theta(1:end-1)+theta(2:end)),deltatheta,theta,'linear','extrap');
deltatheta_grid=repmat(deltatheta',[1 length(phy)]);

max_n=max(n);

spec=NaN(size(n));
for in=0:max_n;
    tmp=legendre(in,sintheta,'norm');
    for im=0:in
        index=find(n==in & m==im);
        if(~isempty(index))
        %Compute ymn over the grid.
        tmp2=tmp(im+1,:)*((-1)^im)*sqrt(1/4);
        ymn=repmat(tmp2',[1 length(phy)]).*repmat(exp(1i*im*phy),[length(theta) 1]);
        cymn=conj(ymn);

        %compute the integral.
        spec(index)=double((2/pi)*deltaphy)*sum(sum(double(cymn.*grid.*cos(theta_grid).*deltatheta_grid),'double'),'double');
        
        if(im~=0)
          grid=grid-spec(index)*ymn-conj(spec(index))*conj(ymn);
        else
          grid=grid-spec(index)*ymn; 
        end
        
        end
    end   
end

return