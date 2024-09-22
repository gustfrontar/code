% Promedios moviles rapidos

%Eventualmente este filtro deberia ser reemplazado por un filtro gaussiano
%que tenga en cuenta las distancias entre los puntos que pueden ser
%variables, sobre todo en un dominio global.

function [campo_filtrado]=fast_filter_fun(campo,n)

%campo es una matriz de nx x ny puntos y n es la cantidad de corrimientos
%que vamos a hacer en X y en Y.

n2=round((n-1)/2);


[nx ny nz]=size(campo);



aux=NaN(nx,ny,2*n);


for iz=1:nz
for j=-n2:n2
        %aux(n2+1:nx-n2,n2+1:ny-n2,j+n2+1)=campo(n2+1:nx-n2,n2+j+1:ny-n2+j,iz);
        jbot=max([1 j+1]);
        jtop=min([ny ny+j]);
        jbot2=max([1 1-j]);
        jtop2=min([ny ny-j]);
        aux(:,jbot:jtop,j+n2+1)=campo(:,jbot2:jtop2,iz);
end

for i=-n2:n2
        ibot=max([1 i+1]);
        itop=min([nx nx+i]);
        ibot2=max([1 1-i]);
        itop2=min([nx nx-i]);
        aux(ibot:itop,:,n+i+n2+1)=campo(ibot2:itop2,:,iz);
        %aux(n2+1:nx-n2,n2+1:ny-n2,n+i+n2+1)=campo(n2+i+1:nx-n2+i,n2+1:ny-n2,iz);    
end

campo_filtrado(:,:,iz)=nanmean(aux,3);
end







