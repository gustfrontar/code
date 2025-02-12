% GENERA ENSAMBLE FUN:
% ESTA FUNCION CREA UN ENSAMBLE A PARTIR DE UNA SOLA SIMULACION, GENERANDO
% CORRIMIENTOS DE N CANTIDAD DE PUNTOS EN TODAS LAS DIRECCIONES.

%Esta version aplica un suavizado pesado, en donde el aporte de cada
%miembro a la media esta pesado por la distancia.

function [campo_suavizado]=suavizaw_fun(campo,n)

%campo es una matriz de nx x ny puntos y n es la cantidad de corrimientos
%que vamos a hacer en todas las direcciones.

[nx ny]=size(campo);

campo_suavizado=zeros(size(campo));

if n==0
    campo_suavizado=campo;
    
else
    
%Calculo los pesos.

W=zeros(2*n+1,2*n+1);
for i=1:2*n+1;
    for j=1:2*n+1;
        dist=sqrt((i-n-1)^2+(j-n-1)^2);
        if(dist> n);
        W(i,j)=0;
        else
        W(i,j)=1;
        end
    end
end
W=W/sum(sum(W));


    

for j=-n:n
    for i=-n:n
        campo_suavizado(n+1:nx-n,n+1:ny-n)=W(i+n+1,j+n+1)*campo(n+i+1:nx-n+i,n+j+1:ny-n+j)+campo_suavizado(n+1:nx-n,n+1:ny-n); 
    end
end

%campo_suavizado=campo_suavizado/((2*n+1)^2);

end
