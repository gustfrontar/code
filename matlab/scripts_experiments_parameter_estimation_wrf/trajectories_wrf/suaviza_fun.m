% GENERA ENSAMBLE FUN:
% ESTA FUNCION CREA UN ENSAMBLE A PARTIR DE UNA SOLA SIMULACION, GENERANDO
% CORRIMIENTOS DE N CANTIDAD DE PUNTOS EN TODAS LAS DIRECCIONES.

%Esta version aplica un suavizado pesado, en donde el aporte de cada
%miembro a la media esta pesado por la distancia.

function [campo_suavizado]=suaviza_fun(campo,n)

%campo es una matriz de nx x ny puntos y n es la cantidad de corrimientos
%que vamos a hacer en todas las direcciones.

[nx ny nz]=size(campo);

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
%W=W/sum(sum(W));
%a=size(campo)
auxiliar=zeros(size(campo));
%     
% 
 for j=-n:n
     for i=-n:n
       if W(i+n+1,j+n+1) > 0
         if(i <= 0)
           imax=nx;imin=1-i;
         else
           imax=nx-i;imin=1;
         end
         if(j <= 0)
           jmax=ny;jmin=1-j;
         else
           jmax=ny-j;jmin=1;
         end
    
         campo_suavizado(imin:imax,jmin:jmax,:)=W(i+n+1,j+n+1)*campo(imin+i:imax+i,jmin+j:jmax+j,:)+campo_suavizado(imin:imax,jmin:jmax,:); 
         
         auxiliar(imin:imax,jmin:jmax,:)=auxiliar(imin:imax,jmin:jmax,:)+W(i+n+1,j+n+1);
       end
     end
 end
% 
 campo_suavizado=campo_suavizado./auxiliar;
% 
% %campo_suavizado=campo_suavizado/((2*n+1)^2);
% 
  end
