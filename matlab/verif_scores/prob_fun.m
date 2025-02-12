% GENERA ENSAMBLE FUN:
% ESTA FUNCION CREA UN ENSAMBLE A PARTIR DE UNA SOLA SIMULACION, GENERANDO
% CORRIMIENTOS DE N CANTIDAD DE PUNTOS EN TODAS LAS DIRECCIONES.

%Esta version aplica un suavizado pesado, en donde el aporte de cada
%miembro a la media esta pesado por la distancia.

function [prob rank enssize]=prob_fun(forecast,obs,umbral,n)

%campo es una matriz de nx x ny puntos y n es la cantidad de corrimientos
%que vamos a hacer en todas las direcciones.

[nx ny nz]=size(forecast);
numb=length(umbral);
campo=false([nx ny nz numb]);

for ii=1:numb
  campo(:,:,:,ii)=(forecast(:,:,:)>umbral(ii));
end

prob=zeros([nx ny nz numb]);
rank=zeros(size(forecast));

%Por algun motivo esta correccion no funciono
forecast=forecast+0.01*(rand(size(forecast))-0.5); 
obs=obs+0.01*(rand(size(obs))-0.5);

if n==0
    prob=double(campo);
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
enssize=sum(sum(W));

auxiliar=zeros(size(forecast));
%     
% 
 for j=-n:n
     for i=-n:n
         if(W(i+n+1,j+n+1) > 0)
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
    
         prob(imin:imax,jmin:jmax,:,:)=W(i+n+1,j+n+1)*double(campo(imin+i:imax+i,jmin+j:jmax+j,:,:))+prob(imin:imax,jmin:jmax,:,:); 
         rank(imin:imax,jmin:jmax,:)=double(forecast(imin+i:imax+i,jmin+j:jmax+j,:) < obs(imin:imax,jmin:jmax,:)) + rank(imin:imax,jmin:jmax,:);
         
         auxiliar(imin:imax,jmin:jmax,:)=auxiliar(imin:imax,jmin:jmax,:)+W(i+n+1,j+n+1);
         end
     end
 end
% 
 for ii=1:numb
 prob(:,:,:,ii)=prob(:,:,:,ii)./auxiliar;
 end
% tmp= (rank == 0 & obs == 0) ;  %Caso en el que el pronostico y la observacion son 0.
% tmp2= floor(rand(size(obs))*enssize);
% rank(tmp)=tmp2(tmp);  %Reasignamos el rank de esos casos en forma random uniforme.
% 
% 
  end
