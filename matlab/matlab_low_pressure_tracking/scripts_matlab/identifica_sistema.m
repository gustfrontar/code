clear all
close all

nx=500;
ny=500;

umbral=2;

anomalia=zeros(nx,ny);

x=1:nx;
y=1:ny;
[X Y]=meshgrid(x,y);

anomalia=anomalia+10*exp(-((X-100).^2+(Y-100).^2)/250)+10*exp(-((X-300).^2+(Y-100).^2)/250);


mascara=zeros(size(anomalia));
mascara(anomalia > umbral)=-1;

sistema=zeros(size(anomalia));

ultimo_sistema=1;
for i=2:nx-1
    for j=2:ny-1
   
        if(mascara(i,j) == -1)
           tengo_vecino=0;
           for i2=-1:1
               for j2=-1:1
                   if(sistema(i+i2,j+j2) > 0 & sistema(i,j)==0 )
                       sistema(i,j)=sistema(i+i2,j+j2);
                       tengo_vecino=1;
                   end
                   if(sistema(i+i2,j+j2) > 0 & sistema(i,j) > 0 & sistema(i,j) ~= sistema(i+i2,j+j2) )
                       auxiliar=max([sistema(i+i2,j+j2) sistema(i,j)]);
                       sistema(sistema == auxiliar)=min([sistema(i+i2,j+j2) sistema(i,j)]);
                   end
               end
           end
           if(tengo_vecino==0)
               sistema(i,j)=ultimo_sistema;
               ultimo_sistema=ultimo_sistema+1;
           end
        end
    end
end



numero_sistemas=max(max(sistema));

if(numero_sistemas > 0);
    

numero_sistemas_final=0    
for is=1:numero_sistemas 
    
    tamanio=sum(sistema==is);
    
    if(tamanio > 10 )
        numero_sistemas_final=numero_sistemas_final+1;
    anomalia_media(numero_sistemas_final)=mean(anomalia(sistema==is));    
    anomalia_maxima(numero_sistemas_final)=max(anomalia(sistema==is));
    end
    
    
    
end

    
end

















