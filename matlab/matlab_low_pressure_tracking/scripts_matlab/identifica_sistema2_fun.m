
function [mascara_out]=identifica_sistema2_fun(geo)


[ny nx]=size(geo);
umbral=-1;        %Umbral de anomalia que voy a usar para definir los sistemas.
umbralmin=-1.5;   %Umbral de anomalia que exijo para determinar la presencia de un minimo.
maxsize=nx;       %Maximo tamanio del cuadrado que voy a usar para buscar los sistemas.
mask=zeros(size(geo));

%Primero buscamos e identificamos los minimos.
nminimos=0;
mini=[];
minj=[];
for ix=2:nx-2
    for iy=2:ny-2
       testmin=0;
       for ii=-1:1
           for jj=-1:1
               if( geo(iy+ii,ix+jj) < geo(iy,ix) )
               testmin=1;
               end
           end
       end
       %El punto en cuestion era un minimo?
       if(testmin==0 & geo(iy,ix) < umbralmin ) %Entonces era un minimo.
           
       nminimos=nminimos+1;
       mini=[mini ix];
       minj=[minj iy];
       mask(iy,ix)=nminimos;
        
    end
end


    
end

pcolor(mask)

[geox geoy]=gradient(-geo);  %Calculamos la pendiente del geopotencial util para decidir a que minimo pertenece un  punto dado.

%Ahora a partir de cada minimo busco las zonas asociadas a dichos minimos.

for imin=1:nminimos
   
    
    %Recorro cuadrados alrededor del minimo cada vez mas grandes viendo si
    %se cumple el umbral y sigo en contacto con el minimo.
    
    for sqsize=1:maxsize
    for inada=1:4
        
        for ii=-sqsize:sqsize
            %Borde superior del cuadrado.
            if(inada == 1)
            i=mini(imin)+ii;
            j=minj(imin)+sqsize;
            elseif(inada == 2)
            %Borde inferior del cuadrado.
            i=minj(imin)+ii;
            j=minj(imin)-sqsize;
            elseif(inada==3)
            %Borde derecho
            i=minj(imin)+sqsize;
            j=minj(imin)+ii;
            elseif(inada==4)
            %Borde izquierdo
            i=minj(imin)-sqsize;
            j=minj(imin)+ii;
            end
            
            
            if( i >= 2 & i <= nx-1 & j >= 2 & j <= ny-1)
                if(geo(j,i)< umbral)
                        vecino=0;
                        for ii=-1:1
                            for jj=-1:1
                                if(mask(j+ii,i+jj)==imin)
                                vecino=1;
                                end
                            end
                        end
                        if(vecino==1 & mask(j,i)==0) %El punto no pertenecia a ningun sistema.
                            mask(j,i)=imin;
                        elseif(vecino ==1 & mask(j,i) > 0)        %El punto pertenece a otro sistema... hay que disputarlo.
                            %Para disputar el punto usamos el gradiente.
                            actual=mask(j,i);  %Minimo al que esta actualmente ligado el punto.
                            dist_actual=(i-mini(actual))^2+(j-minj(actual))^2;
                            dist_retador=(i-mini(imin))^2+(j-minj(imin))^2;
                            grad_actual=[mini(actual)-i minj(actual)-j];
                            grad_retador=[mini(imin)-i minj(imin)-j];
                            proy_actual=geox(j,i)*grad_actual(1)+geoy(j,i)*grad_actual(2);
                            proy_retador=geox(j,i)*grad_retador(1)+geoy(j,i)*grad_retador(2);
                            if(proy_actual < 0 & proy_retador > 0)
                                mask(j,i)=imin;  %Cambiamos la clasificacion de este punto.
                            else
                                %Lo decido por distancias.
                                if(dist_retador <= dist_actual)
                                    mask(j,i)=imin;
                                end
                            end
                            %En los demas casos no hago nada ...  
                            
                            
                        end

                end
            end
        end
    
    end
    end
end


mascara_out=mask;











