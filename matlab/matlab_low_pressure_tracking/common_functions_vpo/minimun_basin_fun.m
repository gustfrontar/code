
function [minimun]=minimun_basin_fun(minimos,var,pathi,pathj,debug,setpathpole)

%Funcion para determinar a que minimo pertenece un punto.
%La idea es arrancar un camino buscando siempre el descenso y ver hasta que
%minimo se llega. Ese sera el minimo buscado.

%Minimos es la mascara con los minimos vale 0 donde no es un minimo y imin
%en el punto correspondiente al minimo.
%pathi pathj es el punto de donde partimos (lo que queremos averiguar es a
%que minimo llegamos si siempre tratamos de descender lo mas posible).

%La variable de entrada setpathpole es una variable logica, si vale true,
%la funcion solo calcula como siguen los caminos que pasan por el polo y
%los guarda en forma permanente para nuevas llamadas de la variable. Esto
%es importante porque ese paso tarda mucho tiempo. 

%Defino la continuacion de los paths en los polos como variables de tipo
%persistent, eso quiere decir que matlab va a "recordar" el valor de las
%variables entre 2 llamadas sucesivas a la funcion.
persistent tmp_min_polen tmp_min_poles pathj_polen pathj_poles

if(setpathpole)
    [ny nx]=size(minimos);
    tmp=var(ny-1,:);
    [tmp_min_polen pathj_polen]=min(tmp);
    tmp=var(2,:);
    [tmp_min_poles pathj_poles]=min(tmp);
    return
end
    


if(minimos(pathi,pathj) >0)
    %Si el punto de partida ya es un minimo entonces no puedo cambiar su
    %pertenecia.
    minimun=minimos(pathi,pathj);
    
else


[ny nx]=size(minimos);

maxiter=round(nx/2);
 
%Comienzo la caminata a partir del punto ipath, jpath a partir del cual
%quiero saber a que minimo llego.

  path_mask=true(size(minimos));  %Esta mascara sirve para "marcar" el camino basicamente evita que la caminata vuelva sobre sus pasos.
  
  path_mask(pathi,pathj)=false;
  
  cont=true;
  
  contador=1;
  tmp_min=var(pathi,pathj);
  
  while(cont)
  
      if(debug)
          fprintf('Estoy en el paso %f\n',contador);
          fprintf('Las coordenadas son %f %f \n',pathi,pathj);
      end
   
   %Busco en el entorno del punto el minimo.
 
   pathi_old=pathi;
   pathj_old=pathj;
   
   %Primero me fijo si estoy en el polo, porque este es un caso particular.
   if(pathi==ny)
       pathi=ny-1; %Como el polo no es un minimo, seguro me voy a mover a la fila siguiente.
       %Asumo que todos los puntos de la ultima fila tienen el mismo valor
       %y representan el polo norte.
       tmp_min=tmp_min_polen;
       pathj=pathj_polen;
       pathi=ny-1;
       %tmp_min va a ser el minimo de la fila ny-1 y pathj va a ser la 
       %posicion de dicho minimo.
   elseif(pathi==1) %Idem pero para el polo sur.
       %Asumo que todos los puntos de la primera fila tienen el mismo valor
       %y que representan al polo sur.
       pathi=2;
       tmp_min=tmp_min_poles;
       pathj=pathj_poles;
   else
   %Si no estoy en el polo entonces busco en el entorno del punto como seguir 
   %con la trayectoria.
   for ii=-1:1
           for jj=-1:1
                %Primero me aseguro de que no se trate del mismo punto.
                if(~( ii==pathi && jj==pathj ))
                   indexi=pathi_old+ii;
                   indexj=pathj_old+jj;
                   
                 [indexi,indexj]=global_boundary_fun(indexi,indexj,nx,ny);
                  
                if(indexi <= ny && indexi >= 1 && indexj <= nx && indexj >= 1 ... 
                && var(indexi,indexj) <= tmp_min && path_mask(indexi,indexj));
                tmp_min=var(indexi,indexj);  
                pathi=indexi;
                pathj=indexj;

                end
                end
           end
   end
   
   end   
           contador=contador+1;
           path_mask(pathi,pathj)=false; %Este punto queda vedado para futuros movimientos, la caminata no puede volver sobre sus pasos.
           
           %Es el punto donde estoy otro minimo?
           if( minimos(pathi,pathj) > 0)
               minimun=minimos(pathi,pathj);
               cont=false;
           end
           %Usamos nx como limitador de la cantidad de pasos, porque en el
           %polo eso puede no significar distancias muy grandes.
           if(contador > maxiter)
               cont=false;
               minimun=NaN;
           end
               

        
  end
  
end

if(debug)
    figure
    pcolor(real(path_mask))
    title('Path mask')
end