
function [minimun]=minimun_basin_fun(minimos,var,pathi,pathj,config);

if(minimos(pathi,pathj) >0)
    %Si el punto de partida ya es un minimo entonces no puedo cambiar su
    %pertenecia.
    minimun=minimos(pathi,pathj);
    
else
%Funcion para determinar a que minimo pertenece un punto.
%La idea es arrancar un camino buscando siempre el descenso y ver hasta que
%minimo se llega. Ese sera el minimo buscado.

%Minimos es la mascara con los minimos vale 0 donde no es un minimo y imin
%en el punto correspondiente al minimo.
%pathi pathj es el punto de donde partimos (lo que queremos averiguar es a
%que minimo llegamos si siempre tratamos de descender lo mas posible).
%config es la configuracion para usar algunos parametros.

[ny nx]=size(minimos);
 
%Comienzo la caminata a partir del punto ipath, jpath a partir del cual
%quiero saber a que minimo llego.

  path_mask=true(size(minimos));  %Esta mascara sirve para "marcar" el camino basicamente evita que la caminata vuelva sobre sus pasos.
  
  path_mask(pathi,pathj)=false;
  
  cont=true;
  
  contador=1;
  tmp_min=var(pathi,pathj);
  
  while(cont)
      

   %Busco en el entorno del punto el minimo.
 
   pathi_old=pathi;
   pathj_old=pathj;
   for ii=-1:1
           for jj=-1:1
                   indexi=pathi_old+ii;
                   indexj=pathj_old+jj;
                   
                   if(config.global) %Condicion de borde global.
                   %=======================================================
                   %Condicion de borde ciclica en X
                   if(indexj > nx)
                   indexj=indexj-nx;
                   end
                   if(indexj < 1)
                   indexj=nx+indexj;
                   end
                   
                   if(indexi > ny)
                   indexi=ny-(indexi-ny-1);
                   aux=round(nx/2);
                   indexj=indexj+aux;
                   end
                   if(indexi < 1)
                   indexi=1-indexi; 
                   aux=round(nx/2);
                   indexj=indexj+aux;
                   end
                   
                   end
                  
                if(indexi <= ny & indexi >= 1 & indexj <= nx & indexj >= 1 )
                if(var(indexi,indexj) < tmp_min & path_mask(indexi,indexj));
                tmp_min=var(indexi,indexj);  
                pathi=indexi;
                pathj=indexj;
              
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
           if(contador > 100)
               cont=false;
               minimun=NaN;
           end
               

        
  end
  
end