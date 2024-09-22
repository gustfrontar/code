
function [minimos]=minimun_id_fun(input,config)



for itime=1:size(input.Data,3);
    
display('Buscando minimos') 
%==========================================================================
% DETECCION DE LOS MINIMOS LOCALES
%==========================================================================

umbralmin=config.umb_min;   %Umbral de anomalia que exijo para determinar la presencia de un minimo.

var=input.Data(:,:,itime);
sistema=zeros(size(input.Data(:,:,itime)));

[ny nx]=size(var);

%Primero buscamos e identificamos los minimos. (Generamos una mascara donde
%solo figuran la posicion de los minimos). El minimo se detecta pidiendo
%que el punto en cuestion sea menor a todos los puntos que lo rodean.
%==========================================================================
nminimos=0;
mini=[];
minj=[];
for j=1:nx
    for i=1:ny
       testmin=0;
       for ii=-1:1
           for jj=-1:1
                   indexi=i+ii;
                   indexj=j+jj;
                   if(config.global) %Condicion de borde global.
                   %=======================================================
                   %Aplicamos la condicion de borde para dominio global.
                   [indexi,indexj]=global_boundary_fun(indexi,indexj,nx,ny);
                   
                   %Condicion de borde regional (no hay)
                   %=======================================================
                   end
                  
               
                if(indexi <= ny && indexi >= 1 && indexj <= nx && indexj >= 1)
                if( var(indexi,indexj) < var(i,j) )
                testmin=1;
                end
               end
           end
       end
       %El punto en cuestion era un minimo?
       if(testmin==0 && var(i,j) < umbralmin ) %Entonces era un minimo.
       
       if(config.global)
       nminimos=nminimos+1;
       mini=[mini i];
       minj=[minj j];
       sistema(i,j)=nminimos;
       minimos(itime).lat(nminimos)=input.lat(i,j);
       minimos(itime).lon(nminimos)=input.lon(i,j);
       minimos(itime).i(nminimos)=i;
       minimos(itime).j(nminimos)=j;
       minimos(itime).consider(nminimos)=true; %Si lo vamos a considerar como posible parte de una trayectoria.
       minimos(itime).value(nminimos)=var(i,j); 
       minimos(itime).partner(nminimos)=nminimos; %Mas adelante 2 o mas minimos pueden quedar "asociados" si estan muy cerca uno de otro.
       
       else
       %Por definicion en un dominio regional el minimo no puede estar en
       %el borde (simplemente porque no tenemos manera de saber si lo es o
       %no).
       if(i ~= 1 && i ~=ny && j ~=1 && j~=nx)
       nminimos=nminimos+1;
       mini=[mini i];
       minj=[minj j];
       sistema(i,j)=nminimos;
       
       minimos(itime).lat(nminimos)=input.lat(i,j);
       minimos(itime).lon(nminimos)=input.lon(i,j);
       minimos(itime).i(nminimos)=i;
       minimos(itime).j(nminimos)=j;
       minimos(itime).consider(nminimos)=true; %Si lo vamos a considerar como posible parte de una trayectoria.
       minimos(itime).value(nminimos)=var(i,j); 
       minimos(itime).partner(nminimos)=nminimos;
       
       
       end
        
       end
       end 
    end
end

%output.minimos=sistema;



%==========================================================================
% ACA TERMINA LA DETECCION DE LOS MINIMOS
%==========================================================================

%Hasta aca tengo detectados los minimos y su cuenca de atraccion. 
%==========================================================================
% Antes de proceder con el calculo de la trayectoria  voy
% a detectar minimos que en realidad corresponden al mismo sistema. La idea
% es partir del minimo A buscando siempre el menor aumento de la anomalia,
% entonces pueden pasar dos cosas: llego a otro minimo B o me salgo de la
% zona donde la anomalia esta por debajo del umbral. Si llego a otro minimo
% B puedo obtener el maximo de anomalia en el camino respecto del minimo A
% si este maximo es pequenio puedo decir que A y B estan en el mismo
% sistema (mas alla de que pueden estar muy lejos).
% Esto evitaria particiones innecesarias de sistemas grandes por presencia
% de minimos multiples.

%min_logical=true(nminimos,1);
%min_fussion=zeros(nminimos,1);
for imin=1:nminimos;
  
%Comienzo la caminata a partir del minimo imin.

  pathi=minimos(itime).i(imin); %La posicion actual de la caminata en i
  pathj=minimos(itime).j(imin); %La posicion actual de la caminata en j

  var_path=var(pathi,pathj);
  path_mask=true(size(var));  %Esta mascara sirve para "marcar" el camino basicamente evita que la caminata vuelva sobre sus pasos.
  

  
  path_mask(pathi,pathj)=false;
  
  cont=true;
  
  contador=1;
  while(cont)
      
   tmp_min=0;
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
                   [indexi,indexj]=global_boundary_fun(indexi,indexj,nx,ny);
                   
                   end
                  
                if(indexi <= ny && indexi >= 1 && indexj <= nx && indexj >= 1 )
                if(var(indexi,indexj) < tmp_min && path_mask(indexi,indexj));
                tmp_min=var(indexi,indexj);  
                pathi=indexi;
                pathj=indexj;
              
                end

                end
           end
   end
           
           contador=contador+1;
           path_mask(pathi,pathj)=false; %Este punto queda vedado para futuros movimientos, la caminata no puede volver sobre sus pasos.
           var_path(contador)=tmp_min;
           %Es el punto donde estoy otro minimo?
           if( output.minimos(pathi,pathj) > 0)
               minimo2=output.minimos(pathi,pathj);
               cont=false; %La trayectoria llego a otro minimo asi que la cortamos.

  
           if(min([abs(max(var_path)-var_path(1)) abs(max(var_path)-var_path(end))]) < config.umb_minbarrier );
           %La barrera de geopotencial entre ambos minimos es menor que el umbral, entonces podr??an ser el mismo sistema!
           %Compruebo si estos m??nimos est??n muy cerca entrte s?? o si se
           %trata de dos sistemas diferentes.
           
           tmp_dist=distll_fun(input.lon(mini(imin),minj(imin)),input.lat(mini(imin),minj(imin)),input.lon(mini(minimo2),minj(minimo2)),input.lat(mini(minimo2),minj(minimo2)));
          
           
           if tmp_dist < config.umbral_cercania

           %Asocio el sistema al minimo mas profundo.
               if( var_path(1) <= var_path(end))
                   minimos(itime).consider(minimo2)=false; %No voy a considerar el minimo a donde llego en el calculo de trayectorias.
                   minimos(itime).partner(minimo2)=imin;   %El minimo a donde llego queda asociado al minimo de partida.
               else
                   minimos(itime).consider(imin)=false;   %No voy a considerar el minimo de donde salio en el calculo de las trayectorias.
                   minimos(itime).partner(imin)=minimo2;
               end
           end
           end
           end
          
    
           %Si la anomalia en la trayectoria aumenta por encima del umbral
           %que se considera un sistema la trayectoria es interrumpida.
           if(tmp_min > config.umb_anom)
           cont=false; 
           end
           
     
  end

    
end 



end




end %End del do sobre los tiempos.


%==========================================================================
% LISTO EL LLOPO!!
%==========================================================================
















