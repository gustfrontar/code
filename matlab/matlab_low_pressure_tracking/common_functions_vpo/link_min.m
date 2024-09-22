
function [minstruct]=link_min(FIELD,minstruct)

%fprintf('Entro en la rutina link min\n')
%ESTA FUNCION VERIFICA SI HAY MINIMOS ASOCIADOS ENTRE SI. SI DETECTA ESTA
%SITUACION AGREGA ESA INFORMACION EN LA ESTRUCTURA MINSTRUCT UTILIZANDO LOS
%CAMPOS LINKED Y LINKEDMIN PERO NO REALIZA NINGUNA ALTERACION DE LOS
%MINIMOS DETECTADOS POR LA RUTINA DE DETECCION.


UMBRAL_ASOCIACION=20;   %20 mgp
UMBRAL_CERCANIA=1e6;    %1000 km

[ny nx]=size(FIELD);

for it=1:size(minstruct,2)

    for imin=1:minstruct(it).nminimos
        
      minstruct(it).linked(imin)=false;
      minstruct(it).linkedmin(imin)=NaN;
        
    end

    
    

for imin=1:minstruct(it).nminimos;
if( ~minstruct(it).linked(imin) )    
  
  
%Comienzo la caminata a partir del minimo imin.

  pathi=minstruct(it).mini(imin); %La posicion actual de la caminata en i
  pathj=minstruct(it).minj(imin); %La posicion actual de la caminata en j

  var_path=FIELD(pathi,pathj);
  path_mask=true(size(FIELD));  %Esta mascara sirve para "marcar" el camino basicamente evita que la caminata vuelva sobre sus pasos.
  

  
  path_mask(pathi,pathj)=false;
  
  cont=true;
  
  contador=1;
  
  
  while(cont)
      
   %Busco en el entorno del punto el minimo.
 
   pathi_old=pathi;
   pathj_old=pathj;
   
   esminimo=true;

   tmp_min=0;
   for ii=-1:1
           for jj=-1:1
                   indexi=pathi_old+ii;
                   indexj=pathj_old+jj;
                   
                   [indexi,indexj]=global_boundary_fun(indexi,indexj,nx,ny);
             
                if(indexi <= ny && indexi >= 1 && indexj <= nx && indexj >= 1 )
                if(FIELD(indexi,indexj) < tmp_min && path_mask(indexi,indexj));
                tmp_min=FIELD(indexi,indexj);  
                pathi=indexi;
                pathj=indexj;
              
                end
                
                if(~(ii==0 && jj==0))  
                if(FIELD(indexi,indexj) <= FIELD(pathi_old,pathj_old) || contador==1)
                    esminimo=false;
                end
                end
                end
               
           end
           
   end


           if(contador > 1000)
               cont=false;
           end
           
           contador=contador+1;
           path_mask(pathi,pathj)=false; %Este punto queda vedado para futuros movimientos, la caminata no puede volver sobre sus pasos.
           var_path(contador)=tmp_min;
           %Es el punto donde estoy otro minimo?

           if( esminimo ) %El camino llego a otro minimo.
               %Veo si es uno de los minimos identificados por el
               %algoritmo.
               imin2=find( minstruct(it).mini==pathi_old & minstruct(it).minj==pathj_old);
               
               if(~isempty(imin2))
               %Si lo es corto el algoritmo de la caminata.
                   cont=false;
               %Veo si ambos minimos se pueden asociar o no.
               if(max([abs(max(var_path)-var_path(1)) abs(max(var_path)-var_path(end))]) < UMBRAL_ASOCIACION );
               minlat1=minstruct(it).minlat(imin);
               minlon1=minstruct(it).minlon(imin);
               minlat2=minstruct(it).minlat(imin2);
               minlon2=minstruct(it).minlon(imin2);
                   
               tmp_dist=distll_fun(minlon1,minlat1,minlon2,minlat2);
          
               if tmp_dist < UMBRAL_CERCANIA
                   %fprintf('Asocie el minimo %10f con el minimo %10f \n',imin,imin2)
                   minstruct(it).linked(imin)=true;
                   minstruct(it).linkedmin(imin)=imin2;
                   minstruct(it).linked(imin2)=true;
                   minstruct(it).linkedmin(imin2)=imin;

               end
                   
               end
           end

  end

    
end 


end
end
end


