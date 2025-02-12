
function [minstruct MASCARA_SIS]=link_min(FIELD,MASCARA_SIS,minstruct,UMBRAL_ASOCIACION,UMBRAL_CERCANIA)

%fprintf('Entro en la rutina link min\n')
%ESTA FUNCION VERIFICA SI HAY MINIMOS ASOCIADOS ENTRE SI. SI DETECTA ESTA
%SITUACION AGREGA ESA INFORMACION EN LA ESTRUCTURA MINSTRUCT UTILIZANDO LOS
%CAMPOS LINKED Y LINKEDMIN PERO NO REALIZA NINGUNA ALTERACION DE LOS
%MINIMOS DETECTADOS POR LA RUTINA DE DETECCION.

[ny nx]=size(FIELD);


for it=1:size(minstruct,2)

minstruct(it).remove=false(size(minstruct(it).minlat));

for imin=1:minstruct(it).nminimos;
  
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

   tmp_min=1000;
   for ii=-1:1
           for jj=-1:1
                   indexi=pathi_old+ii;
                   indexj=pathj_old+jj;
                   
%                   [indexi,indexj]=global_boundary_fun(indexi,indexj,nx,ny);
             
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

               if(~isempty(imin2) && ~minstruct(it).remove(imin2) )
               %Si lo es corto el algoritmo de la caminata.
                   cont=false;
               %Veo si ambos minimos se pueden asociar o no.
               if(min([abs(max(var_path)-var_path(1)) abs(max(var_path)-var_path(end))]) < UMBRAL_ASOCIACION );
               minlat1=minstruct(it).minlat(imin);
               minlon1=minstruct(it).minlon(imin);
               minlat2=minstruct(it).minlat(imin2);
               minlon2=minstruct(it).minlon(imin2);
                   
               tmp_dist=distll_fun(minlon1,minlat1,minlon2,minlat2);
          
               if tmp_dist < UMBRAL_CERCANIA
                   if( minstruct(it).minval(imin) <= minstruct(it).minval(imin2))
                       minstruct(it).remove(imin2)=true;
                       MASCARA_SIS(MASCARA_SIS==minstruct(it).id(imin2))=minstruct(it).id(imin);
                       minstruct(it).id(imin2)=minstruct(it).id(imin);
                   else
                       minstruct(it).remove(imin)=true;
                       MASCARA_SIS(MASCARA_SIS==minstruct(it).id(imin))=minstruct(it).id(imin2);
                       minstruct(it).id(imin)=minstruct(it).id(imin2);
                   end
             

               end
                   
               end
           end

  end

    
end 



end

%for imin=1:minstruct(it).nminimos
minstruct(it).id(minstruct(it).remove)=[];
minstruct(it).minlat(minstruct(it).remove)=[];
minstruct(it).minlon(minstruct(it).remove)=[];
minstruct(it).minval(minstruct(it).remove)=[];
minstruct(it).mini(minstruct(it).remove)=[];
minstruct(it).minj(minstruct(it).remove)=[];
minstruct(it).remove(minstruct(it).remove)=[];
minstruct(it).nminimos=sum(~minstruct(it).remove);
%end


end


