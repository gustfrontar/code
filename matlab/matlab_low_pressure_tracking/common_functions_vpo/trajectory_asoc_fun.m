function [ group trajectories ] = trajectory_asoc_fun( group , trajectories , t_cfd , t_cfgr , t_cftr , t_cft , imember )
%ESTA FUNCION ES LA QUE SE ENCARGA DE VERIFICAR SI UNA DADA TRAYECTORIA
%PUEDE SER ASIGNADA O NO A UN GRUPO Y SI PUEDE SER ASIGNADA LO HACE Y SI
%DEBE REEMPLAZAR A OTRA TRAYECTORIA LA REEMPLAZA.

ngroup=size(group,2);

cfd=NaN(1,ngroup);  %Distance cost function
cft=NaN(1,ngroup);  %Time cost function
cfgr=NaN(1,ngroup); %Group superposition cost function
cftr=NaN(1,ngroup); %Trajectory superposition cost function.

for jgroup=1:ngroup
%Tengo que comparar la trajectoria ii del miembro imember con 
%la media del grupo jgroup y calcular una funcion de costo. Luego
%ver a que grupo deberia asociar la trajectoria.

         isnan_tr=~isnan(group(jgroup).meanlon);
         grouplon=group(jgroup).meanlon(isnan_tr);
         grouplat=group(jgroup).meanlat(isnan_tr);
         grouplength=group(jgroup).meanlength;
         grouptime=times(isnan_tr);
         initime=times(1);
         
 [cfd(jgroup) cft(jgroup) cfgr(jgroup) cftr(jgroup)]=traj_cost_function_fun(grouplon,grouplat,grouptime,grouplength,...
                                                     trajectories(ii).minlonf,trajectories(ii).minlatf,...
                                                     trajectories(ii).daten,initime); 
         
%AGREGAR UN CRITERIO DE SELECCION MAS EXIGENTE QUE NO SOLO TOME EN
%CUENTA LA DISTANCIA MEDIA SINO QUE TAMBIEN LA DISTANCIA AL INICIO
%DEL GRUPO
end  %En del for sobre los grupos para calcular la funcion de costo.


cft( cfd >= t_cfd)=NaN;  %Elimino los grupos que no cumplen el criterio de distancia.
cft( cfgr <= t_cfgr )=NaN;  %Elimino los que no coinciden en al menos un 50 % con el grupo.
cft( cftr <= t_cftr )=NaN;  %Elimino aquellas trayectorias que no estan contenidas al menos en un 50% por el grupo.
       
       
%ACA SE DECIDE QUE GRUPO ES EL MEJOR O EL MAS PROXIMO. POR EL MOMENTO
%USAMOS EL CRITERIO DEL MAS PROXIMO AL ORIGEN DEL GRUPO.
[min_cft min_group]=nanmin(cft);
min_cfd=cfd(min_group);
min_cfgr=cfgr(min_group);
min_cftr=cftr(min_group);
       
%===================================================================
% SI HAY UN GRUPO AL QUE ESTA TRAYECTORIA PUEDE PERTENECER VAMOS A
% ESTUDIAR DIFERENTES POSIBILIDADES.
%===================================================================
asociar=false;
if( ~isnan(min_cft) )  %Si es NaN quiere decir que todos los grupos estaban muy lejos para ser asociados.
          

  %Antes de asociar el sistema al grupo vamos a verificar que la
  %posicion que va a ocupar dicho sistema no este ocupada por otro
  %sistema.
          
%Calculamos los lugares que ocuparia la trayectoria seleccionada.
[nada index] = intersect(times,TrajStruct(imember).trajectories(ii).daten);  
%Vemos si esos lugares estan ocupados.
          
       %===================================================================
       % A) LA TRAYECTORIA YA ESTA ASOCIADA, VEMOS SI EL GRUPO AL QUE FUE
       % ASOCIADA COINCIDE CON EL MEJOR, SI ES ASI LA DEJAMOS DONDE ESTA,
       % SINO VEMOS EL NUEVO GRUPO, SI ESTA LIBRE LA MOVEMOS, SINO
       % CHEQUEAMOS QUE ESTA TRAYECTORIA SEA MEJOR PARA EL NUEVO GRUPO QUE
       % LA QUE EL NUEVO GRUPO TENIA EN ESE LUGAR Y SI ES ASI HACEMOS EL
       % CAMBIO DE GRUPO.
       %===================================================================
  
        if( trajectories(ii).asociada &&  trajectories(ii).idgroup~=min_group) 
        %CASO EN EL QUE LA TRAYECTORIA ESTA ASOCIADA PERO NO AL GRUPO QUE
        %APARECE COMO EL MEJOR AHORA... 
        %TENGO QUE VER SI EL NUEVO GRUPO TIENE UNA TRAYECTORIA ASOCIADA
        %PARA ESE RANGO DE TIEMPO Y PARA ESE MIEMBRO DEL ENSAMBLE, SI ES
        %ASI LAS COMPARO Y VEO CUAL ES MEJOR, SINO ASOCIO DIRECTAMENTE ESTA
        %TRAYECTORIA Y LA DESASOCIO DEL GRUPO ANTERIOR. 
        %Intentamos asociar la trayectoria actual previamente asociada a un
        %nuevo grupo. 
          if(sum(isnan(group(min_group).minlat(imember,index)))==length(index))
            %Si todos los elementos son NaN, no habia nadie ocupando ese lugar...
            %Dale para adelante
            asociar=true;   %Se va a asociar esta trayectorias al grupo min_group.
            %Pero tambien tengo que desasociarla del grupo en el que estaba
            %antes.
            grupo_anterior=trajectories(ii).idgroup;
            index_asociada=(group(grupo_anterior).trajid(imember,:)==ii);
            trajectories(ii).asociada=false; %Momentaneamente cambia el status a no asociada.
            trajectories(ii).idgroup=NaN;
            group(grupo_anterior).minlat(imember,index_asociada)=NaN;
            group(grupo_anterior).minlon(imember,index_asociada)=NaN;
            group(grupo_anterior).minanomsis(imember,index_asociada)=NaN;
            group(grupo_anterior).minarea(imember,index_asociada)=NaN;
            group(grupo_anterior).uvel(imember,index_asociada)=NaN;
            group(grupo_anterior).vvel(imember,index_asociada)=NaN;
            group(grupo_anterior).trajid(imember,index_asociada)=NaN;
            %Recalculo la media del grupo.
            group(grupo_anterior).meanlat=nanmean(group(grupo_anterior).minlat,1);
            for kt=1:ntimes %Cuidado especial hay que tener siempre que se promedian longitudes.
            group(grupo_anterior).meanlon(kt)=mean_lon_fun(group(grupo_anterior).minlon(:,kt));
            end
            group(grupo_anterior).nmember=sum(any(~isnan(group(grupo_anterior).minlat),2));
            aux=sum(~isnan(group(grupo_anterior).minlat),2);
            aux(aux==0)=NaN;        
            group(grupo_anterior).meanlength=nanmean(aux);
            
          else
            %Tengo que chequear cual/es es/son la/s trayectoria/s que esta/s asociada/s en
            %ese lugar y cual es la mejor opcion.
            %Calculo cuanto se superponen cada una de esas trayectorias con
            %el grupo y cuan cerca o lejos estan del origen.
            isnan_tr=~isnan(group(min_group).meanlon);
            traj_asociadas=unique(group(min_group).trajid(imember,index));
            traj_asociadas=traj_asociadas(~isnan(traj_asociadas));
            tmp_cfd=NaN(1,length(traj_asociadas)) ; %Distance cost function
            tmp_cft=NaN(1,length(traj_asociadas)) ; %Time cost function

            tmp_grouplon=group(min_group).meanlon(isnan_tr);
            tmp_grouplat=group(min_group).meanlat(isnan_tr);
            tmp_grouptime=times(isnan_tr);
            tmp_grouplength=group(min_group).meanlength;
            initime=times(1);
            for kk=1:length(traj_asociadas)
                %Calculo la funcion de costo entre las trayectorias
                %asociadas y la trayectoria actual.
                
                [tmp_cfd(kk) tmp_cft(kk)]=traj_cost_function_fun(tmp_grouplon,tmp_grouplat,tmp_grouptime,tmp_grouplength,...
                TrajStruct(imember).trajectories(traj_asociadas(kk)).minlonf,TrajStruct(imember).trajectories(traj_asociadas(kk)).minlatf,...
                TrajStruct(imember).trajectories(traj_asociadas(kk)).daten,initime);
            end
            %
            if( min_cft < min(tmp_cft) )
            %Si la nueva trayectoria esta mas cerca del origen temporal del
            %grupo que la trayectoria que la o las trayectorias que habia
            %antes en ese lugar, me quedo con la nueva.
            %Asocio la nueva trayectoria. Pero primero tengo ademas que
            %desasociar las anteriores.
            asociar=true;
            
            %Desasocio las anteriores
            for kk=1:length(traj_asociadas)
                index_asociada=(group(min_group).trajid(imember,:)==traj_asociadas(kk));
                TrajStruct(imember).trajectories(traj_asociadas(kk)).asociada=false;
                TrajStruct(imember).trajectories(traj_asociadas(kk)).idgroup=NaN;  %Se analizara en breve
                group(min_group).minlat(imember,index_asociada)=NaN;
                group(min_group).minlon(imember,index_asociada)=NaN;
                group(min_group).minanomsis(imember,index_asociada)=NaN;
                group(min_group).minarea(imember,index_asociada)=NaN;
                group(min_group).uvel(imember,index_asociada)=NaN;
                group(min_group).vvel(imember,index_asociada)=NaN;
                group(min_group).trajid(imember,index_asociada)=NaN;
                %Recalculo la media del grupo.
                group(min_group).meanlat=nanmean(group(min_group).minlat,1);
                for kt=1:ntimes %Cuidado especial hay que tener siempre que se promedian longitudes.
                group(min_group).meanlon(kt)=mean_lon_fun(group(min_group).minlon(:,kt));
                end
                group(min_group).nmember=sum(any(~isnan(group(min_group).minlat),2));
                aux=sum(~isnan(group(min_group).minlat),2);
                aux(aux==0)=NaN;        
                group(min_group).meanlength=nanmean(aux);
                
            end
            %Desasocio tambien la trayectoria ii de su grupo anterior.
            grupo_anterior=TrajStruct(imember).trajectories(ii).idgroup;
            index_asociada=(group(grupo_anterior).trajid(imember,:)==ii);
            TrajStruct(imember).trajectories(ii).asociada=true; %Momentaneamente cambia el status a no asociada.
            TrajStruct(imember).trajectories(ii).idgroup=min_group;
            group(grupo_anterior).minlat(imember,index_asociada)=NaN;
            group(grupo_anterior).minlon(imember,index_asociada)=NaN;
            group(grupo_anterior).minanomsis(imember,index_asociada)=NaN;
            group(grupo_anterior).minarea(imember,index_asociada)=NaN;
            group(grupo_anterior).uvel(imember,index_asociada)=NaN;
            group(grupo_anterior).vvel(imember,index_asociada)=NaN;
            group(grupo_anterior).trajid(imember,index_asociada)=NaN;
            %Recalculo la media del grupo.
            group(grupo_anterior).meanlat=nanmean(group(grupo_anterior).minlat,1);
            for kt=1:ntimes %Cuidado especial hay que tener siempre que se promedian longitudes.
            group(grupo_anterior).meanlon(kt)=mean_lon_fun(group(grupo_anterior).minlon(:,kt));
            end
            group(grupo_anterior).nmember=sum(any(~isnan(group(grupo_anterior).minlat),2));
            aux=sum(~isnan(group(grupo_anterior).minlat),2);
            aux(aux==0)=NaN;        
            group(grupo_anterior).meanlength=nanmean(aux);
            
            end
          end
        
        
        end %END DEL IF SOBRE SI LA TRAYECTORIA FUE ASOCIADA AL MISMO GRUPO QUE YA TENIA O NO (EN EL CASO QUE ESTUVIERA ASOCIADA)
        
       %===================================================================
       % B) LA TRAYECTORIA NO ESTA ASOCIADA A NINGUN GRUPO, ENTONCES VEMOS
       % SI EL GRUPO PROPUESTO ESTA LIBRE Y SI LO ESTA LA ASOCIAMOS, SI NO
       % ESTA LIBRE VERIFICAMOS QUE ESTA TRAYECTORIA SEA UNA MEJOR OPCION
       % PARA ES GRUPO QUE LA QUE HABIA ANTERIORMENTE Y SI ES ASI
       % REEMPLAZAMOS LAS TRAYECTORIAS.
       %===================================================================
        
        
          
        if( ~TrajStruct(imember).trajectories(ii).asociada && ...
            ~isempty(TrajStruct(imember).trajectories(ii).length) ) 
        %Intentamos asociar la trayectoria actual (no asociada previamente)
        %a un grupo. 
          if(sum(isnan(group(min_group).minlat(imember,index)))==length(index))
            %Si todos los elementos son NaN, no habia nadie ocupando ese lugar...
            %Dale para adelante
            asociar=true;   %Se va a asociar esta trayectorias al grupo min_group.     
          else
            %Tengo que chequear cual/es es/son la/s trayectoria/s que esta/s asociada/s en
            %ese lugar y cual es la mejor opcion.
            %Calculo cuanto se superponen cada una de esas trayectorias con
            %el grupo y cuan cerca o lejos estan del origen.
            isnan_tr=~isnan(group(min_group).meanlon);
            traj_asociadas=unique(group(min_group).trajid(imember,index));
            traj_asociadas=traj_asociadas(~isnan(traj_asociadas));
            tmp_cfd=NaN(1,length(traj_asociadas)) ; %Distance cost function
            tmp_cft=NaN(1,length(traj_asociadas)) ; %Time cost function

            tmp_grouplon=group(min_group).meanlon(isnan_tr);
            tmp_grouplat=group(min_group).meanlat(isnan_tr);
            tmp_grouptime=times(isnan_tr);
            tmp_meanlength=group(min_group).meanlength;
            initime=times(1);
            for kk=1:length(traj_asociadas)
                %Calculo la funcion de costo entre las trayectorias
                %asociadas y la trayectoria actual.
                
                [tmp_cfd(kk) tmp_cft(kk)]=traj_cost_function_fun(tmp_grouplon,tmp_grouplat,tmp_grouptime,tmp_meanlength,...
                TrajStruct(imember).trajectories(traj_asociadas(kk)).minlonf,TrajStruct(imember).trajectories(traj_asociadas(kk)).minlatf,...
                TrajStruct(imember).trajectories(traj_asociadas(kk)).daten,initime);
            end
            %
            if( min_cft < min(tmp_cft) )
            %Si la nueva trayectoria esta mas cerca del origen temporal del
            %grupo que la trayectoria que la o las trayectorias que habia
            %antes en ese lugar, me quedo con la nueva.
            %Asocio la nueva trayectoria. Pero primero tengo ademas que
            %desasociar las anteriores.
            asociar=true;
            
            %Desasocio las anteriores
            for kk=1:length(traj_asociadas)
                index_asociada=(group(min_group).trajid(imember,:)==traj_asociadas(kk));
                TrajStruct(imember).trajectories(traj_asociadas(kk)).asociada=false;
                TrajStruct(imember).trajectories(traj_asociadas(kk)).idgroup=NaN;
                group(min_group).minlat(imember,index_asociada)=NaN;
                group(min_group).minlon(imember,index_asociada)=NaN;
                group(min_group).minanomsis(imember,index_asociada)=NaN;
                group(min_group).minarea(imember,index_asociada)=NaN;
                group(min_group).uvel(imember,index_asociada)=NaN;
                group(min_group).vvel(imember,index_asociada)=NaN;
                group(min_group).trajid(imember,index_asociada)=NaN;
                %Recalculo la media del grupo.
                group(min_group).meanlat=nanmean(group(min_group).minlat,1);
                for kt=1:ntimes %Cuidado especial hay que tener siempre que se promedian longitudes.
                group(min_group).meanlon(kt)=mean_lon_fun(group(min_group).minlon(:,kt));
                end
                group(min_group).nmember=sum(any(~isnan(group(min_group).minlat),2));
                aux=sum(~isnan(group(min_group).minlat),2);
                aux(aux==0)=NaN;        
                group(min_group).meanlength=nanmean(aux);
                
            end
            
            end
          end
        end

          
          if(asociar)
          %La trayectoria esta "cerca" de uno de los grupos (hay uno en particular que es el que esta mas cerca).
          %Asigno la trayectoria a ese grupo!!

          TrajStruct(imember).trajectories(ii).asociada=true;
          TrajStruct(imember).trajectories(ii).idgroup=min_group; %A que grupo asignamos la trajectoria.
          group(min_group).minlat(imember,index)=TrajStruct(imember).trajectories(ii).minlatf;
          group(min_group).minlon(imember,index)=TrajStruct(imember).trajectories(ii).minlonf;
          group(min_group).minanomsis(imember,index)=TrajStruct(imember).trajectories(ii).minanomsisf;
          group(min_group).minarea(imember,index)=TrajStruct(imember).trajectories(ii).minareaf;
          group(min_group).uvel(imember,index)=TrajStruct(imember).trajectories(ii).uvelf;
          group(min_group).vvel(imember,index)=TrajStruct(imember).trajectories(ii).vvelf;
          group(min_group).trajid(imember,index)=ii;
          %Recalculo la media del grupo.
          group(min_group).meanlat=nanmean(group(min_group).minlat,1);
          for kt=1:ntimes %Cuidado especial hay que tener siempre que se promedian longitudes.
          group(min_group).meanlon(kt)=mean_lon_fun(group(min_group).minlon(:,kt));
          end
          group(min_group).nmember=sum(any(~isnan(group(min_group).minlat),2));
          aux=sum(~isnan(group(min_group).minlat),2);
          aux(aux==0)=NaN;        
          group(min_group).meanlength=nanmean(aux);
          
          end  %Este es el end del if sobre si se asocia o no la trayectoria al grupo correspondiente.
              
       end  %Este es el end del if sobre si existe un grupo posible para esta trayectoria.


end

