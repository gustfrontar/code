%function [group]=group_trajectories_fun(TrajStruct,AnalysisTrajStruct,config)

%INCLUDE SPECIFIC FUNCTIONS DIRECTORY.
addpath('../common_functions_vpo/');


%Esta funcion toma un ensamble de trayectorias y agrupa las diferentes
%trajectorias usando un criterio de cercania y superposicion temporal. Los
%grupos se inicializan con todas las trayectorias detectadas en el miembro
%1 del ensamble. 
%Para cada grupo esta definido por una matriz que contiene informacion
%sobre la ubicacion temporal y espacial de las trajectorias pronosticadas
%por cada miembro del ensamble y la media de dichas propiedades. La
%asociacion de una trajectoria con un dado grupo se hace en funcion de la
%media del grupo. Una vez terminada la primera etapa de asociacion se
%repite el proceso en un loop hasta que los grupos se estabilicen y ninguna
%trayectoria cambie de grupo durante el proceso. 

%INPUT
%TrajStruct es la estructura con todas las trayectorias, tal cual la
%produce la funcion que las calcula. 
%config contiene la fecha inicial, el numero de fechas, la frecuencia
%temporal, etc.

%TEST?--------------------------------------------------
clear all
close all
load ../RESULTS/ANALISIS/TRAJECTORIES/TRAJ_2007060112_L7.mat 

load ../RESULTS/kwbc/TRAJECTORIES/TRAJ_2007060112_L7.mat
TrajStruct=EnsTrajStruct;
config.date_ini='2007060112';
config.date_end=datestr(datenum(config.date_ini,'yyyymmddHH')+7,'yyyymmddHH');
config.timefrec=6;
config.mintrajlength=12;
%-------------------------------------------------------

tr_cost_function=500e3;
MaxGroup=150;             %Maximo numero de grupos que vamos a allocatear de entrada.

CheckGroupMerge=true;
%obtengo el numero de miembros en el ensamble.
enssize=size(TrajStruct,2);

MaxIterations=2;           %Cuantas pasadas del algoritmo para definir los grupos.

fprintf('CONFORMANDO LOS GRUPOS CORRESPONDIENTES A LA FECHA %s\n',config.date_ini);


%==========================================================================
% PASO 1: Tomamos las trayectorias presentes en el tiempo 1 y armamos los
% grupos iniciales.
% EN ESTE PRIMER PASO VAMOS A SER EXIGENTES CON EL CRITERIO DE
% SUPERPOSICION, VAMOS A PEDIR QUE GRAN PARTE DE LA TRAYECTORIA ESTE
% CONTENIDA EN EL GRUPO Y QUE GRAN PARTE DEL GRUPO COINCIDA CON LA
% TRAYECTORIA. 
%==========================================================================
tic
%Compute total number of times.
ntimes=(datenum(config.date_end,'yyyymmddHH')-datenum(config.date_ini,'yyyymmddHH'))*24/config.timefrec+1;
times=datenum(config.date_ini,'yyyymmddHH'):(config.timefrec/24):datenum(config.date_end,'yyyymmddHH');


%INITIALIZE GROUP ARRAYS.
minlat=NaN(enssize,ntimes,MaxGroup);
minlon=NaN(enssize,ntimes,MaxGroup);
minanomsis=NaN(enssize,ntimes,MaxGroup);
minarea=NaN(enssize,ntimes,MaxGroup);
uvel=NaN(enssize,ntimes,MaxGroup);
vvel=NaN(enssize,ntimes,MaxGroup);
meanlat=NaN(ntimes,MaxGroup);
meanlon=NaN(ntimes,MaxGroup);
trajid=NaN(enssize,ntimes,MaxGroup);   %Guarda el id de la trajectoria que fue asignada a cada punto.
nmember=NaN(MaxGroup);
meanlength=NaN(MaxGroup);

minlatanalysis=NaN(ntimes,MaxGroup);
minlonanalysis=NaN(ntimes,MaxGroup);
minareaanalysis=NaN(ntimes,MaxGroup);
minanomsisanalysis=NaN(ntimes,MaxGroup);
uvelanalysis=NaN(ntimes,MaxGroup);
vvelanalysis=NaN(ntimes,MaxGroup);
trajidanalysis=NaN(ntimes,MaxGroup);

ngroup=0;

%OJO ESTE LOOP SE TIENE QUE EJECUTAR UNA SOLA VEZ.


for nloop=1:MaxIterations;

    
for imember=1:enssize
ntraj=size(TrajStruct(imember).trajectories,2);  %Numero de trayectorias en el miembro correspondiente.
for ii=1:ntraj
    
    if(nloop==1)
    TrajStruct(imember).trajectories(ii).asociada=false;  %#ok<SAGROW> %Indica si la trajectoria fue o no asociada, se inicializa como false.
    end
    if( ngroup > 0 && ~isempty(TrajStruct(imember).trajectories(ii).length) && TrajStruct(imember).trajectories(ii).length > 3)
    %Hago un loop sobre todos los grupos para ver si puedo asociar la trajectoria ii del miembro imember con algun grupo.    
       cfd=NaN(1,ngroup);  %Distance cost function
       cft=NaN(1,ngroup);  %Time cost function
       cfgr=NaN(1,ngroup); %Group superposition cost function
       cftr=NaN(1,ngroup); %Trajectory superposition cost function.
       for jgroup=1:ngroup
         %Tengo que comparar la trajectoria ii del miembro imember con 
         %la media del grupo jgroup y calcular una funcion de costo. Luego
         %ver a que grupo deberia asociar la trajectoria.
         isnan_tr=~isnan(meanlon(:,jgroup));
         grouplon=meanlon(isnan_tr,jgroup);
         grouplat=meanlat(isnan_tr,jgroup);
         grouplength=meanlength(jgroup);
         grouptime=times(isnan_tr);
         initime=times(1);
         
         [cfd(jgroup) cft(jgroup) cfgr(jgroup) cftr(jgroup)]=traj_cost_function_fun(grouplon,grouplat,grouptime,grouplength,...
                                                      TrajStruct(imember).trajectories(ii).minlonf,TrajStruct(imember).trajectories(ii).minlatf,...
                                                      TrajStruct(imember).trajectories(ii).daten,initime); 
                                                  
  
         
         %AGREGAR UN CRITERIO DE SELECCION MAS EXIGENTE QUE NO SOLO TOME EN
         %CUENTA LA DISTANCIA MEDIA SINO QUE TAMBIEN LA DISTANCIA AL INICIO
         %DEL GRUPO
       end  %En del for sobre los grupos para calcular la funcion de costo.
       cft( cfd >= tr_cost_function)=NaN;  %Elimino los grupos que no cumplen el criterio de distancia.
       cft( cfgr <= 0.4            )=NaN;  %Elimino los que no coinciden en al menos un 50 % con el grupo.
       cft( cftr <= 0.4            )=NaN;  %Elimino aquellas trayectorias que no estan contenidas al menos en un 50% por el grupo.
    
       
       %Ahora tengo en cuenta la distancia del inicio de la trajectoria al
       %origen del grupo (en tiempo).
       %De todos los posibles grupos, me quedo con el que es mas
       %consistente en terminos del origen de la trayectoria.
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
  
        if( TrajStruct(imember).trajectories(ii).asociada &&  TrajStruct(imember).trajectories(ii).idgroup~=min_group) 
        %CASO EN EL QUE LA TRAYECTORIA ESTA ASOCIADA PERO NO AL GRUPO QUE
        %APARECE COMO EL MEJOR AHORA... 
        %TENGO QUE VER SI EL NUEVO GRUPO TIENE UNA TRAYECTORIA ASOCIADA
        %PARA ESE RANGO DE TIEMPO Y PARA ESE MIEMBRO DEL ENSAMBLE, SI ES
        %ASI LAS COMPARO Y VEO CUAL ES MEJOR, SINO ASOCIO DIRECTAMENTE ESTA
        %TRAYECTORIA Y LA DESASOCIO DEL GRUPO ANTERIOR. 
        %Intentamos asociar la trayectoria actual previamente asociada a un
        %nuevo grupo. 
          if(sum(isnan(minlat(imember,index,min_group)))==length(index))
            %Si todos los elementos son NaN, no habia nadie ocupando ese lugar...
            %Dale para adelante
            asociar=true;   %Se va a asociar esta trayectorias al grupo min_group.
            %Pero tambien tengo que desasociarla del grupo en el que estaba
            %antes.
            grupo_anterior=TrajStruct(imember).trajectories(ii).idgroup;
            index_asociada=(trajid(imember,:,grupo_anterior)==ii);
            TrajStruct(imember).trajectories(ii).idgroup=NaN; %#ok<SAGROW>
            minlat(imember,index_asociada,grupo_anterior)=NaN;
            minlon(imember,index_asociada,grupo_anterior)=NaN;
            minanomsis(imember,index_asociada,grupo_anterior)=NaN;
            minarea(imember,index_asociada,grupo_anterior)=NaN;
            uvel(imember,index_asociada,grupo_anterior)=NaN;
            vvel(imember,index_asociada,grupo_anterior)=NaN;
            trajid(imember,index_asociada,grupo_anterior)=NaN;
            %Recalculo la media del grupo.
            meanlat(:,grupo_anterior)=nanmean(minlat(:,:,grupo_anterior),1);
            for kt=1:ntimes %Cuidado especial hay que tener siempre que se promedian longitudes.
            meanlon(kt,grupo_anterior)=mean_lon_fun(minlon(:,kt,grupo_anterior));
            end
            nmember(grupo_anterior)=sum(any(~isnan(minlat(:,:,grupo_anterior)),2));
            aux=sum(~isnan(minlat(:,:,grupo_anterior)),2);
            aux(aux==0)=NaN;        
            meanlength(grupo_anterior)=nanmean(aux);
            
          else
            %Tengo que chequear cual/es es/son la/s trayectoria/s que esta/s asociada/s en
            %ese lugar y cual es la mejor opcion.
            %Calculo cuanto se superponen cada una de esas trayectorias con
            %el grupo y cuan cerca o lejos estan del origen.
            isnan_tr=~isnan(meanlon(:,min_group));
            traj_asociadas=unique(trajid(imember,index,min_group));
            traj_asociadas=traj_asociadas(~isnan(traj_asociadas));
            tmp_cfd=NaN(1,length(traj_asociadas)) ; %Distance cost function
            tmp_cft=NaN(1,length(traj_asociadas)) ; %Time cost function

            tmp_grouplon=meanlon(isnan_tr,min_group);
            tmp_grouplat=meanlat(isnan_tr,min_group);
            tmp_grouptime=times(isnan_tr);
            tmp_grouplength=meanlength(min_group);
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
                index_asociada=(trajid(imember,:,min_group)==traj_asociadas(kk));
                TrajStruct(imember).trajectories(traj_asociadas(kk)).asociada=false; %#ok<SAGROW>
                TrajStruct(imember).trajectories(traj_asociadas(kk)).idgroup=NaN;  %#ok<SAGROW> %Se analizara en breve
                minlat(imember,index_asociada,min_group)=NaN;
                minlon(imember,index_asociada,min_group)=NaN;
                minanomsis(imember,index_asociada,min_group)=NaN;
                minarea(imember,index_asociada,min_group)=NaN;
                uvel(imember,index_asociada,min_group)=NaN;
                vvel(imember,index_asociada,min_group)=NaN;
                trajid(imember,index_asociada,min_group)=NaN;
                %Recalculo la media del grupo.
                meanlat(:,min_group)=nanmean(minlat(:,:,min_group),1);
                for kt=1:ntimes %Cuidado especial hay que tener siempre que se promedian longitudes.
                meanlon(kt,min_group)=mean_lon_fun(minlon(:,kt,min_group));
                end
                nmember(min_group)=sum(any(~isnan(minlat(:,:,min_group)),2));
                aux=sum(~isnan(minlat(:,:,min_group)),2);
                aux(aux==0)=NaN;        
                meanlength(min_group)=nanmean(aux);
                
            end
            %Desasocio tambien la trayectoria ii de su grupo anterior.
            grupo_anterior=TrajStruct(imember).trajectories(ii).idgroup;
            index_asociada=(trajid(imember,:,grupo_anterior)==ii);
            TrajStruct(imember).trajectories(ii).idgroup=min_group; %#ok<SAGROW>
            minlat(imember,index_asociada,grupo_anterior)=NaN;
            minlon(imember,index_asociada,grupo_anterior)=NaN;
            minanomsis(imember,index_asociada,grupo_anterior)=NaN;
            minarea(imember,index_asociada,grupo_anterior)=NaN;
            uvel(imember,index_asociada,grupo_anterior)=NaN;
            vvel(imember,index_asociada,grupo_anterior)=NaN;
            trajid(imember,index_asociada,grupo_anterior)=NaN;
            %Recalculo la media del grupo.
            meanlat(:,grupo_anterior)=nanmean(minlat(:,:,grupo_anterior),1);
            for kt=1:ntimes %Cuidado especial hay que tener siempre que se promedian longitudes.
            meanlon(kt,grupo_anterior)=mean_lon_fun(minlon(:,kt,grupo_anterior));
            end
            nmember(grupo_anterior)=sum(any(~isnan(minlat(:,:,grupo_anterior)),2));
            aux=sum(~isnan(minlat(:,:,grupo_anterior)),2);
            aux(aux==0)=NaN;        
            meanlength(grupo_anterior)=nanmean(aux);
            
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

          if(sum(isnan(minlat(imember,index,min_group)))==length(index))
            %Si todos los elementos son NaN, no habia nadie ocupando ese lugar...
            %Dale para adelante
            asociar=true;   %Se va a asociar esta trayectorias al grupo min_group.     
          else
            %Tengo que chequear cual/es es/son la/s trayectoria/s que esta/s asociada/s en
            %ese lugar y cual es la mejor opcion.
            %Calculo cuanto se superponen cada una de esas trayectorias con
            %el grupo y cuan cerca o lejos estan del origen.
            isnan_tr=~isnan(meanlon(:,min_group));
            traj_asociadas=unique(trajid(imember,index,min_group));
            traj_asociadas=traj_asociadas(~isnan(traj_asociadas));
            tmp_cfd=NaN(1,length(traj_asociadas)) ; %Distance cost function
            tmp_cft=NaN(1,length(traj_asociadas)) ; %Time cost function

            tmp_grouplon=meanlon(isnan_tr,min_group);
            tmp_grouplat=meanlat(isnan_tr,min_group);
            tmp_grouptime=times(isnan_tr);
            tmp_meanlength=meanlength(min_group);
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
                index_asociada=(trajid(imember,:,min_group)==traj_asociadas(kk));
                TrajStruct(imember).trajectories(traj_asociadas(kk)).asociada=false; %#ok<SAGROW>
                TrajStruct(imember).trajectories(traj_asociadas(kk)).idgroup=NaN; %#ok<SAGROW>
                minlat(imember,index_asociada,min_group)=NaN;
                minlon(imember,index_asociada,min_group)=NaN;
                minanomsis(imember,index_asociada,min_group)=NaN;
                minarea(imember,index_asociada,min_group)=NaN;
                uvel(imember,index_asociada,min_group)=NaN;
                vvel(imember,index_asociada,min_group)=NaN;
                trajid(imember,index_asociada,min_group)=NaN;
                %Recalculo la media del grupo.
                meanlat(:,min_group)=nanmean(minlat(:,:,min_group),1);
                for kt=1:ntimes %Cuidado especial hay que tener siempre que se promedian longitudes.
                meanlon(kt,min_group)=mean_lon_fun(minlon(:,kt,min_group));
                end
                nmember(min_group)=sum(any(~isnan(minlat(:,:,min_group)),2));
                aux=sum(~isnan(minlat(:,:,min_group)),2);
                aux(aux==0)=NaN;        
                meanlength(min_group)=nanmean(aux);
                
            end
            
            end
          end
        end

          
          if(asociar)
          %La trayectoria esta "cerca" de uno de los grupos (hay uno en particular que es el que esta mas cerca).
          %Asigno la trayectoria a ese grupo!!

          TrajStruct(imember).trajectories(ii).asociada=true; %#ok<SAGROW>
          TrajStruct(imember).trajectories(ii).idgroup=min_group; %#ok<SAGROW> %A que grupo asignamos la trajectoria.
          minlat(imember,index,min_group)=TrajStruct(imember).trajectories(ii).minlatf;
          minlon(imember,index,min_group)=TrajStruct(imember).trajectories(ii).minlonf;
          minanomsis(imember,index,min_group)=TrajStruct(imember).trajectories(ii).minanomsisf;
          minarea(imember,index,min_group)=TrajStruct(imember).trajectories(ii).minareaf;
          uvel(imember,index,min_group)=TrajStruct(imember).trajectories(ii).uvelf;
          vvel(imember,index,min_group)=TrajStruct(imember).trajectories(ii).vvelf;
          trajid(imember,index,min_group)=ii;
          %Recalculo la media del grupo.
          meanlat(:,min_group)=nanmean(minlat(:,:,min_group),1);
          for kt=1:ntimes %Cuidado especial hay que tener siempre que se promedian longitudes.
          meanlon(kt,min_group)=mean_lon_fun(minlon(:,kt,min_group));
          end
          nmember(min_group)=sum(any(~isnan(minlat(:,:,min_group)),2));
          aux=sum(~isnan(minlat(:,:,min_group)),2);
          aux(aux==0)=NaN;        
          meanlength(min_group)=nanmean(aux);
          
          end  %Este es el end del if sobre si se asocia o no la trayectoria al grupo correspondiente.
              
       end  %Este es el end del if sobre si existe un grupo posible para esta trayectoria.
    end    %En del if sobre si ngroup > 0.
    
    %Si la trayectoria no fue asociada con ninguna otra entonces veo si
    %podria ser un buen candidato para generar un grupo nuevo.
    %Solo vamos a iniciar nuevos grupos a partir de las trayectorias
    %largas. No vamos a dejar que trajectorias cortitas inicien nuevos
    %grupos. Pero si las trajectorias cortas pueden asociarse a un grupo
    %que contenga al menos una trajectoria larga.
    
    if(~TrajStruct(imember).trajectories(ii).asociada && ...
       ~isempty(TrajStruct(imember).trajectories(ii).length) && ...
       TrajStruct(imember).trajectories(ii).length >= config.mintrajlength ) %Solo las trajectorias de al menos 3 dias inician grupos.
    ngroup=ngroup+1;
    
    %Calculamos los lugares que les corresponde
    [nada index] = intersect(times,TrajStruct(imember).trajectories(ii).daten);
    TrajStruct(imember).trajectories(ii).asociada=true; %#ok<SAGROW>
    TrajStruct(imember).trajectories(ii).idgroup=ngroup; %#ok<SAGROW> %A que grupo asignamos la trajectoria.
    minlat(imember,index,ngroup)=TrajStruct(imember).trajectories(ii).minlatf;
    minlon(imember,index,ngroup)=TrajStruct(imember).trajectories(ii).minlonf;
    minanomsis(imember,index,ngroup)=TrajStruct(imember).trajectories(ii).minanomsisf;
    minarea(imember,index,ngroup)=TrajStruct(imember).trajectories(ii).minareaf;
    uvel(imember,index,ngroup)=TrajStruct(imember).trajectories(ii).uvelf;
    vvel(imember,index,ngroup)=TrajStruct(imember).trajectories(ii).vvelf;
    meanlat(index,ngroup)=TrajStruct(imember).trajectories(ii).minlatf;
    meanlon(index,ngroup)=TrajStruct(imember).trajectories(ii).minlonf;
    trajid(imember,index,ngroup)=ii;
    nmember(ngroup)=1;                %Cuantos miembros aportan trayectorias a este grupo.
    aux=sum(~isnan(minlat(:,:,ngroup)),2);
    aux(aux==0)=NaN;        
    meanlength(ngroup)=nanmean(aux);

    
    end  %End del if sobre la creacion de un nuevo grupo.
    
end   %End del loop sobre las trajectorias del miembro imember


end %End del loop sobre los miembros del ensamble



%==========================================================================
% ASOCIO LAS TRAYECTORIAS DEL ANALISIS A LOS GRUPOS QUE SE FORMARON
%==========================================================================


%LOS GRUPOS ESTAN CONSOLIDADOS Y NO VAN A CAMBIAR EN ESTE PASO ASI QUE NO
%HAY NECESIDAD DE HACER VARIAS ITERACIONES.

ntraj=size(AnalysisTrajStruct,2);  %Numero de trayectorias en el miembro correspondiente.

for ii=1:ntraj
    
    if(nloop==1)
    AnalysisTrajStruct(ii).asociada=false;  %Indica si la trajectoria fue o no asociada, se inicializa como false.
    end
    if( ~isempty(AnalysisTrajStruct(ii).length) )
    %Hago un loop sobre todos los grupos para ver si puedo asociar la trajectoria ii del miembro imember con algun grupo.    
       cfd=NaN(1,ngroup);  %Distance cost function
       cft=NaN(1,ngroup);  %Time cost function
       cfgr=NaN(1,ngroup); %Group superposition cost function
       cftr=NaN(1,ngroup); %Trajectory superposition cost function.
       for jgroup=1:ngroup
         %Tengo que comparar la trajectoria ii del miembro imember con 
         %la media del grupo jgroup y calcular una funcion de costo. Luego
         %ver a que grupo deberia asociar la trajectoria.
         isnan_tr=~isnan(meanlon(:,jgroup));
         grouplon=meanlon(isnan_tr,jgroup);
         grouplat=meanlat(isnan_tr,jgroup);
         grouplength=meanlength(jgroup);
         grouptime=times(isnan_tr);
         initime=times(1);
         
         [cfd(jgroup) cft(jgroup) cfgr(jgroup) cftr(jgroup)]=traj_cost_function_fun(grouplon,grouplat,grouptime,grouplength,...
                                                      AnalysisTrajStruct(ii).minlonf,AnalysisTrajStruct(ii).minlatf,...
                                                      AnalysisTrajStruct(ii).daten,initime); 
         
         %AGREGAR UN CRITERIO DE SELECCION MAS EXIGENTE QUE NO SOLO TOME EN
         %CUENTA LA DISTANCIA MEDIA SINO QUE TAMBIEN LA DISTANCIA AL INICIO
         %DEL GRUPO
       end  %En del for sobre los grupos para calcular la funcion de costo.
       cft( cfd >= tr_cost_function)=NaN;  %Elimino los grupos que no cumplen el criterio de distancia.
       cft( cfgr <= 0.4            )=NaN;  %Elimino los que no coinciden en al menos un 50 % con el grupo.
       cft( cftr <= 0.4            )=NaN;  %Elimino aquellas trayectorias que no estan contenidas al menos en un 50% por el grupo.
    
       
       %Ahora tengo en cuenta la distancia del inicio de la trajectoria al
       %origen del grupo (en tiempo).
       %De todos los posibles grupos, me quedo con el que es mas
       %consistente en terminos del origen de la trayectoria.
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
          [nada index] = intersect(times,AnalysisTrajStruct(ii).daten);  
          %Vemos si esos lugares estan ocupados.
          
       %===================================================================
       % A) LA TRAYECTORIA YA ESTA ASOCIADA, VEMOS SI EL GRUPO AL QUE FUE
       % ASOCIADA COINCIDE CON EL MEJOR, SI ES ASI LA DEJAMOS DONDE ESTA,
       % SINO VEMOS EL NUEVO GRUPO, SI ESTA LIBRE LA MOVEMOS, SINO
       % CHEQUEAMOS QUE ESTA TRAYECTORIA SEA MEJOR PARA EL NUEVO GRUPO QUE
       % LA QUE EL NUEVO GRUPO TENIA EN ESE LUGAR Y SI ES ASI HACEMOS EL
       % CAMBIO DE GRUPO.
       %===================================================================
      
        if( AnalysisTrajStruct(ii).asociada &&  AnalysisTrajStruct(ii).idgroup~=min_group) 
        %CASO EN EL QUE LA TRAYECTORIA ESTA ASOCIADA PERO NO AL GRUPO QUE
        %APARECE COMO EL MEJOR AHORA... 
        %TENGO QUE VER SI EL NUEVO GRUPO TIENE UNA TRAYECTORIA ASOCIADA
        %PARA ESE RANGO DE TIEMPO Y PARA ESE MIEMBRO DEL ENSAMBLE, SI ES
        %ASI LAS COMPARO Y VEO CUAL ES MEJOR, SINO ASOCIO DIRECTAMENTE ESTA
        %TRAYECTORIA Y LA DESASOCIO DEL GRUPO ANTERIOR. 
        %Intentamos asociar la trayectoria actual previamente asociada a un
        %nuevo grupo. 
          if(sum(isnan(minlatanalysis(index,min_group)))==length(index))
            %Si todos los elementos son NaN, no habia nadie ocupando ese lugar...
            %Dale para adelante
            asociar=true;   %Se va a asociar esta trayectorias al grupo min_group.
            %Pero tambien tengo que desasociarla del grupo en el que estaba
            %antes.
            grupo_anterior=AnalysisTrajStruct(ii).idgroup;
            index_asociada=(trajidanalysis(:,grupo_anterior)==ii);
            AnalysisTrajStruct(ii).idgroup=NaN;
            minlatanalysis(index_asociada,grupo_anterior)=NaN;
            minlonanalysis(index_asociada,grupo_anterior)=NaN;
            minanomsisanalysis(index_asociada,grupo_anterior)=NaN;
            minareaanalysis(index_asociada,grupo_anterior)=NaN;
            uvelanalysis(index_asociada,grupo_anterior)=NaN;
            vvelanalysis(index_asociada,grupo_anterior)=NaN;
            trajidanalysis(index_asociada,grupo_anterior)=NaN;
          else
         
            %Tengo que chequear cual/es es/son la/s trayectoria/s que esta/s asociada/s en
            %ese lugar y cual es la mejor opcion.
            %Calculo cuanto se superponen cada una de esas trayectorias con
            %el grupo y cuan cerca o lejos estan del origen.
            isnan_tr=~isnan(meanlon(:,min_group));
            traj_asociadas=unique(trajidanalysis(index,min_group));
            traj_asociadas=traj_asociadas(~isnan(traj_asociadas));
            tmp_cfd=NaN(1,length(traj_asociadas)) ; %Distance cost function
            tmp_cft=NaN(1,length(traj_asociadas)) ; %Time cost function

            tmp_grouplon=meanlon(isnan_tr,min_group);
            tmp_grouplat=meanlat(isnan_tr,min_group);
            tmp_grouptime=times(isnan_tr);
            tmp_grouplength=meanlength(min_group);
            initime=times(1);
            for kk=1:length(traj_asociadas)
                %Calculo la funcion de costo entre las trayectorias
                %asociadas y la trayectoria actual.
                
                [tmp_cfd(kk) tmp_cft(kk)]=traj_cost_function_fun(tmp_grouplon,tmp_grouplat,tmp_grouptime,tmp_grouplength,...
                AnalysisTrajStruct(traj_asociadas(kk)).minlonf,AnalysisTrajStruct(traj_asociadas(kk)).minlatf,...
                AnalysisTrajStruct(traj_asociadas(kk)).daten,initime);
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
                index_asociada=(trajidanalysis(:,min_group)==traj_asociadas(kk));
                AnalysisTrajStruct(traj_asociadas(kk)).asociada=false;
                AnalysisTrajStruct(traj_asociadas(kk)).idgroup=NaN;  %Se analizara en breve
                minlatanalysis(index_asociada,min_group)=NaN;
                minlonanalysis(index_asociada,min_group)=NaN;
                minanomsisanalysis(index_asociada,min_group)=NaN;
                minareaanalysis(index_asociada,min_group)=NaN;
                uvelanalysis(index_asociada,min_group)=NaN;
                vvelanalysis(index_asociada,min_group)=NaN;
                trajidanalysis(index_asociada,min_group)=NaN;  
            end

            %Desasocio tambien la trayectoria ii de su grupo anterior.
            grupo_anterior=AnalysisTrajStruct(ii).idgroup;
            index_asociada=(trajidanalysis(:,grupo_anterior)==ii);
            AnalysisTrajStruct(ii).idgroup=min_group;
            minlatanalysis(index_asociada,grupo_anterior)=NaN;
            minlonanalysis(index_asociada,grupo_anterior)=NaN;
            minanomsisanalysis(index_asociada,grupo_anterior)=NaN;
            minareaanalysis(index_asociada,grupo_anterior)=NaN;
            uvelanalysis(index_asociada,grupo_anterior)=NaN;
            vvelanalysis(index_asociada,grupo_anterior)=NaN;
            trajidanalysis(index_asociada,grupo_anterior)=NaN;

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
        
   
          
        if( ~AnalysisTrajStruct(ii).asociada && ~isempty(AnalysisTrajStruct(ii).length) ) 
            
            
        %Intentamos asociar la trayectoria actual (no asociada previamente)
        %a un grupo. 
          if(sum(isnan(minlatanalysis(index,min_group)))==length(index))
            %Si todos los elementos son NaN, no habia nadie ocupando ese lugar...
            %Dale para adelante
            asociar=true;   %Se va a asociar esta trayectorias al grupo min_group.     
          else
            %Tengo que chequear cual/es es/son la/s trayectoria/s que esta/s asociada/s en
            %ese lugar y cual es la mejor opcion.
            %Calculo cuanto se superponen cada una de esas trayectorias con
            %el grupo y cuan cerca o lejos estan del origen.
            isnan_tr=~isnan(meanlon(:,min_group));
            traj_asociadas=unique(trajidanalysis(index,min_group));
            traj_asociadas=traj_asociadas(~isnan(traj_asociadas));
            tmp_cfd=NaN(1,length(traj_asociadas)) ; %Distance cost function
            tmp_cft=NaN(1,length(traj_asociadas)) ; %Time cost function

            tmp_grouplon=meanlon(isnan_tr,min_group);
            tmp_grouplat=meanlat(isnan_tr,min_group);
            tmp_grouptime=times(isnan_tr);
            tmp_meanlength=meanlength(min_group);
            initime=times(1);
            for kk=1:length(traj_asociadas)
                %Calculo la funcion de costo entre las trayectorias
                %asociadas y la trayectoria actual.
                
                [tmp_cfd(kk) tmp_cft(kk)]=traj_cost_function_fun(tmp_grouplon,tmp_grouplat,tmp_grouptime,tmp_meanlength,...
                AnalysisTrajStruct(traj_asociadas(kk)).minlonf,AnalysisTrajStruct(traj_asociadas(kk)).minlatf,...
                AnalysisTrajStruct(traj_asociadas(kk)).daten,initime);
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
                index_asociada=(trajidanalysis(:,min_group)==traj_asociadas(kk));
                AnalysisTrajStruct(traj_asociadas(kk)).asociada=false;
                AnalysisTrajStruct(traj_asociadas(kk)).idgroup=NaN;
                minlatanalysis(index_asociada,min_group)=NaN;
                minlonanalysis(index_asociada,min_group)=NaN;
                minanomsisanalysis(index_asociada,min_group)=NaN;
                minareaanalysis(index_asociada,min_group)=NaN;
                uvelanalysis(index_asociada,min_group)=NaN;
                vvelanalysis(index_asociada,min_group)=NaN;
                trajidanalysis(index_asociada,min_group)=NaN;
                
            end
            
            end
          end
        end

          
          if(asociar)
          %La trayectoria esta "cerca" de uno de los grupos (hay uno en particular que es el que esta mas cerca).
          %Asigno la trayectoria a ese grupo!!

          AnalysisTrajStruct(ii).asociada=true;
          AnalysisTrajStruct(ii).idgroup=min_group; %A que grupo asignamos la trajectoria.
          minlatanalysis(index,min_group)=AnalysisTrajStruct(ii).minlatf;
          minlonanalysis(index,min_group)=AnalysisTrajStruct(ii).minlonf;
          minanomsisanalysis(index,min_group)=AnalysisTrajStruct(ii).minanomsisf;
          minareaanalysis(index,min_group)=AnalysisTrajStruct(ii).minareaf;
          uvelanalysis(index,min_group)=AnalysisTrajStruct(ii).uvelf;
          vvelanalysis(index,min_group)=AnalysisTrajStruct(ii).vvelf;
          trajidanalysis(index,min_group)=ii;

          end  %Este es el end del if sobre si se asocia o no la trayectoria al grupo correspondiente.
              
       end  %Este es el end del if sobre si existe un grupo posible para esta trayectoria.
    end    %En del if sobre si ngroup > 0.
    
    %Si la trayectoria no fue asociada con ninguna otra entonces veo si
    %podria ser un buen candidato para generar un grupo nuevo.
    %Solo vamos a iniciar nuevos grupos a partir de las trayectorias
    %largas. No vamos a dejar que trajectorias cortitas inicien nuevos
    %grupos. Pero si las trajectorias cortas pueden asociarse a un grupo
    %que contenga al menos una trajectoria larga.
    
    if(~AnalysisTrajStruct(ii).asociada && ~isempty(AnalysisTrajStruct(ii).length) && ...
       AnalysisTrajStruct(ii).length >= config.mintrajlength ) %Solo las trajectorias de al menos 3 dias inician grupos.
    ngroup=ngroup+1;
    
    %Calculo los slots que va a ocupar el analysis
    [nada index] = intersect(times,AnalysisTrajStruct(ii).daten);
    AnalysisTrajStruct(ii).asociada=true;
    AnalysisTrajStruct(ii).idgroup=ngroup; %A que grupo asignamos la trajectoria.
    
    minlatanalysis(index,ngroup)=AnalysisTrajStruct(ii).minlatf;
    minlonanalysis(index,ngroup)=AnalysisTrajStruct(ii).minlonf;
    minareaanalysis(index,ngroup)=AnalysisTrajStruct(ii).minareaf;
    minanomsisanalysis(index,ngroup)=AnalysisTrajStruct(ii).minanomsisf;
    uvelanalysis(index,ngroup)=AnalysisTrajStruct(ii).uvelf;
    vvelanalysis(index,ngroup)=AnalysisTrajStruct(ii).vvelf;
    trajidanalysis(index,ngroup)=ii;
    
    
      
    end  %End del if sobre la creacion de un nuevo grupo.  
end   %End del loop sobre las trajectorias del analysis

end %End del for sobre las iteraciones del algoritmo.


% tiempo=toc;
% fprintf('TIEMPO EMPLEADO EN DEFINIR LOS GRUPOS: %f\n',tiempo);
% 
% 
% tic


tiempo=toc;
fprintf('TIEMPO EMPLEADO EN CONFORMAR LOS GRUPOS Y EN ASIGNAR LAS TRAYECTORIAS DEL ANALYSIS: %f\n',tiempo);

if(CheckGroupMerge)
tic
%==========================================================================
% CHEQUEO DE FUSION DE GRUPOS...
%==========================================================================
% Al final de de cada loop podemos chequear si existen grupos similares
% (que por ahi puedan fusionarse) si es asi. Fusionamos los grupos que a
% veces se separan por razones diversas y una vez que se separan es dificil
% (sino imposible) que todos los miembros que componen uno de los grupos se
% pasen al otro para volverlo a unir.


for ii=1:ngroup
    %Para cada grupo ii, verifico si esta cerca del grupo jj. Si lo esta
    %entonces coloco el grupo ii, en el grupo jj y recalculo sus
    %propiedades. Para determinar si un grupo es cercano a otro uso
    %criterios similares a los que aplique en las trayectorias. 
       cfd=NaN(1,ngroup);  %Distance cost function
       cft=NaN(1,ngroup);  %Time cost function
       cfgr=NaN(1,ngroup); %Group superposition cost function
       cftr=NaN(1,ngroup); %Trajectory superposition cost function.
       for jj=1:ngroup
           if(jj~=ii)
           
         if(sum(~isnan(meanlon(:,jj))) > 0) %Verificamos que el grupo no haya sido vaciado.  
         %Comparo la media de ambos grupos usando la misma funcion de costo
         %que use para las trayectorias.
         isnan_tr=~isnan(meanlon(:,jj));
         grouplon=meanlon(isnan_tr,jj);
         grouplat=meanlat(isnan_tr,jj);
         grouplength=meanlength(jj);
         grouptime=times(isnan_tr);
         
         isnan_tr=~isnan(meanlon(:,ii));
         grouplon2=meanlon(isnan_tr,ii);
         grouplat2=meanlat(isnan_tr,ii);
         grouptime2=times(isnan_tr);
         initime=times(1);
         
         [cfd(jj) cft(jj) cfgr(jj) cftr(jj)]=traj_cost_function_fun(grouplon,grouplat,grouptime,grouplength,...
                                                                    grouplon2,grouplat2,grouptime2,initime); 
         end
         
           end
       end  %En del for sobre los grupos para calcular la funcion de costo.
       cft( cfd >= tr_cost_function)=NaN;  %Elimino los grupos que no cumplen el criterio de distancia.
       cft( cftr <= 0.8            )=NaN;  %Elimino aquellos grupos que no estan contendios al menos en un 50% por el otro grupo.
       %Ahora tengo en cuenta la distancia del inicio de la trajectoria al
       %origen del grupo (en tiempo).
       %De todos los posibles grupos, me quedo con el que es mas
       %consistente en terminos del origen de la trayectoria.
       [min_cft min_group]=nanmin(cft);
       min_cfd=cfd(min_group);
       min_cfgr=cfgr(min_group);
       min_cftr=cftr(min_group);
       
       if(~isnan(min_cft)) %Quiere decir que hay 2 grupos que estan cerca en espacio
                           %y que coinciden en tiempo. Sin importar
                           %demasiado la distancia a los origenes entre
                           %ambos veo si los puedo superponer.
        %Chequeo si el grupo min_group tiene suficientes espacios libres
        %para albergar al grupo ii.
        tmpminlat=minlat(:,:,ii);
        tmpminlatanalysis=minlatanalysis(:,ii);
        
        indexii=~isnan(tmpminlat);
        indexiiana=~isnan(tmpminlatanalysis);
        
        tmpminlat2=minlat(:,:,min_group);
        tmpminlatanalysis2=minlatanalysis(:,min_group);
        if(sum(~isnan(tmpminlat2(indexii))) == 0 && sum(~isnan(tmpminlatanalysis2(indexiiana)))==0)
        %Entonces el grupo ii complementa (aunque no totalmente al grupo
        %min_group).
        fprintf('Asocie el grupo %f con el grupo %f \n ',ii,min_group);
          auxiliar1=minlat(:,:,ii);
          auxiliar2=minlat(:,:,min_group);
          auxiliar2(indexii)=auxiliar1(indexii);
          minlat(:,:,min_group)=auxiliar2;
          auxiliar1=minlon(:,:,ii);
          auxiliar2=minlon(:,:,min_group);
          auxiliar2(indexii)=auxiliar1(indexii);
          minlon(:,:,min_group)=auxiliar2;
          auxiliar1=minanomsis(:,:,ii);
          auxiliar2=minanomsis(:,:,min_group);
          auxiliar2(indexii)=auxiliar1(indexii);
          minanomsis(:,:,min_group)=auxiliar2;
          auxiliar1=minarea(:,:,ii);
          auxiliar2=minarea(:,:,min_group);
          auxiliar2(indexii)=auxiliar1(indexii);
          minarea(:,:,min_group)=auxiliar2;
          auxiliar1=uvel(:,:,ii);
          auxiliar2=uvel(:,:,min_group);
          auxiliar2(indexii)=auxiliar1(indexii);
          uvel(:,:,min_group)=auxiliar2;
          auxiliar1=vvel(:,:,ii);
          auxiliar2=vvel(:,:,min_group);
          auxiliar2(indexii)=auxiliar1(indexii);
          vvel(:,:,min_group)=auxiliar2;
          auxiliar1=trajid(:,:,ii);
          auxiliar2=trajid(:,:,min_group);
          auxiliar2(indexii)=auxiliar1(indexii);
          trajid(:,:,min_group)=auxiliar2;        

          %Recalculo la media del grupo.
          meanlat(:,min_group)=nanmean(minlat(:,:,min_group),1);
          for kt=1:ntimes %Cuidado especial hay que tener siempre que se promedian longitudes.
          meanlon(kt,min_group)=mean_lon_fun(minlon(:,kt,min_group));
          end
          nmember(min_group)=sum(any(~isnan(minlat(:,:,min_group)),2));
          aux=sum(~isnan(minlat(:,:,min_group)),2);
          aux(aux==0)=NaN;        
          meanlength(min_group)=nanmean(aux);
          
          %Transfer the analysis properties.
          minlatanalysis(indexiiana,min_group)=minlatanalysis(indexiiana,ii);
          minlonanalysis(indexiiana,min_group)=minlonanalysis(indexiiana,ii);
          minareaanalysis(indexiiana,min_group)=minareaanalysis(indexiiana,ii);
          minanomsisanalysis(indexiiana,min_group)=minanomsisanalysis(indexiiana,ii);
          uvelanalysis(indexiiana,min_group)=minanomsisanalysis(indexiiana,ii);
          vvelanalysis(indexiiana,min_group)=minanomsisanalysis(indexiiana,ii);
          trajidanalysis(indexiiana,min_group)=trajidanalysis(indexiiana,ii);
          
        %Vacio minlat y minlon del grupo ii para reconocer que fue
        %eliminado. Ademas con esto ya no puede recibir nuevas
        %trayectorias porque la funcion de costo va a dar siempre el valor
        %por defecto.
          minlat(:,:,ii)=NaN;
          minlon(:,:,ii)=NaN;
          meanlat(:,ii)=NaN;
          meanlon(:,ii)=NaN;
          minlatanalysis(:,ii)=NaN;
          minlonanalysis(:,ii)=NaN;
          
        %Ahora tengo que cambiar el numero de grupo de asociacion a todas
        %las trayectorias asociadas al grupo 1.
          for kk=1:enssize
             tmp=trajid(kk,:,ii);
             tmp=tmp(~isnan(tmp));
             if(~isempty(tmp))
                 tmp=unique(tmp); %Busco los valores unicos en tmp.
                 
                 for kkk=1:length(tmp)
                 %Para cada valor de tmp busco esa trayectoria y le cambio el id del grupo al que esta asociada.
                   TrajStruct(kk).trajectories(tmp(kkk)).idgroup=min_group;    %#ok<SAGROW>
                 end
             end
          end
        %Tengo que repetir el mismo procedimiento para las trayectorias
        %asociadas del analysis.
             tmp=trajidanalysis(:,ii);
             tmp=tmp(~isnan(tmp));
             if(~isempty(tmp))
                 tmp=unique(tmp); %Busco los valores unicos en tmp.
                 
                 for kkk=1:length(tmp)
                 %Para cada valor de tmp busco esa trayectoria y le cambio el id del grupo al que esta asociada.
                   AnalysisTrajStruct(tmp(kkk)).idgroup=min_group;   
                 end
             end

          
       
        end
       end
end


%Hasta aca verificamos si algunos grupos podian fusionarse en uno solo.
%Si esto sucedio, entonces habria que recalcular el numero de grupos y
%volverlos consecutidos otra vez.

tiempo=toc;
fprintf('TIEMPO EMPLEADO EN LA FUSION DE GRUPOS: %f\n',tiempo);


end %End del if sobre si chequeo o no la fusion de grupos.





%==========================================================================
% PASO 4: LUEGO DE CONFORMAR LOS GRUPOS, LA MEDIA DE LOS GRUPOS TIENE YA UN
% VALOR MAS ESTABLE. AGREGAMOS LOS SISTEMAS MAS PEQUENIOS RELAJANDO ALGUNOS
% CRITERIOS DE ASOCIACION, PERO NO PERMITIENDO REEMPLAZOS NI MOVIMIENTOS.
%==========================================================================


tic
for imember=1:enssize
ntraj=size(TrajStruct(imember).trajectories,2);  %Numero de trayectorias en el miembro correspondiente.

for ii=1:ntraj
    
if(~TrajStruct(imember).trajectories(ii).asociada  && ...
   ~isempty(TrajStruct(imember).trajectories(ii).length));  %#ok<ALIGN> %Solo nos vamos a ocupar de las trayectorias que no fueron asignadas.
    %Las que fueron asociadas en el loop anterior fueron asociadas con un
    %criterio mas estricto asi que no las vamos a reasociar ni a des
    %asociar.
    
    %Hago un loop sobre todos los grupos para ver si puedo asociar la trajectoria ii del miembro imember con algun grupo.    
       cfd=NaN(1,ngroup);  %Distance cost function
       cft=NaN(1,ngroup);  %Time cost function
       cfgr=NaN(1,ngroup); %Group superposition cost function
       cftr=NaN(1,ngroup); %Trajectory superposition cost function.
       for jgroup=1:ngroup
         %Tengo que comparar la trajectoria ii del miembro imember con 
         %la media del grupo jgroup y calcular una funcion de costo. Luego
         %ver a que grupo deberia asociar la trajectoria.
         isnan_tr=~isnan(meanlon(:,jgroup));
         grouplon=meanlon(isnan_tr,jgroup);
         grouplat=meanlat(isnan_tr,jgroup);
         grouptime=times(isnan_tr);
         grouplength=meanlength(jgroup);
         initime=times(1);
         
         [cfd(jgroup) cft(jgroup) cfgr(jgroup) cftr(jgroup)]=traj_cost_function_fun(grouplon,grouplat,grouptime,grouplength,...
                                                      TrajStruct(imember).trajectories(ii).minlonf,TrajStruct(imember).trajectories(ii).minlatf,...
                                                      TrajStruct(imember).trajectories(ii).daten,initime); 
         
       end  %En del for sobre los grupos para calcular la funcion de costo.
       cft( cfd >= tr_cost_function)=NaN;  %Elimino los grupos que no cumplen el criterio de distancia.
       %cft( cfgr <= 0.3            )=NaN;  %Elimino los que no coinciden en al menos un 50 % con el grupo.
       %Al eliminar el criterio sobre cfgr lo que hago es permitir que la
       %trayectoria sea muy corta respecto del grupo, pero al elevar el
       %otro criterio pido que este contenida casi totalmente por el grupo,
       %ademas no permito que se superponga con ninguna trayectorias
       %preexistente de ese miembro del ensamble para dicho grupo.
       cft( cftr <= 0.9            )=NaN;  %Elimino aquellas trayectorias que no estan contenidas al menos en un 50% por el grupo.
       %Ahora tengo en cuenta la distancia del inicio de la trajectoria al
       %origen del grupo (en tiempo).
       %De todos los posibles grupos, me quedo con el que es mas
       %consistente en terminos del origen de la trayectoria.
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
       % B) EN ESTE CASO HAY UNA SOLA POSIBILIDAD, LA TRAYECTORIA NO ESTA
       % ASOCIADA A NINGUN GRUPO.
       %===================================================================
        
        %Intentamos asociar la trayectoria actual (no asociada previamente)
        %a un grupo. 
          if(sum(isnan(minlat(imember,index,min_group)))==length(index))

          %En este caso a diferencia del anterior, solo asocio la
          %trayectoria si todos los "slots" que la nueva trayectoria va a
          %ocupar estaban libres. No permito que se cambie de lugar una
          %trayectoria que ya estaba asignada porque podria mover alguna
          %del paso anterior.

          %La trayectoria esta "cerca" de uno de los grupos (hay uno en particular que es el que esta mas cerca).
          %Asigno la trayectoria a ese grupo!!

          TrajStruct(imember).trajectories(ii).asociada=true;  %#ok<SAGROW>
          TrajStruct(imember).trajectories(ii).idgroup=min_group; %#ok<SAGROW> %A que grupo asignamos la trajectoria.
          minlat(imember,index,min_group)=TrajStruct(imember).trajectories(ii).minlatf;
          minlon(imember,index,min_group)=TrajStruct(imember).trajectories(ii).minlonf;
          minanomsis(imember,index,min_group)=TrajStruct(imember).trajectories(ii).minanomsisf;
          minarea(imember,index,min_group)=TrajStruct(imember).trajectories(ii).minareaf;
          uvel(imember,index,min_group)=TrajStruct(imember).trajectories(ii).uvelf;
          vvel(imember,index,min_group)=TrajStruct(imember).trajectories(ii).vvelf;
          trajid(imember,index,min_group)=ii;
          %Recalculo la media del grupo. (DO NOT MODIFY THE MEAN USING THE
          %SMALL TRAYECTORIES BECAUSE FOR TRAJECTORIES BASED ONLY ON THE
          %ANALYSIS THIS WILL REDUCE THE CHANCE TO COLLECT ANOTHER
          %TRAJECTORIE).
          %group(min_group).meanlat=nanmean(group(min_group).minlat,1);
          %for kt=1:ntimes %Cuidado especial hay que tener siempre que se promedian longitudes.
          %group(min_group).meanlon(kt)=mean_lon_fun(group(min_group).minlon(:,kt));
          %end
          nmember(min_group)=sum(any(~isnan(minlat(:,:,min_group)),2));
          aux=sum(~isnan(minlat(:,:,min_group)),2);
          aux(aux==0)=NaN;        
          meanlength(min_group)=nanmean(aux);
          
          end  %Este es el end del if sobre si se asocia o no la trayectoria al grupo correspondiente.
              
       end  %Este es el end del if sobre si existe un grupo posible para esta trayectoria.

    end %End del if sobre si la trayectoria estaba o no asociada.
end   %End del loop sobre las trajectorias del miembro imember

end %End del loop sobre los miembros del ensamble




%==========================================================================
% PASO 5: ASOCIO LAS TRAJECTORIAS DEL ANALISIS QUE NO FUERON ASOCIADAS EN EL PASO
% ANTERIOR USANDO UN CRITERIO MAS LAXO. PERO NO PERMITO QUE REEMPLACEN A
% LAS QUE FUERON ASOCIADAS PREVIAMENTE USANDO UN CRITERIO MAS RIGUROSO.
%==========================================================================
ntraj=size(AnalysisTrajStruct,2);
for ii=1:ntraj

   %Solo me voy a ocupar de las trayectorias que no fueron asociadas en el
   %paso anterior.
   if(~AnalysisTrajStruct(ii).asociada && ...
      ~isempty(AnalysisTrajStruct(ii).length));  


    %Hago un loop sobre todos los grupos para ver si puedo asociar la trajectoria ii del miembro imember con algun grupo.    
       cfd=NaN(1,ngroup);  %Distance cost function
       cft=NaN(1,ngroup);  %Time cost function
       cfgr=NaN(1,ngroup); %Group superposition cost function
       cftr=NaN(1,ngroup); %Trajectory superposition cost function.
       for jgroup=1:ngroup
         %Tengo que comparar la trajectoria ii del miembro imember con 
         %la media del grupo jgroup y calcular una funcion de costo. Luego
         %ver a que grupo deberia asociar la trajectoria.
         isnan_tr=~isnan(meanlon(:,jgroup));
         grouplon=meanlon(isnan_tr,jgroup);
         grouplat=meanlat(isnan_tr,jgroup);
         grouplength=meanlength(jgroup);
         grouptime=times(isnan_tr);
         initime=times(1);
         [cfd(jgroup) cft(jgroup) cfgr(jgroup) cftr(jgroup)]=traj_cost_function_fun(grouplon,grouplat,grouptime,grouplength,...
                                                      AnalysisTrajStruct(ii).minlonf,AnalysisTrajStruct(ii).minlatf,...
                                                      AnalysisTrajStruct(ii).daten,initime); 
         
       end  %En del for sobre los grupos para calcular la funcion de costo.
       cft( cfd >= tr_cost_function)=NaN;  %Elimino los grupos que no cumplen el criterio de distancia.
       %cft( cfgr <= 0.3            )=NaN;  %Elimino los que no coinciden en al menos un 50 % con el grupo.
       %Nuevamente al igual que como hice con las trayectorias
       %pronosticadas, en este paso relajo la condicion de longitud de la
       %trayectoria respecto del grupo, pero si pido que la trayectoria
       %este caso toda contenida en el grupo.
       cft( cftr <= 0.5            )=NaN;  %Elimino aquellas trayectorias que no estan contenidas al menos en un 90% por el grupo.
       %Ahora tengo en cuenta la distancia del inicio de la trajectoria al
       %origen del grupo (en tiempo).
       %De todos los posibles grupos, me quedo con el que es mas
       %consistente en terminos del origen de la trayectoria.
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
          [nada index] = intersect(times,AnalysisTrajStruct(ii).daten);  
          %Vemos si esos lugares estan ocupados.
         
        
       %===================================================================
       % SI LA TRAYECTORIA SE PUEDE ASOCIAR CON EL GRUPO Y LOS SLOTS
       % CORRESPONDIENTES ESTAN LIBRES ENTONCES ASOCIO LA TRAYECTORIA.
       %===================================================================
         
        %Intentamos asociar la trayectoria actual (no asociada previamente)
        %a un grupo. 
          if(sum(isnan(minlatanalysis(index,min_group)))==length(index))
            %Si todos los elementos son NaN, no habia nadie ocupando ese lugar...
            %Dale para adelante
     
          %La trayectoria esta "cerca" de uno de los grupos (hay uno en particular que es el que esta mas cerca).
          %Asigno la trayectoria a ese grupo!!

          AnalysisTrajStruct(ii).asociada=true;
          AnalysisTrajStruct(ii).idgroup=min_group; %A que grupo asignamos la trajectoria.
          minlatanalysis(index,min_group)=AnalysisTrajStruct(ii).minlatf;
          minlonanalysis(index,min_group)=AnalysisTrajStruct(ii).minlonf;
          minanomsisanalysis(index,min_group)=AnalysisTrajStruct(ii).minanomsisf;
          minareaanalysis(index,min_group)=AnalysisTrajStruct(ii).minareaf;
          uvelanalysis(index,min_group)=AnalysisTrajStruct(ii).uvelf;
          vvelanalysis(index,min_group)=AnalysisTrajStruct(ii).vvelf;
          trajidanalysis(index,min_group)=ii;
         
          
          end  %Este es el end del if sobre si se asocia o no la trayectoria al grupo correspondiente.
              
       end  %Este es el end del if sobre si existe un grupo posible para esta trayectoria.

   end %End del if sobre si la trayectoria estaba o no asociada.
end   %End del loop sobre las trajectorias del analisis


%==========================================================================
% PASO 6: UNA VEZ QUE TERMINE DE CREAR LOS GRUPOS Y DE ASOCIARLE TRAYECTORIAS
% VERIFICO SI QUEDARON GRUPOS VACIOS Y SI LOS HAY LOS ELIMINO Y RENUMERO
% TODOS LOS OTROS GRUPOS. 
% ATENCION!! A PARTIR DE ESTE PUNTO YA NO VALE LOS NUMEROS DE ASOCIACION DE
% CADA TRAYECTORIA CON SU GRUPO, ESO NO SE CORRIGE PORQUE NO SE USA DE
% AHORA EN ADELANTE EN EL SCRIPT
%==========================================================================
 contador=0;
 for ii=1:ngroup
 if(sum(~isnan(meanlat(:,ii))) > 0 || sum(~isnan(minlatanalysis(:,ii))) > 0) %Si esto pasa es porque el grupo no esta vacio.
        contador=contador+1;
        
minlat(:,:,contador)=minlat(:,:,ii);
minlon(:,:,contador)=minlon(:,:,ii);
minanomsis(:,:,contador)=minanomsis(:,:,ii);
minarea(:,:,contador)=minarea(:,:,ii);
uvel(:,:,contador)=uvel(:,:,ii);
vvel(:,:,contador)=vvel(:,:,ii);
meanlat(:,contador)=meanlat(:,ii);
meanlon(:,contador)=meanlon(:,ii);
trajid(:,:,contador)=trajid(:,:,ii);
nmember(contador)=nmember(ii);
meanlength(contador)=meanlength(ii);

minlatanalysis(:,contador)=minlatanalysis(:,ii);
minlonanalysis(:,contador)=minlonanalysis(:,ii);
minareaanalysis(:,contador)=minareaanalysis(:,ii);
minanomsisanalysis(:,contador)=minanomsisanalysis(:,ii);
uvelanalysis(:,contador)=uvelanalysis(:,ii);
vvelanalysis(:,contador)=vvelanalysis(:,ii);
trajidanalysis(:,contador)=trajidanalysis(:,ii);
    

   end
 end

minlat(:,:,contador+1:end)=[];
minlon(:,:,contador+1:end)=[];
minanomsis(:,:,contador+1:end)=[];
minarea(:,:,contador+1:end)=[];
uvel(:,:,contador+1:end)=[];
vvel(:,:,contador+1:end)=[];
meanlat(:,contador+1:end)=[];
meanlon(:,contador+1:end)=[];
trajid(:,:,contador+1:end)=[];
nmember(contador+1:end)=[];
meanlength(contador+1:end)=[];

minlatanalysis(:,contador+1:end)=[];
minlonanalysis(:,contador+1:end)=[];
minareaanalysis(:,contador+1:end)=[];
minanomsisanalysis(:,contador+1:end)=[];
uvelanalysis(:,contador+1:end)=[];
vvelanalysis(:,contador+1:end)=[];
trajidanalysis(:,contador+1:end)=[]; 
       
ngroup=contador;

tiempo=toc;
fprintf('TIEMPO EMPLEADO EN ASIGNAR LAS PEQUENIAS TRAYECTORIAS %f\n',tiempo);




%ASIGNO LAS DIFERENTES MATRICES A LA ESTRUCTURA GROUP.

        
group.minlat=minlat;
group.minlon=minlon;
group.minanomsis=minanomsis;
group.minarea=minarea;
group.uvel=uvel;
group.vvel=vvel;
group.meanlat=meanlat;
group.meanlon=meanlon;
group.trajid=trajid;
group.nmember=nmember;
group.meanlength=meanlength;

group.minlatanalysis=minlatanalysis;
group.minlonanalysis=minlonanalysis;
group.minareaanalysis=minareaanalysis;
group.minanomsisanalysis=minanomsisanalysis;
group.uvelanalysis=uvelanalysis;
group.vvelanalysis=vvelanalysis;
group.trajidanalysis=trajidanalysis;



%LISTO EL LLOPO, PELADA LA GALLINA.




%==========================================================================
% EMBEDED FUNCTIONS
%==========================================================================


%SOME PLOTS TO CONTROL THE SYSTEM BEHAVIOUR

load coast   
long(long<0)=long(long<0)+360;
for ii=1:(length(long)-1)
    if(abs(long(ii) - long(ii+1)) > 180)
        long(ii)=NaN;
        lat(ii)=NaN;
    end
end



ngroup=size(minlat,3)
for jj=1:ngroup

 figure
plot(long,lat)
hold on
a=jet(20);
icolor=20;   
    
    
%for jj=2


   %Corto las trayectorias que pasan por el antimeridiano.
   %=======================================================================
   tmplonanalysis=minlonanalysis(:,jj);
   for kkk=1:(length(tmplonanalysis)-1)
       if(abs(tmplonanalysis(kkk)-tmplonanalysis(kkk+1)) > 180)
           tmplonanalysis(kkk)=NaN;
       end
   end
   tmplonmean=meanlon(:,jj);
   for kkk=1:(length(tmplonmean)-1)
       if(abs(tmplonmean(kkk)-tmplonmean(kkk+1)) > 180)
           tmplonmean(kkk)=NaN;
       end
   end
   
   for kk=1:enssize %INICIO EL LOOP SOBRE LOS MIEMBROS DEL ENSAMBLE.
       tmplon=minlon(kk,:,jj);
   for kkk=1:(length(tmplon)-1)
       if(abs(tmplon(kkk)-tmplon(kkk+1)) > 180)
           tmplon(kkk)=NaN;
       end
   end
   %=======================================================================
   
   plot(tmplon,minlat(kk,:,jj),'-','Color',a(icolor,:),'LineWidth',1);

   end
   %plot(tmplonmean,tmpstruct.meanlat,'k-','LineWidth',2);
   plot(tmplonanalysis,minlatanalysis(:,jj),'ko--','LineWidth',2);

   %CADA GRUPO VA CON UN COLOR DIFERENTE.
   icolor=icolor+1;
   if(icolor > size(a,1))
       icolor=1;
   end
print('-dpng',['../figuras/grupo' num2str(jj) '.png'])
close(1)

figure
pcolor(trajid(:,:,jj))
print('-dpng',['../figuras/trajid' num2str(jj) '.png'])
close(1)

end



load coast   
long(long<0)=long(long<0)+360;
for ii=1:(length(long)-1)
    if(abs(long(ii) - long(ii+1)) > 180)
        long(ii)=NaN;
        lat(ii)=NaN;
    end
end

% for jj=36

figure
a=jet(20);
icolor=1;
hold on
plot(long,lat)

tmpstruct=AnalysisTrajStruct;

   nst=size(tmpstruct,2);
   for kk=1:nst
       
       

       
       
       tmplon=tmpstruct(kk).minlon;
   for kkk=1:(length(tmplon)-1)
       if(abs(tmplon(kkk)-tmplon(kkk+1)) > 180)
           tmplon(kkk)=NaN;
       end
   end

   %plot(tmplon,TrajStruct(ii).minlatf,'-o','Color',a(icolor,:),'LineWidth',2)
   plot(tmplon,tmpstruct(kk).minlat,'ko-','LineWidth',2);

  end

   %CADA GRUPO VA CON UN COLOR DIFERENTE.
   icolor=icolor+1;
   if(icolor > size(a,1))
       icolor=1;
   end



