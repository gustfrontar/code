function [group]=group_trajectories_fun(TrajStruct,AnalysisTrajStruct,config)

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

Debug=false;

%INPUT
%TrajStruct es la estructura con todas las trayectorias, tal cual la
%produce la funcion que las calcula. 
%config contiene la fecha inicial, el numero de fechas, la frecuencia
%temporal, etc.

%TEST?--------------------------------------------------
% clear all
% close all
% load ../RESULTS/ANALISIS/TRAJECTORIES/TRAJ_2008060112_L7.mat 
% AnalysisTrajStruct=TrajStruct;
% 
% load ../RESULTS/kwbc/TRAJECTORIES/TRAJ_2008060112_L7.mat
% 
% config.date_ini='2008060112';
% config.date_end=datestr(datenum(config.date_ini,'yyyymmddHH')+7,'yyyymmddHH');
% config.timefrec=6;
% config.mintrajlength=12;
%-------------------------------------------------------

tr_cost_function=700e3;
MaxGroup=500;             %Maximo numero de grupos que vamos a allocatear de entrada.

CheckGroupMerge=true;
%obtengo el numero de miembros en el ensamble.
enssize=size(TrajStruct,2);

MaxIterations=3;           %Cuantas pasadas del algoritmo para definir los grupos.
MaxSuperposition=4;        %Maxima superposicion temporal permitida entre 2 trayectorias del mismo miembro que van a pertenecer al mismo grupo.
LittleTraj=true;

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
    TrajStruct(imember).trajectories(ii).asociada=false;  %Indica si la trajectoria fue o no asociada, se inicializa como false.
    TrajStruct(imember).trajectories(ii).idgroup=NaN;
    end

    if( ~isempty(TrajStruct(imember).trajectories(ii).length) )
    %HAGO UN LOOP SOBRE TODOS LOS GRUPOS Y MIDO QUE TAN FACTIBLE ES ASOCIAR LA TRAYECTORIA ACTUAL A CADA UNO.    
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
         %if( jgroup == 18 && imember == 7 )
         %  fprintf('Trayectoria %10f,  con un J de %10f \n',ii,cfd(jgroup))   
         %end
       end  %En del for sobre los grupos para calcular la funcion de costo.
%     
       groups=1:ngroup;
       
       cfd( cfd >= tr_cost_function)=NaN;  
       formagrupo=sum(~isnan(cfd))==0;     %La trayectoria va a poder formar grupos si esta muy lejos de cualquier grupo.
       cft( cfd >= tr_cost_function)=NaN;  %Elimino los grupos que no cumplen el criterio de distancia.
       cfd( cfgr <= 0.2            )=NaN;  %Elimino los que no coinciden en al menos un 50 % con el grupo.
       cfd( cftr <= 0.4            )=NaN;  %Elimino aquellas trayectorias que no estan contenidas al menos en un 50% por el grupo.
       
       groups=groups(~isnan(cfd));
       cfd=cfd(~isnan(cfd));

       %Ordenamos las funciones de costo y los grupos de menor a mayor.
       [cfd isort]=sort(cfd);
       groups=groups(isort);
       
       
       %Acciones por defecto.
       asociar=false;
       desasociar=false;
       
       if( isempty(cfd) )
           %NO HAY NINGUN GRUPO CERCANO, SI LA TRAYECTORIA ESTABA ASOCIADA
           %LA DESASOCIAMOS PORQUE QUIERE DECIR QUE YA NO ESTA ASOCIADA AL
           %GRUPO QUE ESTABA ANTES.
           if(TrajStruct(imember).trajectories(ii).asociada)
               desasociar=true;
           end
           
       else  %EXISTEN GRUPOS POSIBLES, VAMOS A VER SI TIENEN LUGAR.
           
          for jgroup=1:length(groups)  %VAMOS A CONSIDERAR TODOS LOS GRUPOS POSIBLES.
          cgroup=groups(jgroup);
              if( TrajStruct(imember).trajectories(ii).asociada && TrajStruct(imember).trajectories(ii).idgroup==cgroup)
                  %LA TRAYECTORIA ESTABA ASOCIADA A ESTE GRUPO SI ESTA ES
                  %LA MEJOR OPICON ROMPO EL LOOP Y SIGO SIN HACER NADA.
                  break
              end
              %ANALIZO LOS QUE NO SON EL GRUPO QUE YA TENIA ANTES.
              [nada index] = intersect(times,TrajStruct(imember).trajectories(ii).daten);   %#ok<ASGLU>
               if(sum(isnan(minlat(imember,index,cgroup))) >= length(index) - MaxSuperposition )
                %HAY LUGAR (TENIENDO EN CUENTA ALGO DE POSIBLE
                %SUPERPOSICION!!!)
                asociar=true;  
                newgroup=cgroup;
                if( TrajStruct(imember).trajectories(ii).asociada)
                    desasociar=true;
                end
                %SALGO DEL DO PORQUE YA ENCONTRE UN GRUPO AL QUE PUEDO
                %ASOCIAR LA TRAYECTORIA.
                break
                
               else %NO HAY LUGAR, TENGO QUE COMPETIR CON LOS ACTUALES OCUPANTES DEL GRUPO.

               %Tengo que chequear cual/es es/son la/s trayectoria/s que esta/s asociada/s en
               %ese lugar y cual es la mejor opcion.
               %Calculo cuanto se superponen cada una de esas trayectorias con
               %el grupo y cuan cerca o lejos estan del origen.
               isnan_tr=~isnan(meanlon(:,cgroup));
               traj_asociadas=unique(trajid(imember,index,cgroup));
               traj_asociadas=traj_asociadas(~isnan(traj_asociadas));
               tmp_cfd=NaN(1,length(traj_asociadas)) ; %Distance cost function
               tmp_cft=NaN(1,length(traj_asociadas)) ; %Time cost function
               tmp_cfgr=NaN(1,length(traj_asociadas));
               tmp_cftr=NaN(1,length(traj_asociadas));

               tmp_grouplon=meanlon(isnan_tr,cgroup);
               tmp_grouplat=meanlat(isnan_tr,cgroup);
               tmp_grouptime=times(isnan_tr);
               tmp_grouplength=meanlength(cgroup);
               initime=times(1);
                for kk=1:length(traj_asociadas)
                %Calculo la funcion de costo entre las trayectorias
                %asociadas y la trayectoria actual.
                
                 [tmp_cfd(kk) tmp_cft(kk) tmp_cfgr(kk) tmp_cftr(kk)]=traj_cost_function_fun(tmp_grouplon,tmp_grouplat,tmp_grouptime,tmp_grouplength,...
                 TrajStruct(imember).trajectories(traj_asociadas(kk)).minlonf,TrajStruct(imember).trajectories(traj_asociadas(kk)).minlatf,...
                 TrajStruct(imember).trajectories(traj_asociadas(kk)).daten,initime);
                end

                if(  cfd(jgroup)  <  min(tmp_cfd) )
               %LA TRAYECTORIA NUEVA ES MEJOR QUE LAS QUE HABIA ENTONCES LA
               %ASOCIO Y SACO LAS ANTERIORES.
               asociar=true;
               newgroup=cgroup;
               if(TrajStruct(imember).trajectories(ii).asociada)
               %Vamos a setear esto para sacarla de su grupo original.
                desasociar=true;
               end
            
            
              %DESASOCIO LAS TRAYECTORIAS QUE ESTABAN EN EL GRUPO Y QUE SE
              %SUPERPONEN CON LA NUEVA TRAYECTORIA
              asoctraj=unique(trajid(imember,:,cgroup));
              asoctraj(isnan(asoctraj))=[];
               for kkk=1:length(traj_asociadas)
                asoctraj(asoctraj==traj_asociadas(kkk))=[];
                TrajStruct(imember).trajectories(traj_asociadas(kkk)).asociada=false; 
                TrajStruct(imember).trajectories(traj_asociadas(kkk)).idgroup=NaN; 
               end

              %RESETEO EL GRUPO CGROUP Y REASOCIO TODO LO QUE NO TENGO QUE
              %SACAR.
                minlat(imember,:,cgroup)=NaN;
                minlon(imember,:,cgroup)=NaN;
                minanomsis(imember,:,cgroup)=NaN;
                minarea(imember,:,cgroup)=NaN;
                uvel(imember,:,cgroup)=NaN;
                vvel(imember,:,cgroup)=NaN;
                trajid(imember,:,cgroup)=NaN;
                
              %REASOCIO LAS QUE NO TENIA QUE SACAR (ESTO PRESERVA LAS
              %PROPIEDADES DEL GRUPO AUN CON SUPERPOSICION).
                if(~isempty(asoctraj))
                for kkk=1:length(asoctraj)
                 ctraj=asoctraj(kkk);
                 [nada index] = intersect(times,TrajStruct(imember).trajectories(ctraj).daten);
                 minlat(imember,index,cgroup)=nanmean([TrajStruct(imember).trajectories(ctraj).minlatf; minlat(imember,index,cgroup)]);
                 tmplon=[TrajStruct(imember).trajectories(ctraj).minlonf; minlon(imember,index,cgroup)];
                  for kk=1:length(index)
                   minlon(imember,index(kk),cgroup)=mean_lon_fun(tmplon(:,kk));
                  end
                 minanomsis(imember,index,cgroup)=nanmin([TrajStruct(imember).trajectories(ctraj).minanomsisf;minanomsis(imember,index,cgroup)]);
                 minarea(imember,index,cgroup)=nansum([TrajStruct(imember).trajectories(ctraj).minareaf; minarea(imember,index,cgroup)]);
                 uvel(imember,index,cgroup)=nanmean([TrajStruct(imember).trajectories(ctraj).uvelf;uvel(imember,index,cgroup)]);
                 vvel(imember,index,cgroup)=nanmean([TrajStruct(imember).trajectories(ctraj).vvelf;vvel(imember,index,cgroup)]);
                 trajid(imember,index,cgroup)=ctraj;
                end
                end
         
                %RECALCULO LAS PROPIEDADES DEL GRUPO CASO
                meanlat(:,cgroup)=nanmean(minlat(:,:,cgroup),1);
                for kt=1:ntimes %Cuidado especial hay que tener siempre que se promedian longitudes.
                meanlon(kt,cgroup)=mean_lon_fun(minlon(:,kt,cgroup));
                end
                nmember(cgroup)=sum(any(~isnan(minlat(:,:,cgroup)),2));
                aux=sum(~isnan(minlat(:,:,cgroup)),2);
                aux(aux==0)=NaN;        
                meanlength(cgroup)=nanmean(aux);
            end
          end

          end
       end
        
       %TERMINE DE MIRAR TODOS LOS GRUPOS POSIBLES, QUEDAN DOS COSAS POR
       %HACER ASOCIAR LA TRAYECTORIA A SU NUEVO GRUPO (SI FUERA EL CASO) Y
       %DESASOCIARLA DEL ANTERIOR (TAMBIEN SI FUERA EL CASO).
           
        
        %==================================================================
        %DESASOCIO LA TRAYECTORIA DEL GRUPO QUE TENIA ORIGINALMENTE SI ES
        %LO QUE CORRESPONDE HACER.
        %==================================================================
        if( desasociar )   %La trayectoria estaba asociada a otro grupo previamente. 
           %Desasocio la trayectoria ii de su grupo anterior.
           cgroup=TrajStruct(imember).trajectories(ii).idgroup;
           asoctraj=unique(trajid(imember,:,cgroup));
           asoctraj(isnan(asoctraj))=[];
           asoctraj(asoctraj==ii)=[];
           
           TrajStruct(imember).trajectories(ii).asociada=false; 
           TrajStruct(imember).trajectories(ii).idgroup=NaN;  

           %Reseteo el miembro imember del grupo cgroup.
                minlat(imember,:,cgroup)=NaN;
                minlon(imember,:,cgroup)=NaN;
                minanomsis(imember,:,cgroup)=NaN;
                minarea(imember,:,cgroup)=NaN;
                uvel(imember,:,cgroup)=NaN;
                vvel(imember,:,cgroup)=NaN;
                trajid(imember,:,cgroup)=NaN;
                
                %Ahora tengo que reasociar todas las trayectorias salvo la
                %que quiero sacar. Es preciso hacerlo asi porque si hubo
                %superposicion, la unica forma de recalcular adecuadamente
                %el valor de las propiedades es hacerlo de esta manera.
                if(~isempty(asoctraj))
                for kkk=1:length(asoctraj)
                 ctraj=asoctraj(kkk);
                 [nada index] = intersect(times,TrajStruct(imember).trajectories(ctraj).daten);
                 minlat(imember,index,cgroup)=nanmean([TrajStruct(imember).trajectories(ctraj).minlatf; minlat(imember,index,cgroup)]);
                 tmplon=[TrajStruct(imember).trajectories(ctraj).minlonf; minlon(imember,index,cgroup)];
                  for kk=1:length(index)
                   minlon(imember,index(kk),cgroup)=mean_lon_fun(tmplon(:,kk));
                  end
                 minanomsis(imember,index,cgroup)=nanmin([TrajStruct(imember).trajectories(ctraj).minanomsisf;minanomsis(imember,index,cgroup)]);
                 minarea(imember,index,cgroup)=nansum([TrajStruct(imember).trajectories(ctraj).minareaf; minarea(imember,index,cgroup)]);
                 uvel(imember,index,cgroup)=nanmean([TrajStruct(imember).trajectories(ctraj).uvelf;uvel(imember,index,cgroup)]);
                 vvel(imember,index,cgroup)=nanmean([TrajStruct(imember).trajectories(ctraj).vvelf;vvel(imember,index,cgroup)]);
                 trajid(imember,index,cgroup)=ctraj;
                end
                end
         
               %Recalculo la media del grupo (para el caso en el que el analysis
               %defina totalmente el grupo, en ese caso el grupo deberia
               %desapraecer.
               meanlat(:,cgroup)=nanmean(minlat(:,:,cgroup),1);
               for kt=1:ntimes %Cuidado especial hay que tener siempre que se promedian longitudes.
               meanlon(kt,cgroup)=mean_lon_fun(minlon(:,kt,cgroup));
               end
               nmember(cgroup)=sum(any(~isnan(minlat(:,:,cgroup)),2));
               aux=sum(~isnan(minlat(:,:,cgroup)),2);
               aux(aux==0)=NaN;        
               meanlength(cgroup)=nanmean(aux);
               
        end   %END DEL IF SOBRE SI TENGO QUE DESASOCIAR LA TRAYECTORIA DE SU GRUPO ORIGINAL.
        
        
        
       if(asociar)

          %==================================================================
          %ASOCIO LA TRAYECTORIA A UN NUEVO GRUPO SI ES LO QUE CORRESPONDE.
          %==================================================================
          %ASIGNO LA TRAYECTORIA AL GRUPO REQUERIDO.
          if(Debug)
          fprintf('Asocio la trayectoria del %f al grupo %f con un cost function de %f \n',ii,newgroup,min(cfd));
          end
          [nada index] = intersect(times,TrajStruct(imember).trajectories(ii).daten);
          TrajStruct(imember).trajectories(ii).asociada=true;
          TrajStruct(imember).trajectories(ii).idgroup=newgroup; %A que grupo asignamos la trajectoria.
          minlat(imember,index,newgroup)=nanmean([TrajStruct(imember).trajectories(ii).minlatf;minlat(imember,index,newgroup)]);
          tmplon=[TrajStruct(imember).trajectories(ii).minlonf;minlon(imember,index,newgroup)];
          
          
          for ikk=1:size(tmplon,2)
          minlon(imember,index(ikk),newgroup)=mean_lon_fun(tmplon(:,ikk));
          end
          minanomsis(imember,index,newgroup)=nanmin([TrajStruct(imember).trajectories(ii).minanomsisf;minanomsis(imember,index,newgroup)]);
          minarea(imember,index,newgroup)=nansum([TrajStruct(imember).trajectories(ii).minareaf;minarea(imember,index,newgroup)]);
          uvel(imember,index,newgroup)=nanmean([TrajStruct(imember).trajectories(ii).uvelf;uvel(imember,index,newgroup)]);
          vvel(imember,index,newgroup)=nanmean([TrajStruct(imember).trajectories(ii).vvelf;vvel(imember,index,newgroup)]);
          trajid(imember,index,newgroup)=ii;
          
          %Recalculo la media del grupo (para el caso en el que el analysis
          %defina totalmente el grupo, en ese caso el grupo deberia
          %desapraecer.
          meanlat(:,newgroup)=nanmean(minlat(:,:,newgroup),1);
          for kt=1:ntimes %Cuidado especial hay que tener siempre que se promedian longitudes.
          meanlon(kt,newgroup)=mean_lon_fun(minlon(:,kt,newgroup));
          end
          nmember(newgroup)=sum(any(~isnan(minlat(:,:,newgroup)),2));
          aux=sum(~isnan(minlat(:,:,newgroup)),2);
          aux(aux==0)=NaN;        
          meanlength(newgroup)=nanmean(aux);

          
        end  %END DEL IF SOBRE SI ASOCIO LA TRAYECTORIA A SU NUEVO GRUPO.
          
    end
    
    %Si la trayectoria no fue asociada con ninguna otra entonces veo si
    %podria ser un buen candidato para generar un grupo nuevo.
    %Solo vamos a iniciar nuevos grupos a partir de las trayectorias
    %largas. No vamos a dejar que trajectorias cortitas inicien nuevos
    %grupos. Pero si las trajectorias cortas pueden asociarse a un grupo
    %que contenga al menos una trajectoria larga.
    
    if(~TrajStruct(imember).trajectories(ii).asociada && ...
       ~isempty(TrajStruct(imember).trajectories(ii).length) && ...
       TrajStruct(imember).trajectories(ii).length >= config.mintrajlength &&...
       formagrupo ) %Solo las trajectorias de al menos 3 dias inician grupos.
    ngroup=ngroup+1;
    
    if(Debug)
    fprintf('El grupo %10f, lo creo la trayectoria %10f del miembro %10f \n',ngroup,ii,imember);
    end
        

    %Calculamos los lugares que les corresponde
    [nada index] = intersect(times,TrajStruct(imember).trajectories(ii).daten); %#ok<ASGLU>
    TrajStruct(imember).trajectories(ii).asociada=true; 
    TrajStruct(imember).trajectories(ii).idgroup=ngroup;  %A que grupo asignamos la trajectoria.
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
clear min_group
ntraj=size(AnalysisTrajStruct,2);  %Numero de trayectorias en el miembro correspondiente.

for ii=1:ntraj
    
    if(nloop==1)
    AnalysisTrajStruct(ii).asociada=false;  %Indica si la trajectoria fue o no asociada, se inicializa como false.
    AnalysisTrajStruct(ii).idgroup=NaN;
    end
    if( ~isempty(AnalysisTrajStruct(ii).length) )
    %HAGO UN LOOP SOBRE TODOS LOS GRUPOS Y MIDO QUE TAN FACTIBLE ES ASOCIAR LA TRAYECTORIA ACTUAL A CADA UNO.    
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

       %if(jgroup==22)
       %    fprintf('Trayectoria %10f con un J de %10f\n',ii,cfd(jgroup));
       %end
       end  %En del for sobre los grupos para calcular la funcion de costo.
%     

       groups=1:ngroup;
       cfd( cfd >= tr_cost_function)=NaN;  
       formagrupo=sum(~isnan(cfd))==0;     %Condicion para poder formar un grupo.
       cft( cfd >= tr_cost_function)=NaN;  %Elimino los grupos que no cumplen el criterio de distancia.
       cfd( cfgr <= 0.2            )=NaN;  %Elimino los que no coinciden en al menos un 50 % con el grupo.
       cfd( cftr <= 0.4            )=NaN;  %Elimino aquellas trayectorias que no estan contenidas al menos en un 50% por el grupo.


       groups=groups(~isnan(cfd));
       cfd=cfd(~isnan(cfd));

       %Ordenamos las funciones de costo y los grupos de menor a mayor.
       [cfd isort]=sort(cfd);
       groups=groups(isort);
       
       %if(ii==136)
       %    cfd
       %    groups
       %end
       
       %Acciones por defecto.
       asociar=false;
       desasociar=false;
       
       if( isempty(cfd) )
           %NO HAY NINGUN GRUPO CERCANO, SI LA TRAYECTORIA ESTABA ASOCIADA
           %LA DESASOCIAMOS PORQUE QUIERE DECIR QUE YA NO ESTA ASOCIADA AL
           %GRUPO QUE ESTABA ANTES.
           if(AnalysisTrajStruct(ii).asociada)
               desasociar=true;
           end
           
       else  %EXISTEN GRUPOS POSIBLES, VAMOS A VER SI TIENEN LUGAR.
           
          for jgroup=1:length(groups)  %VAMOS A CONSIDERAR TODOS LOS GRUPOS POSIBLES.
          cgroup=groups(jgroup);
              if( AnalysisTrajStruct(ii).asociada && AnalysisTrajStruct(ii).idgroup==cgroup)
                  %LA TRAYECTORIA ESTABA ASOCIADA A ESTE GRUPO SI ESTA ES
                  %LA MEJOR OPICON ROMPO EL LOOP Y SIGO SIN HACER NADA.
                  break
              end
              %ANALIZO LOS QUE NO SON EL GRUPO QUE YA TENIA ANTES.
              [nada index] = intersect(times,AnalysisTrajStruct(ii).daten);   %#ok<ASGLU>
               if(sum(isnan(minlatanalysis(index,cgroup))) >= length(index) - MaxSuperposition )
                %HAY LUGAR (TENIENDO EN CUENTA ALGO DE POSIBLE
                %SUPERPOSICION!!!)
                asociar=true;  
                newgroup=cgroup;
                if( AnalysisTrajStruct(ii).asociada)
                    desasociar=true;
                end
                %SALGO DEL DO PORQUE YA ENCONTRE UN GRUPO AL QUE PUEDO
                %ASOCIAR LA TRAYECTORIA.
                break
                
               else %NO HAY LUGAR, TENGO QUE COMPETIR CON LOS ACTUALES OCUPANTES DEL GRUPO.

               %Tengo que chequear cual/es es/son la/s trayectoria/s que esta/s asociada/s en
               %ese lugar y cual es la mejor opcion.
               %Calculo cuanto se superponen cada una de esas trayectorias con
               %el grupo y cuan cerca o lejos estan del origen.
               isnan_tr=~isnan(meanlon(:,cgroup));
               traj_asociadas=unique(trajidanalysis(index,cgroup));
               traj_asociadas=traj_asociadas(~isnan(traj_asociadas));
               tmp_cfd=NaN(1,length(traj_asociadas)) ; %Distance cost function
               tmp_cft=NaN(1,length(traj_asociadas)) ; %Time cost function
               tmp_cfgr=NaN(1,length(traj_asociadas));
               tmp_cftr=NaN(1,length(traj_asociadas));

               tmp_grouplon=meanlon(isnan_tr,cgroup);
               tmp_grouplat=meanlat(isnan_tr,cgroup);
               tmp_grouptime=times(isnan_tr);
               tmp_grouplength=meanlength(cgroup);
               initime=times(1);
                for kk=1:length(traj_asociadas)
                %Calculo la funcion de costo entre las trayectorias
                %asociadas y la trayectoria actual.
                
                 [tmp_cfd(kk) tmp_cft(kk) tmp_cfgr(kk) tmp_cftr(kk)]=traj_cost_function_fun(tmp_grouplon,tmp_grouplat,tmp_grouptime,tmp_grouplength,...
                 AnalysisTrajStruct(traj_asociadas(kk)).minlonf,AnalysisTrajStruct(traj_asociadas(kk)).minlatf,...
                 AnalysisTrajStruct(traj_asociadas(kk)).daten,initime);
                end
                
                
                if(  cfd(jgroup)  <  min(tmp_cfd) )
               %LA TRAYECTORIA NUEVA ES MEJOR QUE LAS QUE HABIA ENTONCES LA
               %ASOCIO Y SACO LAS ANTERIORES.
               asociar=true;
               newgroup=cgroup;
               if(AnalysisTrajStruct(ii).asociada)
               %Vamos a setear esto para sacarla de su grupo original.
                desasociar=true;
               end
            
            
              %DESASOCIO LAS TRAYECTORIAS QUE ESTABAN EN EL GRUPO Y QUE SE
              %SUPERPONEN CON LA NUEVA TRAYECTORIA
              asoctraj=unique(trajidanalysis(:,cgroup));
              asoctraj(isnan(asoctraj))=[];
               for kkk=1:length(traj_asociadas)
                asoctraj(asoctraj==traj_asociadas(kkk))=[];
                AnalysisTrajStruct(traj_asociadas(kkk)).asociada=false; 
                AnalysisTrajStruct(traj_asociadas(kkk)).idgroup=NaN; 
               end

              %RESETEO EL GRUPO CGROUP Y REASOCIO TODO LO QUE NO TENGO QUE
              %SACAR.
                minlatanalysis(:,cgroup)=NaN;
                minlonanalysis(:,cgroup)=NaN;
                minanomsisanalysis(:,cgroup)=NaN;
                minareaanalysis(:,cgroup)=NaN;
                uvelanalysis(:,cgroup)=NaN;
                vvelanalysis(:,cgroup)=NaN;
                trajidanalysis(:,cgroup)=NaN;
                
              %REASOCIO LAS QUE NO TENIA QUE SACAR (ESTO PRESERVA LAS
              %PROPIEDADES DEL GRUPO AUN CON SUPERPOSICION).
                if(~isempty(asoctraj))
                for kkk=1:length(asoctraj)
                 ctraj=asoctraj(kkk);
                 [nada index] = intersect(times,AnalysisTrajStruct(ctraj).daten);
                 minlatanalysis(index,cgroup)=nanmean([AnalysisTrajStruct(ctraj).minlatf; minlatanalysis(index,cgroup)']);
                 tmplon=[AnalysisTrajStruct(ctraj).minlonf; minlonanalysis(index,cgroup)'];
                  for kk=1:length(index)
                   minlonanalysis(index(kk),cgroup)=mean_lon_fun(tmplon(:,kk));
                  end
                 minanomsisanalysis(index,cgroup)=nanmin([AnalysisTrajStruct(ctraj).minanomsisf;minanomsisanalysis(index,cgroup)']);
                 minareaanalysis(index,cgroup)=nansum([AnalysisTrajStruct(ctraj).minareaf; minareaanalysis(index,cgroup)']);
                 uvelanalysis(index,cgroup)=nanmean([AnalysisTrajStruct(ctraj).uvelf;uvelanalysis(index,cgroup)']);
                 vvelanalysis(index,cgroup)=nanmean([AnalysisTrajStruct(ctraj).vvelf;vvelanalysis(index,cgroup)']);
                 trajidanalysis(index,cgroup)=ctraj;
                end
                end
         
                %RECALCULO LAS PROPIEDADES DEL GRUPO CASO
                meanlat(:,cgroup)=nanmean(minlat(:,:,cgroup),1);
                for kt=1:ntimes %Cuidado especial hay que tener siempre que se promedian longitudes.
                meanlon(kt,cgroup)=mean_lon_fun(minlon(:,kt,cgroup));
                end
                nmember(cgroup)=sum(any(~isnan(minlat(:,:,cgroup)),2));
                aux=sum(~isnan(minlat(:,:,cgroup)),2);
                aux(aux==0)=NaN;        
                meanlength(cgroup)=nanmean(aux);
            end
          end

          end
       end
        
       %TERMINE DE MIRAR TODOS LOS GRUPOS POSIBLES, QUEDAN DOS COSAS POR
       %HACER ASOCIAR LA TRAYECTORIA A SU NUEVO GRUPO (SI FUERA EL CASO) Y
       %DESASOCIARLA DEL ANTERIOR (TAMBIEN SI FUERA EL CASO).
          
       if(ii==46)
           desasociar
           asociar
           newgroup
       end
        
        %==================================================================
        %DESASOCIO LA TRAYECTORIA DEL GRUPO QUE TENIA ORIGINALMENTE SI ES
        %LO QUE CORRESPONDE HACER.
        %==================================================================
        if( desasociar )   %La trayectoria estaba asociada a otro grupo previamente. 
           %Desasocio la trayectoria ii de su grupo anterior.
           cgroup=AnalysisTrajStruct(ii).idgroup;
           %if(ii==136)
           %    cgroup
           %end
           asoctraj=unique(trajidanalysis(:,cgroup));
           asoctraj(isnan(asoctraj))=[];
           asoctraj(asoctraj==ii)=[];
           
           AnalysisTrajStruct(ii).asociada=false; 
           AnalysisTrajStruct(ii).idgroup=NaN;  

           %Reseteo el miembro imember del grupo cgroup.
                minlatanalysis(:,cgroup)=NaN;
                minlonanalysis(:,cgroup)=NaN;
                minanomsisanalysis(:,cgroup)=NaN;
                minareaanalysis(:,cgroup)=NaN;
                uvelanalysis(:,cgroup)=NaN;
                vvelanalysis(:,cgroup)=NaN;
                trajidanalysis(:,cgroup)=NaN;
                
                %Ahora tengo que reasociar todas las trayectorias salvo la
                %que quiero sacar. Es preciso hacerlo asi porque si hubo
                %superposicion, la unica forma de recalcular adecuadamente
                %el valor de las propiedades es hacerlo de esta manera.
                if(~isempty(asoctraj))
                for kkk=1:length(asoctraj)
                 ctraj=asoctraj(kkk);
                 [nada index] = intersect(times,AnalysisTrajStruct(ctraj).daten);
                 minlatanalysis(index,cgroup)=nanmean([AnalysisTrajStruct(ctraj).minlatf; minlatanalysis(index,cgroup)']);
                 tmplon=[AnalysisTrajStruct(ctraj).minlonf; minlonanalysis(index,cgroup)'];
                  for kk=1:length(index)
                   minlonanalysis(index(kk),cgroup)=mean_lon_fun(tmplon(:,kk));
                  end
                 minanomsisanalysis(index,cgroup)=nanmin([AnalysisTrajStruct(ctraj).minanomsisf;minanomsisanalysis(index,cgroup)']);
                 minareaanalysis(index,cgroup)=nansum([AnalysisTrajStruct(ctraj).minareaf; minareaanalysis(index,cgroup)']);
                 uvelanalysis(index,cgroup)=nanmean([AnalysisTrajStruct(ctraj).uvelf;uvelanalysis(index,cgroup)']);
                 vvelanalysis(index,cgroup)=nanmean([AnalysisTrajStruct(ctraj).vvelf;vvelanalysis(index,cgroup)']);
                 trajidanalysis(index,cgroup)=ctraj;
                end
                end
         
               %Recalculo la media del grupo (para el caso en el que el analysis
               %defina totalmente el grupo, en ese caso el grupo deberia
               %desapraecer.
               meanlat(:,cgroup)=nanmean(minlat(:,:,cgroup),1);
               for kt=1:ntimes %Cuidado especial hay que tener siempre que se promedian longitudes.
               meanlon(kt,cgroup)=mean_lon_fun(minlon(:,kt,cgroup));
               end
               nmember(cgroup)=sum(any(~isnan(minlat(:,:,cgroup)),2));
               aux=sum(~isnan(minlat(:,:,cgroup)),2);
               aux(aux==0)=NaN;        
               meanlength(cgroup)=nanmean(aux);
               
        end   %END DEL IF SOBRE SI TENGO QUE DESASOCIAR LA TRAYECTORIA DE SU GRUPO ORIGINAL.
        
        
        
       if(asociar)
          %==================================================================
          %ASOCIO LA TRAYECTORIA A UN NUEVO GRUPO SI ES LO QUE CORRESPONDE.
          %==================================================================
          %La trayectoria esta "cerca" de uno de los grupos (hay uno en particular que es el que esta mas cerca).
          %Asigno la trayectoria a ese grupo!!
          if(Debug)
          fprintf('Asocio la trayectoria del analysis %f al grupo %f con un cost function de %f \n',ii,newgroup,min(cfd));
          end
          [nada index] = intersect(times,AnalysisTrajStruct(ii).daten);
          AnalysisTrajStruct(ii).asociada=true;
          AnalysisTrajStruct(ii).idgroup=newgroup; %A que grupo asignamos la trajectoria.
          minlatanalysis(index,newgroup)=nanmean([AnalysisTrajStruct(ii).minlatf;minlatanalysis(index,newgroup)']);
          tmplon=[AnalysisTrajStruct(ii).minlonf;minlonanalysis(index,newgroup)'];
          
          
          for ikk=1:size(tmplon,2)
          minlonanalysis(index(ikk),newgroup)=mean_lon_fun(tmplon(:,ikk));
          end
          minanomsisanalysis(index,newgroup)=nanmin([AnalysisTrajStruct(ii).minanomsisf;minanomsisanalysis(index,newgroup)']);
          minareaanalysis(index,newgroup)=nansum([AnalysisTrajStruct(ii).minareaf;minareaanalysis(index,newgroup)']);
          uvelanalysis(index,newgroup)=nanmean([AnalysisTrajStruct(ii).uvelf;uvelanalysis(index,newgroup)']);
          vvelanalysis(index,newgroup)=nanmean([AnalysisTrajStruct(ii).vvelf;vvelanalysis(index,newgroup)']);
          trajidanalysis(index,newgroup)=ii;

          end  %END DEL IF SOBRE SI ASOCIO LA TRAYECTORIA A SU NUEVO GRUPO.
        
       
    end  %END SOBRE SI LA TRAYECTORIA ESTA VACIA.
       
    %SI LA TRAYECTORIA NO PUDO SER ASOCIADA A NINGUNO DE LOS GRUPOS
    %POSIBLES ENTONCES VEMOS SI TIENE LAS CARACTERISTICAS PARA INICIAR UN
    %NUEVO GRUPO.
    
    if(~AnalysisTrajStruct(ii).asociada && ~isempty(AnalysisTrajStruct(ii).length) && ...
       AnalysisTrajStruct(ii).length >= config.mintrajlength && ...
       formagrupo ) %Solo las trajectorias de al menos 3 dias inician grupos.
    ngroup=ngroup+1;
    if(Debug)
    fprintf('El grupo %10f, fue creado por la trayectoria del analysis %10f \n',ngroup,ii)
    end
    
    %Calculo los slots que va a ocupar el analysis
    [nada index] = intersect(times,AnalysisTrajStruct(ii).daten); %#ok<ASGLU>
    AnalysisTrajStruct(ii).asociada=true;
    AnalysisTrajStruct(ii).idgroup=ngroup; %A que grupo asignamos la trajectoria.
    

    
    minlatanalysis(index,ngroup)=AnalysisTrajStruct(ii).minlatf;
    minlonanalysis(index,ngroup)=AnalysisTrajStruct(ii).minlonf;
    minareaanalysis(index,ngroup)=AnalysisTrajStruct(ii).minareaf;
    minanomsisanalysis(index,ngroup)=AnalysisTrajStruct(ii).minanomsisf;
    uvelanalysis(index,ngroup)=AnalysisTrajStruct(ii).uvelf;
    vvelanalysis(index,ngroup)=AnalysisTrajStruct(ii).vvelf;
    trajidanalysis(index,ngroup)=ii;
    %Provisoriamente asignamos el analysis como la media del grupo.
    %Esto permite que trayectorias pronosticadas se asocien con esta
    %trayectoria del analysis.
    meanlat(index,ngroup)=AnalysisTrajStruct(ii).minlatf;
    meanlon(index,ngroup)=AnalysisTrajStruct(ii).minlonf;
    meanlength(ngroup)=sum(~isnan(minlatanalysis(:,ngroup)));
      
    end  %End del if sobre la creacion de un nuevo grupo.  
end   %End del loop sobre las trajectorias del analysis


if(CheckGroupMerge)
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
       cfd( cfd >= tr_cost_function)=NaN;  
       cft( cfd >= tr_cost_function)=NaN;  %Elimino los grupos que no cumplen el criterio de distancia.
       cfd( cftr <= 0.8            )=NaN;  %Elimino aquellos grupos que no estan contendios al menos en un 50% por el otro grupo.
       %Ahora tengo en cuenta la distancia del inicio de la trajectoria al
       %origen del grupo (en tiempo).
       %De todos los posibles grupos, me quedo con el que es mas
       %consistente en terminos del origen de la trayectoria.
       [min_cfd min_group]=nanmin(cfd);
       %min_cfd=cfd(min_group);
       %min_cfgr=cfgr(min_group);
       %min_cftr=cftr(min_group);
       
       if(~isnan(min_cfd)) %Quiere decir que hay 2 grupos que estan cerca en espacio
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
        
        
        
        %PERMITO QUE EXISTA UNA SUPERPOSICION ENTRE LOS GRUPOS DE AL MENOS
        %2 PUNTOS POR MIEMBRO DEL ENSAMBLE.
        if(sum(~isnan(tmpminlat2(indexii))) <= 2*config.enssize && sum(~isnan(tmpminlatanalysis2(indexiiana))) <= MaxSuperposition )
        %Entonces el grupo ii complementa (aunque no totalmente al grupo
        %min_group).
        if(Debug)
        fprintf('Asocie el grupo %f con el grupo %f \n ',ii,min_group);
        end
          
          auxiliar1(:,:,1)=minlat(:,:,ii);
          auxiliar1(:,:,2)=minlat(:,:,min_group);
          %auxiliar2=minlat(:,:,min_group);
          %auxiliar2(indexii)=auxiliar1(indexii);
          minlat(:,:,min_group)=nanmean(auxiliar1,3);
          %auxiliar1=minlon(:,:,ii);
          %auxiliar2=minlon(:,:,min_group);
          auxiliar1(:,:,1)=minlon(:,:,ii);
          auxiliar1(:,:,2)=minlon(:,:,min_group);
          %auxiliar2(indexii)=auxiliar1(indexii);
          for ikk=1:size(auxiliar1,1)
              for jkk=1:size(auxiliar1,2)
                  minlon(ikk,jkk,min_group)=mean_lon_fun(auxiliar1(ikk,jkk,:));
              end
          end
          
          %minlon(:,:,min_group)=auxiliar2;
          auxiliar1(:,:,1)=minanomsis(:,:,ii);
          auxiliar1(:,:,2)=minanomsis(:,:,min_group);
          %auxiliar2(indexii)=auxiliar1(indexii);
          minanomsis(:,:,min_group)=nanmin(auxiliar1,[],3);
          auxiliar1(:,:,1)=minarea(:,:,ii);
          auxiliar1(:,:,2)=minarea(:,:,min_group);
          
          %auxiliar2(indexii)=auxiliar1(indexii);
          minarea(:,:,min_group)=nansum(auxiliar1,3);
          
          auxiliar1(:,:,1)=uvel(:,:,ii);
          auxiliar1(:,:,2)=uvel(:,:,min_group);
          %auxiliar2(indexii)=auxiliar1(indexii);
          uvel(:,:,min_group)=nanmean(auxiliar1,3);
          auxiliar1(:,:,1)=vvel(:,:,ii);
          auxiliar1(:,:,2)=vvel(:,:,min_group);
          %auxiliar2(indexii)=auxiliar1(indexii);
          vvel(:,:,min_group)=nanmean(auxiliar1,3);
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
          
%           if(ii==7)
%             minlatanalysis(:,7)
%             minlatanalysis(:,29) 
%             nanmean([minlatanalysis(:,ii)';minlatanalysis(:,min_group)'])
%             minlatanalysis(:,min_group)=nanmean([minlatanalysis(:,ii)';minlatanalysis(:,min_group)']); 
%             minlatanalysis(:,min_group)
%           end
          minlatanalysis(:,min_group)=nanmean([minlatanalysis(:,ii)';minlatanalysis(:,min_group)']);     
          tmplon=[minlonanalysis(:,ii)';minlonanalysis(:,min_group)'];
          for ikk=1:size(tmplon,2)
              minlonanalysis(ikk,min_group)=mean_lon_fun(tmplon(:,ikk));
          end

          %minlonanalysis(indexiiana,min_group)=[minlonanalysis(indexiiana,ii)];
          minareaanalysis(:,min_group)=nansum([minareaanalysis(:,ii)';minareaanalysis(:,min_group)']);
          minanomsisanalysis(:,min_group)=nanmin([minanomsisanalysis(:,ii)';minanomsisanalysis(:,min_group)']);
          uvelanalysis(:,min_group)=nanmean([uvelanalysis(:,ii)';uvelanalysis(:,min_group)']);
          vvelanalysis(:,min_group)=nanmean([vvelanalysis(:,ii)';vvelanalysis(:,min_group)']);
          index=isnan(trajidanalysis(:,min_group));
          trajidanalysis(index,min_group)=trajidanalysis(index,ii);

        %Ahora tengo que cambiar el numero de grupo de asociacion a todas
        %las trayectorias asociadas al grupo 1.
          for kk=1:enssize
             tmp=trajid(kk,:,ii);
             tmp=tmp(~isnan(tmp));
             if(~isempty(tmp))
                 tmp=unique(tmp); %Busco los valores unicos en tmp.
                 
                 for kkk=1:length(tmp)
                 %Para cada valor de tmp busco esa trayectoria y le cambio el id del grupo al que esta asociada.
                   %TrajStruct(kk).trajectories(tmp(kkk)).idgroup=min_group;
                   TrajStruct(kk).trajectories(tmp(kkk)).idgroup=NaN;
                   TrajStruct(kk).trajectories(tmp(kkk)).asociada=false;
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
                   %AnalysisTrajStruct(tmp(kkk)).idgroup=min_group;   
                   AnalysisTrajStruct(tmp(kkk)).idgroup=NaN;
                   AnalysisTrajStruct(tmp(kkk)).asociada=false;
                 end
             end
             
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
          minanomsis(:,:,ii)=NaN;
          minarea(:,:,ii)=NaN;
          minanomsisanalysis(:,ii)=NaN;
          minareaanalysis(:,ii)=NaN;
          uvel(:,:,ii)=NaN;
          vvel(:,:,ii)=NaN;
          uvelanalysis(:,ii)=NaN;
          vvelanalysis(:,ii)=NaN;
          trajid(:,:,ii)=NaN;
          trajidanalysis(:,ii)=NaN;

        end
       end
   
       
end
%Hasta aca verificamos si algunos grupos podian fusionarse en uno solo.
%Si esto sucedio, entonces habria que recalcular el numero de grupos y
%volverlos consecutidos otra vez.



end %End del if sobre si chequeo o no la fusion de grupos.



end %End del for sobre las iteraciones del algoritmo.
% 
%
tiempo=toc;
fprintf('TIEMPO EMPLEADO EN CONFORMAR LOS GRUPOS Y EN ASIGNAR LAS TRAYECTORIAS DEL ANALYSIS: %f\n',tiempo);
% 

if(LittleTraj)
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
                                                  
%           if(imember==4 && jgroup==20)
%              fprintf('Las funciones de costo son %10.1f , %10.1f , %10.1f y %10.1f, traj %4f \n',cfd(jgroup)/1000,cft(jgroup),cfgr(jgroup),cftr(jgroup),ii)
%           end
         
       end  %En del for sobre los grupos para calcular la funcion de costo.
       
       cfd( cfd >= tr_cost_function)=NaN;  
       cft( cfd >= tr_cost_function)=NaN;  %Elimino los grupos que no cumplen el criterio de distancia.
       %cft( cfgr <= 0.3            )=NaN;  %Elimino los que no coinciden en al menos un 50 % con el grupo.
       %Al eliminar el criterio sobre cfgr lo que hago es permitir que la
       %trayectoria sea muy corta respecto del grupo, pero al elevar el
       %otro criterio pido que este contenida casi totalmente por el grupo,
       %ademas no permito que se superponga con ninguna trayectorias
       %preexistente de ese miembro del ensamble para dicho grupo.
       cfd( cftr <= 0.9            )=NaN;  %Elimino aquellas trayectorias que no estan contenidas al menos en un 50% por el grupo.
       %Ahora tengo en cuenta la distancia del inicio de la trajectoria al
       %origen del grupo (en tiempo).
       %De todos los posibles grupos, me quedo con el que es mas
       %consistente en terminos del origen de la trayectoria.
       [min_cft min_group]=nanmin(cfd);
       %min_cfd=cfd(min_group);
       %min_cfgr=cfgr(min_group);
       %min_cftr=cftr(min_group);
       
       %===================================================================
       % SI HAY UN GRUPO AL QUE ESTA TRAYECTORIA PUEDE PERTENECER VAMOS A
       % ESTUDIAR DIFERENTES POSIBILIDADES.
       %===================================================================
       %asociar=false;
       if( ~isnan(min_cft) )  %Si es NaN quiere decir que todos los grupos estaban muy lejos para ser asociados.
          
          
          %Antes de asociar el sistema al grupo vamos a verificar que la
          %posicion que va a ocupar dicho sistema no este ocupada por otro
          %sistema.
          
          %Calculamos los lugares que ocuparia la trayectoria seleccionada.
          [nada index] = intersect(times,TrajStruct(imember).trajectories(ii).daten);   %#ok<ASGLU>
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

          TrajStruct(imember).trajectories(ii).asociada=true;  
          TrajStruct(imember).trajectories(ii).idgroup=min_group; %A que grupo asignamos la trajectoria.
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
       cfd( cfd >= tr_cost_function)=NaN; 
       cft( cfd >= tr_cost_function)=NaN;  %Elimino los grupos que no cumplen el criterio de distancia.
       %cft( cfgr <= 0.3            )=NaN;  %Elimino los que no coinciden en al menos un 50 % con el grupo.
       %Nuevamente al igual que como hice con las trayectorias
       %pronosticadas, en este paso relajo la condicion de longitud de la
       %trayectoria respecto del grupo, pero si pido que la trayectoria
       %este caso toda contenida en el grupo.
       cfd( cftr <= 0.5            )=NaN;  %Elimino aquellas trayectorias que no estan contenidas al menos en un 90% por el grupo.
       %Ahora tengo en cuenta la distancia del inicio de la trajectoria al
       %origen del grupo (en tiempo).
       %De todos los posibles grupos, me quedo con el que es mas
       %consistente en terminos del origen de la trayectoria.
       [min_cft min_group]=nanmin(cfd);
       %min_cfd=cfd(min_group);
       %min_cfgr=cfgr(min_group);
       %min_cftr=cftr(min_group);
       
       %===================================================================
       % SI HAY UN GRUPO AL QUE ESTA TRAYECTORIA PUEDE PERTENECER VAMOS A
       % ESTUDIAR DIFERENTES POSIBILIDADES.
       %===================================================================
       %asociar=false;
       if( ~isnan(min_cft) )  %Si es NaN quiere decir que todos los grupos estaban muy lejos para ser asociados.
          
          
          %Antes de asociar el sistema al grupo vamos a verificar que la
          %posicion que va a ocupar dicho sistema no este ocupada por otro
          %sistema.
          
          %Calculamos los lugares que ocuparia la trayectoria seleccionada.
          [nada index] = intersect(times,AnalysisTrajStruct(ii).daten);   %#ok<ASGLU>
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

end %End del if sobre LittleTraj (si asocio o no las pequenias trayectorias).
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
 
fprintf('El grupo %10f era antes el grupo %10f \n',contador,ii);
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


minarea(minarea==0)=NaN;
minareaanalysis(minareaanalysis==0)=NaN;

%ASIGNO LAS DIFERENTES MATRICES A LA ESTRUCTURA GROUP.

group.ngroup=ngroup;       
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






