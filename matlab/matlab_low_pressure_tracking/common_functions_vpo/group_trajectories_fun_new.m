function [group]=group_trajectories_fun_new(TrajStruct,AnalysisTrajStruct,config)

%NUEVA RUTINA DE AGRUPAMIENTO DE TRAYECTORIAS.

Debug=false;
MaxIterations=3;           %Cuantas pasadas del algoritmo para definir los grupos.
MaxGroup=200;
MaxCostFunction=1;
MinTrajLength=12;
MaxDistance=700e3;


fprintf('CONFORMANDO LOS GRUPOS CORRESPONDIENTES A LA FECHA %s\n',config.date_ini);

%Compute total number of times.
enssize=size(TrajStruct,2);
ntimes=(datenum(config.date_end,'yyyymmddHH')-datenum(config.date_ini,'yyyymmddHH'))*24/config.timefrec+1;
times=datenum(config.date_ini,'yyyymmddHH'):(config.timefrec/24):datenum(config.date_end,'yyyymmddHH');

%==========================================================================
% Paso 0: Preproceso las trayectorias para poder quedarme solo con las que
% estan en latitudes medias.
%==========================================================================
ntraj=NaN(1,enssize);
for imember=1:enssize
ntraj(imember)=size(TrajStruct(imember).trajectories,2);  %Numero de trayectorias en el miembro correspondiente.
end

ntrajanalysis=size(AnalysisTrajStruct,2);  %Numero de trayectorias en el miembro correspondiente.

for ii=1:ntrajanalysis   
    AnalysisTrajStruct(ii).proctraj=true;
end

%==========================================================================
%GUARDO LAS TRAYECTORIAS EN UN ARRAY.

trajectorylat=NaN(enssize,ntimes,max(ntraj));
trajectorylon=NaN(enssize,ntimes,max(ntraj));

trajectorylatanalysis=NaN(ntimes,max(ntrajanalysis));
trajectorylonanalysis=NaN(ntimes,max(ntrajanalysis));

for imember=1:enssize
  for ii=1:ntraj(imember)
    if(max(abs(TrajStruct(imember).trajectories(ii).minlatf)) > 20 && max(abs(TrajStruct(imember).trajectories(ii).minlatf)) < 80 ...
        && ~isempty(TrajStruct(imember).trajectories(ii).length) )
        proctraj(imember,ii)=true;
        [nada index]=intersect(times,TrajStruct(imember).trajectories(ii).daten);
    
        trajectorylat(imember,index,ii)=TrajStruct(imember).trajectories(ii).minlatf;
        trajectorylon(imember,index,ii)=TrajStruct(imember).trajectories(ii).minlonf;
    
    end
  end

end

proctrajanalysis=false(1,ntrajanalysis);
for ii=1:ntrajanalysis   
    if(max(abs(AnalysisTrajStruct(ii).minlatf)) > 20 && max(abs(AnalysisTrajStruct(ii).minlatf)) < 80 ...
            && ~isempty(AnalysisTrajStruct(ii).length) )
        proctrajanalysis(ii)=true;
        [nada index]=intersect(times,AnalysisTrajStruct(ii).daten);
        
        trajectorylatanalysis(index,ii)=AnalysisTrajStruct(ii).minlatf;
        trajectorylonanalysis(index,ii)=AnalysisTrajStruct(ii).minlonf;     
    end
end

ngroup=0; 
asoctraj=false(enssize,max(ntraj));
creategroup=true(enssize,max(ntraj));
groupidtraj=NaN(enssize,max(ntraj));
trajectories=cell(enssize,MaxGroup);

asoctrajanalysis=false(1,max(ntrajanalysis));
creategroupanalysis=true(1,max(ntrajanalysis));
groupidtrajanalysis=NaN(1,max(ntrajanalysis));
trajectoriesanalysis=cell(MaxGroup,1);

tic


%==========================================================================
% COMIENZO EL CALCULO DE LOS GRUPOS
%==========================================================================

meanlat=NaN(ntimes,MaxGroup);
meanlon=NaN(ntimes,MaxGroup);
numtraj=NaN(ntimes,MaxGroup);


for nloop=1:MaxIterations;
  fprintf('Realizando iteracion %10f, de %10f\n',nloop,MaxIterations); 
  
  %========================================================================
  % ANTES DE ASOCIAR TRAYECTORIAS VERIFICO QUE NO HAYA CONFLICTOS ENTRE
  % GRUPOS, SI HAY DOS GRUPOS QUE ESTAN MUY CERCANOS DE ACUERDO CON EL
  % CRITERIO UTILIZADO POR LA COST FUNCTION ENTONCES VOY A TOMAR EL MAS
  % CHICO (EL HILO SE CORTA SIEMPRE POR LO MAS DELGADO), LO VOY A ELIMINAR
  % Y VOY A IMPEDIR QUE SUS MIEMBROS PUEDAN OTRA VEZ FORMAR GRUPOS.
  %========================================================================
  
  for ii=1:ngroup
       cost_function=NaN(1,ngroup);
       for jj=1:ngroup
           if(jj~=ii)
           cost_function(jj)=group_cost_function(meanlon(:,ii),meanlat(:,ii), ...
                                                     meanlon(:,jj),meanlat(:,jj),MaxDistance);
           else
           cost_function(jj)=1e10;
           end
       end
      
      [min_cost min_group]=min(cost_function);
      if( min_cost < MaxCostFunction)
      %Entonces hay conflicto entre el grupo ii y el jj.
      totnumtrajii=nansum(numtraj(:,ii));
      totnumtrajjj=nansum(numtraj(:,min_group));
      if( totnumtrajii < totnumtrajjj)
          elimina_grupo=ii;
          tmpg=min_group; %Solo para el cartelito.
      else
          elimina_grupo=min_group;
          tmpg=ii;
      end
      %VOY A ELIMINAR EL GRUPO "ELIMINA_GRUPO".
      %if(Debug)
      fprintf('El grupo %10f va a ser eliminado por un conflicto con el grupo %10f\n',elimina_grupo,tmpg);
      %end
      meanlat(:,elimina_grupo)=NaN;
      meanlon(:,elimina_grupo)=NaN;
      numtraj(:,elimina_grupo)=NaN;
      

      %cambio el status de todas las trayectorias asociadas a este grupo.
      for im=1:enssize
          for it=1:length(trajectories{im,elimina_grupo})
              ct=trajectories{im,elimina_grupo}(it);
              asoctraj(im,ct)=false;
              creategroup(im,ct)=false;  %EN ESTA LINEA IMPIDO QUE LOS MIEMBROS DE ESTE GRUPO VUELVA A FORMAR UN GRUPO APARTE.
              groupidtraj(im,ct)=NaN;
          end
              trajectories{im,elimina_grupo}=[];
          for it=1:length(trajectoriesanalysis{elimina_grupo})
              ct=trajectoriesanalysis{elimina_grupo}(it);
              asoctrajanalysis(ct)=false;
              creategroupanalysis(ct)=false;
              groupidtrajanalysis(ct)=false;  
          end
      end 
      
      
      end
  end %End del do sobre los diferentes grupos.
  
  %========================================================================
  % FIN DEL CHEQUEO DE LOS CONFLICOS ENTRE GRUPOS.
  %========================================================================
    
for imember=1:enssize

for ii=1:ntraj(imember)
    if(proctraj(imember,ii)) %Solo chequeo la trajectoria si cumple con las condiciones previas (posicion).
    
    %group=1:ngroup;
    cost_function=NaN(1,ngroup);
    for igroup=1:ngroup      
       cost_function(igroup)=group_cost_function(meanlon(:,igroup),meanlat(:,igroup), ...
                                                 trajectorylon(imember,:,ii)',trajectorylat(imember,:,ii)',MaxDistance);
    end
       %Busco el minimo de la funcion de costo para ver que grupo es el mas
       %adecuado.
       if(ngroup > 0)
       [min_cost min_group]=min(cost_function);
       else
       min_cost=1e10;
       end

       if( (min_cost > MaxCostFunction & asoctraj(imember,ii) ) | ...
           ( min_cost < MaxCostFunction & asoctraj(imember,ii) & groupidtraj(imember,ii)~=min_group ) )
       %EL SISTEMA ESTABA ASOCIADO A UN GRUPO DISTINTO, HAY QUE SACARLO DE
       %AHI Y TRAERLO AL NUEVO GRUPO.
       
           grupo_anterior=groupidtraj(imember,ii);
           asoctraj(imember,ii)=false;
           groupidtraj(imember,ii)=NaN;
           tmp=trajectories{imember,grupo_anterior};
           tmp(tmp==ii)=[];
           trajectories{imember,grupo_anterior}=tmp;
           clear tmp
                   
       %ACTUALIZO LA MEDIA DEL GRUPO ANTERIOR
           tmplat=NaN(enssize*10,ntimes);
           tmplon=NaN(enssize*10,ntimes);
           contador=0;
           for im=1:enssize
              for itr=1:length(trajectories{im,grupo_anterior});
              contador=contador+1;
              tmplat(contador,:)=trajectorylat(im,:,trajectories{im,grupo_anterior}(itr));
              tmplon(contador,:)=trajectorylon(im,:,trajectories{im,grupo_anterior}(itr));
              end     
           end 
           meanlat(:,grupo_anterior)=nanmean(tmplat(1:contador,:));
           numtraj(:,grupo_anterior)=nansum(~isnan(tmplat(1:contador,:)));
           for it=1:ntimes
           meanlon(it,grupo_anterior)=mean_lon_fun(tmplon(1:contador,it));
           end
           clear tmplat tmplon;
           if(Debug)
               fprintf('Desasocie la trayectoria %10f del miembro %10f, del grupo %10f \n',ii,imember,grupo_anterior);
           end
       end
       
       if( min_cost < MaxCostFunction & ~asoctraj(imember,ii) )
       %ASOCIO LA TRAYECTORIA POR PRIMERA VEZ
           
           trajectories{imember,min_group}=[trajectories{imember,min_group} ii];
           asoctraj(imember,ii)=true;
           groupidtraj(imember,ii)=min_group;
       %ACTUALIZO LA MEDIA DEL GRUPO.    
           tmplat=NaN(enssize*10,ntimes);
           tmplon=NaN(enssize*10,ntimes);
           contador=0;
           for im=1:enssize
              for itr=1:length(trajectories{im,min_group});
              contador=contador+1;
              tmplat(contador,:)=trajectorylat(im,:,trajectories{im,min_group}(itr));
              tmplon(contador,:)=trajectorylon(im,:,trajectories{im,min_group}(itr));
              end     
           end 
           meanlat(:,min_group)=nanmean(tmplat(1:contador,:));
           numtraj(:,min_group)=nansum(~isnan(tmplat(1:contador,:)));
           
           for it=1:ntimes
           meanlon(it,min_group)=mean_lon_fun(tmplon(1:contador,it));
           end
           clear tmplat tmplon;   
           if(Debug)
               fprintf('Asocie la trayectoria %10f del miembro %10f, del grupo %10f \n',ii,imember,min_group);
           end
       end
       
       %VEO SI LA TRAYECTORIA PODRIA ESTAR CREANDO UN NUEVO GRUPO.
       if( min_cost > MaxCostFunction & ~asoctraj(imember,ii) & sum(~isnan(trajectorylat(imember,:,ii))) > MinTrajLength ...
           & creategroup(imember,ii) )
       %EN ESTE CASO LA TRAYECTORIA NO ESTA ASOCIADA Y TIENE EL LARGO SUFICIENTE PARA INICIAR UN NUEVO GRUPO.
         ngroup=ngroup+1;
         meanlat(:,ngroup)=trajectorylat(imember,:,ii);
         meanlon(:,ngroup)=trajectorylon(imember,:,ii);
         asoctraj(imember,ii)=true;
         groupidtraj(imember,ii)=ngroup;
         trajectories{imember,ngroup}=ii;
         %if(Debug)
               fprintf('La trayectoria %10f del miembro %10f, creo el grupo %10f \n',ii,imember,ngroup);
         %end

       end
    end  %End sobre el if de procesar o no la trajectoria.
    
    if(asoctraj(imember,ii) & isnan(groupidtraj(imember,ii)))
       fprintf('WARNING: la trayectoria %10f, del miembro %10f, esta asociada, pero su groupid es NaN\n',ii,imember); 
    end
    
    
end  %End del do sobre las trayectorias del miembro imember


end  %End del do sobre los miembros del ensamble.

%==========================================================================
% ASOCIO LAS TRAYECTORIAS DEL ANALYSIS A LOS DIFERENTES GRUPOS.
%==========================================================================

% for ii=1:ntrajanalysis
%     if(proctrajanalysis(ii)) %Solo chequeo la trajectoria si cumple con las condiciones previas (posicion).
%     
%     %group=1:ngroup;
%     cost_function=NaN(1,ngroup);
%     for igroup=1:ngroup      
%        cost_function(igroup)=group_cost_function(meanlon(:,igroup),meanlat(:,igroup), ...
%                                                  trajectorylonanalysis(:,ii),trajectorylatanalysis(:,ii),MaxDistance);
%     end
%        %Busco el minimo de la funcion de costo para ver que grupo es el mas
%        %adecuado.
%        if(ngroup > 0)
%        [min_cost min_group]=min(cost_function);
%        else
%        min_cost=1e10;
%        end
% 
%        if( (min_cost > MaxCostFunction & asoctrajanalysis(ii) ) | ...
%            ( min_cost < MaxCostFunction & asoctrajanalysis(ii) & groupidtrajanalysis(ii)~=min_group ) )
%        %EL SISTEMA ESTABA ASOCIADO A UN GRUPO DISTINTO, HAY QUE SACARLO DE
%        %AHI Y TRAERLO AL NUEVO GRUPO.
%        
%            grupo_anterior=groupidtrajanalysis(ii);
%            asoctrajanalysis(ii)=false;
%            groupidtrajanalysis(ii)=NaN;
%            tmp=trajectoriesanalysis{grupo_anterior};
%            tmp(tmp==ii)=[];
%            trajectoriesanalysis{grupo_anterior}=tmp;
%            clear tmp
%                    
%            if(Debug)
%                fprintf('Desasocie la trayectoria del analysis %10f , del grupo %10f \n',ii,grupo_anterior);
%            end
%        end
%        
%        if( min_cost < MaxCostFunction & ~asoctrajanalysis(ii) )
%        %ASOCIO LA TRAYECTORIA POR PRIMERA VEZ
%            
%            trajectoriesanalysis{min_group}=[trajectoriesanalysis{min_group} ii];
%            asoctrajanalysis(ii)=true;
%            groupidtrajanalysis(ii)=min_group;
% 
%            %if(Debug)
%                fprintf('Asocie la trayectoria %10f del analysis, al grupo %10f \n',ii,min_group);
%            %end
%        end
%        
%        %VEO SI LA TRAYECTORIA PODRIA ESTAR CREANDO UN NUEVO GRUPO.
%        if( min_cost > MaxCostFunction & ~asoctrajanalysis(ii) & sum(~isnan(trajectorylatanalysis(:,ii))) > MinTrajLength ...
%            & creategroupanalysis(ii) )
%        %EN ESTE CASO LA TRAYECTORIA NO ESTA ASOCIADA Y TIENE EL LARGO SUFICIENTE PARA INICIAR UN NUEVO GRUPO.
%          ngroup=ngroup+1;
%          meanlat(:,ngroup)=trajectorylatanalysis(:,ii);
%          meanlon(:,ngroup)=trajectorylonanalysis(:,ii);
%          asoctrajanalysis(ii)=true;
%          groupidtrajanalysis(ii)=ngroup;
%          trajectoriesanalysis{ngroup}=ii;
%          %if(Debug)
%                fprintf('La trayectoria %10f del analysis, creo el grupo %10f \n',ii,ngroup);
%          %end
% 
%        end
%     end  %End sobre el if de procesar o no la trajectoria.
%     
%     if(asoctrajanalysis(ii) & isnan(groupidtrajanalysis(ii)))
%        fprintf('WARNING: la trayectoria %10f, del analysis, esta asociada, pero su groupid es NaN\n',ii); 
%     end
%     
%     
% end  %End del do sobre las trayectorias del analysis



end %End del for sobre las iteraciones del algoritmo.
% 

tiempo=toc;
fprintf('TIEMPO EMPLEADO EN CONFORMAR LOS GRUPOS Y EN ASIGNAR LAS TRAYECTORIAS DEL ANALYSIS: %f\n',tiempo);
% 


%ASIGNO LAS DIFERENTES MATRICES A LA ESTRUCTURA GROUP.

group.ngroup=ngroup;
group.meanlat=meanlat(:,1:ngroup);
group.meanlon=meanlon(:,1:ngroup);
group.numtraj=numtraj;
group.trajectories=trajectories;


for igroup=1:ngroup
    contador=0;
    for im=1:enssize
        for itr=1:length(trajectories{im,igroup});
            contador=contador+1;
            tmplat(contador,:)=trajectorylat(im,:,trajectories{im,igroup}(itr));
            tmplon(contador,:)=trajectorylon(im,:,trajectories{im,igroup}(itr));
        end     
    end 
    if(~isempty(trajectories{im,igroup}))
    group.trajinfo(igroup).minlat=tmplat(1:contador,:);
    group.trajinfo(igroup).minlon=tmplon(1:contador,:);
    else
    group.trajinfo(igroup).minlat=NaN(1,ntimes);
    group.trajinfo(igroup).minlon=NaN(1,ntimes);
    end
    
%     clear tmplat tmplon
%     for itr=1:length(trajectoriesanalysis{igroup});
%        tmplat(itr,:)=trajectorylatanalysis(:,trajectoriesanalysis{igroup}(itr));
%        tmplon(itr,:)=trajectorylonanalysis(:,trajectoriesanalysis{igroup}(itr));    
%     end
%     if(~isempty(trajectoriesanalysis{igroup}))
%      group.trajinfo(igroup).minlatanalysis=tmplat;
%      group.trajinfo(igroup).minlonanalysis=tmplon;
%     else
%      group.trajinfo(igroup).minlatanalysis=NaN(1,ntimes);
%      group.trajinfo(igroup).minlonanalysis=NaN(1,ntimes);
%     end
    
end

%TODO: AGREGAR LAS TRAYECTORIAS DEL ANALYSIS, SEGUIR AGREGANDO PROPIEDADES
%AL GRUPO.

%group.ngroup=ngroup;       
%group.minlat=minlat;
%group.minlon=minlon;
%group.minanom=minanom;
%group.minlap=minlap;
%group.uvel=uvel;
%group.vvel=vvel;
%group.meanlat=meanlat;
%group.meanlon=meanlon;
%group.trajid=trajid;
%group.nmember=nmember;
%group.meanlength=meanlength;

%group.minlatanalysis=minlatanalysis;
%group.minlonanalysis=minlonanalysis;
%group.minlapanalysis=minlapanalysis;
%group.minanomanalysis=minanomanalysis;
%group.uvelanalysis=uvelanalysis;
%group.vvelanalysis=vvelanalysis;
%group.trajidanalysis=trajidanalysis;



%LISTO EL LLOPO, PELADA LA GALLINA.






