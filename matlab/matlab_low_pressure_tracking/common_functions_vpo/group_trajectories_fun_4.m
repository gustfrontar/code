function [group]=group_trajectories_fun_2(TrajStruct,AnalysisTrajStruct,config)

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

%EN ESTA VERSION AUMENTAMOS LA SUPERPOSICION POSIBLE Y VEMOS DE CAMBIAR LA
%FUNCION DE COSTO Y LO QUE SUCEDE CUANDO 2 GRUPOS SE SUPERPONEN.

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

tr_cost_function=1;
MaxDist=900e3;
MaxGroup=200;          %Maximo numero de grupos que vamos a allocatear de entrada.

CheckGroupMerge=true;
%obtengo el numero de miembros en el ensamble.
enssize=size(TrajStruct,2);

MaxIterations=4;           %Cuantas pasadas del algoritmo para definir los grupos.
MaxSuperposition=29;        %Maxima superposicion temporal permitida entre 2 trayectorias del mismo miembro que van a pertenecer al mismo grupo.
MinBreakTraj=5;             %A partir de cuantos puntos vamos a crear una nueva trayectoria a partir de una trayectoria rota.
%LittleTraj=false;

fprintf('CONFORMANDO LOS GRUPOS CORRESPONDIENTES A LA FECHA %s\n',config.date_ini);

%==========================================================================
% Paso 0: Preproceso las trayectorias para poder quedarme solo con las que
% estan en latitudes medias.
%==========================================================================

for imember=1:enssize
ntraj=size(TrajStruct(imember).trajectories,2);  %Numero de trayectorias en el miembro correspondiente.

for ii=1:ntraj
    TrajStruct(imember).trajectories(ii).proctraj=true;
    TrajStruct(imember).trajectories(ii).iniciagrupo=true;
    if(max(abs(TrajStruct(imember).trajectories(ii).minlatf)) < 20 )
        TrajStruct(imember).trajectories(ii).proctraj=false;
    end
    if(max(abs(TrajStruct(imember).trajectories(ii).minlatf)) > 80 )
        TrajStruct(imember).trajectories(ii).proctraj=false;
    end
    %DEGUG DEBUG
    if( max(TrajStruct(imember).trajectories(ii).minlatf) > -20 | min(TrajStruct(imember).trajectories(ii).minlonf) < 100)
    TrajStruct(imember).trajectories(ii).proctraj=false;
    TrajStruct(imember).trajectories(ii).iniciagrupo=false;
    end
    %DEBUG DEBUG
end





end

ntraj=size(AnalysisTrajStruct,2);  %Numero de trayectorias en el miembro correspondiente.

for ii=1:ntraj
    
    AnalysisTrajStruct(ii).proctraj=true;
    AnalysisTrajStruct(ii).iniciagrupo=true;

    if(max(abs(AnalysisTrajStruct(ii).minlatf)) < 20)
        AnalysisTrajStruct(ii).proctraj=false;
    end
    if(max(abs(AnalysisTrajStruct(ii).minlatf)) > 80) 
        AnalysisTrajStruct(ii).proctraj=false;
    end
end

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
minanom=NaN(enssize,ntimes,MaxGroup);
minlap=NaN(enssize,ntimes,MaxGroup);
uvel=NaN(enssize,ntimes,MaxGroup);
vvel=NaN(enssize,ntimes,MaxGroup);
meanlat=NaN(ntimes,MaxGroup);
meanlon=NaN(ntimes,MaxGroup);
trajid=NaN(enssize,ntimes,MaxGroup);   %Guarda el id de la trajectoria que fue asignada a cada punto.
nmember=NaN(MaxGroup);
meanlength=NaN(MaxGroup);

minlatanalysis=NaN(ntimes,MaxGroup);
minlonanalysis=NaN(ntimes,MaxGroup);
minlapanalysis=NaN(ntimes,MaxGroup);
minanomanalysis=NaN(ntimes,MaxGroup);
uvelanalysis=NaN(ntimes,MaxGroup);
vvelanalysis=NaN(ntimes,MaxGroup);
trajidanalysis=NaN(ntimes,MaxGroup);

ngroup=0;

%OJO ESTE LOOP SE TIENE QUE EJECUTAR UNA SOLA VEZ.
MinLengthAsoc=5;   %Conformamos los grupos inicialmente con las trayectorias mas largas que 5 elementos.

for nloop=1:MaxIterations;
  fprintf('Realizando iteracion %10f, de %10f\n',nloop,MaxIterations); 
  if(nloop==MaxIterations)
      %En el ultimo loop agregamos tambien las trayectorias mas cortas, una
      %vez que el algoritmo es mas estable.
      MinLengthAsoc=1;
      tr_cost_function=700e3;
      MaxSuperposition=2;
  end
    
for imember=1:enssize
ntraj=size(TrajStruct(imember).trajectories,2);  %Numero de trayectorias en el miembro correspondiente.

for ii=1:ntraj
    
    if(nloop==1)
    TrajStruct(imember).trajectories(ii).asociada=false;  %Indica si la trajectoria fue o no asociada, se inicializa como false.
    TrajStruct(imember).trajectories(ii).idgroup=NaN;
    end

    if( ~isempty(TrajStruct(imember).trajectories(ii).length) && TrajStruct(imember).trajectories(ii).proctraj &&  ...
        TrajStruct(imember).trajectories(ii).length >= MinLengthAsoc )
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
         %grouplength=meanlength(jgroup);
         grouptime=times(isnan_tr);
         %initime=times(1);
         
         
         [cfd(jgroup)]=group_cost_function(grouplon,grouplat,grouptime,...
                                              TrajStruct(imember).trajectories(ii).minlonf,TrajStruct(imember).trajectories(ii).minlatf,...
                                              TrajStruct(imember).trajectories(ii).daten,MaxDist); 
%           if( jgroup == 2 && imember == 5)
%           fprintf('Trayectoria %10f,  con un J de %10f \n',ii,cfd(jgroup))   
%           end
       end  %En del for sobre los grupos para calcular la funcion de costo.
%     

       
       groups=1:ngroup;
       
       
       groups=groups( cfd <= tr_cost_function );
       cfd=cfd( cfd <= tr_cost_function );
       
       formagrupo=isempty(groups);     %La trayectoria va a poder formar grupos si esta muy lejos de cualquier grupo.

       %Ordenamos las funciones de costo y los grupos de menor a mayor.
       [cfd isort]=sort(cfd);
       groups=groups(isort);
       
        if( imember == 5 && ii==246)
           cfd
           groups
           TrajStruct(imember).trajectories(ii).asociada
           TrajStruct(imember).trajectories(ii).idgroup
        end
       
       
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
               if( sum(~isnan(minlat(imember,index,cgroup))) == 0)
               %EN ESTE CASO PUEDO ASOCIAR LA TRAYECTORIA ENTERA HAY LUGAR EN EL GRUPO.    
                asociar=true;
                newgroup=cgroup;
                indexasociar=1:length(TrajStruct(imember).trajectories(ii).daten); %Todos los puntos van a ser asociados.
                if(TrajStruct(imember).trajectories(ii).asociada)
                    desasociar=true;
                end
               end 
               
               if(~asociar && sum(~isnan(minlat(imember,index,cgroup))) <=  MaxSuperposition )
                %HAY LUGAR PERO PARTE DE LA TRAYECTORIA SE SUPERPONE CON
                %OTRAS PREEXISTENTES. VOY A COMPRAR LA FUNCION DE COSTO DE
                %ESOS TRAMOS PARA LO QUE HAY EN EL GRUPO Y PARA LA NUEVA
                %TRAYECTORIA. SI LO QUE HABIA ES MEJOR QUE LA NUEVA
                %TRAYECTORIA, ROMPO LA NUEVA TRAYECTORIA Y SOLO ASIGNO LA
                %PARTE QUE NO SE SUPERPONE CON LO QUE HAY.
                tmpindex=false(1,size(minlat,2));
                tmpindex(index)=true;
                indexsuper=find(~isnan(minlat(imember,:,cgroup)) & tmpindex);
                tmplon1=minlon(imember,indexsuper,cgroup);
                tmplat1=minlat(imember,indexsuper,cgroup);
                tmptime1=times(indexsuper);
               
                tmplong=meanlon(indexsuper,cgroup);
                tmplatg=meanlat(indexsuper,cgroup);
                tmptimeg=times(indexsuper);
                
                [nada indexsupertraj]=intersect(TrajStruct(imember).trajectories(ii).daten,tmptime1);
                
                tmplon2=TrajStruct(imember).trajectories(ii).minlon(indexsupertraj);
                tmplat2=TrajStruct(imember).trajectories(ii).minlat(indexsupertraj);
                tmptime2=TrajStruct(imember).trajectories(ii).daten(indexsupertraj);
                
                %ALGUNAS CONDICIONES EXTRAS PARA QUE ESTO SE PUEDA HACER.
                %VERIFICAMOS QUE LA NUEVA TRAYECTORIA NO CAIGA "EN EL
                %MEDIO" DE UNA YA ASOCIADA AL GRUPO.
                
                requisito1=false;
                requisito2=false;
                if( indexsuper(1)==1 || indexsuper(end)==size(minlat,2) || ...
                    trajid(imember,indexsuper(1)-1,cgroup) ~= trajid(imember,indexsuper(end)+ 1,cgroup))
                    requisito1=true;
                end
                if( indexsupertraj(1)==1 || indexsupertraj(end)==length(TrajStruct(imember).trajectories(ii).daten))
                    requisito2=true;
                    %ACA PIDO QUE LA ZONA DE CONFLICTO NO QUEDE EN EL MEDIO
                    %DE LA NUEVA TRAYECTORIA QUE ESTOY QUERIENDO AGREGAR.
                end
                
                
                if( requisito1 && requisito2 )
                  %SOLO SI SE CUMPLEN AMBOS REQUISITOS VOY A CALCULAR LAS FUNCIONES DE COSTO.  
                  [cfd1]=group_cost_function(tmplong,tmplatg,tmptimeg,...
                                             tmplon1,tmplat1,tmptime1,MaxDist);
                  [cfd2]=group_cost_function(tmplong,tmplatg,tmptimeg,...
                                             tmplon2,tmplat2,tmptime2,MaxDist);                  
                                         
                  if(cfd1 < cfd2)
                  %LO QUE HABIA ERA MEJOR SOLO ASOCIO LA PARTE DE LA
                  %TRAYECTORIA QUE NO SE SUPERPONE CON EL GRUPO.
                  tmpindex=false(1,size(minlat,2));
                  tmpindex(index)=true;
                  tmpindex=isnan(minlat(imember,:,cgroup)) & tmpindex;
                  %obtengo los subindices de la trayectoria que no
                  %coinciden con lo que hay en el grupo.
                  [nada indexasociar]=intersect(TrajStruct(imember).trajectories(ii).daten,times(tmpindex));
                  %fprintf('Corte una trayectoria del miembro %10f para asociarla al grupo %10f\n',imember,cgroup)
                  %Verifico ademas si la parte de la trayectoria que voy a
                  %asignar al grupo cumple con la funcion de costo.
                  tmplon1=TrajStruct(imember).trajectories(ii).minlonf(indexasociar);
                  tmplat1=TrajStruct(imember).trajectories(ii).minlatf(indexasociar);
                  tmptime1=TrajStruct(imember).trajectories(ii).daten(indexasociar);
                  tmplong=meanlon(tmpindex,cgroup);
                  tmplatg=meanlat(tmpindex,cgroup);
                  tmptimeg=times(tmpindex);
                  [tmpcdf]=group_cost_function(tmplong,tmplatg,tmptimeg,...
                                             tmplon1,tmplat1,tmptime1,MaxDist);                     
                                         
                  if(tmpcdf <  tr_cost_function) 
                     
                  %Si lo que queda aufera de la trayectoria es muy largo
                  %entonces rompo la trayectoria original y creo una nueva
                  %trayectoria.
                  if( length(TrajStruct(imember).trajectories(ii).daten) - length(indexasociar) > MinBreakTraj)
                     if(indexsupertraj(1)==1)
                     newtrajindex=indexsupertraj(end)+1:length(TrajStruct(imember).trajectories(ii).daten);
                     end
                     if(indexsupertraj(end)==length(TrajStruct(imember).trajectories(ii).daten))
                     newtrajindex=1:min(indexsupertraj)-1;    
                     end
                     
                     maxtraj=size(TrajStruct(imember).trajectories,2); 
                     TrajStruct(imember).trajectories(maxtraj+1).minlatf=TrajStruct(imember).trajectories(ii).minlatf(newtrajindex);
                     TrajStruct(imember).trajectories(maxtraj+1).minlonf=TrajStruct(imember).trajectories(ii).minlonf(newtrajindex);
                     TrajStruct(imember).trajectories(maxtraj+1).daten=TrajStruct(imember).trajectories(ii).daten(newtrajindex);
                     TrajStruct(imember).trajectories(maxtraj+1).minlap=TrajStruct(imember).trajectories(ii).minlap(newtrajindex);
                     TrajStruct(imember).trajectories(maxtraj+1).minanomf=TrajStruct(imember).trajectories(ii).minanomf(newtrajindex);
                     TrajStruct(imember).trajectories(maxtraj+1).uvelf=TrajStruct(imember).trajectories(ii).uvelf(newtrajindex);
                     TrajStruct(imember).trajectories(maxtraj+1).vvelf=TrajStruct(imember).trajectories(ii).vvelf(newtrajindex);
                     TrajStruct(imember).trajectories(maxtraj+1).velf=TrajStruct(imember).trajectories(ii).velf(newtrajindex);
                     TrajStruct(imember).trajectories(maxtraj+1).asociada=false;
                     TrajStruct(imember).trajectories(maxtraj+1).groupid=NaN;
                     
                     %Elimino la nueva trayectoria de la trayectoria
                     %original
                     TrajStruct(imember).trajectories(ii).minlatf(newtrajindex)=[];
                     TrajStruct(imember).trajectories(ii).minlonf(newtrajindex)=[];
                     TrajStruct(imember).trajectories(ii).daten(newtrajindex)=[];
                     TrajStruct(imember).trajectories(ii).minlap(newtrajindex)=[];
                     TrajStruct(imember).trajectories(ii).minanomf(newtrajindex)=[];
                     TrajStruct(imember).trajectories(ii).uvelf(newtrajindex)=[];
                     TrajStruct(imember).trajectories(ii).vvelf(newtrajindex)=[];
                     TrajStruct(imember).trajectories(ii).velf(newtrajindex)=[];
                     %Modifico la asociacion de la trayectoria. Asocio toda
                     %la trayectoria.
                     indexasociar=1:length(TrajStruct(imember).trajectories(ii).daten);
                     
                  end
                  %Dejo esto seteado para que la trajectoria ii se asocie
                  %al grupo (una parte de la misma al menos).
                  asociar=true;  
                  newgroup=cgroup;
                  end
                  
                
                  else
                  %LA NUEVA TRAYECTORIA ES MEJOR, ENTONCES LA ASOCIO TODA Y
                  %REMUEVO LO QUE HABIA ANTES.
                  indexasociar=1:length(TrajStruct(imember).trajectories(ii).daten);
                  asociar=true;
                  newgroup=cgroup;
                  %REMUEVO EN FORMA PROLIJA LO QUE HABIA ANTES EN ESE
                  %LUGAR.
                  %Solo voy a desasociar aquellas trayectorias que estan
                  %totalmente superpuestas con la nueva trayectoria.
                   trajlist=unique(trajid(imember,indexsuper,cgroup));
                   trajlist=trajlist(~isnan(trajlist));
                   
                   for kk=1:length(trajlist)
                     itraj=trajlist(kk);
                     if( sum(trajid(imember,~indexsuper,cgroup)==itraj)==0)
                      %La trayectoria trajlist(kk) no tiene puntos fuera de
                      %la superposicion, entonces tengo que desasociarla.
                      %No es necesario que la desasocie del grupo porque
                      %eso va a ocurrir automaticamente cuando asocie la
                      %nueva trayectoria, pero si la tengo que desasociar
                      %en la estructura de las trayectorias.
                      TrajStruct(imember).trajectories(itraj).asociada=false; 
                      TrajStruct(imember).trajectories(itraj).idgroup=NaN;     
                     end
                     if( sum(trajid(imember,~indexsuper,cgroup)==itraj) > 0 && sum(trajid(imember,indexsuper,cgroup)==itraj) >= MinBreakTraj)
                     %La parte de la trayectoria que estaba asociada al
                     %grupo y que voy a remover es grande podria crear una
                     %nueva trayectoria a partir de eso. 
                     [nada newtrajindex]=intersect(TrajStruct(imember).trajectories(itraj).daten,times(indexsuper));
                     %Creo una nueva trayectoria a partir de la porcion que
                     %voy a remvoer del grupo.
                     maxtraj=size(TrajStruct(imember).trajectories,2); 
                     TrajStruct(imember).trajectories(maxtraj+1).minlatf=TrajStruct(imember).trajectories(itraj).minlatf(newtrajindex);
                     TrajStruct(imember).trajectories(maxtraj+1).minlonf=TrajStruct(imember).trajectories(itraj).minlonf(newtrajindex);
                     TrajStruct(imember).trajectories(maxtraj+1).daten=TrajStruct(imember).trajectories(itraj).daten(newtrajindex);
                     TrajStruct(imember).trajectories(maxtraj+1).minlap=TrajStruct(imember).trajectories(itraj).minlap(newtrajindex);
                     TrajStruct(imember).trajectories(maxtraj+1).minanomf=TrajStruct(imember).trajectories(itraj).minanomf(newtrajindex);
                     TrajStruct(imember).trajectories(maxtraj+1).uvelf=TrajStruct(imember).trajectories(itraj).uvelf(newtrajindex);
                     TrajStruct(imember).trajectories(maxtraj+1).vvelf=TrajStruct(imember).trajectories(itraj).vvelf(newtrajindex);
                     TrajStruct(imember).trajectories(maxtraj+1).velf=TrajStruct(imember).trajectories(itraj).velf(newtrajindex);
                     TrajStruct(imember).trajectories(maxtraj+1).asociada=false;
                     TrajStruct(imember).trajectories(maxtraj+1).groupid=NaN;
                     
                     %Elimino la nueva trayectoria de la trayectoria
                     %original
                     TrajStruct(imember).trajectories(itraj).minlatf(newtrajindex)=[];
                     TrajStruct(imember).trajectories(itraj).minlonf(newtrajindex)=[];
                     TrajStruct(imember).trajectories(itraj).daten(newtrajindex)=[];
                     TrajStruct(imember).trajectories(itraj).minlap(newtrajindex)=[];
                     TrajStruct(imember).trajectories(itraj).minanomf(newtrajindex)=[];
                     TrajStruct(imember).trajectories(itraj).uvelf(newtrajindex)=[];
                     TrajStruct(imember).trajectories(itraj).vvelf(newtrajindex)=[];
                     TrajStruct(imember).trajectories(itraj).velf(newtrajindex)=[];

                     end
                     
                   end
                  end
                  

                  
                if( TrajStruct(imember).trajectories(ii).asociada)
                    desasociar=true;
                end
                %SALGO DEL DO PORQUE YA ENCONTRE UN GRUPO AL QUE PUEDO
                %ASOCIAR LA TRAYECTORIA.
                break
                    
                end
                
              
               end
                
               if( ~asociar && nloop < MaxIterations)
               %LA TRAYECTORIA TODAVIA NO FUE ASOCIADA, ESO QUIERE DECIR
               %QUE O NO HABIA LUGAR O NO SE DIERON LAS CONDICIONES DE
               %SUPERPOSICION OPTIMAS. COMPITO CON LO QUE HAY DE FORMA DE
               %DECIDIR SI ME QUEDO CON LO QUE HAY O REEMPLAZO POR LA NUEVA
               %TRAYECTORIA.

               %Tengo que chequear cual/es es/son la/s trayectoria/s que esta/s asociada/s en
               %ese lugar y cual es la mejor opcion.
               %Calculo cuanto se superponen cada una de esas trayectorias con
               %el grupo y cuan cerca o lejos estan del origen.
               isnan_tr=~isnan(meanlon(:,cgroup));
               traj_asociadas=unique(trajid(imember,index,cgroup));
               traj_asociadas=traj_asociadas(~isnan(traj_asociadas));
               tmp_cfd=NaN(1,length(traj_asociadas)) ; %Distance cost function
               %tmp_cft=NaN(1,length(traj_asociadas)) ; %Time cost function
               %tmp_cfgr=NaN(1,length(traj_asociadas));
               %tmp_cftr=NaN(1,length(traj_asociadas));

               tmp_grouplon=meanlon(isnan_tr,cgroup);
               tmp_grouplat=meanlat(isnan_tr,cgroup);
               tmp_grouptime=times(isnan_tr);
               %tmp_grouplength=meanlength(cgroup);
               %initime=times(1);
                for kk=1:length(traj_asociadas)
                %Calculo la funcion de costo entre las trayectorias
                %asociadas y la trayectoria actual.
                
                 [tmp_cfd(kk)]=group_cost_function(tmp_grouplon,tmp_grouplat,tmp_grouptime,...
                 TrajStruct(imember).trajectories(traj_asociadas(kk)).minlonf,TrajStruct(imember).trajectories(traj_asociadas(kk)).minlatf,...
                 TrajStruct(imember).trajectories(traj_asociadas(kk)).daten,MaxDist);
                end

           if(  cfd(jgroup)  <  min(tmp_cfd) )
               %LA TRAYECTORIA NUEVA ES MEJOR QUE LAS QUE HABIA ENTONCES LA
               %ASOCIO Y SACO LAS ANTERIORES.
               asociar=true;
               newgroup=cgroup;
               indexasociar=1:length(TrajStruct(imember).trajectories(ii).daten); %Todos los puntos van a ser asociados.
               if(TrajStruct(imember).trajectories(ii).asociada)
               %Vamos a setear esto para sacarla de su grupo original.
                desasociar=true;
               end
   
              %SACO LAS TRAYECTORIAS QUE ESTAN EN LA LISTA.
                for kk=1:length(traj_asociadas)
                 ctraj=traj_asociadas(kk);
                 TrajStruct(imember).trajectories(ctraj).asociada=false;
                 TrajStruct(imember).trajectories(ctraj).idgroup=NaN;
                 index=trajid(imember,:,cgroup)==ctraj;
                 minlat(imember,index,cgroup)=NaN;
                 minlon(imember,index,cgroup)=NaN;
                 minanom(imember,index,cgroup)=NaN;
                 minlap(imember,index,cgroup)=NaN;
                 uvel(imember,index,cgroup)=NaN;
                 vvel(imember,index,cgroup)=NaN;
                 trajid(imember,index,cgroup)=NaN;
                end
                %NO RECALCULO LAS PROPIEDADES DEL GRUPO PORQUE ESO LO VOY A
                %HACER CUANDO AGREGUE LA NUEVA TRAYECTORIA.
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
        if( desasociar && nloop < MaxIterations)   %La trayectoria estaba asociada a otro grupo previamente. 
           %Desasocio la trayectoria ii de su grupo anterior.
           cgroup=TrajStruct(imember).trajectories(ii).idgroup;
           
           TrajStruct(imember).trajectories(ii).asociada=false; 
           TrajStruct(imember).trajectories(ii).idgroup=NaN; 
           
           index=trajid(imember,:,cgroup)==ii;

           %Elimino la trayectoria del grupo.
                minlat(imember,index,cgroup)=NaN;
                minlon(imember,index,cgroup)=NaN;
                minanom(imember,index,cgroup)=NaN;
                minlap(imember,index,cgroup)=NaN;
                uvel(imember,index,cgroup)=NaN;
                vvel(imember,index,cgroup)=NaN;
                trajid(imember,index,cgroup)=NaN;

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
          
          [nada index] = intersect(times,TrajStruct(imember).trajectories(ii).daten(indexasociar));
          TrajStruct(imember).trajectories(ii).asociada=true;
          TrajStruct(imember).trajectories(ii).idgroup=newgroup; %A que grupo asignamos la trajectoria.
          minlat(imember,index,newgroup)=TrajStruct(imember).trajectories(ii).minlatf(indexasociar);
          minlon(imember,index,newgroup)=TrajStruct(imember).trajectories(ii).minlonf(indexasociar);

          minanom(imember,index,newgroup)=TrajStruct(imember).trajectories(ii).minanomf(indexasociar);
          minlap(imember,index,newgroup)=TrajStruct(imember).trajectories(ii).minlap(indexasociar);
          uvel(imember,index,newgroup)=TrajStruct(imember).trajectories(ii).uvelf(indexasociar);
          vvel(imember,index,newgroup)=TrajStruct(imember).trajectories(ii).vvelf(indexasociar);
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
          
    
    
    %Si la trayectoria no fue asociada con ninguna otra entonces veo si
    %podria ser un buen candidato para generar un grupo nuevo.
    %Solo vamos a iniciar nuevos grupos a partir de las trayectorias
    %largas. No vamos a dejar que trajectorias cortitas inicien nuevos
    %grupos. Pero si las trajectorias cortas pueden asociarse a un grupo
    %que contenga al menos una trajectoria larga.
    
    if(~TrajStruct(imember).trajectories(ii).asociada && ...
       ~isempty(TrajStruct(imember).trajectories(ii).length) && ...
       TrajStruct(imember).trajectories(ii).length >= config.mintrajlength &&...
       formagrupo && nloop~=MaxIterations && ...
       TrajStruct(imember).trajectories(ii).iniciagrupo ) %Solo las trajectorias de al menos 3 dias inician grupos.
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
    minanom(imember,index,ngroup)=TrajStruct(imember).trajectories(ii).minanomf;
    minlap(imember,index,ngroup)=TrajStruct(imember).trajectories(ii).minlap;
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
    
    end %End del if sobre las condiciones para procesar la trayectoria.
    
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
    
    if( ~isempty(AnalysisTrajStruct(ii).length) && AnalysisTrajStruct(ii).proctraj && ...
         AnalysisTrajStruct(ii).length >= MinLengthAsoc )
    %HAGO UN LOOP SOBRE TODOS LOS GRUPOS Y MIDO QUE TAN FACTIBLE ES ASOCIAR LA TRAYECTORIA ACTUAL A CADA UNO.    
       cfd=NaN(1,ngroup);  %Distance cost function
       
       for jgroup=1:ngroup
         %Tengo que comparar la trajectoria ii del miembro imember con 
         %la media del grupo jgroup y calcular una funcion de costo. Luego
         %ver a que grupo deberia asociar la trajectoria.
         isnan_tr=~isnan(meanlon(:,jgroup));
         grouplon=meanlon(isnan_tr,jgroup);
         grouplat=meanlat(isnan_tr,jgroup);
         grouptime=times(isnan_tr);
         
         [cfd(jgroup)]=group_cost_function(grouplon,grouplat,grouptime,...
                                               AnalysisTrajStruct(ii).minlonf,AnalysisTrajStruct(ii).minlatf,...
                                               AnalysisTrajStruct(ii).daten,MaxDist); 

%        if(jgroup==4)
%            fprintf('Trayectoria %10f con un J de %10f\n',ii,cfd(jgroup));
%        end
       end  %En del for sobre los grupos para calcular la funcion de costo.
%     

       groups=1:ngroup;

       groups=groups(cfd <= tr_cost_function);
       cfd=cfd(cfd <= tr_cost_function);
       
       formagrupo=isempty(groups);     %Condicion para poder formar un grupo.
       
%            if(ii==17)
%                cfd
%                groups
%            end
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
          
          [nada index] = intersect(times,AnalysisTrajStruct(ii).daten);   %#ok<ASGLU>
          
          
               if( sum(~isnan(minlatanalysis(index,cgroup))) == 0)
               %EN ESTE CASO PUEDO ASOCIAR LA TRAYECTORIA ENTERA HAY LUGAR EN EL GRUPO.    
                asociar=true;
                newgroup=cgroup;
                indexasociar=1:length(AnalysisTrajStruct(ii).daten); %Todos los puntos van a ser asociados.
                if(AnalysisTrajStruct(ii).asociada)
                    desasociar=true;
                end
               end 
               
               if(~asociar && sum(~isnan(minlatanalysis(index,cgroup))) <=  MaxSuperposition )
                %HAY LUGAR PERO PARTE DE LA TRAYECTORIA SE SUPERPONE CON
                %OTRAS PREEXISTENTES. VOY A COMPRAR LA FUNCION DE COSTO DE
                %ESOS TRAMOS PARA LO QUE HAY EN EL GRUPO Y PARA LA NUEVA
                %TRAYECTORIA. SI LO QUE HABIA ES MEJOR QUE LA NUEVA
                %TRAYECTORIA, ROMPO LA NUEVA TRAYECTORIA Y SOLO ASIGNO LA
                %PARTE QUE NO SE SUPERPONE CON LO QUE HAY.
                tmpindex=false(size(minlatanalysis,1),1);
                tmpindex(index)=true;
                indexsuper=find(~isnan(minlatanalysis(:,cgroup)) & tmpindex);
                tmplon1=minlonanalysis(indexsuper,cgroup);
                tmplat1=minlatanalysis(indexsuper,cgroup);
                tmptime1=times(indexsuper);
               
                tmplong=meanlon(indexsuper,cgroup);
                tmplatg=meanlat(indexsuper,cgroup);
                tmptimeg=times(indexsuper);
                
                [nada indexsupertraj]=intersect(AnalysisTrajStruct(ii).daten,tmptime1);
                
                tmplon2=AnalysisTrajStruct(ii).minlonf(indexsupertraj);
                tmplat2=AnalysisTrajStruct(ii).minlatf(indexsupertraj);
                tmptime2=AnalysisTrajStruct(ii).daten(indexsupertraj);
                
                %ALGUNAS CONDICIONES EXTRAS PARA QUE ESTO SE PUEDA HACER.
                %VERIFICAMOS QUE LA NUEVA TRAYECTORIA NO CAIGA "EN EL
                %MEDIO" DE UNA YA ASOCIADA AL GRUPO.
                
                requisito1=false;
                requisito2=false;
                if( indexsuper(1)==1 || indexsuper(end)==size(minlatanalysis,1) || ...
                    trajidanalysis(indexsuper(1)-1,cgroup) ~= trajidanalysis(indexsuper(end)+ 1,cgroup))
                    requisito1=true;
                end
                if( indexsupertraj(1)==1 || indexsupertraj(end)==length(AnalysisTrajStruct(ii).daten))
                    requisito2=true;
                    %ACA PIDO QUE LA ZONA DE CONFLICTO NO QUEDE EN EL MEDIO
                    %DE LA NUEVA TRAYECTORIA QUE ESTOY QUERIENDO AGREGAR.
                end
                if( requisito1 && requisito2)
                  %SOLO SI SE CUMPLEN AMBOS REQUISITOS VOY A CALCULAR LAS FUNCIONES DE COSTO.  
                  [cfd1]=group_cost_function(tmplong,tmplatg,tmptimeg,...
                                             tmplon1,tmplat1,tmptime1,MaxDist);
                  [cfd2]=group_cost_function(tmplong,tmplatg,tmptimeg,...
                                             tmplon2,tmplat2,tmptime2,MaxDist);
                  if(cfd1 < cfd2)
                  %LO QUE HABIA ERA MEJOR SOLO ASOCIO LA PARTE DE LA
                  %TRAYECTORIA QUE NO SE SUPERPONE CON EL GRUPO.
                  tmpindex=false(size(minlatanalysis,1),1);
                  tmpindex(index)=true;
                  tmpindex=isnan(minlatanalysis(:,cgroup)) & tmpindex;
                  %obtengo los subindices de la trayectoria que no
                  %coinciden con lo que hay en el grupo.
                  [nada indexasociar]=intersect(AnalysisTrajStruct(ii).daten,times(tmpindex));
                  %fprintf('Corte una trayectoria del miembro %10f para asociarla al grupo %10f\n',imember,cgroup)
                
                  else
                  %LA NUEVA TRAYECTORIA ES MEJOR, ENTONCES LA ASOCIO TODA Y
                  %REMUEVO LO QUE HABIA ANTES.
                  indexasociar=1:length(AnalysisTrajStruct(ii).daten);
                  %REMUEVO EN FORMA PROLIJA LO QUE HABIA ANTES EN ESE
                  %LUGAR.
                  %Solo voy a desasociar aquellas trayectorias que estan
                  %totalmente superpuestas con la nueva trayectoria.
                   trajlist=unique(trajidanalysis(indexsuper,cgroup));
                   trajlist=trajlist(~isnan(trajlist));
                   
                   for kk=1:length(trajlist)
                   if( sum(trajidanalysis(~indexsuper,cgroup)==trajlist(kk))==0)
                      %La trayectoria trajlist(kk) no tiene puntos fuera de
                      %la superposicion, entonces tengo que desasociarla.
                      %No es necesario que la desasocie del grupo porque
                      %eso va a ocurrir automaticamente cuando asocie la
                      %nueva trayectoria, pero si la tengo que desasociar
                      %en la estructura de las trayectorias.
                      AnalysisTrajStruct(trajlist(kk)).asociada=false; 
                      AnalysisTrajStruct(trajlist(kk)).idgroup=NaN;     
                   end  
                   end
                  end
                  
                  asociar=true;  
                  newgroup=cgroup;
                  
                if( AnalysisTrajStruct(ii).asociada)
                    desasociar=true;
                end
                %SALGO DEL DO PORQUE YA ENCONTRE UN GRUPO AL QUE PUEDO
                %ASOCIAR LA TRAYECTORIA.
                break               
                end
               end
                
               if( ~asociar && nloop < MaxIterations)
               %LA TRAYECTORIA TODAVIA NO FUE ASOCIADA, ESO QUIERE DECIR
               %QUE O NO HABIA LUGAR O NO SE DIERON LAS CONDICIONES DE
               %SUPERPOSICION OPTIMAS. COMPITO CON LO QUE HAY DE FORMA DE
               %DECIDIR SI ME QUEDO CON LO QUE HAY O REEMPLAZO POR LA NUEVA
               %TRAYECTORIA.

               %Tengo que chequear cual/es es/son la/s trayectoria/s que esta/s asociada/s en
               %ese lugar y cual es la mejor opcion.
               %Calculo cuanto se superponen cada una de esas trayectorias con
               %el grupo y cuan cerca o lejos estan del origen.
               
               traj_asociadas=unique(trajidanalysis(index,cgroup));
               traj_asociadas=traj_asociadas(~isnan(traj_asociadas));
               tmp_cfd=NaN(1,length(traj_asociadas)) ; %Distance cost function

               isnan_tr=~isnan(meanlon(:,cgroup));
               tmp_grouplon=meanlon(isnan_tr,cgroup);
               tmp_grouplat=meanlat(isnan_tr,cgroup);
               tmp_grouptime=times(isnan_tr);

                for kk=1:length(traj_asociadas)
                ctraj=traj_asociadas(kk);
                %Calculo la funcion de costo entre las trayectorias
                %asociadas y la trayectoria actual.
                
                 [tmp_cfd(kk)]=group_cost_function(tmp_grouplon,tmp_grouplat,tmp_grouptime,...
                 AnalysisTrajStruct(ctraj).minlonf,AnalysisTrajStruct(ctraj).minlatf,...
                 AnalysisTrajStruct(ctraj).daten,MaxDist);
                end

           if(  cfd(jgroup)  <  min(tmp_cfd) )
               %LA TRAYECTORIA NUEVA ES MEJOR QUE LAS QUE HABIA ENTONCES LA
               %ASOCIO Y SACO LAS ANTERIORES.
               asociar=true;
               newgroup=cgroup;
               indexasociar=1:length(AnalysisTrajStruct(ii).daten); %Todos los puntos van a ser asociados.
               if(AnalysisTrajStruct(ii).asociada)
               %Vamos a setear esto para sacarla de su grupo original.
                desasociar=true;
               end
   
              %SACO LAS TRAYECTORIAS QUE ESTAN EN LA LISTA.
                for kk=1:length(traj_asociadas)
                 ctraj=traj_asociadas(kk);
                 AnalysisTrajStruct(ctraj).asociada=false;
                 AnalysisTrajStruct(ctraj).idgroup=NaN;
                 index=trajidanalysis(:,cgroup)==ctraj;
                 minlatanalysis(index,cgroup)=NaN;
                 minlonanalysis(index,cgroup)=NaN;
                 minanomanalysis(index,cgroup)=NaN;
                 minlapanalysis(index,cgroup)=NaN;
                 uvelanalysis(index,cgroup)=NaN;
                 vvelanalysis(index,cgroup)=NaN;
                 trajidanalysis(index,cgroup)=NaN;
                end
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
        if( desasociar && nloop < MaxIterations)   %La trayectoria estaba asociada a otro grupo previamente. 
           %Desasocio la trayectoria ii de su grupo anterior.
           cgroup=AnalysisTrajStruct(ii).idgroup;
           %if(ii==136)
           %    cgroup
           %end
           
           AnalysisTrajStruct(ii).asociada=false; 
           AnalysisTrajStruct(ii).idgroup=NaN; 
           index=trajidanalysis(:,cgroup)==ii;

           %Elimino la trayectoria ii del grupo al que estaba asociada.
                minlatanalysis(index,cgroup)=NaN;
                minlonanalysis(index,cgroup)=NaN;
                minanomanalysis(index,cgroup)=NaN;
                minlapanalysis(index,cgroup)=NaN;
                uvelanalysis(index,cgroup)=NaN;
                vvelanalysis(index,cgroup)=NaN;
                trajidanalysis(index,cgroup)=NaN;
                
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
          [nada index] = intersect(times,AnalysisTrajStruct(ii).daten(indexasociar));
          AnalysisTrajStruct(ii).asociada=true;
          AnalysisTrajStruct(ii).idgroup=newgroup; %A que grupo asignamos la trajectoria.
          minlatanalysis(index,newgroup)=AnalysisTrajStruct(ii).minlatf(indexasociar);
          minlonanalysis(index,newgroup)=AnalysisTrajStruct(ii).minlonf(indexasociar);
          minanomanalysis(index,newgroup)=AnalysisTrajStruct(ii).minanomf(indexasociar);
          minlapanalysis(index,newgroup)=AnalysisTrajStruct(ii).minlap(indexasociar);
          uvelanalysis(index,newgroup)=AnalysisTrajStruct(ii).uvelf(indexasociar);
          vvelanalysis(index,newgroup)=AnalysisTrajStruct(ii).vvelf(indexasociar);
          trajidanalysis(index,newgroup)=ii;
          
          
          %Modifico la media del grupo, en aqullos puntos donde la media es
          %NaN y la trayectoria del analysis no es NaN, hago que la
          %trayectoria del analysis sea la media del grupo.
          tmpind=isnan(meanlat(:,newgroup));
          meanlat(tmpind,newgroup)=minlatanalysis(tmpind,newgroup);
          meanlon(tmpind,newgroup)=minlonanalysis(tmpind,newgroup);
          

          end  %END DEL IF SOBRE SI ASOCIO LA TRAYECTORIA A SU NUEVO GRUPO.
        
      
       
    %SI LA TRAYECTORIA NO PUDO SER ASOCIADA A NINGUNO DE LOS GRUPOS
    %POSIBLES ENTONCES VEMOS SI TIENE LAS CARACTERISTICAS PARA INICIAR UN
    %NUEVO GRUPO.
    
    if(~AnalysisTrajStruct(ii).asociada && ~isempty(AnalysisTrajStruct(ii).length) && ...
       AnalysisTrajStruct(ii).length >= config.mintrajlength && ...
       formagrupo && nloop~=MaxIterations && ...
       AnalysisTrajStruct(ii).iniciagrupo ) %Solo las trajectorias de al menos 3 dias inician grupos.
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
    minlapanalysis(index,ngroup)=AnalysisTrajStruct(ii).minlap;
    minanomanalysis(index,ngroup)=AnalysisTrajStruct(ii).minanomf;
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
    
    end  %End del if sobre las condiciones para procesar la trayectoria.
end   %End del loop sobre las trajectorias del analysis


if(CheckGroupMerge)
%==========================================================================
% CHEQUEO GRUPOS CERCANOS.
%==========================================================================
% SI 2 GRUPOS ESTAN CERCANOS, TOMO EL MAS CHICO Y LO ELIMINO PARA QUE EL
% CONFLICTO NO CONTINUE EN EL FUTURO, TODAS LAS TRAYECTORIAS QUE COMPONEN
% EL GRUPO NO PUEDEN FORMAR NUEVOS GRUPOS.


for ii=1:ngroup


    %Para cada grupo ii, verifico si esta cerca del grupo jj. Si lo esta
    %entonces coloco el grupo ii, en el grupo jj y recalculo sus
    %propiedades. Para determinar si un grupo es cercano a otro uso
    %criterios similares a los que aplique en las trayectorias. 
       cfd=NaN(1,ngroup);  %Distance cost function
       for jj=1:ngroup
           if(jj~=ii)
           
         if(sum(~isnan(meanlon(:,jj))) > 0) %Verificamos que el grupo no haya sido vaciado.  
         %Comparo la distancia entre la media de ambos grupos.
         
         isnan_tr=~isnan(meanlon(:,jj));
         grouplon=meanlon(isnan_tr,jj);
         grouplat=meanlat(isnan_tr,jj);
         grouptime=times(isnan_tr);
         
         isnan_tr=~isnan(meanlon(:,ii));
         grouplon2=meanlon(isnan_tr,ii);
         grouplat2=meanlat(isnan_tr,ii);
         grouptime2=times(isnan_tr);
         

         
         [cfd(jj)]=group_cost_function(grouplon,grouplat,grouptime,...
                                       grouplon2,grouplat2,grouptime2,MaxDist);
                                   
         end
         
           end
       end  %En del for sobre los grupos para calcular la funcion de costo.
       cfd( cfd >=  tr_cost_function )=NaN;  
       
       %Veo cual es el grupo que esta mas cerca y que me puede traer
       %problemas.
       [min_cfd min_group]=nanmin(cfd);

       
       
       
       if(~isnan(min_cfd)) %Quiere decir que hay 2 grupos que estan cerca en espacio
                           %y que coinciden en tiempo. Sin importar
                           %demasiado la distancia a los origenes entre
                           %ambos veo si los puedo superponer.
                           
        %Calculo el tamanio de ii y de jj.
        sizeii=sum(sum(~isnan(minlat(:,:,ii))));
        sizejj=sum(sum(~isnan(minlat(:,:,min_group))));
        
        if( sizeii > sizejj)
            geliminar=min_group;
            fprintf('Voy a eliminar el grupo %10f por un conflicto con el grupo %10f \n',geliminar,ii);
        else
            geliminar=ii;
            fprintf('Voy a eliminar el grupo %10f por un conflicto con el grupo %10f \n',geliminar,min_group);
        end
        
        
        %Ahora tengo que cambiar el numero de grupo de asociacion a todas
        %las trayectorias asociadas al grupo 1.
          for kk=1:enssize
             tmp=trajid(kk,:,geliminar);
             tmp=tmp(~isnan(tmp));
             if(~isempty(tmp))
                 tmp=unique(tmp); %Busco los valores unicos en tmp.
                 
                 for kkk=1:length(tmp)
                 %Para cada valor de tmp busco esa trayectoria y le cambio el id del grupo al que esta asociada.
                   %TrajStruct(kk).trajectories(tmp(kkk)).idgroup=min_group; 
                   TrajStruct(kk).trajectories(tmp(kkk)).idgroup=NaN;
                   TrajStruct(kk).trajectories(tmp(kkk)).asociada=false;
                   TrajStruct(kk).trajectories(tmp(kkk)).iniciagrupo=false;
                 end
             end
          end
        %Tengo que repetir el mismo procedimiento para las trayectorias
        %asociadas del analysis.
             tmp=trajidanalysis(:,geliminar);
             tmp=tmp(~isnan(tmp));
             if(~isempty(tmp))
                 tmp=unique(tmp); %Busco los valores unicos en tmp.
                 
                 for kkk=1:length(tmp)
                 %Para cada valor de tmp busco esa trayectoria y le cambio el id del grupo al que esta asociada.
                   %AnalysisTrajStruct(tmp(kkk)).idgroup=min_group; 
                   AnalysisTrajStruct(tmp(kkk)).idgroup=NaN;
                   AnalysisTrajStruct(tmp(kkk)).asociada=false;
                   AnalysisTrajStruct(tmp(kkk)).iniciagrupo=false;
                 end
             end
             
        %Vacio minlat y minlon del grupo ii para reconocer que fue
        %eliminado. Ademas con esto ya no puede recibir nuevas
        %trayectorias porque la funcion de costo va a dar siempre el valor
        %por defecto.
          minlat(:,:,geliminar)=NaN;
          minlon(:,:,geliminar)=NaN;
          meanlat(:,geliminar)=NaN;
          meanlon(:,geliminar)=NaN;
          minlatanalysis(:,geliminar)=NaN;
          minlonanalysis(:,geliminar)=NaN;
          minanom(:,:,geliminar)=NaN;
          minlap(:,:,geliminar)=NaN;
          minanomanalysis(:,geliminar)=NaN;
          minlapanalysis(:,geliminar)=NaN;
          uvel(:,:,geliminar)=NaN;
          vvel(:,:,geliminar)=NaN;
          uvelanalysis(:,geliminar)=NaN;
          vvelanalysis(:,geliminar)=NaN;
          trajid(:,:,geliminar)=NaN;
          trajidanalysis(:,geliminar)=NaN;

        end
       %end
   
       
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
minanom(:,:,contador)=minanom(:,:,ii);
minlap(:,:,contador)=minlap(:,:,ii);
uvel(:,:,contador)=uvel(:,:,ii);
vvel(:,:,contador)=vvel(:,:,ii);
meanlat(:,contador)=meanlat(:,ii);
meanlon(:,contador)=meanlon(:,ii);
trajid(:,:,contador)=trajid(:,:,ii);
nmember(contador)=nmember(ii);
meanlength(contador)=meanlength(ii);

minlatanalysis(:,contador)=minlatanalysis(:,ii);
minlonanalysis(:,contador)=minlonanalysis(:,ii);
minlapanalysis(:,contador)=minlapanalysis(:,ii);
minanomanalysis(:,contador)=minanomanalysis(:,ii);
uvelanalysis(:,contador)=uvelanalysis(:,ii);
vvelanalysis(:,contador)=vvelanalysis(:,ii);
trajidanalysis(:,contador)=trajidanalysis(:,ii);
    

   
 end
 end

minlat(:,:,contador+1:end)=[];
minlon(:,:,contador+1:end)=[];
minanom(:,:,contador+1:end)=[];
minlap(:,:,contador+1:end)=[];
uvel(:,:,contador+1:end)=[];
vvel(:,:,contador+1:end)=[];
meanlat(:,contador+1:end)=[];
meanlon(:,contador+1:end)=[];
trajid(:,:,contador+1:end)=[];
nmember(contador+1:end)=[];
meanlength(contador+1:end)=[];

minlatanalysis(:,contador+1:end)=[];
minlonanalysis(:,contador+1:end)=[];
minlapanalysis(:,contador+1:end)=[];
minanomanalysis(:,contador+1:end)=[];
uvelanalysis(:,contador+1:end)=[];
vvelanalysis(:,contador+1:end)=[];
trajidanalysis(:,contador+1:end)=[]; 
       
ngroup=contador;

tiempo=toc;
fprintf('TIEMPO EMPLEADO EN ASIGNAR LAS PEQUENIAS TRAYECTORIAS %f\n',tiempo);


%minlap(minlap==0)=NaN;
%minlapanalysis(minlapanalysis==0)=NaN;

%ASIGNO LAS DIFERENTES MATRICES A LA ESTRUCTURA GROUP.

group.ngroup=ngroup;       
group.minlat=minlat;
group.minlon=minlon;
group.minanom=minanom;
group.minlap=minlap;
group.uvel=uvel;
group.vvel=vvel;
group.meanlat=meanlat;
group.meanlon=meanlon;
group.trajid=trajid;
group.nmember=nmember;
group.meanlength=meanlength;

group.minlatanalysis=minlatanalysis;
group.minlonanalysis=minlonanalysis;
group.minlapanalysis=minlapanalysis;
group.minanomanalysis=minanomanalysis;
group.uvelanalysis=uvelanalysis;
group.vvelanalysis=vvelanalysis;
group.trajidanalysis=trajidanalysis;



%LISTO EL LLOPO, PELADA LA GALLINA.






