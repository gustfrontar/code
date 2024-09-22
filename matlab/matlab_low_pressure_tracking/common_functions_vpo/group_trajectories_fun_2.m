function [group TrajStruct AnalysisTrajStruct]=group_trajectories_fun_2(TrajStruct,AnalysisTrajStruct,config)

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
MaxDist=800e3;
MaxGroup=500;          %Maximo numero de grupos que vamos a allocatear de entrada.

CheckGroupMerge=true;
%obtengo el numero de miembros en el ensamble.
enssize=size(TrajStruct,2);

MaxIterations=4;           %Cuantas pasadas del algoritmo para definir los grupos.
MaxSuperposition=29;       %Maxima superposicion temporal permitida entre 2 trayectorias del mismo miembro que van a pertenecer al mismo grupo.
MinLength=2;               %No asociamos fragmentos de trayectorias mas cortos que este umbral.  
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
%     %DEGUG DEBUG
%     if( max(TrajStruct(imember).trajectories(ii).minlatf) > -20 | min(TrajStruct(imember).trajectories(ii).minlonf) < 100)
%     TrajStruct(imember).trajectories(ii).proctraj=false;
%     TrajStruct(imember).trajectories(ii).iniciagrupo=false;
%     end
%     %DEBUG DEBUG
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
minarea=NaN(enssize,ntimes,MaxGroup);
meananom=NaN(enssize,ntimes,MaxGroup);
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
minareaanalysis=NaN(ntimes,MaxGroup);
meananomanalysis=NaN(ntimes,MaxGroup);
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
      MinLengthAsoc=3;
  end
  
  
%==========================================================================
% ASOCIO ENTRE SI LAS TRAYECTORIAS DE LOS PRONOSTICOS
%==========================================================================

for imember=1:enssize
ntraj=size(TrajStruct(imember).trajectories,2);  %Numero de trayectorias en el miembro correspondiente.

for ii=1:ntraj
    
    if(nloop==1)
    TrajStruct(imember).trajectories(ii).asociada=false;  %Indica si la trajectoria fue o no asociada, se inicializa como false.
    TrajStruct(imember).trajectories(ii).idgroup=[];
    end

    if( ~isempty(TrajStruct(imember).trajectories(ii).length) && TrajStruct(imember).trajectories(ii).proctraj &&  ...
        TrajStruct(imember).trajectories(ii).length >= MinLengthAsoc )
    %HAGO UN LOOP SOBRE TODOS LOS GRUPOS Y MIDO QUE TAN FACTIBLE ES ASOCIAR LA TRAYECTORIA ACTUAL A CADA UNO.    
       cfd=NaN(1,ngroup);  %Distance cost function
       inii=NaN(1,ngroup);

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
         
         
         [cfd(jgroup) inii(jgroup)]=group_cost_function(grouplon,grouplat,grouptime,...
                                              TrajStruct(imember).trajectories(ii).minlonf,TrajStruct(imember).trajectories(ii).minlatf,...
                                              TrajStruct(imember).trajectories(ii).daten,MaxDist);
%           if( jgroup == 3 && imember == 8)
%           fprintf('Trayectoria %10f,  con un J de %10f con un inii de %10f \n',ii,cfd(jgroup),inii(jgroup))   
%           end
       end  %En del for sobre los grupos para calcular la funcion de costo.
%     


       
       groups=1:ngroup;
       
       indexg= cfd <= tr_cost_function*1.5;
       groups=groups(indexg);
       cfd=cfd(indexg);
       inii=inii(indexg);
      
       
       formagrupo=isempty(groups);     %La trayectoria va a poder formar grupos si esta muy lejos de cualquier grupo.

       %Ordenamos las funciones de costo y los grupos de menor a mayor.
       [cfd isort]=sort(cfd);
       groups=groups(isort);
       inii=inii(isort);
       
      
%          if( imember == 13 && ii==22)
%             cfd
%             groups
%             TrajStruct(imember).trajectories(ii).asociada
%             TrajStruct(imember).trajectories(ii).idgroup
%          end
       

          %VEMOS SI HAY ALGUN GRUPO DEL CUAL HAY QUE DESASOCIAR LA
          %TRAYECTORIA.
          if( TrajStruct(imember).trajectories(ii).asociada)
             for kk=1:length(TrajStruct(imember).trajectories(ii).idgroup)
                kgroup=TrajStruct(imember).trajectories(ii).idgroup(kk);
                if(~any(groups==kgroup))
                    %Si kgroup no figura entre las nuevas asociaciones...
                    desasociarlist=[desasociarlist kgroup];
                end 
             end
          end
       
       %Acciones por defecto.
       desasociarlist=[];  %Lista de grupos que vamos a desasociar.
       asociarlist=[];     %Lista de grupos que vamos a asociar.
       indexasoc=cell(1);

           
          
          if(~isempty(cfd)) %QUIERE DECIR QUE HAY GRUPOS POSIBLES PARA ASOCIAR.
          for jgroup=1:length(groups)  %VAMOS A CONSIDERAR TODOS LOS GRUPOS POSIBLES.
          cgroup=groups(jgroup);
          ini=inii(jgroup);

              %ANALIZO LOS QUE NO SON EL GRUPO QUE YA TENIA ANTES.
              
          if( any(TrajStruct(imember).trajectories(ii).idgroup==cgroup) )
          %SI LA TRAYECTORIA ESTABA ASOCIADA A ESTE GRUPO ENTONCES LA DESASOCIO
          %Y VEO NUEVAMENTE SI LA PUEDO ASOCIAR O NO. ESTO ES BASICAMENTE
          %POR DOS MOTIVOS. 
          %A) PUEDE SER QUE LAS CONDICIONES DE ASOCIACION HAYAN CAMBIANDO Y
          %AHORA PUEDA ASOCIAR UNA PARTE MAYOR DE LA TRAYECTORIA AL GRUPO.
          %B) ES MAS ROBUSTO ESTIMAR LA PERTENENCIA DE LA TRAYECTORIA AL
          %GRUPO CUANDO LA MISMA TRAYECTORIA NO ES PARTE DEL GRUPO.
           
           %Desasocio la trayectoria ii de su grupo anterior.
           index=TrajStruct(imember).trajectories(ii).idgroup==cgroup;
           TrajStruct(imember).trajectories(ii).idgroup(index)=[];
           if(isempty(TrajStruct(imember).trajectories(ii).idgroup))
             TrajStruct(imember).trajectories(ii).asociada=false;
           end
           dindex=trajid(imember,:,cgroup)==ii;

           %Elimino la trayectoria del grupo.
                minlat(imember,dindex,cgroup)=NaN;
                minlon(imember,dindex,cgroup)=NaN;
                minanom(imember,dindex,cgroup)=NaN;
                minlap(imember,dindex,cgroup)=NaN;
                uvel(imember,dindex,cgroup)=NaN;
                vvel(imember,dindex,cgroup)=NaN;
                trajid(imember,dindex,cgroup)=NaN;
                minarea(imember,dindex,cgroup)=NaN;
                meananom(imember,dindex,cgroup)=NaN;

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
          end   %END DEL IF SOBRE SI TENGO QUE DESASOCIAR LA TRAYECTORIA DE ALGUN GRUPO   
                  
                  
                  
                 
              %SOLO BUSCO ASOCIAR LA TRAYECTORIA A UN GRUPO SI ES QUE LA TRAYECTORIA NO ESTABA ASOCIADA A ESTE GRUPO.    
              [nada index] = intersect(times,TrajStruct(imember).trajectories(ii).daten(ini:end));   %#ok<ASGLU>
              asoccurrentgroup=false;
               if( sum(~isnan(minlat(imember,index,cgroup))) == 0 && cfd(jgroup) < tr_cost_function )
               %EN ESTE CASO PUEDO ASOCIAR LA TRAYECTORIA ENTERA HAY LUGAR
               %EN EL GRUPO.  
                if(length(TrajStruct(imember).trajectories(ii).daten)-ini+1 > MinLength)
                asociarlist=[asociarlist cgroup]; %Agrego el grupo a la lista de asociacion.
                indexasoc{length(asociarlist)}=ini:length(TrajStruct(imember).trajectories(ii).daten); %Todos los puntos indicados por la 
                                                                                                       %cost function van a ser asociados.
                end
                                                                                                       

               elseif(sum(~isnan(minlat(imember,index,cgroup))) <=  MaxSuperposition && sum(~isnan(minlat(imember,index,cgroup))) > 0)
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

                
                
                %Solo comparo la parte que la funcion de costo me permite
                %asociar a este grupo. 
                [nada indexsupertraj]=intersect(TrajStruct(imember).trajectories(ii).daten,tmptime1);
                indexsupertraj(indexsupertraj < ini)=[];
                
                tmplon2=TrajStruct(imember).trajectories(ii).minlonf(indexsupertraj);
                tmplat2=TrajStruct(imember).trajectories(ii).minlatf(indexsupertraj);
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
                if( indexsupertraj(1)==ini || indexsupertraj(end)==length(TrajStruct(imember).trajectories(ii).daten))
                    requisito2=true;
                    %ACA PIDO QUE LA ZONA DE CONFLICTO NO QUEDE EN EL MEDIO
                    %DE LA NUEVA TRAYECTORIA QUE ESTOY QUERIENDO AGREGAR.
                end
                
                
                
                if( requisito1 )
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
                  [nada iasoc]=intersect(TrajStruct(imember).trajectories(ii).daten,times(tmpindex));
                  %fprintf('Corte una trayectoria del miembro %10f para asociarla al grupo %10f\n',imember,cgroup)
                  %Verifico ademas si la parte de la trayectoria que voy a
                  %asignar al grupo cumple con la funcion de costo.
                  iasoc(iasoc < ini)=[];
                  

                  
                  if(~isempty(iasoc) && length(iasoc) > MinLength && requisito2 )
                  %En este caso solo asocio la trayectoria si es que la region de 
                  %conflicto entre la trayectoria y el grupo no queda en el
                  %medio de las dos.
                    tmplon1=TrajStruct(imember).trajectories(ii).minlonf(iasoc);
                    tmplat1=TrajStruct(imember).trajectories(ii).minlatf(iasoc);
                    tmptime1=TrajStruct(imember).trajectories(ii).daten(iasoc);
                    tmplong=meanlon(tmpindex,cgroup);
                    tmplatg=meanlat(tmpindex,cgroup);
                    tmptimeg=times(tmpindex);
                    [tmpcdf]=group_cost_function(tmplong,tmplatg,tmptimeg,...
                                             tmplon1,tmplat1,tmptime1,MaxDist);                     

                                         
                                         
                   if(tmpcdf <  tr_cost_function)
                    asociarlist=[asociarlist cgroup]; %#ok<AGROW> %Agrego el grupo a la lista de asociacion.
                    indexasoc{length(asociarlist)}=iasoc; %#ok<AGROW> %Todos los puntos van a ser asociados.
                  
                   end
                  end

                  elseif(cfd(jgroup) < tr_cost_function)
                  %LA NUEVA TRAYECTORIA ES MEJOR, ENTONCES ASOCIO TODO LO
                  %QUE LA COST FUNCTION ME PERMITE ASOCIAR
                  %En este caso voy a asociar toda la trayectoria, asi que no me 
                  %importa si la zona de conflicto entre el grupo y la
                  %trayectoria esta en el medio de la nueva trayectoria
                  asociarlist=[asociarlist cgroup]; %Agrego el grupo a la lista de asociacion.
                  indexasoc{length(asociarlist)}=ini:length(TrajStruct(imember).trajectories(ii).daten); %Todos los puntos van a ser asociados.
                  
                  %REMUEVO EN FORMA PROLIJA LO QUE HABIA ANTES EN ESE
                  %LUGAR.
                  %Desasocio totalmente aquellas trayectorias a las que
                  %solo le queden menos de 3 puntos fuera de la region que
                  %va a ser reemplazada por la nueva trayectoria.
                   trajlist=unique(trajid(imember,indexsuper,cgroup));
                   trajlist=trajlist(~isnan(trajlist));
  
                   for kk=1:length(trajlist)
                     itraj=trajlist(kk);
                     if( sum(trajid(imember,~indexsuper,cgroup)==itraj) < MinLength)
                      %La trayectoria trajlist(kk) no tiene puntos fuera de
                      %la superposicion, entonces tengo que desasociarla.
                      %No es necesario que la desasocie del grupo porque
                      %eso va a ocurrir automaticamente cuando asocie la
                      %nueva trayectoria, pero si la tengo que desasociar
                      %en la estructura de las trayectorias.
                      %TrajStruct(imember).trajectories(itraj).asociada=false; 
                      index=TrajStruct(imember).trajectories(itraj).idgroup==cgroup;
                      TrajStruct(imember).trajectories(itraj).idgroup(index)=[];
                      if(isempty(TrajStruct(imember).trajectories(itraj).idgroup))
                          TrajStruct(imember).trajectories(itraj).asociada=false;
                      end
                     end  
                   end
                  end 
                end
               end
              end 
          end
          
        
       %TERMINE DE MIRAR TODOS LOS GRUPOS POSIBLES, QUEDAN DOS COSAS POR
       %HACER ASOCIAR LA TRAYECTORIA A SU NUEVO GRUPO (SI FUERA EL CASO) Y
       %DESASOCIARLA DEL ANTERIOR (TAMBIEN SI FUERA EL CASO).
           
        
        %==================================================================
        %DESASOCIO LA TRAYECTORIA DE LOS GRUPOS CON LOS QUE YA NO SE
        %RELACIONA.
        %==================================================================
        if( ~isempty(desasociarlist) && nloop < MaxIterations)   %La trayectoria estaba asociada a otro grupo previamente. 
           %Desasocio la trayectoria ii de su grupo anterior.
           for kk=1:length(desasociarlist)
           dgroup=desasociarlist(kk);
          
           index=TrajStruct(imember).trajectories(ii).idgroup==dgroup;
           TrajStruct(imember).trajectories(ii).idgroup(index)=[];
           if(isempty(TrajStruct(imember).trajectories(ii).idgroup))
             TrajStruct(imember).trajectories(ii).asociada=false;
           end
           dindex=trajid(imember,:,dgroup)==ii;

           %Elimino la trayectoria del grupo.
                minlat(imember,dindex,dgroup)=NaN;
                minlon(imember,dindex,dgroup)=NaN;
                minanom(imember,dindex,dgroup)=NaN;
                minlap(imember,dindex,dgroup)=NaN;
                uvel(imember,dindex,dgroup)=NaN;
                vvel(imember,dindex,dgroup)=NaN;
                trajid(imember,dindex,dgroup)=NaN;
                minarea(imember,dindex,dgroup)=NaN;
                meananom(imember,dindex,dgroup)=NaN;

               %Recalculo la media del grupo (para el caso en el que el analysis
               %defina totalmente el grupo, en ese caso el grupo deberia
               %desapraecer.
               meanlat(:,dgroup)=nanmean(minlat(:,:,dgroup),1);
               for kt=1:ntimes %Cuidado especial hay que tener siempre que se promedian longitudes.
               meanlon(kt,dgroup)=mean_lon_fun(minlon(:,kt,dgroup));
               end
               nmember(dgroup)=sum(any(~isnan(minlat(:,:,dgroup)),2));
               aux=sum(~isnan(minlat(:,:,dgroup)),2);
               aux(aux==0)=NaN;        
               meanlength(dgroup)=nanmean(aux);
               
           end
               
        end   %END DEL IF SOBRE SI TENGO QUE DESASOCIAR LA TRAYECTORIA DE ALGUN GRUPO
        
       if( ~isempty(asociarlist))

          %==================================================================
          %ASOCIO LA TRAYECTORIA A UN NUEVO GRUPO SI ES LO QUE CORRESPONDE.
          %==================================================================
          %ASIGNO LA TRAYECTORIA AL GRUPO REQUERIDO.
          if(Debug)
          fprintf('Asocio la trayectoria del %f al grupo %f con un cost function de %f \n',ii,newgroup,min(cfd));
          end
          for kk=1:length(asociarlist)
          newgroup=asociarlist(kk);
          asocindex=indexasoc{kk};
          
              
          [nada index] = intersect(times,TrajStruct(imember).trajectories(ii).daten(asocindex));
          TrajStruct(imember).trajectories(ii).asociada=true;
          id=TrajStruct(imember).trajectories(ii).idgroup;
          TrajStruct(imember).trajectories(ii).idgroup=[id newgroup]; %Actualizamos la lista de grupos.
          minlat(imember,index,newgroup)=TrajStruct(imember).trajectories(ii).minlatf(asocindex);
          minlon(imember,index,newgroup)=TrajStruct(imember).trajectories(ii).minlonf(asocindex);

          minanom(imember,index,newgroup)=TrajStruct(imember).trajectories(ii).minanomf(asocindex);
          minlap(imember,index,newgroup)=TrajStruct(imember).trajectories(ii).minlap(asocindex);
          uvel(imember,index,newgroup)=TrajStruct(imember).trajectories(ii).uvelf(asocindex);
          vvel(imember,index,newgroup)=TrajStruct(imember).trajectories(ii).vvelf(asocindex);
          minarea(imember,index,newgroup)=TrajStruct(imember).trajectories(ii).minarea(asocindex);
          meananom(imember,index,newgroup)=TrajStruct(imember).trajectories(ii).meananom(asocindex);

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
          
          end
          
        end  %END DEL IF SOBRE SI ASOCIO LA TRAYECTORIA A NUEVOS GRUPOS.
          
    
    
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
    minarea(imember,index,ngroup)=TrajStruct(imember).trajectories(ii).minarea;
    meananom(imember,index,ngroup)=TrajStruct(imember).trajectories(ii).meananom;
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
    AnalysisTrajStruct(ii).idgroup=[];
    end
    
    %if(ii==85)
    %AnalysisTrajStruct(ii).idgroup
    %AnalysisTrajStruct(ii).asociada
    %end
    
    if( ~isempty(AnalysisTrajStruct(ii).length) && AnalysisTrajStruct(ii).proctraj && ...
         AnalysisTrajStruct(ii).length >= MinLengthAsoc )
    %HAGO UN LOOP SOBRE TODOS LOS GRUPOS Y MIDO QUE TAN FACTIBLE ES ASOCIAR LA TRAYECTORIA ACTUAL A CADA UNO.    
       cfd=NaN(1,ngroup);  %Distance cost function

    
       for jgroup=1:ngroup
           
           %Voy a comparar la trayectoria del analysis con cada miembro del
           %nesamble.
         %Tengo que comparar la trajectoria ii del miembro imember con 
         %la media del grupo jgroup y calcular una funcion de costo. Luego
         %ver a que grupo deberia asociar la trajectoria.
         isnan_tr=~isnan(meanlon(:,jgroup));
         grouplon=meanlon(isnan_tr,jgroup);
         grouplat=meanlat(isnan_tr,jgroup);
         grouptime=times(isnan_tr);
         
         [cfd(jgroup) inii(jgroup)]=group_cost_function(grouplon,grouplat,grouptime,...
                                               AnalysisTrajStruct(ii).minlonf,AnalysisTrajStruct(ii).minlatf,...
                                               AnalysisTrajStruct(ii).daten,MaxDist);
                                           
        end  %En del for sobre los grupos para calcular la funcion de costo.
%     
           
                 
%           if( jgroup == 17)
%           fprintf('Trayectoria %10f,  con un J de %10f \n',ii,cfd(jgroup))   
%          end
       groups=1:ngroup;
       
       indexg= cfd <= tr_cost_function*1.5;
       groups=groups(indexg);
       cfd=cfd(indexg);
       inii=inii(indexg);
      
       
       formagrupo=isempty(groups);     %La trayectoria va a poder formar grupos si esta muy lejos de cualquier grupo.

       %Ordenamos las funciones de costo y los grupos de menor a mayor.
       [cfd isort]=sort(cfd);
       groups=groups(isort);
       inii=inii(isort);
        
       
       %Acciones por defecto.
       desasociarlist=[];  %Lista de grupos que vamos a desasociar.
       asociarlist=[];     %Lista de grupos que vamos a asociar.
       indexasoc=cell(1);

           
          %VEMOS SI HAY ALGUN GRUPO DEL CUAL HAY QUE DESASOCIAR LA
          %TRAYECTORIA.
          if( AnalysisTrajStruct(ii).asociada)
             for kk=1:length(AnalysisTrajStruct(ii).idgroup)
                kgroup=AnalysisTrajStruct(ii).idgroup(kk);
                if(~any(groups==kgroup))
                    %Si kgroup no figura entre las nuevas asociaciones...
                    desasociarlist=[desasociarlist kgroup];
                end 
             end
          end
          
          if(~isempty(cfd)) %QUIERE DECIR QUE HAY GRUPOS POSIBLES PARA ASOCIAR.
          for jgroup=1:length(groups)  %VAMOS A CONSIDERAR TODOS LOS GRUPOS POSIBLES.
          cgroup=groups(jgroup);
          ini=inii(jgroup);              %ANALIZO LOS QUE NO SON EL GRUPO QUE YA TENIA ANTES.
              
          if( any(AnalysisTrajStruct(ii).idgroup==cgroup) )
          %SI LA TRAYECTORIA ESTABA ASOCIADA A ESTE GRUPO ENTONCES LA DESASOCIO
          %Y VEO NUEVAMENTE SI LA PUEDO ASOCIAR O NO. ESTO ES BASICAMENTE
          %POR DOS MOTIVOS. 
          %A) PUEDE SER QUE LAS CONDICIONES DE ASOCIACION HAYAN CAMBIANDO Y
          %AHORA PUEDA ASOCIAR UNA PARTE MAYOR DE LA TRAYECTORIA AL GRUPO.
          %B) ES MAS ROBUSTO ESTIMAR LA PERTENENCIA DE LA TRAYECTORIA AL
          %GRUPO CUANDO LA MISMA TRAYECTORIA NO ES PARTE DEL GRUPO.
           
           %Desasocio la trayectoria ii de su grupo anterior.
           index=AnalysisTrajStruct(ii).idgroup==cgroup;
           AnalysisTrajStruct(ii).idgroup(index)=[];
           if(isempty(AnalysisTrajStruct(ii).idgroup))
             AnalysisTrajStruct(ii).asociada=false;
           end
           dindex=trajidanalysis(:,cgroup)==ii;

           %Elimino la trayectoria del grupo.
                minlatanalysis(dindex,cgroup)=NaN;
                minlonanalysis(dindex,cgroup)=NaN;
                minanomanalysis(dindex,cgroup)=NaN;
                minlapanalysis(dindex,cgroup)=NaN;
                uvelanalysis(dindex,cgroup)=NaN;
                vvelanalysis(dindex,cgroup)=NaN;
                trajidanalysis(dindex,cgroup)=NaN;
                minareaanalysis(dindex,cgroup)=NaN;
                meananomanalysis(dindex,cgroup)=NaN;

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
          end   %END DEL IF SOBRE SI TENGO QUE DESASOCIAR LA TRAYECTORIA DE ALGUN GRUPO 
          
          %AHORA CON LA TRAYECTORIA DESASOCIADA DEL GRUPO EN CUESTION
          %REANALIZO LA SITUACION PARA VER SI HAY ALGUN CAMBIO EN LA FORMA
          %DE ASOCIACION.
                 
              %SOLO BUSCO ASOCIAR LA TRAYECTORIA A UN GRUPO SI ES QUE LA TRAYECTORIA NO ESTABA ASOCIADA A ESTE GRUPO.    
              [nada index] = intersect(times,AnalysisTrajStruct(ii).daten(ini:end));   %#ok<ASGLU>

               if( sum(~isnan(minlatanalysis(index,cgroup))) == 0 && cfd(jgroup) < tr_cost_function )
               %EN ESTE CASO PUEDO ASOCIAR LA TRAYECTORIA ENTERA HAY LUGAR
               %EN EL GRUPO. 
               
                if(length(AnalysisTrajStruct(ii).daten)-ini+1 > MinLength)
                    
                asociarlist=[asociarlist cgroup]; %Agrego el grupo a la lista de asociacion.
                indexasoc{length(asociarlist)}=ini:length(AnalysisTrajStruct(ii).daten); %Todos los puntos indicados por la 
                                                                                         %cost function van a ser asociados.
                end
                                                                                                       

               elseif(sum(~isnan(minlatanalysis(index,cgroup))) <=  MaxSuperposition && sum(~isnan(minlatanalysis(index,cgroup))) > 0)
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
                
                %Solo comparo la parte que la funcion de costo me permite
                %asociar a este grupo. 
                [nada indexsupertraj]=intersect(AnalysisTrajStruct(ii).daten,tmptime1);
                indexsupertraj(indexsupertraj < ini)=[];
                
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
                if( indexsupertraj(1)==ini || indexsupertraj(end)==length(AnalysisTrajStruct(ii).daten))
                    requisito2=true;
                    %ACA PIDO QUE LA ZONA DE CONFLICTO NO QUEDE EN EL MEDIO
                    %DE LA NUEVA TRAYECTORIA QUE ESTOY QUERIENDO AGREGAR.
                end
                
                
                if( requisito1 )
                  %SOLO SI SE CUMPLEN AMBOS REQUISITOS VOY A CALCULAR LAS FUNCIONES DE COSTO.  
                  [cfd1]=group_cost_function(tmplong,tmplatg,tmptimeg,...
                                             tmplon1,tmplat1,tmptime1,MaxDist);
                  [cfd2]=group_cost_function(tmplong,tmplatg,tmptimeg,...
                                             tmplon2,tmplat2,tmptime2,MaxDist);                  
                                         
                  if(cfd1 < cfd2)
                      if(ii==85)
                          display('Caso 1')
                      end
                      
                  %LO QUE HABIA ERA MEJOR SOLO ASOCIO LA PARTE DE LA
                  %TRAYECTORIA QUE NO SE SUPERPONE CON EL GRUPO.
                  tmpindex=false(size(minlatanalysis,1),1);
                  tmpindex(index)=true;
                  tmpindex=isnan(minlatanalysis(:,cgroup)) & tmpindex;
                  %obtengo los subindices de la trayectoria que no
                  %coinciden con lo que hay en el grupo.
                  [nada iasoc]=intersect(AnalysisTrajStruct(ii).daten,times(tmpindex));
                  %fprintf('Corte una trayectoria del miembro %10f para asociarla al grupo %10f\n',imember,cgroup)
                  %Verifico ademas si la parte de la trayectoria que voy a
                  %asignar al grupo cumple con la funcion de costo.
                  iasoc(iasoc < ini)=[];
                  

                  
                  if(~isempty(iasoc) && length(iasoc) > MinLength && requisito2)
                    %Solo asocio si la zona donde es posible asociar no
                    %quedo partida en el medio por lo que habia en el grupo
                    %(eso es el requisito2).
                    tmplon1=AnalysisTrajStruct(ii).minlonf(iasoc);
                    tmplat1=AnalysisTrajStruct(ii).minlatf(iasoc);
                    tmptime1=AnalysisTrajStruct(ii).daten(iasoc);
                    tmplong=meanlon(tmpindex,cgroup);
                    tmplatg=meanlat(tmpindex,cgroup);
                    tmptimeg=times(tmpindex);
                    [tmpcdf]=group_cost_function(tmplong,tmplatg,tmptimeg,...
                                             tmplon1,tmplat1,tmptime1,MaxDist);                     
                                         
                   if(tmpcdf <  tr_cost_function) 
                    asociarlist=[asociarlist cgroup]; %#ok<AGROW> %Agrego el grupo a la lista de asociacion.
                    indexasoc{length(asociarlist)}=iasoc; %#ok<AGROW> %Todos los puntos van a ser asociados.
                  
                   end
                  end

                  elseif(cfd(jgroup) < tr_cost_function)
                  %LA NUEVA TRAYECTORIA ES MEJOR, ENTONCES ASOCIO TODO LO
                  %QUE LA COST FUNCTION ME PERMITE ASOCIAR
                  %Voy a asociar toda la trayectoria o al menos desde el
                  %la parte asociable en adelante asi que no me preocupo
                  %por el requisito2.
                  asociarlist=[asociarlist cgroup]; %Agrego el grupo a la lista de asociacion.
                  indexasoc{length(asociarlist)}=ini:length(AnalysisTrajStruct(ii).daten); %Todos los puntos van a ser asociados.
                  
                  %REMUEVO EN FORMA PROLIJA LO QUE HABIA ANTES EN ESE
                  %LUGAR.
                  %Desasocio totalmente aquellas trayectorias a las que
                  %solo le queden menos de 3 puntos fuera de la region que
                  %va a ser reemplazada por la nueva trayectoria.
                   trajlist=unique(trajidanalysis(indexsuper,cgroup));
                   trajlist=trajlist(~isnan(trajlist));
  
                   for kk=1:length(trajlist)
                     itraj=trajlist(kk);
                     if( sum(trajidanalysis(~indexsuper,cgroup)==itraj) < MinLength)
                      %La trayectoria trajlist(kk) no tiene puntos fuera de
                      %la superposicion, entonces tengo que desasociarla.
                      %No es necesario que la desasocie del grupo porque
                      %eso va a ocurrir automaticamente cuando asocie la
                      %nueva trayectoria, pero si la tengo que desasociar
                      %en la estructura de las trayectorias.
                      %TrajStruct(imember).trajectories(itraj).asociada=false; 
                      index=AnalysisTrajStruct(itraj).idgroup==cgroup;
                      AnalysisTrajStruct(itraj).idgroup(index)=[];
                      if(isempty(AnalysisTrajStruct(itraj).idgroup))
                          AnalysisTrajStruct(itraj).asociada=false;
                      end
                     end  
                   end
                  end 
                end
               end
              end 
          end
        
       %TERMINE DE MIRAR TODOS LOS GRUPOS POSIBLES, QUEDAN DOS COSAS POR
       %HACER ASOCIAR LA TRAYECTORIA A SU NUEVO GRUPO (SI FUERA EL CASO) Y
       %DESASOCIARLA DEL ANTERIOR (TAMBIEN SI FUERA EL CASO).
           
        
        %==================================================================
        %DESASOCIO LA TRAYECTORIA DE LOS GRUPOS CON LOS CUALES YA NO SE
        %RELACIONA.
        %==================================================================
        if( ~isempty(desasociarlist) && nloop < MaxIterations)   %La trayectoria estaba asociada a otro grupo previamente. 
           %Desasocio la trayectoria ii de su grupo anterior.
           for kk=1:length(desasociarlist)
           dgroup=desasociarlist(kk);
          
           index=AnalysisTrajStruct(ii).idgroup==dgroup;
           AnalysisTrajStruct(ii).idgroup(index)=[];
           if(isempty(AnalysisTrajStruct(ii).idgroup))
             AnalysisTrajStruct(ii).asociada=false;
           end
           dindex=trajidanalysis(:,dgroup)==ii;

           %Elimino la trayectoria del grupo.
                minlatanalysis(dindex,dgroup)=NaN;
                minlonanalysis(dindex,dgroup)=NaN;
                minanomanalysis(dindex,dgroup)=NaN;
                minlapanalysis(dindex,dgroup)=NaN;
                uvelanalysis(dindex,dgroup)=NaN;
                vvelanalysis(dindex,dgroup)=NaN;
                trajidanalysis(dindex,dgroup)=NaN;
                minareaanalysis(dindex,dgroup)=NaN;
                meananomanalysis(dindex,dgroup)=NaN;

               %Recalculo la media del grupo (para el caso en el que el analysis
               %defina totalmente el grupo, en ese caso el grupo deberia
               %desapraecer.
               meanlat(:,dgroup)=nanmean(minlat(:,:,dgroup),1);
               for kt=1:ntimes %Cuidado especial hay que tener siempre que se promedian longitudes.
               meanlon(kt,dgroup)=mean_lon_fun(minlon(:,kt,dgroup));
               end
               nmember(dgroup)=sum(any(~isnan(minlat(:,:,dgroup)),2));
               aux=sum(~isnan(minlat(:,:,dgroup)),2);
               aux(aux==0)=NaN;        
               meanlength(dgroup)=nanmean(aux);
               
           end
               
        end   %END DEL IF SOBRE SI TENGO QUE DESASOCIAR LA TRAYECTORIA DE ALGUN GRUPO
        
       if( ~isempty(asociarlist))

          %==================================================================
          %ASOCIO LA TRAYECTORIA A UN NUEVO GRUPO O A LOS QUE YA TENIA
          %SI ES LO QUE CORRESPONDE.
          %==================================================================
          %ASIGNO LA TRAYECTORIA AL GRUPO REQUERIDO.
          if(Debug)
          fprintf('Asocio la trayectoria del %f al grupo %f con un cost function de %f \n',ii,newgroup,min(cfd));
          end
          for kk=1:length(asociarlist)
          newgroup=asociarlist(kk);
          asocindex=indexasoc{kk};
              
          [nada index] = intersect(times,AnalysisTrajStruct(ii).daten(asocindex));
          AnalysisTrajStruct(ii).asociada=true;
          id=AnalysisTrajStruct(ii).idgroup;
          AnalysisTrajStruct(ii).idgroup=[id newgroup]; %Actualizamos la lista de grupos.
          minlatanalysis(index,newgroup)=AnalysisTrajStruct(ii).minlatf(asocindex);
          minlonanalysis(index,newgroup)=AnalysisTrajStruct(ii).minlonf(asocindex);

          minanomanalysis(index,newgroup)=AnalysisTrajStruct(ii).minanomf(asocindex);
          minlapanalysis(index,newgroup)=AnalysisTrajStruct(ii).minlap(asocindex);
          minareaanalysis(index,newgroup)=AnalysisTrajStruct(ii).minarea(asocindex);
          meananomanalysis(index,newgroup)=AnalysisTrajStruct(ii).meananom(asocindex);
          uvelanalysis(index,newgroup)=AnalysisTrajStruct(ii).uvelf(asocindex);
          vvelanalysis(index,newgroup)=AnalysisTrajStruct(ii).vvelf(asocindex);
          trajidanalysis(index,newgroup)=ii;
          
          %Modifico la media del grupo, en aqullos puntos donde la media es
          %NaN y la trayectoria del analysis no es NaN, hago que la
          %trayectoria del analysis sea la media del grupo.
          tmpind=isnan(meanlat(:,newgroup));
          meanlat(tmpind,newgroup)=minlatanalysis(tmpind,newgroup);
          meanlon(tmpind,newgroup)=minlonanalysis(tmpind,newgroup);
          
          end
          
        end  %END DEL IF SOBRE SI ASOCIO LA TRAYECTORIA A NUEVOS GRUPOS.
      
       
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
    minareaanalysis(index,ngroup)=AnalysisTrajStruct(ii).minarea;
    meananomanalysis(index,ngroup)=AnalysisTrajStruct(ii).meananom;
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
       cfd( cfd >=  tr_cost_function*1.5 )=NaN;  
       
       %Veo cual es el grupo que esta mas cerca y que me puede traer
       %problemas.
       [min_cfd min_group]=nanmin(cfd);

       if(~isnan(min_cfd)) %Quiere decir que hay 2 grupos que estan cerca en espacio
                           %y que coinciden en tiempo. Sin importar
                           %demasiado la distancia a los origenes entre
                           %ambos veo si los puedo superponer.
                           
        %Calculo el laplaciano medio de ambos grupos (me quiero quedar con el grupo + intenso). 
        
        %sizeii=-nanmean(nanmean((minlap(:,:,ii))));
        %sizejj=-nanmean(nanmean((minlap(:,:,min_group))));
        
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
                 %Para cada valor de tmp busco esa trayectoria y le cambio
                 %el id del grupo al que esta asociada.
                   index=TrajStruct(kk).trajectories(tmp(kkk)).idgroup==geliminar;
                   TrajStruct(kk).trajectories(tmp(kkk)).idgroup(index)=[];
                   if(isempty(TrajStruct(kk).trajectories(tmp(kkk)).idgroup))
                     TrajStruct(kk).trajectories(tmp(kkk)).asociada=false;
                   end
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
                 %Para cada valor de tmp busco esa trayectoria y le cambio
                 %el id del grupo al que esta asociada.
                   index=AnalysisTrajStruct(tmp(kkk)).idgroup==geliminar;
                   AnalysisTrajStruct(tmp(kkk)).idgroup(index)=[];
                   if(isempty(AnalysisTrajStruct(tmp(kkk)).idgroup))
                     AnalysisTrajStruct(tmp(kkk)).asociada=false;
                   end
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
          minarea(:,:,geliminar)=NaN;
          meananom(:,:,geliminar)=NaN;
          minanomanalysis(:,geliminar)=NaN;
          minlapanalysis(:,geliminar)=NaN;
          minareaanalysis(:,geliminar)=NaN;
          meananomanalysis(:,geliminar)=NaN;
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
% UNA VEZ QUE TERMINE DE CREAR LOS GRUPOS Y DE ASOCIARLE TRAYECTORIAS
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
minarea(:,:,contador)=minarea(:,:,ii);
meananom(:,:,contador)=meananom(:,:,ii);
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
minareaanalysis(:,contador)=minareaanalysis(:,ii);
meananomanalysis(:,contador)=meananomanalysis(:,ii);
uvelanalysis(:,contador)=uvelanalysis(:,ii);
vvelanalysis(:,contador)=vvelanalysis(:,ii);
trajidanalysis(:,contador)=trajidanalysis(:,ii);
 
 end
 end

minlat(:,:,contador+1:end)=[];
minlon(:,:,contador+1:end)=[];
minanom(:,:,contador+1:end)=[];
minlap(:,:,contador+1:end)=[];
minarea(:,:,contador+1:end)=[];
meananom(:,:,contador+1:end)=[];
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
minareaanalysis(:,contador+1:end)=[];
meananomanalysis(:,contador+1:end)=[];
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
group.minarea=minarea;
group.meananom=meananom;
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
group.minareaanalysis=minareaanalysis;
group.meananomanalysis=meananomanalysis;

%LISTO EL LLOPO, PELADA LA GALLINA.






