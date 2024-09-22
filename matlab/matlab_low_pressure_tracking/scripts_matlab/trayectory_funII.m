
function [trayectorias]=trayectory_funII(trayectorias,input,config)
%Esta funcion toma la mascara que identifico cada sistema y calcula
%caracteristicas medias y selecciona algunos sistemas de acuerdo a
%criterios de tamanio, intensidad, etc.
%LA IDEA DE ESTA VERSION ES SEPARAR DEFINITIVAMENTE EL CALCULO DE LAS
%TRAYECTORIAS DEL CALCULO DE LAS CARACTERISTICAS DEL SISTEMA (ESO SE HARA
%EN UN SEGUNDO PASO EN FUNCION DE LAS TRAYECTORIAS CALCULADAS).

%umbral_area=config.umbral_area;       
intervalo=config.intervalo;                  

%==========================================================================
% EMPIEZO EL LOOP PARA EL CALCULO DE LAS TRAYECTORIAS.
%==========================================================================

for itime=1:ntiempos
    
nsistemas=sum(minimos(itime).consider(:)); %Cuento la cantidad de minimos que pueden formar parte de una trayectoria disponibles para el tiempo T.    


% INICIALIZO LAS TRAYECTORIAS PROPIAMENTE DICHAS SI SE TRATA DEL PRIMER
% TIEMPO.
%==========================================================================

%Guardo la cantidad de sistemas de cada tiempo.

 if(itime==1)

    isis=0;
    for i=1:nsistemas
    
    if(sis_area(i) > umbral_area)
     isis=isis+1;
      %EN ESTA FUNCION SOLO NOS PREOCUPA LA TRAYECTORIA EN SI, NO LAS
      %CARACTERISTICAS DEL SISTEMA.
      trayectorias(isis).lat=minimos(itime).lat(i);
      trayectorias(isis).lon=minimos(itime).lon(i);
      trayectorias(isis).date_sis=input.times(itime);
     end
    end
    

 else  %Si no es el primer tiempo...
     
 
% ESTA PARTE DEL ALGORITMO ES EL MATCHING ENTRE LOS SISTEMAS DETECTADOS EN 
% EL TIEMPO T Y LOS DETECTADOS EN EL TIEMPO T-1. NOTAR QUE ESTA PARTE ES
% INDEPENDIENTE DE COMO DETECTAMOS LOS SISTEMAS.
% LA IDEA ES BARRER TODOS LOS SISTEMAS DETECTADOS EN EL TIEMPO T Y VER SI
% ESTAN CERCA DE ALGUNO DETECTADO EN EL TIEMPO T-1, SI UN SISTEMA DETECTADO
% EN EL TIEMPO T NO SE PUEDE ASOCIAR A NINGUN SISTEMA DETECTADO EN EL
% TIEMPO T-1 ENTONCES SE TRATA COMO UN NUEVO SISTEMA Y SE INICIALIZA UNA
% NUEVA TRAYECTORIA.
%==========================================================================   

    %Hago un loop sobre las trayectorias verificando las que puedan tener
    %continuaciones en este tiempo
    
    %Existen 2 situaciones a tener en cuenta, la primera es mas de un
    %sistema esta al alcance del ultimo punto de una trayectoria (entonces
    %deberia elegir el mas cercano y usar ademas un criterio de velocidad
    %que impida que haya cambios bruscos en la direccion de la trayectoria)
    
    %La segunda situacion es cuando 2 trayectorias podrian tener como
    %continuidad un mismo sistema, en ese caso tengo que guardar la
    %distancia con la que fue asociada cada sistema, porque podria ser que
    %la trayectoria que analice mas adelante este mas cerca de dicho
    %sistema que a la que fue asignado originalmente.
    
    if(nsistemas > 0)
        asociado=false(nsistemas,1);
        traj_asoc=zeros(nsistemas,1);
        dist_asoc=NaN(nsistemas,1);

       for isis=1:nsistemas
       %Para cada sistema calculo la distancia a los sistemas conocidos
       %siempre y cuando hayan sido detectados en el ultimo tiempo o en el
       %inmediatamente anterior.

       if( ~ISEMPTY(trayectorias) ) %Si hay trayectorias detectadas...


          min_dist=config.umbral_distancia;
          for isisref=1:length(trayectorias)
              %Verifico que el tiempo del sistema este dentro del rango de
              %tolerancia.
              if(input.times(itime)-trayectorias(isisref).date_sis(end) <=  intervalo/24);
              
              %Calculo la distancia entre el sistema actual numero isis y
              %el sistema de los tiempos anteriores isisref en el ultimo
              %tiempo en que fue detectado.
                 dist=distll_fun(minimos(itime).lat(isis),minimos(itime).lon(isis),trayectorias(isisref).lon(end),trayectorias(isisref).lat(end));
                 continuacion=false;
                 
                 if(~config.asymetric_propagation) %Uso un criterio de distancia "isotropico" el sistema puede propagarse en cualquier 
                                                   %direccion con igual probabilidad.
                     if( dist < min_dist)
                        continuacion=true;
                     end
                 end
                     
                 
                 if(config.asymetric_propagation)  %Uso un criterio donde el desplazamiento en las diferentes direcciones no tiene
                                                   %la misma probabilidad.
                       tmp_mean_lat=0.5*(minimos(itime).lat(isis)+trayectorias(isisref).lat(end));
                       tmp_mean_lon=0.5*(minimos(itime).lon(isis)+trayectorias(isisref).lon(end));
                       %Calculamos las componentes del desplazamiento en x
                       %y en y.
                       tmp_dx=distll_fun(minimos(itime).lon(isis),tmp_mean_lat,trayectorias(isisref).lon(end),tmp_mean_lat);
                       tmp_dy=distll_fun(tmp_mean_lon,minimos(itime).lat(isis),tmp_mean_lon,trayectorias(isisref).lat(end));
                       tmp_dlon=(minimos(itime).lon(isis)-trayectorias(isisref).lon(end));
                       %tmp_dlat=(cent_lat(isis)-trayectorias(isisref).lat(end));
                       if(tmp_dlon > 0 || tmp_dlon < -180) %Desplazamiento hacia el oeste.
                       test_dist=(tmp_dx^2)/(config.max_eastpropagation^2)+(tmp_dy^2)/(config.max_nspropagation^2);
                          if(test_dist < 1)
                          continuacion=true;
                          end
                       end
                       if(tmp_dlon < 0 || tmp_dlon > 180) %Desplazamiento hacia el este.
                       test_dist=(tmp_dx^2)/(config.max_westpropagation^2)+(tmp_dy^2)/(config.max_nspropagation^2);
                          if(test_dist < 1)
                          continuacion=true;
                          end 
                       end                  
                    end
                    
                    if(continuacion)
                    %Tengo que verificar que no exista algun otro sistema
                    %que haya sido asignado a la trayectoria isisref y en
                    %dicho caso si la distancia de asignacion fue mayor
                    %entonces lo desasocio.
                    
                    if(sum(traj_asoc==isisref) >=1) %Si pasa esto la trayectoria isisref ya habia encontrado una continuidad en este tiempo
                    
                    for isis2=1:isis-1
                       %Veo que continuidad es mejor si la que tenia o la
                       %nueva, en terminos solo de distancia.

                       if( traj_asoc(isis2)==isisref && dist < dist_asoc(isis2))
                       %En este caso tengo que desasociar el sistema
                       %anterior.
                       asociado(isis2)=false;
                       traj_asoc(isis2)=0;
                       dist_asoc(isis2)=NaN;
                       asociado(isis)=true;
                       traj_asoc(isis)=isisref;
                       dist_asoc(isis)=dist;
                       end
                    end
                    
                    else
                    %En este caso la trayectoria isisref no habia
                    %encontrado continuidad todavia, entonces digo que este
                    %sistema es la continuidad de isisref.
                    min_dist=dist;      %Ahora el umbral lo hago mas exigente, solo puede superarlo un sistema que este mas cerca
                                        %que el que habia encontrado antes.
                    asociado(isis)=true;
                    traj_asoc(isis)=isisref; %Numero de sistema de referencia con el cual se produjo el macheo.
                    dist_asoc(isis)=dist;
                    
                    end
                    


                 end 

              end
          end
         
       end
       
       end %Termina el loop sobre los sistemas.
       %Hasta aca sabemos de que sistema es continuacion cada uno de los
       %nuevos sistemas encontrados. Ahora con esa informacion actualizamos
       %las trayectorias. No se puede hacer esto dentro del loop anterior,
       %porque hasta ultimo momento no sabemos si una trayectoria puede
       %tener continuacion en un sistema u otro por eso primero hay que
       %analizar todos los sistemas y luego expandir las trayectorias al
       %tiempo t.

       
       for isis=1:nsistemas
       
          if( asociado(isis) ) %Entonces encontre un sistema cercano.
          %Completo la trayectoria del sistema con el valor en este tiempo.
          trayectorias(traj_asoc(isis)).lat=    [trayectorias(traj_asoc(isis)).lat minimos(itime).lat(isis)];
          trayectorias(traj_asoc(isis)).lon=    [trayectorias(traj_asoc(isis)).lon minimos(itime).lon(isis)];
          trayectorias(traj_asoc(isis)).date_sis=   [trayectorias(traj_asoc(isis)).date_sis input.times(itime)];
         
          else %Si es false quiere decir que no encontre el sistema en la lista de referencia.
          %ergo tengo que agregar este sistema a la lista!
          %Lo voy a hacer solo si verifica el criterio de area. Aca es
          %donde interviene el criterio de area (para no crear sistemas al
          %pepe, pero si ya existe el sistema no uso el criterio de area
          %para priorizar la continuidad del sistema).
          if ( sis_area(isis) > umbral_area)
          sistemas_detectados=sistemas_detectados+1; 

          trayectorias(sistemas_detectados).lat=minimos(itime).lat(isis);
          trayectorias(sistemas_detectados).lon=minimos(itime).lon(isis);
          trayectorias(sistemas_detectados).date_sis=input.times(itime);
          end
             

          end         
       end
       
       end

  
 end %End del if sobre si es el primer tiempo o no (tener en cuenta el else).
  
end %End sobre el for de los tiempos.

%==========================================================================
% LISTO EL LLOPO!!
%==========================================================================
















