
function [trayectorias]=trayectory_fun(trayectorias,input,config)
%Esta funcion toma la mascara que identifico cada sistema y calcula
%caracteristicas medias y selecciona algunos sistemas de acuerdo a
%criterios de tamanio, intensidad, etc.


umbral_distancia=config.umbral_distancia;
umbral_area=config.umbral_area;       
intervalo=config.intervalo;                  

%Una vez hecho eso crea una matrices comunes con la info de cada sistema en
%los diferentes miembros y en las observaciones.

[nfilas ncols ntiempos]=size(trayectorias.mask);
nvars=size(input.Data_unfilt,4); %Cuantas variables no filtradas van a caracterizar el sistema.
mascara=trayectorias.mask;
lat=input.lat;
lon=input.lon;

%==========================================================================
%CALCULO EL AREA DE CADA PUNTO DE RETICULA PARA LUEGO USAR EN LA ESTIMACION
%DEL AREA DEL SISTEMA.
%==========================================================================
[ny nx]=size(lat);

delta_lat(2:ny,1:nx)=lat(2:ny,:)-lat(1:ny-1,:);
delta_lon(1:ny,2:nx)=lon(:,2:nx)-lon(:,1:nx-1);

delta_lon(:,1)=lon(:,2)-lon(:,1);
delta_lat(1,:)=lat(2,:)-lat(1,:);

%Si la linea de cambios de fecha queda en el medio del dominio entonces
%habria un deltalon que seria mayor a 360 grados. Si esto pasa le resto a
%esos delta_lon 360 y listo.

delta_lon(delta_lon > 360)=delta_lon(delta_lon > 360)-360;

box_size=(111000^2)*(delta_lon.*cos(lat*3.14159/180).*delta_lat);

%==========================================================================
% EMPIEZO EL LOOP PARA EL CALCULO DE LAS TRAYECTORIAS.
%==========================================================================

for itime=1:ntiempos
    
nsistemas=0;
max_sistema=max(max(mascara(:,:))); %Busco el maximo numero de la mascara.
aux_mask=mascara(:,:,itime);



% CALCULO LAS CARACTERISTICAS DE TODOS LOS SISTEMAS PARA EL TIEMPO ITIME,
% NO ELIMINO NINGUN SISTEMA EN ESTE PUNTO. 
%==========================================================================

for isis=1:max_sistema
    aux_mask=reshape(aux_mask,[nx*ny 1]);
    auxiliar=aux_mask==isis;
    
    if(sum(auxiliar) > 0) %Si el sistema no tiene tamanio 0.
    nsistemas=nsistemas+1;
    
    sis_area(nsistemas)=sum(box_size(auxiliar));
    
    %Calculamos minima y media dentro del sistema para todas las variables
    %que se nos ocurran!
    for ivar=1:nvars
    temp_var=squeeze(input.Data_unfilt(:,:,itime,ivar));

    min_var(nsistemas,ivar)=min(temp_var(auxiliar)); %#ok<AGROW>
    max_var(nsistemas,ivar)=max(temp_var(auxiliar)); %#ok<NASGU>
    mean_var(nsistemas,ivar)=mean(temp_var(auxiliar));
    end
    
    %Aca se pueden agregar otras variables... aparte de las usadas para
    %definir el sistema.
    tmp_data=squeeze(input.Data(:,:,itime));
    
    min_anom_sis=min(tmp_data(auxiliar)); %Valor minimo de anomalia en el sistema.
    %Calculo los puntos del sistema donde la anomalia es
    %umbral_anom_posicion por encima del minimo de dicho sistema.
    auxiliar2=auxiliar & (reshape(tmp_data,[nx*ny 1]) < min_anom_sis + config.umbral_anom_posicion);  
    cent_lat(nsistemas)=mean(lat(auxiliar2).*(tmp_data(auxiliar2).^config.wexp));
    cent_lat(nsistemas)=cent_lat(nsistemas)/mean((tmp_data(auxiliar2).^config.wexp));
    %Cuidado especial hay que tener al calcular el centroide en longitud,
    %ya que el sistema puede caer en la linea -180/180 o 0/360.
    tmp_lon=lon(auxiliar2);
    if(max(tmp_lon)-min(tmp_lon) > 180)
       if(max(tmp_lon) > 180)
           tmp_lon(tmp_lon > 180)=tmp_lon(tmp_lon> 180)-360;
           cent_lon(nsistemas)=mean(tmp_lon.*(tmp_data(auxiliar2).^config.wexp));
           if(cent_lon(nsistemas) < 0)
               cent_lon(nsistemas)=cent_lon(nsistemas)+360;
           end
       end
       if(min(tmp_lon) < -180)
         tmp_lon(tmp_lon < -180)=tmp_lon(tmp_lon < -180)+360;
         cent_lon(nsistemas)=mean(tmp_lon.*(tmp_data(auxiliar2).^config.wexp));
         if(cent_lon(nsistemas) > 180)
             cent_lon(nsistemas)=cent_lon(nsistemas)-360;
         end 
       end
    else
    cent_lon(nsistemas)=mean(lon(auxiliar2).*(tmp_data(auxiliar2).^config.wexp));
    end
    
    cent_lon(nsistemas)=cent_lon(nsistemas)/mean((tmp_data(auxiliar2).^config.wexp));

    %Aca tambien se puede calcular algun parametro que indique la forma del
    %sistema.
    %La posicion del sistema tambien se puede estimar a partir de la
    %posicion del minimo... o bien como un promedio de ambas estimaciones.
    
    end
end

if(nsistemas ==0) %No encontre ningun sistema este dia :(
   sis_area=NaN;
   min_var=NaN;
   mean_var=NaN;
   cent_lat=NaN;
   cent_lon=NaN;
end

% INICIALIZO LAS TRAYECTORIAS PROPIAMENTE DICHAS SI SE TRATA DEL PRIMER
% TIEMPO.
%==========================================================================

%Guardo la cantidad de sistemas de cada tiempo.
%trayectorias.nsistemas(itime)=nsistemas;

 if(itime==1)

    isis=0;
    for i=1:nsistemas
    
    if(sis_area(i) > umbral_area)
     isis=isis+1;
      trayectorias(isis).size=sis_area(i);
      trayectorias(isis).lat=cent_lat(i);
      trayectorias(isis).lon=cent_lon(i);
      trayectorias(isis).minvar=min_var(i,:);
      trayectorias(isis).maxvar=max_var(i,:);
      trayectorias(isis).meanvar=mean_var(i,:);
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
    sistemas_detectados=length(trayectorias); %Obtengo el numero de sistemas que han sido detectados hasta ahora.

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

       if( sistemas_detectados > 0)


          min_dist=umbral_distancia;
          for isisref=1:sistemas_detectados
              %Verifico que el tiempo del sistema este dentro del rango de
              %tolerancia.
              if(input.times(itime)-trayectorias(isisref).date_sis(end) <=  intervalo/24);
              
              %Calculo la distancia entre el sistema actual numero isis y
              %el sistema de los tiempos anteriores isisref en el ultimo
              %tiempo en que fue detectado.
                 dist=distll_fun(cent_lon(isis),cent_lat(isis),trayectorias(isisref).lon(end),trayectorias(isisref).lat(end));
                 continuacion=false;
                 
                 if(~config.asymetric_propagation) %Uso un criterio de distancia "isotropico" el sistema puede propagarse en cualquier 
                                                   %direccion con igual probabilidad.
                     if( dist < min_dist)
                        continuacion=true;
                     end
                 end
                     
                 
                 if(config.asymetric_propagation)  %Uso un criterio donde el desplazamiento en las diferentes direcciones no tiene
                                                   %la misma probabilidad.
                       tmp_mean_lat=0.5*(cent_lat(isis)+trayectorias(isisref).lat(end));
                       tmp_mean_lon=0.5*(cent_lon(isis)+trayectorias(isisref).lon(end));
                       %Calculamos las componentes del desplazamiento en x
                       %y en y.
                       tmp_dx=distll_fun(cent_lon(isis),tmp_mean_lat,trayectorias(isisref).lon(end),tmp_mean_lat);
                       tmp_dy=distll_fun(tmp_mean_lon,cent_lat(isis),tmp_mean_lon,trayectorias(isisref).lat(end));
                       tmp_dlon=(cent_lon(isis)-trayectorias(isisref).lon(end));
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
          trayectorias(traj_asoc(isis)).size=   [trayectorias(traj_asoc(isis)).size sis_area(isis)];
          trayectorias(traj_asoc(isis)).lat=    [trayectorias(traj_asoc(isis)).lat cent_lat(isis)];
          trayectorias(traj_asoc(isis)).lon=    [trayectorias(traj_asoc(isis)).lon cent_lon(isis)];
          trayectorias(traj_asoc(isis)).minvar= [trayectorias(traj_asoc(isis)).minvar min_var(isis,:)];
          trayectorias(traj_asoc(isis)).maxvar = [trayectorias(traj_asoc(isis)).maxvar max_var(isis,:)];
          trayectorias(traj_asoc(isis)).meanvar=[trayectorias(traj_asoc(isis)).meanvar mean_var(isis,:)];
          trayectorias(traj_asoc(isis)).date_sis=   [trayectorias(traj_asoc(isis)).date_sis input.times(itime)];
         
          else %Si es false quiere decir que no encontre el sistema en la lista de referencia.
          %ergo tengo que agregar este sistema a la lista!
          %Lo voy a hacer solo si verifica el criterio de area. Aca es
          %donde interviene el criterio de area (para no crear sistemas al
          %pepe, pero si ya existe el sistema no uso el criterio de area
          %para priorizar la continuidad del sistema).
          if ( sis_area(isis) > umbral_area)
          sistemas_detectados=sistemas_detectados+1; 

          trayectorias(sistemas_detectados).size=sis_area(isis);
          trayectorias(sistemas_detectados).lat=cent_lat(isis);
          trayectorias(sistemas_detectados).lon=cent_lon(isis);
          trayectorias(sistemas_detectados).minvar=min_var(isis,:);
          trayectorias(sistemas_detectados).maxvar=max_var(isis,:);
          trayectorias(sistemas_detectados).meanvar=mean_var(isis,:);
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
















