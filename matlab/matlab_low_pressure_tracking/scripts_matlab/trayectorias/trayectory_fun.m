
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
       
    cent_lat(nsistemas)=mean(lat(auxiliar));
    
    %Cuidado especial hay que tener al calcular el centroide en longitud,
    %ya que el sistema puede caer en la linea -180/180 o 0/360.
    tmp_lon=lon(auxiliar);
    if(max(tmp_lon)-min(tmp_lon) > 180)
       if(max(tmp_lon) > 180)
           tmp_lon(tmp_lon > 180)=tmp_lon(tmp_lon> 180)-360;
           cent_lon(nsistemas)=mean(tmp_lon);
           if(cent_lon(nsistemas) < 0)
               cent_lon(nsistemas)=cent_lon(nsistemas)+360;
           end
       end
       if(min(tmp_lon) < -180)
         tmp_lon(tmp_lon < -180)=tmp_lon(tmp_lon < -180)+360;
         cent_lon(nsistemas)=mean(tmp_lon);
         if(cent_lon(nsistemas) > 180)
             cent_lon(nsistemas)=cent_lon(nsistemas)-360;
         end 
       end
    else
    cent_lon(nsistemas)=mean(lon(auxiliar));
    end
    
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

    %Hago un loop sobre los sistemas que detecte en este tiempo..
    if(nsistemas > 0)
       for isis=1:nsistemas
       %Para cada sistema calculo la distancia a los sistemas conocidos
       %siempre y cuando hayan sido detectados en el ultimo tiempo o en el
       %inmediatamente anterior.

       if( sistemas_detectados > 0)
          numero_sis=0;
          min_dist=umbral_distancia;
          for isisref=1:sistemas_detectados
              %Verifico que el tiempo del sistema este dentro del rango de
              %tolerancia.
              if(input.times(itime)-trayectorias(isisref).date_sis(end) <=  intervalo/24);
              
              %Calculo la distancia entre el sistema actual numero isis y
              %el sistema de los tiempos anteriores isisref en el ultimo
              %tiempo en que fue detectado.
                 dist=distll_fun(cent_lon(isis),cent_lat(isis),trayectorias(isisref).lon(end),trayectorias(isisref).lat(end));
                 if( dist < min_dist)
                    numero_sis=isisref; %Numero de sistema de referencia con el cual se produjo el macheo.
                    min_dist=dist;      %Ahora el umbral lo hago mas exigente, solo puede superarlo un sistema que este mas cerca
                                        %que el que habia encontrado antes.
                 end   
              end
          end
          %Al terminar este ciclo pueden pasar dos cosas. Encontre un
          %sistema cerca o no lo encontre.
          if( numero_sis > 0) %Entonces encontre un sistema cercano.
          %Completo la trayectoria del sistema con el valor en este tiempo.
          trayectorias(numero_sis).size=   [trayectorias(numero_sis).size sis_area(isis)];
          trayectorias(numero_sis).lat=    [trayectorias(numero_sis).lat cent_lat(isis)];
          trayectorias(numero_sis).lon=    [trayectorias(numero_sis).lon cent_lon(isis)];
          trayectorias(numero_sis).minvar= [trayectorias(numero_sis).minvar min_var(isis,:)];
          trayectorias(numero_sis).maxvar= [trayectorias(numero_sis).maxvar max_var(isis,:)];
          trayectorias(numero_sis).meanvar=[trayectorias(numero_sis).meanvar mean_var(isis,:)];
          trayectorias(numero_sis).date_sis=   [trayectorias(numero_sis).date_sis input.times(itime)];
         
          else %Si numero_sis es 0 quiere decir que no encontre el sistema en la lista de referencia.
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
    end
    
    
    
    
 end


  
end %End sobre el for de los tiempos.

%==========================================================================
% LISTO EL LLOPO!!
%==========================================================================
















