
function  [minimos]=find_minimun_global(Data,DataLat,DataLon,AditionalData,config)
%==========================================================================
%Esta funcion encuentra todos los minimos locales en un campo bidimensional
%asumiendo que se trata de una reticula global regular. Utiliza la funcion
%global_boundary_fun para aplicar las condiciones de borde globales.
%==========================================================================

%Data es una variable 2D que representa un campo global de una variable
%meteorologica (por ahora geopotencial).

%Datalat y Datalon son las matrices de latitudes y longitudes
%correspondientes a Data. Se asume que la reticula es global y regular en
%lat y lon.

%AditionalData es un conjunto de campos en la misma reticula que Datalat
%a partir de los cuales el programa puede calcular caracteristicas del
%sistema. Por ejemplo pordria ser la temperatura asociada al sistema, etc.

%config es una estructura con la configuracion necesaria para realizar la
%identificacion de los sistemas dentro de la funcion.

%Algunas variables imporatnes:
% mascara_minimos es una mascara que tiene 0 salvo en la posicion de los
% minimos, en esta posicion el valor de la mascara es igual al ID del
% minimo correspondiente.
% mascara es una mascara que tiene 0 salvo en los puntos que corresponden a
% uno de los sistemas asociados a alguno de los minimos. 


%La salida llamada minimo tiene los siguientes campos:
%   minimos.nminimos=nminimos;                      %N de minimos.
%   minimos.mini=mini;                              %i de cada minimo.
%   minimos.minj=minj;                              %j de cada minimo.
%   minimos.id=min_id;                              %id de cada minimo
%   minimos.minlat=minlat;                          %lat de c/ minimo.
%   minimos.minlon=minlon;                          %lon de c/ minimo.
%   minimos.minlatint=minlatint;                    %lat de c/ minimo int.
%   minimos.minlonint=minlonint;                    %lon de c/ minimo int.
%   minimos.indice=min_indice;                      %indices de c/ minimo.
%   minimos.area=min_area;                          %area de c/ minimo.
%   minimos.minaditionaldata=min_aditional_data;    %caracteristicas add.
%   minimos.maxaditionaldata=max_aditional_data;    %...
%   minimos.meanaditionaldata=mean_aditional_data;  %...
%   minimos.mindata=min_anom_sis;                   %min de anomalia
%   minimos.meandata=mean_anom_sis;                 %media de anomalia
%   minimos.centlat=cent_lat;                       %centroide lat
%   minimos.centlon=cent_lon;                       %centroide lon

%PARA MAS INFORMACION SOBRE LOS METODOS DE CALCULO DE LAS DIFERENTES
%CANTIDADES VER LOS COMENTARIOS DEL CODIGO.
minimos=[];


DEBUG=true;   % Cambiar a true para obtener un debug del proceso de 
               % identificacion de los minimos.

%==========================================================================
%          PASO I: ENCUENTRO LOS MINIMOS LOCALES DEL CAMPO.
%==========================================================================


[ny nx]=size(Data);
mascara_minimos=zeros(size(Data));



if(config.filterdata)
%tic
Data=fast_filter_fun(Data,config.filterlength);
%tiempo=toc;
%fprintf('El tiempo empleado en filtrar la funcion fue %3.3f\n',tiempo)
end

%tic
nminimos=0;
mini=[];
minj=[];
for j=1:nx
    for i=2:ny-1
       %Si el punto no califica ni siquiera para ser parte de un sistema
       %entonces no me fijo si es o no un minimo. 
       if(Data(i,j) < config.umb_anom);
       testmin=0;
       for ii=-1:1
           for jj=-1:1
                   indexi=i+ii;
                   indexj=j+jj;

                   [indexi,indexj]=global_boundary_fun(indexi,indexj,nx,ny);
     
                if(indexi <= ny && indexi >= 1 && indexj <= nx && indexj >= 1 ...
                  && Data(indexi,indexj) < Data(i,j) )
                
                testmin=1;
               
               end
           end
       end
       %El punto en cuestion era un minimo?
       if(testmin==0)
       nminimos=nminimos+1;
       mini=[mini i]; %#ok<AGROW>
       minj=[minj j]; %#ok<AGROW>
       mascara_minimos(i,j)=nminimos;
       end
       end
    end 
end

%Vamos a considerar en forma especial los minimos en el polo.
if( sum(Data(1,:) < Data(2,:))==nx)
    %Entonces el polo sur es un minimo.
    nminimos=nminimos+1;
    mini=[mini 1];
    minj=[minj 1];
    mascara_minimos(1,:)=nminimos;
end
if( sum(Data(end,:) < Data(end-1,:))==nx)
    %Entonces el polo norte es un minimo.
    nminimos=nminimos+1;
    mini=[mini ny];
    minj=[minj 1];
    mascara_minimos(end,:)=nminimos;
end


min_id=1:nminimos;

%DEBUG
if(DEBUG)
    figure
    pcolor(Data)
    hold on
    plot(minj,mini,'ko','MarkerSize',7)
    shading flat
    figure
    pcolor(mascara_minimos)
    shading flat
end
%tiempo=toc;
%tiempotot=tiempo;
%fprintf('El tiempo empleado en encontrar los minimos fue %3.3f \n',tiempo);



%==========================================================================
%          PASO II: ENCUENTRO EL "SISTEMA" ASOCIADO A CADA MINIMO.
%==========================================================================

%tic
mascara=zeros(size(mascara_minimos));
 
%Preparo la funcion para el calculo.
minimun_basin_fun(mascara_minimos,Data,i,j,false,true);

for j=1:nx
  for i=1:ny
    if(Data(i,j) < config.umb_anom)
    [mascara(i,j)]=minimun_basin_fun(mascara_minimos,Data,i,j,false,false);
    if(isnan(mascara(i,j)))
        fprintf('Guarda, el sistema dio NaN para %f %f \n',i,j)
        [mascara(i,j)]=minimun_basin_fun(mascara_minimos,Data,i,j,true,false);
     return
    end
    end
  end
end
        
%tiempo=toc;
%tiempotot=tiempo+tiempotot;
%fprintf('El tiempo empleado en determinar el sistema asociada a cada minimo fue %3.3f\n',tiempo);

%DEBUG
if(DEBUG)
    figure
    pcolor(mascara)
    shading flat
    figure
    pcolor(mascara_minimos)
    shading flat
end

%==========================================================================
%          PASO III: JUNTO LOS SISTEMAS QUE SON CERCANOS Y QUE NO SE PUEDEN
%          DIFERENCIAR ADECUADAMENTE UNO DEL OTRO.
%==========================================================================


%Hasta aca tengo detectados los minimos y su cuenca de atraccion. 
%==========================================================================
% Antes de "revestir" el minimo con los puntos que componen el sistema voy
% a detectar minimos que en realidad corresponden al mismo sistema. La idea
% es partir del minimo A buscando siempre el menor aumento de la anomalia,
% entonces pueden pasar dos cosas: llego a otro minimo B o me salgo de la
% zona donde la anomalia esta por debajo del umbral. Si llego a otro minimo
% B puedo obtener el maximo de anomalia en el camino respecto del minimo A
% si este maximo es pequenio puedo decir que A y B estan en el mismo
% sistema (mas alla de que pueden estar muy lejos).
% Esto evitaria particiones innecesarias de sistemas grandes por presencia
% de minimos multiples.


%Al final del algoritmo la variable new_mascara_minimos va a contener solo
%los minimos que han sobrevivido. Mientras que mascara_minimos tendra todos
%los minimos originales (aunque algunos con el numero cambiado).
%tic
%new_mascara_minimos=mascara_minimos;

for imin=1:nminimos;
  
%Comienzo la caminata a partir del minimo imin.

  pathi=mini(imin); %La posicion actual de la caminata en i
  pathj=minj(imin); %La posicion actual de la caminata en j

  Data_path=Data(pathi,pathj);
  path_mask=true(size(Data));  %Esta mascara sirve para "marcar" el camino 
                               %basicamente evita que la caminata vuelva
                               %sobre sus pasos.
  
  path_mask(pathi,pathj)=false;
  
  cont=true;
  
  contador=1;
  while(cont)
      
   tmp_min=0;
   %Busco en el entorno del punto el minimo.
   pathi_old=pathi;
   pathj_old=pathj;
   for ii=-1:1
           for jj=-1:1
              indexi=pathi_old+ii;
              indexj=pathj_old+jj;

              [indexi,indexj]=global_boundary_fun(indexi,indexj,nx,ny);

              if(indexi <= ny && indexi >= 1 && indexj <= nx && indexj >= 1 )
                if(Data(indexi,indexj) < tmp_min && path_mask(indexi,indexj));
                  tmp_min=Data(indexi,indexj);  
                  pathi=indexi;
                  pathj=indexj;
                end
              end
           end
   end
           
   contador=contador+1;
    %Este punto queda vedado para futuros movimientos, la caminata 
    %no puede volver sobre sus pasos.
   path_mask(pathi,pathj)=false;
   Data_path(contador)=tmp_min;
   %Es el punto donde estoy otro minimo?
   if( mascara_minimos(pathi,pathj) > 0)
     minimo2=mascara_minimos(pathi,pathj);
     cont=false;
     %La trayectoria llego a otro minimo asi que la cortamos.
     endvarpath=length(Data_path);
     if(min([abs(max(Data_path)-Data_path(1)) abs(max(Data_path)-Data_path(endvarpath))]) < config.umb_minbarrier );
       
       %La barrera de geopotencial entre ambos minimos es menor que el umbral,
       %entonces podr??an ser el mismo sistema!
       %Compruebo si estos m??nimos est??n muy cerca entrte s?? o si se
       %trata de dos sistemas diferentes.
      
       tmp_dist=distll_fun(DataLon(mini(imin),minj(imin)),DataLat(mini(imin),minj(imin)),...
                           DataLon(mini(minimo2),minj(minimo2)),DataLat(mini(minimo2),minj(minimo2)));

       if tmp_dist < config.umbral_cercania
       %Si esto es cierto entonces los dos minimos estan lo suficientemente
       %cerca como para que pueda sospechar que son el mismo.
       %Ahora bien, faltaria decidir cual sobrevive, para eso vamos a tomar
       %todos los minimos asociados al numero minimo2 (pueden ser varios
       %en principio todos los que se fueron fusionando con ese minimo).
       %Entonces calculo el minimo valor de todos los minimos asociados con
       %el id minimo2. Tengo que hacer lo mismo con el minimo imin, porque
       %eventualmente tambien podria representar varios minimos.
        index_minimo=mascara_minimos==imin;
        minimo_min=min(Data(index_minimo));
        index_minimo2=mascara_minimos==minimo2;
        minimo_min2=min(Data(index_minimo2));

         if( minimo_min < minimo_min2 )
           %EL MINIMO 1 ES EL MAS PROFUNDO (O ESTA ASOCIADO A UN MINIMO QUE
           %ES MAS PROFUNDO QUE TODOS LOS MINIMOS ASOCIADOS EN MINIMO 2).
           %Reasigno el minimo en la mascara original
           mascara_minimos(index_minimo2)=imin;
           mascara(mascara==minimo2)=imin;
           
           %Elimino el minimo en la mascara nueva.
           %new_mascara_minimos(index_minimo2)=0;
           min_id(minimo2)=imin;
           mini(minimo2)=mini(imin);
           minj(minimo2)=minj(imin);
         else
           %EN ESTE CASO EL MINIMO 2 ES MAS PROFUNDO QUE TODOS LOS MINIMOS
           %AGRUPADOS EN MINIMO 1.
           mascara_minimos(index_minimo)=minimo2;
           mascara(mascara==imin)=minimo2;
           %Elimino el minimo en la mascara nueva.
           %new_mascara_minimos(index_minimo)=0;
           min_id(imin)=minimo2;
           mini(imin)=mini(minimo2);
           minj(imin)=minj(minimo2);
         end
        end
      end
    end
          
    
    %Si la anomalia en la trayectoria aumenta por encima del umbral
    %que se considera un sistema la trayectoria es interrumpida.
    if(tmp_min > config.umb_anom)
      cont=false; 
    end
           
     
  end
  
end 

%Reasignamos los ID de los minimos para que vuelvan a ser consecutivos y
%eso haga mas facil la aplicacion del resto del algoritmo.
%Recalculamos ademas el vector de las posiciones en forma acorde y la
%cantidad total de minimos encontrados.

count=0;
for ii=1:nminimos
   index=(mascara==ii);
   if(sum(index(:))>0)
     count=count+1;
     mascara(index)=count;
     mini(count)=mini(ii);
     minj(count)=minj(ii);
     min_id(count)=count;
%     new_mascara_minimos(mini(count),minj(count))=count;
   end   
end
mini=mini(1:count);
minj=minj(1:count);
nminimos=count;

%tiempotot=tiempo+tiempotot;
%tiempo=toc;
%fprintf('El tiempo empleado en fusionar minimos fue %3.3f\n',tiempo);


%DEBUG
if(DEBUG)
    figure
    pcolor(mascara)
    shading flat
    figure
    pcolor(mascara_minimos)
    shading flat
end

%Asignamos a mascara minimos el valor modificado luego de la fusion de
%minimos.
%mascara_minimos=new_mascara_minimos;

%==========================================================================
%          PASO IV: PARA CADA MINIMO DE LA LISTA OBTENEMOS LA LATITUD Y LA
%          LONGITUD DEL MINIMO ORIGINAL Y LOS VALORES CORRESPONDIENTES AL
%          MINIMO INTERPOLADO.
%==========================================================================

%tic
minlat=NaN(size(mini));
minlon=NaN(size(mini));

minlatint=NaN(size(minlat));
minlonint=NaN(size(minlon));


for ii=1:nminimos
    
    %1) Calculamos la posicion (lat y lon) del punto correspondiente al
    %minimo.
   
    minlat(ii)=DataLat(mini(ii),minj(ii));
    minlon(ii)=DataLon(mini(ii),minj(ii));
    
    %2) Vamos a calcular un minimo aproximado usando la informacion de los
    %puntos vecinos. Uso un promedio pesado por la anomalia (donde la
    %anomalia es mayor voy a obtener un mayor peso y por ende la media de
    %las posiciones va a estar sesgada hacia el sector donde la anomalia es
    %mayor. Voy a promediar la latitud del punto en cuestion y la de los
    %puntos adyacentes.
    

    
    if( mini(ii) == 1 || mini(ii)==ny)
        %En el polo se complica mucho aplicar el algoritmo.
        minlatint(ii)=minlat(ii);
        minlonint(ii)=minlon(ii);
    else
    tmplat=NaN(3,3);
    tmplon=NaN(3,3);
    tmpdat=NaN(3,3);
        
        for jj=-1:1
            index=minj(ii)+jj;
            if(index < 1)
                index=nx;
            elseif(index > nx)
                index=1;
            end
            tmpi=mini(ii)-1:mini(ii)+1;
            tmplat(:,jj+2)=DataLat(tmpi,index);
            tmplon(:,jj+2)=DataLon(tmpi,index);
            tmpdat(:,jj+2)=Data(tmpi,index);
            
            
            
        end
        
    
    %Calculamos la media en latitudes
    minlatint(ii)=mean(tmplat(:).*tmpdat(:))/mean(tmpdat(:));
    %Calculamos la media en longitudes teniendo en cuenta todos los
    %posibles casos.
    if(max(tmplon(:))-min(tmplon(:)) > 180)
       if(max(tmplon(:)) > 180)
           tmplon(tmplon > 180)=tmplon(tmplon > 180)-360;
           minlonint(ii)=mean(tmplon(:).*tmpdat(:))/mean(tmpdat(:));
           if(minlonint(ii) < 0)
               minlonint(ii)=minlonint(ii)+360;
           end
       elseif(min(tmplon(:)) < 0)
         tmplon(tmplon < 0)=tmplon(tmplon < 0)+360;
         minlonint(ii)=mean(tmplon(:).*tmpdat(:))/mean(tmpdat(:));
         if(minlonint(ii) > 180)
             minlonint(ii)=minlonint(ii)-360;
         end 
       end
    else
    minlonint(ii)=mean(tmplon(:).*tmpdat(:))/mean(tmpdat(:));
    end
    
    %En esta parte tambien se puede usar una interpolacion por splines
    %bidimensionales como en el programa de Manuel. 
    end
    
       
end
%tiempo=toc;
%tiempotot=tiempo+tiempotot;
%fprintf('El tiempo empleado en recalcular la posicion de los sistemas fue %3.3f\n',tiempo);

%==========================================================================
%          PASO V: EN ESTE PASO CALCULAMOS TODOS LOS ATRIBUTOS DE LOS 
%          MINIMOS:
%          POSICION DEL CENTROIDE
%          AREA
%          INTENSIDAD
%          MAXIMO, MINIMO Y MEDIA DE GEOPOTENCIAL
%          MAXIMO, MINIMO Y MEDIA DE OTRAS VARIABLES ASOCIADAS.
%==========================================================================

%tic
%==========================================================================
%CALCULO EL AREA DE CADA PUNTO DE RETICULA PARA LUEGO USAR EN LA ESTIMACION
%DEL AREA DEL SISTEMA.
%==========================================================================

delta_lat(2:ny,1:nx)=DataLat(2:ny,:)-DataLat(1:ny-1,:);
delta_lon(1:ny,2:nx)=DataLon(:,2:nx)-DataLon(:,1:nx-1);

delta_lon(:,1)=DataLon(:,2)-DataLon(:,1);
delta_lat(1,:)=DataLat(2,:)-DataLat(1,:);

%Si la linea de cambios de fecha queda en el medio del dominio entonces
%habria un deltalon que seria mayor a 360 grados. Si esto pasa le resto a
%esos delta_lon 360 y listo.

delta_lon(delta_lon > 360)=delta_lon(delta_lon > 360)-360;
%Obtengo una matriz que contiene el area asociada a cada punto de reticula.
box_size=(111000^2)*(delta_lon.*cos(DataLat*3.14159/180).*delta_lat);

%==========================================================================
% CALCULO LAS PROPIEDADES DE LOS SISTEMAS
%==========================================================================

for ii=1:nminimos;
    
    %Guardo en una estructura la lista de subindices de los puntos que
    %pertenecen a cada uno de los sistemas. Esto me permite reconstruir la
    %mascara en forma rapida y eficiente sin necesidad de guardar todo el
    %campo o toda la mascara.
    min_indice{ii}=find(mascara==min_id(ii)); %#ok<AGROW>

    indices=min_indice{ii};

    %Calculamos el area en km2 del sistema asociado a cada minimo.
    min_area(ii)=sum(box_size(indices)); %#ok<AGROW>
    
    %Calculamos minima y media dentro del sistema para todas las variables
    %que se nos ocurran!
    
    if(~isempty(AditionalData))
    for ivar=1:size(AditionalData,3);
    temp=squeeze(AditionalData(:,:,ivar));

    min_aditional_data(ii,ivar)=min(temp(indices)); %#ok<AGROW>
    max_aditional_data(ii,ivar)=max(temp(indices));  %#ok<AGROW>
    mean_aditional_data(ii,ivar)=mean(temp(indices)); %#ok<AGROW>
    end
    
    end

    %Calculo la anomalia minima y la media.
    min_anom_sis(ii)=min(Data(indices)); %#ok<AGROW> %Valor minimo de anomalia en el sistema.
    mean_anom_sis(ii)=mean(Data(indices)); %#ok<AGROW> %Valor medio de la anomalia en el sistema.
 
    %Calculo el centroide del sistema.
    cent_lat(ii)=mean(DataLat(indices).*(Data(indices).^config.wexp))/mean(Data(indices).^config.wexp); %#ok<AGROW>
    %Cuidado especial hay que tener al calcular el centroide en longitud,
    %ya que el sistema puede caer en la linea -180/180 o 0/360.
    tmplon=DataLon(indices);
    if(max(tmplon)-min(tmplon) > 180)
       if(max(tmplon) > 180)
           tmplon(tmplon > 180)=tmplon(tmplon> 180)-360;
           cent_lon(ii)=mean(tmplon.*(Data(indices).^config.wexp))/mean((Data(indices).^config.wexp)); %#ok<AGROW>
           if(cent_lon(ii) < 0)
               cent_lon(ii)=cent_lon(ii)+360; %#ok<AGROW>
           end
       elseif(min(tmplon) < 0)
         tmplon(tmplon < 0)=tmplon(tmplon < 0)+360;
         cent_lon(ii)=mean(tmplon.*(tmp_data(indices).^config.wexp))/mean((Data(indices).^config.wexp)); %#ok<AGROW>
         if(cent_lon(ii) > 180)
             cent_lon(ii)=cent_lon(ii)-360; %#ok<AGROW>
         end 
       end
    else
    cent_lon(ii)=mean(DataLon(indices).*(Data(indices).^config.wexp))/mean((Data(indices).^config.wexp)); %#ok<AGROW>
    end
      
end

%==========================================================================
%               VI: ARMO UNA ESTRUCTURA CON TODO LO QUE QUIERO QUE SEAN
%               VARIABLES DE SALIDA.
%==========================================================================

%El nombre de la estructura va a ser minimos.

   minimos.nminimos=nminimos;
   minimos.mini=mini;
   minimos.minj=minj;
   minimos.id=min_id;
   minimos.minlat=minlat;
   minimos.minlon=minlon;
   minimos.minlatint=minlatint;
   minimos.minlonint=minlonint;
   minimos.indice=min_indice;
   minimos.area=min_area;
   if(~isempty(AditionalData))
    minimos.minaditionaldata=min_aditional_data;
    minimos.maxaditionaldata=max_aditional_data;
    minimos.meanaditionaldata=mean_aditional_data;
   else
    minimos.minaditionaldata=NaN;
    minimos.maxaditionaldata=NaN;
    minimos.meanaditionaldata=NaN;
   end
   minimos.mindata=min_anom_sis;
   minimos.meandata=mean_anom_sis;
   minimos.centlat=cent_lat;
   minimos.centlon=cent_lon;
   
   %DEBUG
if(DEBUG)
    figure
    pcolor(DataLon,DataLat,mascara)
    shading flat
    hold on
    plot(minimos.minlon,minimos.minlat,'o')
    plot(minimos.minlonint,minimos.minlatint,'v')
    plot(minimos.centlon,minimos.centlat,'+')
end
   
%tiempo=toc;
%tiempotot=toc;
%fprintf('El tiempo empleado en calcular las propiedades de los sistemas es %3.3f\n',tiempo);
%fprintf('El tiempo total es %3.3f\n',tiempotot)
end
