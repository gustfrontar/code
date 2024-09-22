
function [mascara_out]=system_id_fun(input,config)

%Predefinimos la mascara como ceros.
mascara_out=zeros(size(input.Data));

for itime=1:size(input.Data,3);
    

    
if(config.id_type==1)
    
%==========================================================================
% DETECCION DE LOS SISTEMAS POR EL METODO DE LA ANOMALIA Y
% EL LAPLACIANO.
%==========================================================================
    
%Calculamos el laplaciano en coordenadas esfericas.  
[laplaciano]=laplaciano_fun(input.Data(:,:,itime),input.lat,input.lon);
%Creamos la mascara auxiliar (que se usa en la deteccion de los sistemas).
mascara=zeros(size(input.Data(:,:,itime)));
mascara( input.Data(:,:,itime) < config.umb_anom & laplaciano > config.umb_lap )=1;   


sistema=zeros(size(mascara));
[ny nx]=size(sistema);

ultimo_sistema=1;


for i=1:ny
    for j=1:nx
   
        if(mascara(i,j) == 1)
           tengo_vecino=0;
           for i2=-1:1
               for j2=-1:1
                   %Definiciones extra para imponer las condiciones de
                   %borde.
                   
                   indexi=i+i2;
                   indexj=j+j2;
                   if(config.global) %Condicion de borde global.
                   %=======================================================
                   %Condicion de borde ciclica en X
                   if(indexj > nx)
                   indexj=indexj-nx;
                   end
                   if(indexj < 1)
                   indexj=nx+indexj;
                   end
                   
                   if(indexi > ny)
                   indexi=ny-(indexi-ny-1);
                   aux=round(nx/2);
                   indexj=indexj+aux;
                   end
                   if(i < 1)
                   indexi=1-indexi; 
                   aux=round(nx/2);
                   indexj=indexj+aux;
                   end
                   
                   
                   else
                   %Condicion de borde regional (no hay)
                   %=======================================================
                   

                   end
                       
                   if(indexi <= ny && indexi >=1 && indexj <= nx && indexj >=1)
                   %Solo hago esto si el punto esta dentro del dominio.
                   if(sistema(indexi,indexj) > 0 && sistema(i,j)==0 )
                       sistema(i,j)=sistema(indexi,indexj);
                       tengo_vecino=1;
                   end
                   if(sistema(indexi,indexj) > 0 && sistema(i,j) > 0 && sistema(i,j) ~= sistema(indexi,indexj) )
                       auxiliar=max([sistema(indexi,indexj) sistema(i,j)]);
                       sistema(sistema == auxiliar)=min([sistema(indexi,indexj) sistema(i,j)]);
                   end
                   end
               end
           end
           if(tengo_vecino==0)
               sistema(i,j)=ultimo_sistema;
               ultimo_sistema=ultimo_sistema+1;
           end
        end
    end
end



end

%==========================================================================
% ACA TERMINA LA DETECCION DE LOS SISTEMAS POR EL METODO DE LA ANOMALIA Y
% EL LAPLACIANO.
%==========================================================================

if(config.id_type==2)
display('Buscando minimos') 
%==========================================================================
% DETECCION DE LOS SISTEMAS POR EL METODO DEL MINIMO REVESTIDO
%==========================================================================

umbral=config.umb_anom;     %Umbral de anomalia que voy a usar para definir los sistemas.
umbralmin=config.umb_min;   %Umbral de anomalia que exijo para determinar la presencia de un minimo.
maxsize=config.maxsissize;  %Maximo tamanio del cuadrado que voy a usar para buscar los sistemas.

var=input.Data(:,:,itime);
sistema=zeros(size(input.Data(:,:,itime)));

[ny nx]=size(var);

%Primero buscamos e identificamos los minimos. (Generamos una mascara donde
%solo figuran la posicion de los minimos). El minimo se detecta pidiendo
%que el punto en cuestion sea menor a todos los puntos que lo rodean.
%==========================================================================
nminimos=0;
mini=[];
minj=[];
for j=1:nx
    for i=1:ny
       testmin=0;
       for ii=-1:1
           for jj=-1:1
                   indexi=i+ii;
                   indexj=j+jj;
                   if(config.global) %Condicion de borde global.
                   %=======================================================
                   %Condicion de borde ciclica en X
                   if(indexj > nx)
                   indexj=indexj-nx;
                   end
                   if(indexj < 1)
                   indexj=nx+indexj;
                   end
                   
                   if(indexi > ny)
                   indexi=ny-(indexi-ny-1);
                   aux=round(nx/2);
                   indexj=indexj+aux;
                   end
                   if(indexi < 1)
                   indexi=1-indexi; 
                   aux=round(nx/2);
                   indexj=indexj+aux;
                   end
                   
                   else
                   %Condicion de borde regional (no hay)
                   %=======================================================
                   end
                  
               
               if(indexi <= ny && indexi >= 1 && indexj <= nx && indexj >= 1)
                if( var(indexi,indexj) < var(i,j) )
                testmin=1;
                end
               end
           end
       end
       %El punto en cuestion era un minimo?
       if(testmin==0 && var(i,j) < umbralmin ) %Entonces era un minimo.
       
       if(config.global)
       nminimos=nminimos+1;
       mini=[mini i];
       minj=[minj j];
       sistema(i,j)=nminimos;
       else
       %Por definicion en un dominio regional el minimo no puede estar en
       %el borde (simplemente porque no tenemos manera de saber si lo es o
       %no).
       if(i ~= 1 && i ~=ny && j ~=1 && j~=nx)
       nminimos=nminimos+1;
       mini=[mini i];
       minj=[minj j];
       sistema(i,j)=nminimos;
       end
        
       end
       end 
    end
end

output.minimos=sistema;



%[varx vary]=gradient(var);  %Calculamos el gradiente de la variable.

% Aca empieza la segunda etapa, a partir de cada minimo busco el sistema
% asociado que se va a extender en un entorno del minimo.
%==========================================================================

for imin=1:nminimos
   
    
    %Recorro cuadrados alrededor del minimo cada vez mas grandes viendo si
    %se cumple el umbral y sigo en contacto con el minimo.
    
    for sqsize=1:maxsize
    for inada=1:4
        
        for isq=-sqsize:sqsize
            %Borde superior del cuadrado.
            if(inada == 1)
            i=mini(imin)+sqsize;
            j=minj(imin)+isq;
            elseif(inada == 2)
            %Borde inferior del cuadrado.
            i=mini(imin)-sqsize;
            j=minj(imin)-isq;
            elseif(inada==3)
            %Borde derecho
            i=mini(imin)+isq;
            j=minj(imin)+sqsize;
            elseif(inada==4)
            %Borde izquierdo
            i=mini(imin)+isq;
            j=minj(imin)-sqsize;
            end

                   if(config.global) %Condicion de borde global.
                   %=======================================================
                   
                   %Condicion de borde polar
                   if(i > ny)
                   i=ny-(i-ny-1);
                   aux=round(nx/2);
                   j=j+aux;
                   end
                   if(i < 1)
                   i=1-i; 
                   aux=round(nx/2);
                   j=j+aux;
                   end
                   
                   %Condicion de borde ciclica en X
                   if(j > nx)
                   j=j-nx;
                   end
                   if(j < 1)
                   j=nx+j;
                   end
                   
                   else
                   %Condicion de borde regional (no hay)
                   %=======================================================
                   end

                   if( i <= ny && i >= 1 && j <= nx && j >= 1)
                   if( var(i,j) < umbral ) %#ok<ALIGN>
                        vecino=0;
                        for ii=-1:1
                            for jj=-1:1
                               iindex=i+ii;
                               jindex=j+jj;
                               if(config.global) %Condicion de borde global.
                                %=======================================================
                                %Condicion de borde ciclica en X
                                if(jindex > nx)
                                jindex=jindex-nx;
                                end
                                if(jindex < 1)
                                jindex=nx+jindex;
                                end
                   
                                if(iindex > ny)
                                iindex=ny-(iindex-ny-1);
                                aux=round(nx/2);
                                jindex=jindex+aux;
                                end
                                if(iindex < 1)
                                iindex=1-iindex; 
                                aux=round(nx/2);
                                jindex=jindex+aux;
                                end
                   
                               else
                                %Condicion de borde regional (no hay)
                                %=======================================================
                               end
                                
                                if( iindex <= ny && iindex >= 1 && jindex <= nx && jindex >= 1)
                                    if(sistema(iindex,jindex)==imin)
                                        vecino=1;
                                    end
                                end
                            end
                        end
                        if(vecino==1 && sistema(i,j)==0) %El punto no pertenecia a ningun sistema.
                            [minimo_conv]=minimun_basin_fun(output.minimos,var,i,j,config);
                            if(minimo_conv == imin)
                            sistema(i,j)=imin;
                            end
                        %elseif(vecino ==1 & sistema(i,j) > 0)        %El punto pertenece a otro sistema... hay que disputarlo.
                        %    actual=sistema(i,j);  %Minimo al que esta actualmente ligado el punto.

                            
                            %Calculo la componente del gradiente en la
                            %direccion del punto al minimo del sistema.
                            %grad_actual=[minj(actual)-j mini(actual)-i];
                            %grad_retador=[minj(imin)-j mini(imin)-i];
                            %proy_actual=varx(i,j)*grad_actual(1)+vary(i,j)*grad_actual(2);
                            %proy_retador=varx(i,j)*grad_retador(1)+vary(i,j)*grad_retador(2);
                            %if(proy_actual > 0 & proy_retador < 0)
                            %    sistema(i,j)=imin;  %Cambiamos la clasificacion de este punto.
                            %else
                         %   [minimo_conv]=minimun_basin_fun(output.minimos,var,i,j,config);
                                %Lo decido por distancias.
                         %       if(isnan(minimo_conv)) %No convergio a ningun minimo.
                         %       dist_actual=(i-mini(actual))^2+(j-minj(actual))^2;
                         %       dist_retador=(i-mini(imin))^2+(j-minj(imin))^2;
                         %       if(dist_retador <= dist_actual)
                         %       sistema(i,j)=imin;
                         %       end
                         %       else
                         %       sistema(i,j)=minimo_conv; %Es del minimo al cual tiende la trayectoria.
                         %       end
                            %En los demas casos no hago nada ...  
                            
                           
                        end

                end  %En del if sobre si se verifica el umbral.
                   end   %En del if sobre si el punto i,j esta dentro de la matriz.
        end  %End del do sobre los puntos del lado del cuadrado.
    end   %En del do sobre los 4 lados del cuadrado.
    
    end %End del do sobre los cuadrados de diferente tamanio.
end %End del do sobre los minimos.


%==========================================================================
% ACA TERMINA LA DETECCION DE LOS SISTEMAS POR EL METODO DEL MINIMO REVESTIDO
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

min_logical=true(nminimos,1);
min_fussion=zeros(nminimos,1);
for imin=1:nminimos;
  
%Comienzo la caminata a partir del minimo imin.

  pathi=mini(imin); %La posicion actual de la caminata en i
  pathj=minj(imin); %La posicion actual de la caminata en j

  var_path=var(pathi,pathj);
  path_mask=true(size(var));  %Esta mascara sirve para "marcar" el camino basicamente evita que la caminata vuelva sobre sus pasos.
  
  
  
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
                   
                   if(config.global) %Condicion de borde global.
                   %=======================================================
                   %Condicion de borde ciclica en X
                   if(indexj > nx)
                   indexj=indexj-nx;
                   end
                   if(indexj < 1)
                   indexj=nx+indexj;
                   end
                   
                   if(indexi > ny)
                   indexi=ny-(indexi-ny-1);
                   aux=round(nx/2);
                   indexj=indexj+aux;
                   end
                   if(indexi < 1)
                   indexi=1-indexi; 
                   aux=round(nx/2);
                   indexj=indexj+aux;
                   end
                   
                   end
                  
                if(indexi <= ny && indexi >= 1 && indexj <= nx && indexj >= 1 )
                if(var(indexi,indexj) < tmp_min && path_mask(indexi,indexj));
                tmp_min=var(indexi,indexj);  
                pathi=indexi;
                pathj=indexj;
              
                end

                end
           end
   end
           
           contador=contador+1;
           path_mask(pathi,pathj)=false; %Este punto queda vedado para futuros movimientos, la caminata no puede volver sobre sus pasos.
           var_path(contador)=tmp_min;
           %Es el punto donde estoy otro minimo?
           if( output.minimos(pathi,pathj) > 0)
               minimo2=output.minimos(pathi,pathj);
               cont=false; %La trayectoria llego a otro minimo asi que la cortamos.
           %Entonces es otro minimo!
%            %DEBUG......
%            figure
%            subplot(1,2,1)
%            plot(var_path)
%            subplot(1,2,2)
%            pcolor(real(path_mask)+sistema);
%            shading flat
%            %DEBUG......
  
           if(min([abs(max(var_path)-var_path(1)) abs(max(var_path)-var_path(end))]) < config.umb_minbarrier );
           %La barrera de geopotencial entre ambos minimos es menor que el umbral, entonces son el mismo sistema!
           %Me quedo con el minimo mas produndo.
               if( var_path(1) <= var_path(end))
                   min_logical(imin)=true; %El minimo desde donde salio la trayectoria sobrevive.
                   min_logical(minimo2)=false; %El minimo a donde llego la trayectoria se elimina.
                   min_fussion(minimo2)=imin;
               else
                   min_logical(imin)=false; %El minimo desde donde salio la trayectoria se eliminara.
                   min_logical(minimo2)=true; %Sobrevive el minimo a donde llego la trayectoria.
                   min_fussion(imin)=minimo2;
               end
               
           end   
           end
          
    
           %Si la anomalia en la trayectoria aumenta por encima del umbral
           %que se considera un sistema la trayectoria es interrumpida.
           if(tmp_min > config.umb_anom)
           min_logical(imin)=true; %El minimo desde donde salio la trayectoria sobrevive.
           cont=false; 
           end
           
     
  end
%    DEBUG.....
%    figure
%    subplot(1,2,1)
%    pcolor(real(path_mask))
%    shading flat
%    subplot(1,2,2)
%    pcolor(real(sistema==imin))
%    shading flat
%    DEBUG.....
    
    
end 

%Hasta aca tengo un array logico que dice que minimos sobreviven (true) y
%cuales no (false).

%Primero corrijo la mascara eliminando los minimos que no van a estar y
%reacomodo sus numeros para que sigan siendo consecutivos (sino se produce
%una falla en lo que resta del algoritmo).

for imin=1:nminimos
   if(~min_logical(imin))
   sistema(sistema==imin)=min_fussion(imin);
   end
end

%Luego corrijo los arrays que voy a usar para detectar los sistemas y
%recalculo el numero de minimos.

output.mini=mini(min_logical);
output.minj=minj(min_logical);

%nminimos=sum(min_logical);

output.minimos=sistema;

% HASTA ACA TERMINE DE DEFINIR LOS SISTEMAS
%==========================================================================

%SI EL DOMINIO ES REGIONAL, Y SI EL USUARIO LO DESEA SE ELIMINAN TODOS LOS
%SISTEMAS QUE ESTEN EN CONTACTO CON EL BORDE.

if(~config.global && config.borderdelete)
   temp=zeros(max(sistema),1);
   %Elimino los sistemas en contacto con el borde. 
   
   %La variable temp(i) guarda cuantos puntos del sistema i
   %estan en contacto con alguno de los 4 bordes.
   
   for i=1:ny
      if(sistema(i,1) > 0)
          temp(sistema(i,1))=temp(sistema(i,1))+1;
      end
      if(sistema(i,nx) > 0)
          temp(sistema(i,1))=temp(sistema(i,1))+1;
      end 
   end
   for j=1:nx
      if(sistema(1,j) > 0)
          temp(sistema(1,j))=temp(sistema(1,j))+1;
      end
      if(sistema(ny,j) > 0)
          temp(sistema(ny,j))=temp(sistema(ny,j))+1;
      end
   end
   
   for i=1:length(temp);
       if( temp(i) > config.umb_border)
           sistema(sistema==i)=0; %Elimino el sistema.
       end
   end   
end

% FALTA CORREGIR LO QUE PASA CON LOS SISTEMAS EN EL POLO.
%==========================================================================


end

mascara_out(:,:,itime)=sistema;

    
end


%==========================================================================
%   EN ALGUNOS CASOS EL METODO DEL MINIMO REVESTIDO PUEDE CONDICUR A LA
%   SEPARACION PREMATURA DE SISTEMAS O BIEN A LA DIVISION DE SISTEMAS EN
%   DONDE APARECEN DOS MINIMOS
%==========================================================================

%La idea de esta porcion del codigo es detectar sistemas que se dividen a
%un tiempo pero al tiempo siguiente se vuelven a unir y considerarlos como
%un sistema unido en todos los tiempos.

if(config.splitcheck)

umbral_percent=config.umbral_percent; %Umbral por encima del cual considero mucha coincidencia espacial.

times=size(mascara_out,3);

for it=2:times-1
   mascara_prev=mascara_out(:,:,it-1);
   mascara_c=   mascara_out(:,:,it);
   mascara_next=mascara_out(:,:,it+1);
    
   nsistemas_c=max(max(mascara_c));
   nsistemas_prev=max(max(mascara_prev));
   nsistemas_next=max(max(mascara_next));
   
   %Busco para cada sistema actual si hay algun sistema previo con el cual
   %hay una coincidencia espacial mayor al umbral_percent.
   sist_prev=zeros(nsistemas_c,1);
   sist_next=zeros(nsistemas_c,1);
   
   for isc=1:nsistemas_c
      areac=sum(sum(mascara_c==isc));
      if(areac > 0)
      for isp=1:nsistemas_prev 
          %Calculo el area de coincidencia entre el sistema isc y el isp.
          areaconj=sum(sum(mascara_c==isc & mascara_prev==isp));
          if(areaconj/areac > umbral_percent)
          %Una gran parte del sistema isc esta contenido en el sistema isp.    
          sist_prev(isc)=isp;    
          end
      end
      for isn=1:nsistemas_next
          %Calculo el area de coincidencia entre el sistema isc y el isp.
          areaconj=sum(sum(mascara_c==isc & mascara_next==isn));
          if(areaconj/areac > umbral_percent)
          %Una gran parte del sistema isc esta contenido en el sistema isp.    
          sist_next(isc)=isn;    
          end
      end
      end

   end
   
   %Ahora tengo que encontrar sistemas que esten contenidos en el mismo 
   %sistema previo y en el mismo sistema posterior. 


   for isis=1:length(sist_next);
       if(sist_prev(isis) > 0 && sist_next(isis) > 0)
       %Pregunto si este sistema esta contenido en un sistema previo y en
       %un sistema posterior (sino nunca puede ser lo que estamos buscando.
       csis_prev=sist_prev(isis);
       csis_next=sist_next(isis); %Guardamos los sistemas prev y next al cual esta asociado el sistema isis.
       
       %Ahora busco en los demas sistemas alguno que este asociado a los
       %mismos sistemas previos y posteriores.
       %Hago los cambios de forma tal de poder empezar a buscar de este
       %sistema en adelante.
       for isis2=isis:length(sist_next)
          if(sist_prev(isis2) == csis_prev && sist_next(isis2) == csis_next) 
          %Esto quiere decir que el sistema isis y el sistema isis2
          %comparten los mismos sistemas anteriores y posteriores (segun el
          %criterio de coincidencia que se aplico para determinar esto).
          mascara_c(mascara_c==isis2)=isis;
          sist_prev(isis2)=0;
          sist_next(isis2)=0;   
          end
          if(sist_prev(isis2)== csis_prev && sist_next(isis2) == 0)
          %Este es el caso de un sistema que se separa de otro justo antes de desaparecer.
          mascara_c(mascara_c==isis2)=isis;
          sist_prev(isis2)=0;
          sist_next(isis2)=0;
          end
          if(sist_prev(isis2)==0 && sist_next(isis2)== csis_next)
          %Este es el caso de un sistema que nace separado pero que
          %inmediatamente se funde con otro.
          mascara_c(mascara_c==isis2)=isis;
          sist_prev(isis2)=0;
          sist_next(isis2)=0;
          end
          
       end
       end
       
   end
   
   mascara_out(:,:,it)=mascara_c;
 
end %End del do sobre los tiempos.


end
%==========================================================================
% LISTO EL LLOPO!!
%==========================================================================
















