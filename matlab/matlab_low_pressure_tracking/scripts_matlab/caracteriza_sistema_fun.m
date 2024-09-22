
function [sistemas]=caracteriza_sistema_fun(mascara,mascarao,lat,lon,geo,geoo)
%Esta funcion toma la mascara que identifico cada sistema y calcula
%caracteristicas medias y selecciona algunos sistemas de acuerdo a
%criterios de tamanio, intensidad, etc.


umbral_distancia=500*1e3;

%Una vez hecho eso crea una matrices comunes con la info de cada sistema en
%los diferentes miembros y en las observaciones.

[ny nx nmembers]=size(mascara);

%Calculos auxiliares para calcular el area del sistema.
delta_lat=(lat(3:ny)-lat(1:ny-2))/2;
delta_lon=(lon(3:nx)-lon(1:nx-2))/2;
delta_lat=[delta_lat(1) delta_lat delta_lat(end)];
delta_lon=[delta_lon(1) delta_lon delta_lon(end)];

[delta_lon delta_lat]=meshgrid(delta_lon,delta_lat);

[lon lat]=meshgrid(lon,lat);



box_size=(111000^2)*(delta_lon.*cos(lat*3.14159/180).*delta_lat);

%Identificamos los sistemas que cumplen con determinados criterios de
%posicion y tamanio para cada miembro del ensamble.



for iens=1:nmembers
nsistemas=0;
max_sistema=max(max(mascara(:,:,iens))); %Busco el maximo numero de la mascara.
aux_mask=mascara(:,:,iens);
aux_geo=geo(:,:,iens);
for isis=1:max_sistema
    

    auxiliar=aux_mask==isis;
    
    area_sistema=sum(box_size(auxiliar));
    
    centroide_lat=mean(lat(auxiliar));
    
    if(area_sistema > 3.14*(400000^2) )
        if( centroide_lat > 20 | centroide_lat < -20)
        
       nsistemas=nsistemas+1;
       
       sis_area{iens}(nsistemas)=area_sistema;
       
       min_geo{iens}(nsistemas)=min(aux_geo(auxiliar));
       mean_geo{iens}(nsistemas)=mean(aux_geo(auxiliar));
       
       cent_lat{iens}(nsistemas)=centroide_lat;
       cent_lon{iens}(nsistemas)=mean(lon(auxiliar));
       
       aux_mask(auxiliar)=nsistemas;
        else
            
        aux_mask(auxiliar)=0;
        end
        
    else
      aux_mask(auxiliar)=0;
    end
    
    
end

if(nsistemas ==0) %No encontre ningun sistema que cumpla el criterio de area
   sis_area{iens}=NaN;
   min_geo{iens}=NaN;
   mean_geo{iens}=NaN;
   min_geo{iens}=NaN;
   cent_lat{iens}=NaN;
   cent_lon{iens}=NaN;
end

   

mascara(:,:,iens)=aux_mask;
end




%Repito para la observacion.

nsistemas=0;
max_sistema=max(max(mascarao)); %Busco el maximo numero de la mascara.
aux_mask=mascarao;
aux_geo=geoo;

for isis=1:max_sistema
    

    auxiliar=aux_mask==isis;
    
    area_sistema=sum(box_size(auxiliar));
    
    centroide_lat=mean(lat(auxiliar));
    
    if(area_sistema > 3.14*(400000^2) )
        if( centroide_lat > 20 | centroide_lat < -20)
        
       nsistemas=nsistemas+1;
       
       sis_areao(nsistemas)=area_sistema;
       
       min_geoo(nsistemas)=min(aux_geo(auxiliar));
       mean_geoo(nsistemas)=mean(aux_geo(auxiliar));
       
       cent_lato(nsistemas)=centroide_lat;
       cent_lono(nsistemas)=mean(lon(auxiliar));
       
       aux_mask(auxiliar)=nsistemas;
        else
            
        aux_mask(auxiliar)=0;
        end
        
    else
      aux_mask(auxiliar)=0;
    end

end

if(nsistemas ==0) %No encontre ningun sistema que cumpla el criterio de area
   sis_areao=NaN;
   min_geoo=NaN;
   mean_geoo=NaN;
   min_geoo=NaN;
   cent_lato=NaN;
   cent_lono=NaN;
end



mascarao=aux_mask;
   

%Ahora unificamos la informacion disponible en los diferentes miembros y
%las observaciones.

%Vamos a definir diferentes matrices con tantas filas como sistemas
%(observados y/o pronosticados) y tantas columnas como miembros hay en el
%ensamble.
%La matriz deteccion tendra 1 si el miembro j capturo el sistema i y 0 de
%lo contrario. 
%Los vectores lat_ref y lon_ref son las lat y lon de referencia de los
%sistemas, para los observados los centroides de los sistemas observados en
%el caso que sean solo pronosticados los centroides en el primer miembro
%donde se detecto su presencia.
%Para asumir que 2 sistemas son el mismo se busca el sistema mas cercano y
%ademas que la distancia no supere un determinado umbral.


%Inicializamos en base a los sistemas observados.
if( sum(~isnan(cent_lato)) ~= 0)
nobs=length(cent_lato);
else
nobs=0
end

%Utilizamos la estructura sistemas para guardar la info sobre los sistemas.
sistemas.size=NaN(nobs,nmembers);      %Array con el tamanio de cada sistema en cada miembro del ensamble.
sistemas.lat=NaN(nobs,nmembers);         %Latitud de cada sistema en cada miembro del ensamble.
sistemas.lon=NaN(nobs,nmembers);         %Longitud de cada sistema en cada miembro del ensamble.
sistemas.mingeo=NaN(nobs,nmembers);      %Minimo de geopotencial de cada sistema en cada miembro del ensamble.
sistemas.meangeo=NaN(nobs,nmembers);     %Promedio de geopotencial en cada sistema y en cada miembro del ensamble.

sistemas.ref_lat=cent_lato;              %Latitud de referencia usada para hacer el matching de los sistemas.
sistemas.ref_lon=cent_lono;              %Longitud de referencia.
sistemas.nref=length(cent_lato);
sistemas.ref_size=sis_areao;

sistemas.isfor=zeros(nobs,nmembers);   %Matriz que indica la presencia del sistema en los pronosticos.
sistemas.isobs=ones(nobs,1);           %Vector que indica si el sistema en cuestion fue observado o no.

%Comienza el macheo
%Busco en cada miembro del ensamble.
for iens=1:nmembers
    
    %Hago un loop sobre los sistemas que detecto el miembro iens.
    nsis=sum(~isnan(sis_area{iens}));
    if(nsis > 0)
       for isis=1:nsis
       %Para cada sistema calculo la distancia a los sistemas conocidos.

       if( sistemas.nref > 0)
          numero_sis=0;
          min_dist=umbral_distancia;
          for isisref=1:sistemas.nref
              %Calculo la distancia entre el sistema ambos sistemas.
                 dist=distll_fun(cent_lon{iens}(isis),cent_lat{iens}(isis),sistemas.ref_lon(isisref),sistemas.ref_lat(isisref));
                 if( dist < min_dist)
                    numero_sis=isisref; %Numero de sistema de referencia con el cual se produjo el macheo.
                 end   
          end
          %Hasta aca recorri todos los sistemas en la lista de sistemas
          %detectados. Existen dos posibilidades. Encontre un sistema
          %cercano en la lista o no lo encontre.
          if( numero_sis > 0) %Entonces encontre un sistema cercano.
             sistemas.isfor(numero_sis,iens)=1;
             sistemas.size(numero_sis,iens)=sis_area{iens}(isis);      
             sistemas.lat(numero_sis,iens)=cent_lat{iens}(isis);         
             sistemas.lon(numero_sis,iens)=cent_lon{iens}(isis);        
             sistemas.mingeo(numero_sis,iens)=min_geo{iens}(isis);     
             sistemas.meangeo(numero_sis,iens)=mean_geo{iens}(isis);     
          else %Si numero_sis es 0 quiere decir que no encontre el sistema en la lista de referencia.
               %ergo tengo que agregar este sistema a la lista!
             sistemas.nref=sistemas.nref+1; 
             ultimo=sistemas.nref;
             sistemas.ref_lat(ultimo)=cent_lat{iens}(isis);
             sistemas.ref_lon(ultimo)=cent_lon{iens}(isis);
             sistemas.isobs(ultimo)=0;   %Sino estaba en la lista de referencia es porque no habia sido observado.
             sistemas.ref_area(ultimo)=sis_area{iens}(isis);
             
             
             %Inicializo la nueva fila en las matrices
             sistemas.isfor(ultimo,:)=0;
             sistemas.size(ultimo,:)=NaN;      
             sistemas.lat(ultimo,:)=NaN;         
             sistemas.lon(ultimo,:)=NaN;        
             sistemas.mingeo(ultimo,:)=NaN;     
             sistemas.meangeo(ultimo,:)=NaN;  
             
             %Completo el valor correspondiente de la nueva fila para el
             %miembro actual.
             sistemas.isfor(ultimo,iens)=1;
             sistemas.size(ultimo,iens)=sis_area{iens}(isis);      
             sistemas.lat(ultimo,iens)=cent_lat{iens}(isis);         
             sistemas.lon(ultimo,iens)=cent_lon{iens}(isis);        
             sistemas.mingeo(ultimo,iens)=min_geo{iens}(isis);     
             sistemas.meangeo(ultimo,iens)=mean_geo{iens}(isis);  

          end
          
          
          
          
       end
       end
    end
    
    
    
    
end



%pcolor(mascarao)

%figure
%pcolor(mascara(:,:,1))
















