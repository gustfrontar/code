clear all
close all
%*************************************************************************
%Este script lee los datos salljex de un archivo de texto con el siguiente
%formato:
% yyyy mm dd hh lat lon dato
%los datos contenidos en ese archivo fueron filtrados con un programa
%fortran y no contienen undef ni contienen los famosos 0 unknown.
%*************************************************************************
% Juan Ruiz - 2006


%*************************************************************************
%PARAMETROS MODIFICABLES

%Ruta completa del archivo que vamos a abrir
archivo='/mnt/windows/precip-SALLJEX/salljex_precipitation.dat'

%Ruta completa del archivo binario de salida
archivo_sal='/pronostico/breeding/precipitacion/precip_salljex_1x1.bin'

%Ruta completa del archivo matlab de salida
archivo_mat='/pronostico/breeding/precipitacion/precip_salljex_1x1.mat'

%Llave para el control de calidad

control=1; %Si es 1 se hace el control, si es 0 no se hace.


%Definimos los bordes del dominio que vamos a usar.
lat_min=-60;
lat_max=0;
lon_min=-80;
lon_max=-40;

%Definimos el tamaño del pixel o caja (en grados) (Debe ser tal que entre
%un numero entero de cajas en un grado.
%Las cajas son cuadradas.

box_size=1;

%Definimos un rango horario para filtrar los datos.
%En hora UTC, todos los datos fuera de este rango horario no van a 
%ser tenidos en cuenta. 

hora_max=14;
hora_min=10;

%Fecha de inicio.
%El formato es dd-mmm-yyyy (el mes son las 3 primeras letras en ingles)

ini_date='15-Nov-2002';
end_date='15-Feb-2003';

%Valor del undef
%Los datos de entrada no tienen undef, pero el control de calidad puede
%generar algunos undef si considera que algun dato es malo

undef=-9999;

%*************************************************************************


%Abrimos el archivo de salida 

bin = fopen(archivo_sal,'w');

%Cargamos los datos del archivo en una matriz
%data(:,1) es el año
%data(:,2) es el mes
%data(:,3) es el día
%data(:,4) es la hora
%data(:,5) es la latitud
%data(:,6) es la longitud
%data(:,7) es el dato de lluvia (en el mejor de los casos =)

data=load(archivo,'ascii');

%Calculamos el número de cajas

nx=(lon_max-lon_min)/box_size;
ny=(lat_max-lat_min)/box_size;


%Genero un número que identifica la fecha de hoy.

date_num=datenum(ini_date);
ini_date_num=datenum(ini_date);
 

%Hago lo mismo para la fecha de fin
end_date_num=datenum(end_date);

%Primero corrigo las latitudes que están mal
%Esto lo tengo que hacer porque una estacion tiene los datos de latitud
%ingresados incorrectament.

corr_lat=find( data(:,5) < -90 );
data(corr_lat,5)=data(corr_lat,5)/100;
    
%SE INICIA EL CICLO SOBRE LAS FECHAS!!

while (date_num <= end_date_num)
    %Genero un vector con las componentes de la fecha. yyyy mm dd
    
    date_vec=datevec(date_num);
    
    %Buscamos las filas de data que corresponden a la fecha y al rango
    %horario deseado.
    
    i_daily=find( data(:,1) == date_vec(1) & data(:,2) == date_vec(2) & data(:,3) == date_vec(3) & data(:,4) <= hora_max & data(:,4) >= hora_min );
    
    %Genero una matriz con los datos del dia (lat lon lluvia)
    aux=data(i_daily,5:7); %primero genero una variable aux
    
    clear i_daily %Tengo que borrar i_daily para que no se cuelen datos de otros días.
    
    n=length(aux(:,1));     %calculo el numero de datos (con repeticiones).
    
    
    %**********************************************************************
    %CONTROL DE CALIDAD PREVIO AL CALCULO

    %Voy a chequear que no existan datos repetidos!
    %Si existe dato repetido nos quedamos con el que informa lluvia.
    %Si los 2 informan lluvia y son distintos:
    %- si difieren en más de 2.5 mm se coloca un undef
    %- si difieren en menos (a veces es un problema de redondeo de las
    %cifras decimales) se promedian ambos datos.
    
    if (control == 1) %El control se hace si la variable control es 1.
    
    daily_data(1,:)=aux(1,:);
    j=2;
    alerta=0;
    for i=2:n
        repetido=0;

        n_filt=length(daily_data(:,1));
        for ii=1:n_filt
           if(aux(i,1) == daily_data(ii,1) && aux(i,2) == daily_data(ii,2))
               repetido=1; %Me fijo si el dato esta repetido para sacarlo y no promediarlo 2 veces.
               if(aux(i,3) ~= daily_data(ii,3))  
                   if(aux(i,3) >= daily_data(ii,3) && aux(i,3) == 0) 
                       daily_data(ii,3)=aux(i,3); %Me quedo con reporte de lluvia e ignoro el 0. La inconsistencia no es tan grave porque esto pasa frecuentemente en los datos del GTS.
                   end              
                   if(aux(i,3) ~= 0 && daily_data(ii,3) ~= 0)
                       %aux(i,:)
                       %daily_data(ii,:)
                       if (abs(aux(i,3)-daily_data(ii,3)) > 2.5)
                       daily_data(ii,3)=undef; %En este caso es una inconsistencia grave, le coloco un undef al dato.
                       alerta=alerta+1; %Me fijo si el dato de lluvia coincide en las estaciones repetidas.
                       else
                       daily_data(ii,3)=(daily_data(ii,3)+aux(i,3))/2;
                       end                    
                   end
               end
           end
        end
        %Elimino los datos repetidos y pido que el valor de la
        %precipitacion acumulada sea inferior a 300 mm.
        if(repetido ~= 1 && aux(i,3) <= 300 )
            
            daily_data(j,:)=aux(i,:);
            j=j+1;
        end           
    end
    
    else
        
        daily_data=aux;
    
    end
    
    %**********************************************************************
    %ACA DEBE COMENZAR EL CALCULO DE LA PRECIPITACION POR AREAS.
    
    %Genero el campo inicial como una matriz de undef.
    
    daily_field=ones(ny,nx)*undef; %variable para guardar los promedios de la lluvia.
    num_data=zeros(ny,nx);         %variable para guardar la cantidad de datos que intervino en cada promedio.
    
    %comienza el ciclo sobre las cajas
    
      for ilat=1:ny
          for ilon=1:nx
         %Defino los bordes de la caja.
         lat_s=lat_min+(ilat-1)*box_size;
         lat_n=lat_s+box_size;
         
         lon_w=lon_min+(ilon-1)*box_size;
         lon_e=lon_w+box_size;
          
         %Busco las estaciones que están dentro de la caja.
         %*****************************************************************
         %aux_lat=squeeze(daily_data(:,1));
         %aux_lon=squeeze(daily_data(:,2));
         %aux_pp=squeeze(daily_data(:,3));
         %i_box=find( aux_lat <= lat_n & aux_lat > lat_s & aux_lon <= lon_e & aux_lon > lon_w & aux_pp ~= undef);
         i_box=find( daily_data(:,1) < lat_n & daily_data(:,1) >= lat_s & daily_data(:,2) < lon_e & daily_data(:,2) >= lon_w & daily_data(:,3) ~= undef);
         num_data(ilat,ilon)=length(i_box);
         if(length(i_box) > 0)
         aux_data=squeeze(daily_data(i_box,3));
         %Si el numero de datos en la caja no es 0 hacemos el promedio.
         daily_field(ilat,ilon)=mean(aux_data);
         end
         %*****************************************************************

         
        
                  
         clear i_box aux_lat aux_lon aux_pp aux_data
         
         
          end
      end
      
         %Asignamos el valor del campo a una variable.
         %La variable precip va a contener todos los datos de precipitacion
         %ordenados de la siguiente manera:
         % La primera dimension es el tiempo medido contando como 1 el
         % primer dia del rango de fechas seleccionado
         %la segunda dimension es la longitud (X), la tercera la latitud
         %(Y) y la cuarta dimension permite obtener o bien el valor de
         %precipitacion (si vale 1) o bien el número de datos que
         %intervinieron en el promedio (si vale 2).
      
         precip_sall(date_num-ini_date_num+1,:,:,1)=daily_field(:,:);
         precip_sall(date_num-ini_date_num+1,:,:,2)=num_data(:,:);
         
         %Escribimos en un archivo binario de acceso directo para leer con
         %grads.
    
         fwrite(bin,daily_field','float32',0,'ieee-le');
         fwrite(bin,num_data','float32',0,'ieee-le');
    
    
    if(control == 1)     
    alerta
    date_vec
    end
    
    date_num=date_num+1;
    clear aux daily_data daily_field num_data i_daily %Si no borramos estas variables pueden quedar datos de dias anteriores.
end %ESTE ES EL END DEL CICLO SOBRE LAS FECHAS!!


%Guardamos todos los campos en un archivo .mat (la idea sería generar este
%archivo para todas las fuentes y tener todas las fuentes en el mismo
%formato en distintas resoluciones.

 save(archivo_mat,'precip_sall');
