clear all
close all
%*************************************************************************
%Este script lee los datos ANA16 de un archivo de texto con el siguiente
%formato:
% yyyy mm dd hh lat lon dato (el archivo fue generado con el programa
% precip_ana16.for a partir de los datos que nos dio Brant Liebmann)
%La interpolacion se hace a una reticula arbitraria que se lee de un
%archivo mat. En este caso sera la reticula del modelo WRF.

%En particular este script calcula los acumulados para la reticula
%breeding.
%Voy a generar un set de datos corregido en donde pueda tener en cuenta la
%forma irregular de la reticula lambert del WRF.
%*************************************************************************
% Juan Ruiz - 2006 - Adaptado para interpolar a reticulas Lambert en Julio
% de 2007.


%*************************************************************************
%PARAMETROS MODIFICABLES

%Ruta completa del archivo que vamos a abrir
%archivo='/home/juan/trabajos/Tesis/precip-SALLJEX/salljex_precipitation.dat'
data=load('/home/juan/trabajos/Tesis/precip_brant/ana16/precip_ana16.dat');



%Ruta completa del archivo matlab de salida
archivo_mat='/home/juan/trabajos/Tesis/salidas_breeding/matlab/precip_ana16_breeding_corregido.mat'

%Llave para el control de calidad

control=1; %Si es 1 se hace el control, si es 0 no se hace.

%Archivo con la ubicacion en lat y lon de los puntos de la reticula WRF.
load('/home/juan/trabajos/Tesis/salidas_breeding/matlab/wrf_breeding_domain.mat') %El nombre de las variables es lat_wrf y lon_wrf 
[nx_w ny_w]=size(lat_wrf);
lon_wrf=lon_wrf-360;




%Definimos un rango horario para filtrar los datos.
%En hora UTC, todos los datos fuera de este rango horario no van a 
%ser tenidos en cuenta. 

hora_max=14;
hora_min=10;

%Fecha de inicio.
%El formato es dd-mmm-yyyy (el mes son las 3 primeras letras en ingles)

ini_date='15-Dec-2002';
end_date='15-Feb-2003';


%*************************************************************************

%Cargamos los datos del archivo en una matriz
%data(:,1) es el a??o
%data(:,2) es el mes
%data(:,3) es el d??a
%data(:,4) es la hora
%data(:,5) es la latitud
%data(:,6) es la longitud
%data(:,7) es el dato de lluvia (en el mejor de los casos =)

%data=load(archivo,'ascii');


%Genero un n??mero que identifica la fecha de hoy.

date_num=datenum(ini_date);
ini_date_num=datenum(ini_date);
 

%Hago lo mismo para la fecha de fin
end_date_num=datenum(end_date);

    
%SE INICIA EL CICLO SOBRE LAS FECHAS!!

pp_sall=NaN(nx_w,ny_w,end_date_num-ini_date_num+1);
num_data=NaN(nx_w,ny_w,end_date_num-ini_date_num+1);
sig_data=NaN(nx_w,ny_w,end_date_num-ini_date_num+1);
cero_flag=NaN(nx_w,ny_w,end_date_num-ini_date_num+1);

while (date_num <= end_date_num)
    %Genero un vector con las componentes de la fecha. yyyy mm dd
    
    date_vec=datevec(date_num);
    
    %Buscamos las filas de data que corresponden a la fecha y al rango
    %horario deseado.
    
    i_daily=( data(:,3) == date_vec(3) & data(:,2) == date_vec(2) & data(:,1) == date_vec(1) & data(:,7) >= 0 );
    
    %Genero una matriz con los datos del dia (lat lon lluvia)
    aux=data(i_daily,5:7); %primero genero una variable aux
    
    clear i_daily %Tengo que borrar i_daily para que no se cuelen datos de otros d??as.
    
    n=length(aux(:,1));     %calculo el numero de datos (con repeticiones).
    
    
    %**********************************************************************
    %CONTROL DE CALIDAD PREVIO AL CALCULO

    %Voy a chequear que no existan datos repetidos!
    %Si existe dato repetido nos quedamos con el que informa lluvia.
    %Si los 2 informan lluvia y son distintos:
    %- si difieren en m??s de 2.5 mm se coloca un undef
    %- si difieren en menos (a veces es un problema de redondeo de las
    %cifras decimales) se promedian ambos datos.
    %- si el valor de lluvia es superior a 300 mm entonces se descarta el
    %dato.
    
    if (control == 1) %El control se hace si la variable control es 1.
    daily_data=NaN(n,3);
    daily_data(1,:)=aux(1,:);
    
    j=2;
    alerta=0;
    
    for i=2:n
        
        repetido=0;
        ir=(daily_data(:,1) == aux(i,1) & daily_data(:,2) == aux(i,2) );
        if(sum(ir) == 1 )
            repetido=1;
            repeticion(1,:)=daily_data(ir,:);
            repeticion(2,:)=aux(i,:);
            repeticion
            if(daily_data(ir,3)==0 & aux(i,3) > 0 )
                daily_data(ir,3)=aux(i,3); %Si el dato repetido no es 0 entonces me quedo con el que no es 0.
            end
            if(daily_data(ir,3) ~= 0 & aux(i,3) ~= 0 & daily_data(ir,3) ~= aux(i,3))
               if(abs(aux(i,3)-daily_data(ir,3)) > 2.5)
                   %Inconsistencia grave
                   daily_data(ir,3)=-999;
                   alerta=alerta+1;
               else
                   daily_data(ir,3)=(daily_data(ir,3)+aux(i,3))/2;
               
               end
            end
                     
        end


        %Elimino los datos repetidos y pido que el valor de la
        %precipitacion acumulada sea inferior a 400 mm.
        if(repetido ~= 1 && aux(i,3) <= 400  )
            
            daily_data(j,:)=aux(i,:);
            j=j+1;
        end           
    end
    
    else

        daily_data=aux;
    
    end
    
    %**********************************************************************
    %ACA DEBE COMENZAR EL CALCULO DE LA PRECIPITACION POR AREAS.
    
    %comienza el ciclo sobre las cajas
    
    for ilat=2:ny_w-1
       for ilon=2:nx_w-1
           
         %Defino la latitud y longitud de los bordes de la caja que no es
         %cuadrada sino que tiene forma de rombo y la orientacion del rombo
         %depende de cuan cerca este el punto al centro del dominio (justo
         %en el centro es un cuadrado).
         %Estas esquinas definen un area centrada en el punto de reticula
         %i,j que tiene el tamanio del area representada por dicho punto y
         %su mimsa forma.
         
         lat1=0.25*(lat_wrf(ilat,ilon)+lat_wrf(ilat+1,ilon)+lat_wrf(ilat+1,ilon-1)+lat_wrf(ilat,ilon-1));
         lon1=0.25*(lon_wrf(ilat,ilon)+lon_wrf(ilat+1,ilon)+lon_wrf(ilat+1,ilon-1)+lon_wrf(ilat,ilon-1));
        
         lat2=0.25*(lat_wrf(ilat,ilon)+lat_wrf(ilat+1,ilon)+lat_wrf(ilat+1,ilon+1)+lat_wrf(ilat,ilon+1));
         lon2=0.25*(lon_wrf(ilat,ilon)+lon_wrf(ilat+1,ilon)+lon_wrf(ilat+1,ilon+1)+lon_wrf(ilat,ilon+1));
                  
         lat3=0.25*(lat_wrf(ilat,ilon)+lat_wrf(ilat,ilon-1)+lat_wrf(ilat-1,ilon-1)+lat_wrf(ilat-1,ilon));
         lon3=0.25*(lon_wrf(ilat,ilon)+lon_wrf(ilat,ilon-1)+lon_wrf(ilat-1,ilon-1)+lon_wrf(ilat-1,ilon));
                           
         lat4=0.25*(lat_wrf(ilat,ilon)+lat_wrf(ilat,ilon+1)+lat_wrf(ilat-1,ilon)+lat_wrf(ilat-1,ilon+1));
         lon4=0.25*(lon_wrf(ilat,ilon)+lon_wrf(ilat,ilon+1)+lon_wrf(ilat-1,ilon)+lon_wrf(ilat-1,ilon+1));
         
         %Ahora a partir de las rectas definidas por los limites de la caja
         %romboidal calculo como se proyecta la latitud de cada estacion
         %sobre dichas rectas y en base a eso determino si la estacion
         %pertenece o no pertenece. 
         
         lat_estacion=daily_data(:,1);
         lon_estacion=daily_data(:,2);
         
         %En las rectas que van aprox N-S tengo que tener cuidado cerca del
         %centro del dominio porque se pueden volver verticales y entonces
         %lon3-lon1 seria 0, en ese caso la recta es un valor de latitud
         %constante.
         
         
         lon_a=lon1+(lat_estacion-lat1)*(lon3-lon1)/(lat3-lat1);
         lon_b=lon2+(lat_estacion-lat2)*(lon4-lon2)/(lat4-lat2);
         
         lat_c=lat1+(lon_estacion-lon1)*(lat2-lat1)/(lon2-lon1);
         lat_d=lat3+(lon_estacion-lon3)*(lat4-lat3)/(lon4-lon3);

         %Busco las estaciones que est??n dentro de la caja.
         %*****************************************************************
         i_box=( lon_estacion < lon_b & lon_estacion >= lon_a & lat_estacion < lat_c & lat_estacion >= lat_d & daily_data(:,3) >= 0 );

        
         num_data(ilat,ilon,date_num-ini_date_num+1)=sum(i_box);                        %Numero de datos en la caja
         
         
         
         if(sum(i_box) > 0)
         %Si el numero de datos en la caja no es 0 hacemos el promedio.
         sig_data(ilat,ilon,date_num-ini_date_num+1)=std(squeeze(daily_data(i_box,3))); %Sigma dentro de la caja
         pp_sall(ilat,ilon,date_num-ini_date_num+1)=nanmean(squeeze(daily_data(i_box,3)));
  
         end
         %*****************************************************************     
         clear i_box aux_lat aux_lon aux_pp aux_data
         
         
          end
      end
      
    
    if(control == 1)     
    alerta
    date_vec
    end

%Escribimos las salidas en formato GrADS.
aux_data=pp_sall(:,:,date_num-ini_date_num+1);
aux_num=num_data(:,:,date_num-ini_date_num+1);
aux_sig=sig_data(:,:,date_num-ini_date_num+1);
open_file=(strcat('./PRECIP_ANA16_',datestr(date_num,'yyyymmdd'),'.bin'));
bin = fopen(open_file,'w');

 fwrite(bin,aux_data(:,:)','float32',0,'ieee-le');
 fwrite(bin,aux_num(:,:)','float32',0,'ieee-le'); 
 fwrite(bin,aux_sig(:,:)','float32',0,'ieee-le');
 

fclose(bin);



    
    date_num=date_num+1;
    clear aux daily_data daily_field i_daily %Si no borramos estas variables pueden quedar datos de dias anteriores.
end %ESTE ES EL END DEL CICLO SOBRE LAS FECHAS!!


%Guardamos todos los campos en un archivo .mat
pp_ana16=pp_sall;


 save(archivo_mat,'pp_ana16','num_data','sig_data');
