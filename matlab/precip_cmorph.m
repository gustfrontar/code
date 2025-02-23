clear all
close all
%*************************************************************************
%Este script lee los datos CMORPH en su formato original
%Los datos CMORPH vienen con una resolución de 0.25
%Los datos originales están cada 3 horas. 
%*************************************************************************
% Juan Ruiz - 2006
%*************************************************************************
%PARAMETROS MODIFICABLES

%Ruta del directorio donde estan los archivos CMORPH.
archivo='D:/SALLJEXprecip/'

%Hora de acumulación

hora_acum=12; %Como los datos CMORPH vienen cada 3 horas podemos acumular
%a distintas horas. Las 12 sería el comparable con los datos SALLJEX.
%Definimos los bordes del dominio que vamos a usar.
%Este es el dominio que nos vamos a quedar. 

lat_min=-60;
lat_max=0;
lon_min=-80; 
lon_max=-40;

%Definimos el tamaño del pixel o caja (en grados) (Debe ser tal que entre
%un numero entero de cajas en un grado.
%Las cajas son cuadradas.

box_size=1;

%Ruta completa del archivo binario de salida
archivo_sal=strcat('D:/SALLJEXprecip/precip_cmorph_',num2str(box_size),'x',num2str(box_size),'.bin')

%Ruta completa del archivo matlab de salida
archivo_mat=strcat('D:/SALLJEXprecip/precip_cmorph_',num2str(box_size),'x',num2str(box_size),'1.mat')

%Otras características del CMORPH

delta_t=3;  %Intervalo de tiempo entre los datos en horas.
hora_ini=0; %hora inicial de cada archivo. El primer dato del archivo es el
%acumulado en las 3 horas previas correspondiente a las 00 UTC.
nx_c=1440;  %Número de puntos en x en el archivo original
ny_c=480;   %Número de puntos en y en el archivo origianl
dx_c=0.25;  %Resolución en grados de los datos originales.
lat_ini=-59.875 %valor de latitud de donde arrancan los datos.
lon_ini=0.125   %valor de longitud de donde arrancan los datos.

%Fecha de inicio.
%El formato es dd-mmm-yyyy (el mes son las 3 primeras letras en ingles)

ini_date='07-Dec-2002';
end_date='22-Dec-2002';

%Valor del undef
%Los datos de entrada no tienen undef, pero el control de calidad puede
%generar algunos undef si considera que algun dato es malo

undef=-9999;

%*************************************************************************
%La idea general es que vamos a abrir 1 archivo para leer y luego, guardar
%parte de la informacion para que sea usada en el paso siguiente y asi
%poder acumular los datos de precipitacion de 1 día por ejemplo de 12 UTC
%a 12 UTC.
%Para ubicar las cajas vamos a generar una matriz que tenga las latitudes
%de cada punto y una con las longitudes de cada punto, el finde se hará
%utilizando dichas matrices.
%*************************************************************************
%Abrimos el archivo de salida 

bin = fopen(archivo_sal,'w');

%Calculamos el número de cajas
nx=(lon_max-lon_min)/box_size;
ny=(lat_max-lat_min)/box_size;

%Genero un número que identifica la fecha de hoy.
date_num=datenum(ini_date);
ini_date_num=datenum(ini_date);

%Hago lo mismo para la fecha de fin
end_date_num=datenum(end_date);
old_file=-1;

%Definimos la matriz de latitud y la matriz de longitud necesaria para
%recortar nuestro dominio.
    for i=1:ny_c
    latitud(i)=lat_ini+dx_c*(ny_c-i); %Las latitudes est�n al rev�s en los datos CMORPH.
    end
    for i=1:nx_c
        if(lon_ini+dx_c*(i-1) <= 180 )
    longitud(i)=lon_ini+dx_c*(i-1);
        else
    longitud(i)=lon_ini+dx_c*(i-1)-360;
        end
    end
    %Recortamos el dominio para quedarnos con una subregi�n y que eso
    %acelere las cuentas.
    i_lat=find( latitud <= lat_max & latitud >= lat_min);
    i_lon=find( longitud <= lon_max & longitud >= lon_min );
    latitud_regional=latitud(i_lat);
    longitud_regional=longitud(i_lon); 

    %COMIENZA EL CICLO EN TIEMPO.
while (date_num <= end_date_num)
    
    %Genero un vector con las componentes de la fecha. yyyy mm dd
    %genero un número con la fecha en formato yyyymmdd
    %lo paso a string y lo uso para generar el nombre del archivo que vamos
    %a abrir.
    
    date_vec=datevec(date_num);
    fecha=date_vec(1)*10000+date_vec(2)*100+date_vec(3);
    fecha2=num2str(fecha);
    open_file=strcat(archivo,fecha2,'_3hr-025deg_cpc+comb.')
    new_file=fopen(open_file,'r','b');   
    %Preguntamos si existe el archivo de este día y el del día anterior.
    
if(old_file ~= -1 & new_file ~= -1) %SI LOS ARCHIVOS EXISTEN LAS MATRICES SE LLENAN CON DATOS.
a=1
    %primero leemos todo lo que hay en el archivo.
    %El archivo consta de 8 tiempos (est�n cada 3 horas y cubren 24 horas).
    %consta ademas de 2 campos con dimensiones nx,ny
    %uno es la estimacion cruda de microondas sin advecci�n ni morphing
    %el segundo es la estimacion de precipitaci�n usando adveccion y
    %morphing.
    %los datos estan en mm/hora 
    %Vamos a leer el new_file, el old_file se supone que lo leimos en el
    %paso anterior
    
    for i=1:8 %for sobre los tiempos del archivo.
       
    aux_read(:,:,i)=fread(new_file,[nx_c ny_c],'single')';  %La estimacion sin morphing no me interesa por el momento.
    new_data(:,:,i)=fread(new_file,[nx_c ny_c],'single')';
    
    end
    
    new_data_regional=new_data(i_lat,i_lon,:); %Recorto todos los tiempos del archivo.
    new_data_regional(find(new_data_regional==undef))=NaN; %Pongo en NaN los undef para poder calcular el promedio.
    
    %Acumulo los tiempos para obtener el acumulado en 24 horas.
    
    ntiempos=hora_acum/delta_t+1; %Numero de tiempos del archivo de hoy que vamos a usar para el acumulado.   
    acum_data_new=squeeze(nanmean(new_data_regional(:,:,1:ntiempos),3))*ntiempos*delta_t; %Sumamos y pasamos a mm
    acum_data_new2=squeeze(nanmean(new_data_regional(:,:,ntiempos:8),3))*ntiempos*delta_t; %Sumo el resto de los tiempos para el dia siguiente
    daily_data=acum_data_new+acum_data_old; %Obtengo el acumulado cada 24 horas.
    acum_data_old=acum_data_new2; %Guardo el acum_data_old para el d�a siguiente.
    
    %comienza el ciclo sobre las cajas
    %Si la resoluci�n a la que quiero interpolar es mucho menor que la
    %resolucion original uso promedio por cajas.
    
      for ilat=1:ny
          for ilon=1:nx
         %Defino los bordes de la caja.
         lat_s=lat_min+(ilat-1)*box_size;
         lat_n=lat_s+box_size;
         lon_w=lon_min+(ilon-1)*box_size;
         lon_e=lon_w+box_size;
          
         %Busco las estaciones que están dentro de la caja.
         %*****************************************************************
         i_box_lat=find( latitud_regional < lat_n & latitud_regional >= lat_s);
         i_box_lon=find( longitud_regional < lon_e & longitud_regional >= lon_w);
         num_data(ilat,ilon)=length(i_box_lat)*length(i_box_lon);
         
         if(num_data(ilat,ilon) > 0)
         daily_field(ilat,ilon)=squeeze(nanmean(nanmean(daily_data(i_box_lat,i_box_lon),2)));
         end
         %*****************************************************************
         
         clear i_box_lat i_box_lon i_mean
                  
          end
      end
      
end %ESTE ES EL END DEL IF SOBRE LA EXISTENCIA DE LOS ARCHIVOS. SI NO EXISTEN IGUAL TENGO QUE ESCRIBIR EN EL ARCHIVO.

if  (old_file == -1 & new_file ~= -1)
         %En este caso no podemos calcular daily data pero podemos leer new
         %file para que sirva para el tiempo siguiente.
         
    for i=1:8 %for sobre los tiempos del archivo.
    aux_read(:,:,i)=fread(new_file,[nx_c ny_c],'single')';  %La estimacion sin morphing no me interesa por el momento.
    new_data(:,:,i)=fread(new_file,[nx_c ny_c],'single')';
    end
    
    new_data_regional=new_data(i_lat,i_lon,:); %Recorto todos los tiempos del archivo.
    
    %Acumulo los tiempos para obtener el acumulado en 24 horas.
    
    ntiempos=hora_acum/delta_t+1; %Numero de tiempos del archivo de hoy que vamos a usar para el acumulado.   
    acum_data_new=squeeze(mean(new_data_regional(:,:,1:ntiempos),3))*ntiempos*delta_t;   %Sumo los ntiempos primeros tiempos para el dia de hoy.
    acum_data_new2=squeeze(mean(new_data_regional(:,:,ntiempos:8),3))*ntiempos*delta_t; %Sumo el resto de los tiempos para el dia siguiente
    acum_data_old=acum_data_new2; %Guardo el acum_data_old para el d�a siguiente.
    %Creo el daily data como una matriz de undef.
    for ilat=1:ny
          for ilon=1:nx
         num_data(ilat,ilon)=undef;
         daily_field(ilat,ilon)=undef;        
          end
      end
    
end %ESTE ES EL END DEL SEGUNDO IF SOBRE LA EXISTENCIA DE LOS ARCHIVOS.
if(new_file == -1)
    %En este caso no tenemos archivo nuevo... no puedo hacer nada y en el
    %paso siguiente tampoco.
     for ilat=1:ny
         for ilon=1:nx
         num_data(ilat,ilon)=undef;
         daily_field(ilat,ilon)=undef;        
          end
     end
      
end %ESTE ES EL END DEL TERCER IF SOBRE LA EXISTENCIA DE LOS ARCHIVOS.
 
         old_file=new_file; %Paso el n�mero de archivo nuevo al viejo.
         
         
         %Asignamos el valor del campo a una variable.
         %La variable precip va a contener todos los datos de precipitacion
         %ordenados de la siguiente manera:
         % La primera dimension es el tiempo medido contando como 1 el
         % primer dia del rango de fechas seleccionado
         %la segunda dimension es la longitud (X), la tercera la latitud
         %(Y) y la cuarta dimension permite obtener o bien el valor de
         %precipitacion (si vale 1) o bien el número de datos que
         %intervinieron en el promedio (si vale 2).
         
         daily_field(find(daily_field == NaN))=undef;  %Vuelvo a poner undef donde tenia NaN.
         precip_cmo(date_num-ini_date_num+1,:,:,1)=daily_field(:,:);
         precip_cmo(date_num-ini_date_num+1,:,:,2)=num_data(:,:);
         
         %Escribimos en un archivo binario de acceso directo para leer con
         %grads.
    
         fwrite(bin,daily_field','float32',0,'ieee-le');
         fwrite(bin,num_data','float32',0,'ieee-le');
    
    
    date_num=date_num+1;
    %clear daily_data daily_field num_data %Si no borramos estas variables pueden quedar datos de dias anteriores.

end %ESTE ES EL END DEL CICLO SOBRE LAS FECHAS!!


%Guardamos todos los campos en un archivo .mat (la idea sería generar este
%archivo para todas las fuentes y tener todas las fuentes en el mismo
%formato en distintas resoluciones.

 save(archivo_mat,'precip_cmo');
