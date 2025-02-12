clear all
close all
%*************************************************************************
%Este script lee los datos CMORPH en su formato original
%y los interpola por vecino mas cercano a la lista de estaciones
%disponibles que tenemos del SUPERENSEMBLE.
%*************************************************************************
% Juan Ruiz - 2006
%*************************************************************************
%PARAMETROS MODIFICABLES

%Ruta del directorio donde estan los archivos CMORPH.
archivo='d:/trabajos/TrabajoSLAF/precipitacion/cmorph2005/'

%Definimos el tamaño del pixel o caja (en grados) (Debe ser tal que entre
%un numero entero de cajas en un grado.
%Las cajas son cuadradas.

hora_acum=12; %Como los datos CMORPH vienen cada 3 horas podemos acumular
%a distintas horas. Las 12 sería el comparable con los datos SALLJEX y los 
%datos de pluviometros que por lo general se miden a esta hora.
%Definimos los bordes del dominio que vamos a usar.
%Este es el dominio que nos vamos a quedar. 

hora=num2str(hora_acum);


%Hora de acumulación


lat_min=-44;
lat_max=5;
lon_min=-90; 
lon_max=-30;


%Otras características del CMORPH

delta_t=3;  %Intervalo de tiempo entre los datos en horas.
nx_c=1440;  %Número de puntos en x en el archivo original
ny_c=480;   %Número de puntos en y en el archivo origianl
dx_c=0.25;  %Resolución en grados de los datos originales.
lat_ini=-59.875 %valor de latitud de donde arrancan los datos.
lon_ini=0.125   %valor de longitud de donde arrancan los datos.
hora_ini=0; %hora inicial de cada archivo. El primer dato del archivo es el
undef=-9999;

%acumulado en las 3 horas previas correspondiente a las 00 UTC.

%Fecha de inicio.
%El formato es dd-mmm-yyyy (el mes son las 3 primeras letras en ingles)

ini_date='01-Oct-2005';
end_date='29-Nov-2005';

%*************************************************************************
%La idea general es que vamos a abrir 1 archivo para leer y luego, guardar
%parte de la informacion para que sea usada en el paso siguiente y asi
%poder acumular los datos de precipitacion de 1 día por ejemplo de 12 UTC
%a 12 UTC.
%Para ubicar las cajas vamos a generar una matriz que tenga las latitudes
%de cada punto y una con las longitudes de cada punto, el finde se hará
%utilizando dichas matrices.
%*************************************************************************

%Cargo la lista con las estaciones a donde vamos a interpolar los datos
%CMORPH.
%La primera columna es el ID de estacion, la segunda es la lat y la tercera
%la lon.
lista=load(strcat(archivo,'lista_superensemble.txt'));

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
    open_file=strcat(archivo,fecha2,'_3hr-025deg_cpc+comb')
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
    acum_data_new2=squeeze(nanmean(new_data_regional(:,:,ntiempos+1:8),3))*(8-ntiempos)*delta_t; %Sumo el resto de los tiempos para el dia siguiente
    daily_data=acum_data_new+acum_data_old; %Obtengo el acumulado cada 24 horas.
    acum_data_old=acum_data_new2; %Guardo el acum_data_old para el d�a siguiente.
    
    %Daily data es la variable que tiene la precipitacion acumulada en 24
    %horas para la fecha en cuestion.
    
    %ahora tengo que interpolar el campo CMORPH de pp acumulada a los
    %puntos y lo hago por la tecnica de vecino mas cercano aprovechando la
    %alta resolucion de los CMORPH. 
    
    %abro el archivo de salida
    fid = fopen(strcat(archivo,fecha2,'cmorph_12z'), 'wt');
    for iest=1:length(lista(:,1))
        
      %Siempre asumo que i_cmo y j_cmo son únicos (que no esta equidistante
      %a 2 puntos)
      i_cmo=find( abs(latitud_regional-lista(iest,2))==min(abs(latitud_regional-lista(iest,2))));
      j_cmo=find( abs(longitud_regional-lista(iest,3))==min(abs(longitud_regional-lista(iest,3))));
      
      p24(iest,1:3)=lista(iest,:);
      %Verifico que el vecino m�s cercano no sea muy lejano :)
      if(min(abs(latitud_regional-lista(iest,2)))>=1 | min(abs(longitud_regional-lista(iest,3)))>=1)
      p24(iest,4)=999.0;
      else
      p24(iest,4)=daily_data(i_cmo(1),j_cmo(1));
      end
      
      %Escribo en el archivo de salida.
      fprintf(fid, '%s %7.0f %7.2f %7.2f %7.2f\n',fecha2,p24(iest,:));
      clear i_cmo j_cmo
    end
    %cierro el archivo de salida (1 archivo por dia)
    fclose(fid)
    
    

                  
      
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
    acum_data_new2=squeeze(mean(new_data_regional(:,:,ntiempos+1:8),3))*(8-ntiempos)*delta_t; %Sumo el resto de los tiempos para el dia siguiente
    acum_data_old=acum_data_new2; %Guardo el acum_data_old para el d�a siguiente.

    %abro el archivo de salida
    fid = fopen(strcat(archivo,fecha2,'cmorph_12z'), 'wt');
    for iest=1:length(lista(:,1))
        
      p24(iest,1:3)=lista(iest,:);
      p24(iest,4)=999.0;
      
      %Escribo en el archivo de salida.
      fprintf(fid, '%s %7.0f %7.2f %7.2f %7.2f\n',fecha2,p24(iest,:));

    end
    %cierro el archivo de salida (1 archivo por dia)
    fclose(fid)
    
    
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
     
    %abro el archivo de salida
    fid = fopen(strcat(archivo,fecha2,'cmorph_12z'), 'wt');
    for iest=1:length(lista(:,1))
        
      p24(iest,1:3)=lista(iest,:);
      p24(iest,4)=999.0;
      
      %Escribo en el archivo de salida.
      fprintf(fid, '%s %7.0f %7.2f %7.2f %7.2f\n',fecha2,p24(iest,:));

    end
    %cierro el archivo de salida (1 archivo por dia)
    fclose(fid)
     
      
end %ESTE ES EL END DEL TERCER IF SOBRE LA EXISTENCIA DE LOS ARCHIVOS.
 
         old_file=new_file; %Paso el n�mero de archivo nuevo al viejo.
         
         
    
    
    date_num=date_num+1;
    %clear daily_data daily_field num_data %Si no borramos estas variables pueden quedar datos de dias anteriores.

    clear p24
end %ESTE ES EL END DEL CICLO SOBRE LAS FECHAS!!

