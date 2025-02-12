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
archivo='/media/data/data/satelite/CMORPH/'

%Definimos el tamaño del pixel o caja (en grados) (Debe ser tal que entre
%un numero entero de cajas en un grado.
%Las cajas son cuadradas.


hora_acum=12; %Como los datos CMORPH vienen cada 3 horas podemos acumular
%a distintas horas. Las 12 sería el comparable con los datos SALLJEX y los 
%datos de pluviometros que por lo general se miden a esta hora.
%Definimos los bordes del dominio que vamos a usar.
%Este es el dominio que nos vamos a quedar. 



%Archivo CMORPH
archivo_mat=strcat('/home/juan/trabajos/Tesis/salidas_breeding/matlab/precip_cmorph_2012.mat');


%Archivo con la ubicacion en lat y lon de los puntos de la reticula WRF.
load('../matlab/wrf_breeding_domain.mat') %El nombre de las variables es lat_wrf y lon_wrf 
[nx_w ny_w]=size(lat_wrf);
lon_wrf=lon_wrf-360;

lat_min=-60;
lat_max=0;
lon_min=-120; 
lon_max=-10;


%Otras características del CMORPH

delta_t=3;  %Intervalo de tiempo entre los datos en horas.
nx_c=1440;  %Número de puntos en x en el archivo original
ny_c=480;   %Número de puntos en y en el archivo origianl
dx_c=0.25;  %Resolución en grados de los datos originales.
lat_ini=-59.875 %valor de latitud de donde arrancan los datos.
lon_ini=0.125   %valor de longitud de donde arrancan los datos.
hora_ini=0; %hora inicial de cada archivo. El primer dato del archivo es el
%acumulado en las 3 horas previas correspondiente a las 00 UTC.

%Fecha de inicio.
%El formato es dd-mmm-yyyy (el mes son las 3 primeras letras en ingles)

ini_date='15-Dec-2002';
end_date='15-Feb-2003';

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





%Genero un número que identifica la fecha de hoy.
date_num=datenum(ini_date);
ini_date_num=datenum(ini_date);

%Hago lo mismo para la fecha de fin
end_date_num=datenum(end_date);
old_file=-1;

%Definimos la matriz de latitud y la matriz de longitud necesaria para
%recortar nuestro dominio.
    for j=1:ny_c
            for i=1:nx_c
    latitud(j,i)=lat_ini+dx_c*(ny_c-j); %Las latitudes est�n al rev�s en los datos CMORPH.

        if(lon_ini+dx_c*(i-1) <= 180 )
    longitud(j,i)=lon_ini+dx_c*(i-1);
        else
    longitud(j,i)=lon_ini+dx_c*(i-1)-360;
        end
            end
    end
    
    i_reg=find(  latitud < lat_max & latitud >= lat_min & longitud < lon_max & longitud >= lon_min );
    latitud_reg=latitud(i_reg);
    longitud_reg=longitud(i_reg);
    

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
    auxiliar(:,:)=fread(new_file,[nx_c ny_c],'single')';
    new_data(:,:,i)=auxiliar(i_reg);
    end
    

    new_data(find(new_data==undef))=NaN; %Pongo en NaN los undef para poder calcular el promedio.
    
    %Acumulo los tiempos para obtener el acumulado en 24 horas.
    
    ntiempos=hora_acum/delta_t; %Numero de tiempos del archivo de hoy que vamos a usar para el acumulado.   
    acum_data_new=squeeze(nanmean(new_data(:,:,1:ntiempos),3))*ntiempos*delta_t; %Sumamos y pasamos a mm
    acum_data_new2=squeeze(nanmean(new_data(:,:,ntiempos+1:8),3))*(8-ntiempos)*delta_t; %Sumo el resto de los tiempos para el dia siguiente
    daily_data=acum_data_new+acum_data_old; %Obtengo el acumulado cada 24 horas.
    acum_data_old=acum_data_new2; %Guardo el acum_data_old para el d�a siguiente.
    
    %comienza el ciclo sobre las cajas
    %Si la resoluci�n a la que quiero interpolar es mucho menor que la
    %resolucion original uso promedio por cajas.
    


     for ilat=1:ny_w
          for ilon=1:nx_w
         %Defino los bordes de la caja considerando todas las
         %posibilidades.
         if(ilat > 1 & ilat < ny_w-1)
         lat_s=lat_wrf(ilat,ilon)+(lat_wrf(ilat,ilon)-lat_wrf(ilat+1,ilon))/2;
         lat_n=lat_wrf(ilat,ilon)-(lat_wrf(ilat-1,ilon)-lat_wrf(ilat,ilon))/2;
         end
         if(ilon > 1 & ilon < nx_w-1)
         lon_w=lon_wrf(ilat,ilon)+(lon_wrf(ilat,ilon)-lon_wrf(ilat,ilon+1))/2;
         lon_e=lon_wrf(ilat,ilon)-(lon_wrf(ilat,ilon-1)-lon_wrf(ilat,ilon))/2;
         end
         if(ilat==1)
         lat_s=lat_wrf(ilat,ilon)+(lat_wrf(ilat,ilon)-lat_wrf(ilat+1,ilon))/2;
         lat_n=lat_wrf(ilat,ilon)-(lat_wrf(ilat,ilon)-lat_wrf(ilat+1,ilon))/2;
         end
         if(ilat==ny_w)
         lat_s=lat_wrf(ilat,ilon)+(lat_wrf(ilat-1,ilon)-lat_wrf(ilat,ilon))/2;
         lat_n=lat_wrf(ilat,ilon)-(lat_wrf(ilat-1,ilon)-lat_wrf(ilat,ilon))/2;
         end
         if(ilon==1)
         lon_w=lon_wrf(ilat,ilon)+(lon_wrf(ilat,ilon)-lon_wrf(ilat,ilon+1))/2;
         lon_e=lon_wrf(ilat,ilon)-(lon_wrf(ilat,ilon)-lon_wrf(ilat,ilon+1))/2;
         end
         if(ilon==nx_w)
         lon_w=lon_wrf(ilat,ilon)+(lon_wrf(ilat,ilon-1)-lon_wrf(ilat,ilon))/2;
         lon_e=lon_wrf(ilat,ilon)-(lon_wrf(ilat,ilon-1)-lon_wrf(ilat,ilon))/2; 
         end
         
          
         %Busco los puntos dentro de la caja definida previamente.
         %*****************************************************************
         i_box=find(  latitud_reg < lat_n & latitud_reg >= lat_s & longitud_reg < lon_e & longitud_reg >= lon_w );

         num_data(ilat,ilon,i)=length(i_box(:));

         precip_cmorph(ilat,ilon,date_num-ini_date_num+1)=squeeze(nanmean(daily_data(i_box)));

         %*****************************************************************
         
        
                  
          end
      end
      
end %ESTE ES EL END DEL IF SOBRE LA EXISTENCIA DE LOS ARCHIVOS. SI NO EXISTEN IGUAL TENGO QUE ESCRIBIR EN EL ARCHIVO.

if  (old_file == -1 & new_file ~= -1)
         %En este caso no podemos calcular daily data pero podemos leer new
         %file para que sirva para el tiempo siguiente.
         
    for i=1:8 %for sobre los tiempos del archivo.
    aux_read(:,:,i)=fread(new_file,[nx_c ny_c],'single')';  %La estimacion sin morphing no me interesa por el momento.
    auxiliar(:,:)=fread(new_file,[nx_c ny_c],'single')';
    new_data(:,:,i)=auxiliar(i_reg);
    end
    
    %Acumulo los tiempos para obtener el acumulado en 24 horas.
    
    ntiempos=hora_acum/delta_t+1; %Numero de tiempos del archivo de hoy que vamos a usar para el acumulado.   
    acum_data_new=squeeze(mean(new_data(:,:,1:ntiempos),3))*ntiempos*delta_t;   %Sumo los ntiempos primeros tiempos para el dia de hoy.
    acum_data_new2=squeeze(mean(new_data(:,:,ntiempos+1:8),3))*(8-ntiempos)*delta_t; %Sumo el resto de los tiempos para el dia siguiente
    acum_data_old=acum_data_new2; %Guardo el acum_data_old para el d�a siguiente.
    %Creo el daily data como una matriz de undef.
    for ilat=1:ny_w
          for ilon=1:nx_w
         num_data(ilat,ilon)=undef;
         precip_cmorph(ilat,ilon,date_num-ini_date_num+1)=undef;        
          end
      end
    
end %ESTE ES EL END DEL SEGUNDO IF SOBRE LA EXISTENCIA DE LOS ARCHIVOS.
if(new_file == -1)
    %En este caso no tenemos archivo nuevo... no puedo hacer nada y en el
    %paso siguiente tampoco.
     for ilat=1:ny_w
         for ilon=1:nx_w
         num_data(ilat,ilon)=undef;
         precip_cmorph(ilat,ilon,date_num-ini_date_num+1)=undef;        
          end
     end
      
end %ESTE ES EL END DEL TERCER IF SOBRE LA EXISTENCIA DE LOS ARCHIVOS.
 
         old_file=new_file; %Paso el n�mero de archivo nuevo al viejo.
         
         
    
    
    date_num=date_num+1;
    %clear daily_data daily_field num_data %Si no borramos estas variables pueden quedar datos de dias anteriores.

end %ESTE ES EL END DEL CICLO SOBRE LAS FECHAS!!


%Guardamos todos los campos en un archivo .mat (la idea sería generar este
%archivo para todas las fuentes y tener todas las fuentes en el mismo
%formato en distintas resoluciones.

 save(archivo_mat,'precip_cmorph');
