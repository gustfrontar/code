clear all
close all
%**************************************************************************
%Vamos a usar los datos del GTS (los valores originales medidos en el
%pluviómetro) para hacer una verificación punto a punto del WRF. Para eso
%vamos a usar las salidas del WRF en la retícula original y vamos a
%interpolar a partir de esa retícula. Si usamos los datos ya interpolados
%vamos a introducir un error extra que no tiene sentido. 
%Por el momento la interpolacion se de las salidas del WRF a las estaciones
%se realiza utilizando el método de vecino más cercano. Este método es
%bastante razonable teniendo en cuenta una resolucion de 50 Km es decir que
%el error máximo de posición es de tan solo 25 Km.
%IMPORTANTE: En los archivos originales, no se encuentran 0 de
%precipitacion, solo 999.0 por eso se asumio que este valor correspondia a
%no lluvia y se convirtio en 0 todos los 999.0. Por otra parte existen
%valores negativos -0.1 que no se sabe a que corresponden. 
%**************************************************************************
% Juan Ruiz - 2006
%**************************************************************************
%PARAMETROS MODIFICABLES

%Ruta a los archivos:

region=1;
fuente=1;


if(region==1)
    reg='sur';
end
if(region==2)
    reg='norte'
end



path_wrf='d:/trabajos/TrabajoSLAF/precipitacion/slaf2006/ensemble/'


%Numero de miembros del ensamble

ens=9;

%umbrales verificacion
%variable vectorial con los diversos umbrales que vamos a usar en la
%verificacion. Por ejemplo para el calculo del ETS. Los umbrales se definen
%como en Hamill y Colucci.


%Extremos del subdominio que vamos a utilizar para la calibracion.

if(region==2)
%Region tropical (menor densidad de datos).
lat_n=0;
lat_s=-25;
lon_e=-40;
lon_w=-80;
end
if(region==1)
%Region centro norte de Argentina 
lat_n=-25;
lat_s=-45;
lon_e=-40;
lon_w=-80;
end


%Defino algunas variables previas

p24=[];
p48=[];
est_data=[];


%Fecha de inicio.
%El formato es dd-mmm-yyyy (el mes son las 3 primeras letras en ingles)

ini_date='01-Oct-2006';
end_date='31-Dec-2006';

%Otras características del WRF

delta_t=12;  %Intervalo de tiempo entre los datos en horas.
nx_c=99;  %Número de puntos en x en el archivo original
ny_c=99;   %Número de puntos en y en el archivo origianl
resol_wrf=0.49; %Resolución aproximada en grados.
undef=-9999;

%Ruta del archivo con las lats y lons de la retícula del modelo.
%Como la retícula no es regular, este archivo contiene la información de
%lat y lon de cada punto.
archivo_latlon='d:/trabajos/TrabajoSLAF/precipitacion/slaf2006/ensemble/wrf_grid.dat'
nlatlon=fopen(archivo_latlon,'r','l');

nada=fread(nlatlon,[2 1],'single')'; %Es sequential leo el primer registro.
topo_wrf=fread(nlatlon,[nx_c ny_c],'single')';
nada=fread(nlatlon,[2 1],'single')'; %Es sequential leo el primer registro.
nada=fread(nlatlon,[nx_c ny_c],'single')';
nada=fread(nlatlon,[2 1],'single')'; %Es sequential leo el primer registro.
lat_wrf=fread(nlatlon,[nx_c ny_c],'single')';
nada=fread(nlatlon,[2 1],'single')'; %Es sequential leo el primer registro.
lon_wrf=fread(nlatlon,[nx_c ny_c],'single')';
lon_wrf=lon_wrf-360; %Las longitudes del WRF vienen de 0 360.
nada=fread(nlatlon,[2 1],'single')'; %Es sequential leo el primer registro.
mask_wrf=fread(nlatlon,[nx_c ny_c],'single')';


i_lat=find(lat_wrf(:,1) >= lat_s & lat_wrf(:,1) <= lat_n);
i_lon=find(lon_wrf(1,:) >= lon_w & lon_wrf(1,:) <= lon_e);
lat_wrf_sub=lat_wrf(i_lat,1);
lon_wrf_sub=lon_wrf(1,i_lon);


%Genero un número que identifica la fecha de hoy.
date_num=datenum(ini_date);
ini_date_num=datenum(ini_date);

%Hago lo mismo para la fecha de fin
end_date_num=datenum(end_date);


ndato_24=1; %Utilizo estos contadores para agregar datos a la variable dato
ndato_48=1;
%COMIENZA EL CICLO EN TIEMPO.

while (date_num <= end_date_num)
    time=date_num-ini_date_num+1; %Contador del tiempo.
    
    date_vec=datevec(date_num);
    fecha=date_vec(1)*10000+date_vec(2)*100+date_vec(3);
    fecha2=num2str(fecha)

%leemos los datos GTS de ese día.  




%leemos y acumulamos las precipitacion del WRF en su retícula original.

      %Defino los archivos que voy a usar para acumular la lluvia pronosticada.   
      %Pronosticos a 24 horas.
      file24{1}=strcat(path_wrf,fecha2,'00f12.dat'); %Archivos que tengo que sumar para el prono a 24 horas.
      file24{2}=strcat(path_wrf,fecha2,'12f24.dat');
      %Pronosticos a 48 horas.
      file48{1}=strcat(path_wrf,fecha2,'00f36.dat'); %Archivos que tengo que sumar para el prono a 48 horas.
      file48{2}=strcat(path_wrf,fecha2,'12f48.dat');
   
    
    %Vamos a leer los archivos en cuestión.
    
    data_24=zeros(ny_c,nx_c,ens);
    data_48=zeros(ny_c,nx_c,ens);
    narchivos_24=0;
    narchivos_48=0;

        
      %Leo para 24.
      for ifile=1:2
      filen=fopen(file24{ifile},'r','l');
      
       if(filen ~= -1)
         for j=1:11*ens %Leo las otras variables.
          nada=fread(filen,[2 1],'single')';
          nada=fread(filen,[nx_c ny_c],'single')';
         end 
         for iens=1:ens
          nada=fread(filen,[2 1],'single')';
          precip(:,:,iens)=fread(filen,[nx_c ny_c],'single')'; %Precipitacion total acumulada en 12 horas.
          precip(find(precip >= 10000))=NaN; %Convierto a NaN los undef.
         end
        fclose(filen);
        data_24=data_24+precip;
        clear precip nada
        narchivos_24=narchivos_24+1; %Cuento el numero de archivos que encontre.
       end
      end
      
      %Leo para 48.
      for ifile=1:2
      filen=fopen(file48{ifile},'r','l');
      
       if(filen ~= -1)
         for j=1:11*ens %Leo las otras variables.
          nada=fread(filen,[2 1],'single')';
          nada=fread(filen,[nx_c ny_c],'single')';
         end 
         for iens=1:ens
          nada=fread(filen,[2 1],'single')';
          precip(:,:,iens)=fread(filen,[nx_c ny_c],'single')'; %Precipitacion total acumulada en 12 horas.
          precip(find(precip >= 10000))=NaN; %Convierto a NaN los undef.
         end
        fclose(filen);
        data_48=data_48+precip;
        clear precip nada
        narchivos_48=narchivos_48+1; %Cuento el numero de archivos que encontre.
       end
      end
      
      


  data_24_sub(:,:,:)=data_24(i_lat,i_lon,:); %Recorto para quedarme con un subdominio.
  data_48_sub(:,:,:)=data_48(i_lat,i_lon,:);
  
for i_e=1:ens
  aux_diff24(:,:,i_e)=data_24_sub(:,:,i_e)-mean(data_24_sub,3);
  aux_diff48(:,:,i_e)=data_48_sub(:,:,i_e)-mean(data_48_sub,3);
end
  spread_24(date_num-ini_date_num+1)= squeeze(nanmean(nanmean(nanmean((aux_diff24.^2)))))
  spread_48(date_num-ini_date_num+1)= squeeze(nanmean(nanmean(nanmean((aux_diff48.^2)))))

  

  
  
  

date_num=date_num+1;
end %ESTE ES EL END DEL CICLO SOBRE LAS FECHAS!!

plot(spread_24)
hold on
plot(spread_48,'r')

mean(spread_24)
mean(spread_48)

