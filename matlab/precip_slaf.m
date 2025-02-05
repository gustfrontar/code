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

region=2;
fuente=1;


if(region==1)
    reg='sur';
end
if(region==2)
    reg='norte'
end
if(fuente==1)
    fue='gts';
    i_precip=6;
    path_gts='/home/juan/trabajos/TrabajoSLAF/precipitacion/datoscpc2006/'
end
if(fuente==2)
    fue='cmo';
    i_precip=5;
    path_gts='/home/juan/trabajos/TrabajoSLAF/precipitacion/cmorph2006/'
end


path_wrf='/home/juan/trabajos/TrabajoSLAF/precipitacion/slaf2006/ensemble/'

%Archivo donde guardamos los resultados.
archivo3=strcat('/home/juan/trabajos/TrabajoSLAF/precipitacion/slaf2006/matlab/slaf2006_',reg,'_',fue,'.mat');

%Numero de miembros del ensamble

ens=9;

%Fit order: orden del polinomio que vamos a ajustar entre cmorph y WRF.

fit_order=1;

%umbrales verificacion
%variable vectorial con los diversos umbrales que vamos a usar en la
%verificacion. Por ejemplo para el calculo del ETS. Los umbrales se definen
%como en Hamill y Colucci.

umbral=[0.01 0.10 0.25 0.5 1 1.5 2]*25.4;

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

undef_gts=999;

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
archivo_latlon='/home/juan/trabajos/TrabajoSLAF/precipitacion/slaf2006/ensemble/wrf_grid.dat'
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

if(fuente==1)
archivo_datos=strcat(path_gts,'sa_12z.',fecha2);
end
if(fuente==2)
archivo_datos=strcat(path_gts,fecha2,'cmorph_12z.');  
end


datos_file=fopen(archivo_datos);
if(datos_file~= -1)              %Pregunto si existe el archivo GTS de ese d�a.
datos=load('-ascii',archivo_datos);

if(fecha==datos(1,1))            %Verifico que la fecha del archivo coincida con la fecha que tiene adentro. 

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
      
      
%En gts tengo los datos del gts, en data_24 y data_48 los pronosticos del
%WRF.
%Ahora tengo que crear las variables dato 24 y dato 48.

%**************************************************************************
%Uso vecino mas cercano utilizando la funcion find.

  %Encuentro los valores que no son undef
  if(fuente==1)
  datos(find(datos(:,i_precip)==999),i_precip)=0; %Conviernto en 0 los undef (porque no hay 0) (Solo para el GTS).
  end
  i_gts=find(datos(:,i_precip) >=  0 & datos(:,i_precip) ~= 999 & datos(:,3) >= lat_s & datos(:,3) <= lat_n & datos(:,4) >= lon_w & datos(:,4) <= lon_e & datos(:,i_precip) <= 300);
  clear aux aux24 aux48
  aux=datos(i_gts,:);
  n_aux=length(aux(:,1));

  data_24_sub(:,:,:)=data_24(i_lat,i_lon,:); %Recorto para quedarme con un subdominio.
  data_48_sub(:,:,:)=data_48(i_lat,i_lon,:);
  

  
  if (narchivos_24 == 2)
  for j=1:n_aux
      %Siempre asumo que i_gts y j_gts son únicos (que no esta equidistante
      %a 2 puntos)
      i_gts(j)=find( abs(lat_wrf_sub-aux(j,3))==min(abs(lat_wrf_sub-aux(j,3))));
      j_gts(j)=find( abs(lon_wrf_sub-aux(j,4))==min(abs(lon_wrf_sub-aux(j,4))));

      if(min(abs(lat_wrf_sub-aux(j,3))) <= 1 & min(abs(lon_wrf_sub-aux(j,4)))<=1);
      aux24(j,2:ens+1)=data_24_sub(i_gts(j),j_gts(j),:);
      else
      aux24(j,2:ens+1)=NaN;
      end


  aux24(j,1)=aux(j,i_precip); %La primera columna es el dato.

 
  end
  
  else 
      aux24(1:n_aux,2:ens+1)=NaN; %Si no esta el ensemble relleno con NaN.
  
  end
  
  if (narchivos_48 == 2)
  for j=1:n_aux
     %if (narchivos_24  < 2) %Si esta el archivo de 24 horas esto ya esta calculado, sino lo calculo en este paso.
      i_gts(j)=find( abs(lat_wrf_sub-aux(j,3))==min(abs(lat_wrf_sub-aux(j,3))));
      j_gts(j)=find( abs(lon_wrf_sub-aux(j,4))==min(abs(lon_wrf_sub-aux(j,4))));
     %end

      if(min(abs(lat_wrf_sub-aux(j,3))) <= 1 & min(abs(lon_wrf_sub-aux(j,4)))<=1);
      aux48(j,2:ens+1)=data_48_sub(i_gts(j),j_gts(j),:);
      else
      aux48(j,2:ens+1)=NaN;
      end


  aux48(j,1)=aux(j,i_precip); %La primera columna es el dato.

  end
  
  else 
      aux48(1:n_aux,2:ens+1)=NaN; %Si no esta el ensemble relleno con NaN.
  
  end
  
  %Ahora genero la variable est_data para este dia.
  
  %El formato de est_data es dia // estacion (ID OMM) // lat // lon
  
  est_data=[est_data ; aux(:,1:4)];
  
  %Agrego los datos de hoy a p24 y p48.
  
  p24=[p24 ; aux24];
  p48=[p48 ; aux48];
  
end %Estos 2 end corresponden a la pregunta de si existe el archivo GTS y si la fecha coincide con la de los datos.
end
  
  
%**************************************************************************
%Fin de la interpolacion por vecino mas cercano.
%************************************************************************** 
  
clear i_gts j_gts  %Importante, sino limpiamos esta variable puede haber errores.
date_num=date_num+1;
end %ESTE ES EL END DEL CICLO SOBRE LAS FECHAS!!


save(archivo3,'p24','p48','est_data')


%**************************************************************************
%Hasta aca la generaci�n del ensemble en formato matlab.
%A partir de aca algunos chequeos b�sicos.
%**************************************************************************


%Calculo la media del ensemble

media24=mean(p24(:,2:ens+1),2);
media48=mean(p48(:,2:ens+1),2);


%Calculo del ETS para todo el periodo (sirve para detectar errores).
umbral=[ 0.10 0.25 0.5 1 1.5 2]*25.4;
[ets_24 hit_rate far]=ets_fun(p24(:,1),p24(:,2:ens+1),umbral)
[ets_48 hit_rate far]=ets_fun(p48(:,1),p48(:,2:ens+1),umbral)

[ets_media_24 hit_rate_media far_media]=ets_fun(p24(:,1),media24,umbral);
%[ets_media_48 hit_rate_media far_media]=ets_fun(p48(:,1),media48,umbral);

figure
plot(umbral,ets_24)
hold on
plot(umbral,ets_media_24,'bo-')
legend('C00','P1P','P1N','P2P','P2N','P3P','P3N','P4P','P4N','MEDIA')
axis([0 50 0 0.5]);
xlabel('Umbral (mm)');
ylabel('ETS');
title('ETS 24 horas');

[bias_area24]=biasarea_fun(p24(:,1),p24(:,2:ens+1),umbral)
[bias_area_media_24]=biasarea_fun(p24(:,1),media24,umbral)

figure
plot(umbral,bias_area24)
hold on
plot(umbral,bias_area_media_24,'bo-')
legend('C00','P1P','P1N','P2P','P2N','P3P','P3N','P4P','P4N','MEDIA')
axis([0 50 0 2]);
xlabel('Umbral (mm)');
ylabel('BIAS AREAL');
title('BIAS AREAL 24 horas');

%Calculamos el n�mero de datos faltantes para cada modelo.

aux24=sum(isnan(p24));
aux48=sum(isnan(p48));

%Vamos a ver como es (el mundo del reves...) no!, el rank histogram del
%slaf ensemble


%Primero me quedo unicamente con los dias donde tengo disponibles todos los
%miembros del ensemble para ahorrar complicaciones.

[filas24 columnas24]=size(p24);
[filas48 columnas48]=size(p48);

i_nonan24=find(any(isnan(p24),2)==0);
%i_nonan48=find(any(isnan(p48),2)==0);

temp=p24(i_nonan24,:);
clear p24
p24=temp;
clear temp
%temp=p48(i_nonan48,:);
%clear p48
%p48=temp;
%clear temp


[rank_hist_24 low_std_24 hi_std_24] = pre_cal_fun(p24(:,1),p24(:,2:columnas24),1);
[rank_hist_48 low_std_48 hi_std_48] = pre_cal_fun(p48(:,1),p48(:,2:columnas24),1);

%Grafico...
%**************************************************************************
figure
axis_limits=[ 0 columnas24+1 0 0.6]
subplot(241)
bar(rank_hist_24(1,:))
title('Rank 24 Baja dispersion');
axis(axis_limits);

subplot(242)
bar(rank_hist_24(2,:))
title('Rank 24 Media dispersion');
axis(axis_limits);

subplot(243)
bar(rank_hist_24(3,:))
title('Rank 24 Alta dispersion');
axis(axis_limits);

subplot(244)
bar(rank_hist_24(4,:))
title('Rank 24 Total');
axis(axis_limits);

subplot(245)
bar(rank_hist_48(1,:))
title('Rank 48 Baja dispersion');
axis(axis_limits);

subplot(246)
bar(rank_hist_48(2,:))
title('Rank 48 Media dispersion');
axis(axis_limits);

subplot(247)
bar(rank_hist_48(3,:))
title('Rank 48 Alta dispersion');
axis(axis_limits);

subplot(248)
bar(rank_hist_48(4,:))
title('Rank 48 Total');
axis(axis_limits);


