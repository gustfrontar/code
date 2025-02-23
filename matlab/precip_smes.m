%   Completa la matriz del superensamble con los datos cmorph y los datos
%   GTS. Y vuelve a guardar la matriz completa en mismo lugar q estaba.

clear all
close all

load pp_superensemble.mat

archivo_cmorph='/datos/cmorph/';


S=load('-ascii', 'fech.txt');
aa=num2str(S);
b=datenum(aa,'yyyymmdd');
c=b-4;
e=b-5;
d=datevec(c);
f=datevec(e);
tiempo=datestr(d, 'yyyymmdd'); 
tiempo_dos=datestr(f,'yyyymmdd');
t=str2num(tiempo); 

%**********************************PARA 24********************************
tiempo_24=(pp_ensemble_24(:,1)==t);
p24=pp_ensemble_24(tiempo_24,:);

%**************************************CMORPH******************************
nx_c=1440;  %Número de puntos en x en el archivo original
ny_c=480;   %Número de puntos en y en el archivo original
dx_c=0.25;  %Resolución en grados de los datos originales.
delta_t=3;  %Intervalo de tiempo entre los datos en horas.
hora_acum=12;
undef=-9999;
lat_ini=-59.875; %valor de latitud de donde arrancan los datos.
lon_ini=0.125;   %valor de longitud de donde arrancan los datos.

%Definimos la matriz de latitud y la matriz de longitud necesaria para recortar nuestro dominio.
for i=1:ny_c
    latitud(i)=lat_ini+dx_c*(ny_c-i); %Las latitudes estan al reves en los datos CMORPH.
end
for i=1:nx_c
    if(lon_ini+dx_c*(i-1) <= 180 )
        longitud(i)=lon_ini+dx_c*(i-1);
    else
        longitud(i)=lon_ini+dx_c*(i-1)-360;
    end
end

%Abro los archivos CMORPH
arch_cmorph=strcat(archivo_cmorph,tiempo,'_3hr-025deg_cpc+comb');
arch_cmorph_dos=strcat(archivo_cmorph,tiempo_dos,'_3hr-025deg_cpc+comb');
narch=fopen(arch_cmorph,'r','b');
narch_dos=fopen(arch_cmorph_dos,'r','b');

%Averiguo si existen los archivos
if (narch ~= -1 & narch_dos ~= -1) 
    for i=1:8 %for sobre los tiempos del archivo.
    time_read(:,:,i)=fread(narch,[nx_c ny_c],'single')';  %La estimacion sin morphing no me interesa por el momento.
    time_dos_read(:,:,i)=fread(narch_dos,[nx_c ny_c],'single')';
    end
    time_read(find( time_read==undef))=NaN; %Pongo en NaN los undef para poder calcular el promedio.
    time_dos_read(find(time_dos_read==undef))=NaN; %Pongo en NaN los undef para poder calcular el promedio.
    %Acumulo los tiempos para obtener el acumulado en 24 horas.
    ntiempos=hora_acum/delta_t+1; %Numero de tiempos del archivo de hoy que vamos a usar para el acumulado.
    pp_cmorph=(nanmean(time_dos_read(:,:,ntiempos+1:8),3)*ntiempos*delta_t)+(nanmean(time_read(:,:,1:ntiempos),3)*ntiempos*delta_t);
    
    tam_24=size(p24,1);
    
    for i=1:tam_24
           lat_est=p24(i,3);
           lon_est=p24(i,4);
           dist_lat=abs(latitud-lat_est); 
           dist_lon=abs(longitud-lon_est);
           [r,ei]=min(dist_lat);
           [rr,ej]=min(dist_lon);
           cmorph=pp_cmorph(ei,ej);
           p24(i,6)=cmorph;
    end
end

%************************************GTS*************************************
archivo_datos=strcat('/datos/cpc/','sa_12z.',tiempo);
datos_file=fopen(archivo_datos);
if(datos_file~= -1)              %Pregunto si existe el archivo GTS de ese d�a.
    datos=load('-ascii',archivo_datos);
    tam_24=size(p24,1);
    if(t==datos(1,1))            %Verifico que la fecha del archivo coincida con la fecha que tiene adentro. 
       datos(find(datos(:,6)==999),6)=NaN; %Conviernto en 0 los undef (porque no hay 0) (Solo para el GTS).
       for i=1:tam_24
           lat_est=p24(i,3);
           lon_est=p24(i,4);
           gts=find(datos(:,3)==lat_est & datos(:,4)==lon_est);
           tam_gts=size(gts,1);
           if isempty(gts)==0
           if (tam_gts~=1)
               gts=gts(1,:);
           end
               p24(i,5)=datos(gts,6);
           end
       end
    end
end
    
pp_ensemble_24(tiempo_24,:)=p24;

clear time_read time_dos_read ntiempos pp_cmorph  

observa_24=NaN(tam_24,2);
observa_24(:,1)=t;
observa_24(:,2)=p24(:,5);  


%********************************PARA 48**********************************

tiempo_48=(pp_ensemble_48(:,1)==t);
p48=pp_ensemble_48(tiempo_48,:);

%**************************************CMORPH******************************
%Averiguo si existen los archivos
if (narch ~= -1 & narch_dos ~= -1) 
    for i=1:8 %for sobre los tiempos del archivo.
    time_read(:,:,i)=fread(narch,[nx_c ny_c],'single')';  %La estimacion sin morphing no me interesa por el momento.
    time_dos_read(:,:,i)=fread(narch_dos,[nx_c ny_c],'single')';
    end
    time_read(find( time_read==undef))=NaN; %Pongo en NaN los undef para poder calcular el promedio.
    time_dos_read(find(time_dos_read==undef))=NaN; %Pongo en NaN los undef para poder calcular el promedio.
    %Acumulo los tiempos para obtener el acumulado en 24 horas.
    ntiempos=hora_acum/delta_t+1; %Numero de tiempos del archivo de hoy que vamos a usar para el acumulado.
    pp_cmorph=(nanmean(time_dos_read(:,:,ntiempos+1:8),3)*ntiempos*delta_t)+(nanmean(time_read(:,:,1:ntiempos),3)*ntiempos*delta_t);
    
    tam_48=size(p48,1);
    
    for i=1:tam_24
           lat_est=p48(i,3);
           lon_est=p48(i,4);
           dist_lat=abs(latitud-lat_est); 
           dist_lon=abs(longitud-lon_est);
           [r,ei]=min(dist_lat);
           [rr,ej]=min(dist_lon);
           cmorph=pp_cmorph(ei,ej);
           p48(i,6)=cmorph;
    end
end

%************************************GTS***********************************
if(datos_file~= -1)              %Pregunto si existe el archivo GTS de ese d�a.
    datos=load('-ascii',archivo_datos);
    tam_48=size(p48,1);
    if(t==datos(1,1))            %Verifico que la fecha del archivo coincida con la fecha que tiene adentro. 
       datos(find(datos(:,6)==999),6)=NaN; %Conviernto en 0 los undef (porque no hay 0) (Solo para el GTS).
       for i=1:tam_48
           lat_est=p48(i,3);
           lon_est=p48(i,4);
           gts=find(datos(:,3)==lat_est & datos(:,4)==lon_est);
           tam_gts=size(gts,1);
           if isempty(gts)==0
           if (tam_gts~=1)
               gts=gts(1,:);
           end
              p48(i,5)=datos(gts,6);
           end
       end
    end
end
    
pp_ensemble_48(tiempo_48,:)=p48;

observa_48=NaN(tam_48,2);
observa_48(:,1)=t;
observa_48(:,2)=p48(:,5);  

save pp_superensemble.mat pp_ensemble_24 pp_ensemble_48



%Guarda los datos nuevos si es q existe el archivo y sino crea el archivo
%para guardar los datos
file=fopen('pp_obs.mat');
if(file ~= -1)
%Va guardando todos los dias, el nuevo dato 
load pp_obs.mat
observacion_24=[observacion_24;observa_24];
observacion_48=[observacion_48;observa_48];
save pp_obs.mat observacion_24 observacion_48
end


%Genera el archivo q va ir guardando los datos de la region WRF
%Donde el primer lugar corresponde a los umbrales, el segundo a
%los distintos pronosticos, el tercero a las variables y el cuarto a los
%tiempos
if(file==-1)
    observacion_24=observa_24;
    observacion_48=observa_48;  
save pp_obs.mat observacion_24 observacion_48
end


