clear all
close all

%Ruta del directorio donde estan los archivos.
archivo_cmorph='/datos/cmorph/';
archivo_wrf='/WRFV2/wrfsi/domains/operativo/archivo/3d/';
%Genero la fecha q voy a usar para abrir los archivos
%b=datenum(date);
S=load('-ascii', 'fecha.txt');

fech=num2str(S);
fech=fech(1:8);
b=datenum(fech,'yyyymmdd');
%Atrasamos la fecha unos 4 dias porque los datos cmorph siempre vienen con
%atraso.
c=b-5;
e=b-6;
d=datevec(c);
f=datevec(e);
tiempo=datestr(d, 'yyyymmdd'); %Fecha para el CMORPH y fecha para el pronostico WRF
tiempo_dos=datestr(f,'yyyymmdd'); %Fecha para el pronostico WRF del dia anterior
t=str2num(tiempo);  % Genera la fecha como un string
year=tiempo(1:4);
month=tiempo(5:6);
day=tiempo(7:8);
dia=[ str2num(year) str2num(month) str2num(day)]; % Genera la fecha como un vector

%************************************WRF***********************************
nx=101; %Cantidad de ptos en x
ny=110; %Cantidad de ptos en y
nlevs=6; %Cantidad de niveles q tiene

load lat_lon_wrf.mat lat lon

%*************************************CMORPH*******************************
delta_t=3;  %Intervalo de tiempo entre los datos en horas.
nx_c=1440;  %Número de puntos en x en el archivo original
ny_c=480;   %Número de puntos en y en el archivo origianl
dx_c=0.25;  %Resolución en grados de los datos originales.
lat_ini=-59.875; %valor de latitud de donde arrancan los datos.
lon_ini=0.125;   %valor de longitud de donde arrancan los datos.
hora_ini=0; %hora inicial de cada archivo. El primer dato del archivo es el acumulado en las 3 horas previas correspondiente a las 00 UTC.
undef=-9999;
hora_acum=12;
%Defino el area 
lat_min_c=-60;
lat_max_c=10;
lon_min_c=-110;
lon_max_c=-20;
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
%Recortamos el dominio para quedarnos con una subregion y que eso acelere las cuentas.
i_lat=find( latitud <= lat_max_c & latitud >= lat_min_c);
i_lon=find( longitud <= lon_max_c & longitud >= lon_min_c );
latitud_regional=latitud(i_lat)';
longitud_regional=longitud(i_lon);
i_lon_med=(lon(2)-lon(1))/2;
% save lat_lon_cmorph.mat latitud_regional longitud_regional

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
    time_regional=time_read(i_lat,i_lon,:); %Recorto todos los tiempos del archivo.
    time_dos_regional=time_dos_read(i_lat,i_lon,:); %Recorto todos los tiempos del archivo.
    time_regional(find( time_regional==undef))=NaN; %Pongo en NaN los undef para poder calcular el promedio.
    time_dos_regional(find(time_dos_regional==undef))=NaN; %Pongo en NaN los undef para poder calcular el promedio.
    %Acumulo los tiempos para obtener el acumulado en 24 horas.
    ntiempos=hora_acum/delta_t+1; %Numero de tiempos del archivo de hoy que vamos a usar para el acumulado.
    pp_cmorph=(nanmean(time_dos_regional(:,:,ntiempos+1:8),3)*ntiempos*delta_t)+(nanmean(time_regional(:,:,1:ntiempos),3)*ntiempos*delta_t);
    
    % Genero la transecta para graficar el hovmoller. Defino variables y llamo a la funcion
    XX=longitud_regional(180:190);
    YY=latitud_regional(140:150);
    XXs=-62;
    XXe=-65;
    YYs=-24;
    YYe=-27;
    pp_cmorph_tra=pp_cmorph(140:150,180:190);
    
    [transecta]=cmorph_tra(XX,YY,XXs,XXe,YYs,YYe,pp_cmorph_tra);
    
    
    
    %Guarda los datos nuevos si es q existe el archivo y sino crea el archivo
%para guardar los datos
file=fopen('transecta_cmorph.mat');
if(file ~= -1)
%Va guardando todos los dias, el nuevo dato en la region de la region WRF
load transecta_cmorph.mat
a=size(pp_transecta);
b=a(1)+1;
pp_transecta(b,1)=t;
pp_transecta(b,2:18)=transecta;
save transecta_cmorph.mat pp_transecta -v6
end

if(file==-1)
%Genera el archivo q va ir guardando los datos de la region WRF
%Donde el primer lugar corresponde a el numero de variables, el segundo a
%los tiempos y el tercero a la cantidad de dias almacenado en el archivo
pp_transecta(1,1)=t;
pp_transecta(1,2:18)=transecta;
pp_transecta(2,1)=t;
pp_transecta(2,2:18)=transecta;
save transecta_cmorph.mat pp_transecta -v6
end
    
    
  end

