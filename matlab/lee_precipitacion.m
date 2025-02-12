clear all
close all
%   Este script lee las salidas del WRF operativo y acumula por separado
%   los pronosticos que verifican en un mismo dia. Pronosticos
%   correspondientes a distintas fechas de inicializacion se identifican
%   como pp_prono_24tot, pp_prono_36tot ... etc. 
%   Despues lee los archivos del CMORPH con resolucion de 0.125 grados y
%   cada 3 horas y los suma para obtener la precipitacion estimada para la
%   misma fecha que los pronosticos del wrf. (siempre entre 12 y 12 utc).
%   Luego interpola los datos cmorph usando box_averaging a la retícula del
%   WRF.
%   Finalmente archiva todos estos campos en un archivo en formato matlab
%   (pp_wrf.mat pp_cmorph.mat)
%   Tambien lee los datos de pluviometros para las distintas estaciones y
%   guerda en una matriz estos datos juntos con los pronosticos de WRF
%   (pp_est.mat)

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

%Me armo un vector para poder abrir los archivos
t_72=[1 1 1 1]; % Lo defino porq sino me lo genera de 5
t_72(1)=72;
for i=1:3
    t_72(i+1)=72-i*6;
end
t_60=t_72-12;
t_48=t_60-12;
t_36=t_48-12;
t_24=t_36-12;
%Pasamos el numero a string para poder leer los archivos
T_72=num2str(t_72);
T_60=num2str(t_60);
T_48=num2str(t_48);
T_36=num2str(t_36);
T_24=num2str(t_24);
%Genero el nombre de los archivos y los abro con la funcion ya crea
%read_file
%Pronostico a 72 horas
arch_wrf7272=strcat(archivo_wrf,tiempo,'12f',T_72(1:2),'.3d.dat');
arch_wrf7266=strcat(archivo_wrf,tiempo,'06f',T_72(5:6),'.3d.dat');
arch_wrf7260=strcat(archivo_wrf,tiempo,'00f',T_72(9:10),'.3d.dat');
arch_wrf7254=strcat(archivo_wrf,tiempo_dos,'18f',T_72(13:14),'.3d.dat');
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf7272);
precipitacion(:,:,1)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf7266);
precipitacion(:,:,2)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf7260);
precipitacion(:,:,3)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf7254);
precipitacion(:,:,4)=variable(:,:,37);
%Pronostico a 60 horas
arch_wrf6060=strcat(archivo_wrf,tiempo,'12f',T_60(1:2),'.3d.dat');
arch_wrf6054=strcat(archivo_wrf,tiempo,'06f',T_60(5:6),'.3d.dat');
arch_wrf6048=strcat(archivo_wrf,tiempo,'00f',T_60(9:10),'.3d.dat');
arch_wrf6042=strcat(archivo_wrf,tiempo_dos,'18f',T_60(13:14),'.3d.dat');
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf6060);
precipitacion(:,:,5)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf6054);
precipitacion(:,:,6)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf6048);
precipitacion(:,:,7)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf6042);
precipitacion(:,:,8)=variable(:,:,37);
%Pronostico a 48 horas
arch_wrf4848=strcat(archivo_wrf,tiempo,'12f',T_48(1:2),'.3d.dat');
arch_wrf4842=strcat(archivo_wrf,tiempo,'06f',T_48(5:6),'.3d.dat');
arch_wrf4836=strcat(archivo_wrf,tiempo,'00f',T_48(9:10),'.3d.dat');
arch_wrf4830=strcat(archivo_wrf,tiempo_dos,'18f',T_48(13:14),'.3d.dat');
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf4848);
precipitacion(:,:,9)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf4842);
precipitacion(:,:,10)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf4836);
precipitacion(:,:,11)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf4830);
precipitacion(:,:,12)=variable(:,:,37);
%Pronostico a 36 horas
arch_wrf3636=strcat(archivo_wrf,tiempo,'12f',T_36(1:2),'.3d.dat');
arch_wrf3630=strcat(archivo_wrf,tiempo,'06f',T_36(5:6),'.3d.dat');
arch_wrf3624=strcat(archivo_wrf,tiempo,'00f',T_36(9:10),'.3d.dat');
arch_wrf3618=strcat(archivo_wrf,tiempo_dos,'18f',T_36(13:14),'.3d.dat');
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf3636);
precipitacion(:,:,13)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf3630);
precipitacion(:,:,14)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf3624);
precipitacion(:,:,15)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf3618);
precipitacion(:,:,16)=variable(:,:,37);
%Pronostico a 24 horas
arch_wrf2424=strcat(archivo_wrf,tiempo,'12f',T_24(1:2),'.3d.dat');
arch_wrf2418=strcat(archivo_wrf,tiempo,'06f',T_24(5:6),'.3d.dat');
arch_wrf2412=strcat(archivo_wrf,tiempo,'00f',T_24(9:10),'.3d.dat');
arch_wrf2406=strcat(archivo_wrf,tiempo_dos,'18f',T_24(14),'.3d.dat');
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf2424);
precipitacion(:,:,17)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf2418);
precipitacion(:,:,18)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf2412);
precipitacion(:,:,19)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf2406);
precipitacion(:,:,20)=variable(:,:,37);

%*************************Acumulo la pp de 24 hs***********************
pp_prono_72tot=(nanmean(precipitacion(:,:,1:4),3))*4; %Pp acumuluda 
%desde las 12UTC del primer dia hasta las 12UTC del segundo dia de pronostico
pp_prono_60tot=(nanmean(precipitacion(:,:,5:8),3))*4; 
pp_prono_48tot=(nanmean(precipitacion(:,:,9:12),3))*4; 
pp_prono_36tot=(nanmean(precipitacion(:,:,13:16),3))*4; 
pp_prono_24tot=(nanmean(precipitacion(:,:,17:20),3))*4; 

%Guarda los datos nuevos si es q existe el archivo y sino crea el archivo
%para guardar los datos
file=fopen('pp_wrf.mat');
if(file ~= -1)
%Va guardando todos los dias, el nuevo dato en la region de la region WRF
load pp_wrf.mat
a=size(pp_acum_wrf);
b=a(4)+1;
pp_acum_wrf(:,:,1,b)=pp_prono_24tot;
pp_acum_wrf(:,:,2,b)=pp_prono_36tot;
pp_acum_wrf(:,:,3,b)=pp_prono_48tot;
pp_acum_wrf(:,:,4,b)=pp_prono_60tot;
pp_acum_wrf(:,:,5,b)=pp_prono_72tot;
fecha_archivo(:,b)=dia';
save pp_wrf.mat pp_acum_wrf fecha_archivo -v6
end

if(file==-1)
%Genera el archivo q va ir guardando los datos de la region WRF
%Donde el primer lugar corresponde a el numero de variables, el segundo a
%los tiempos y el tercero a la cantidad de dias almacenado en el archivo
pp_acum_wrf(:,:,1,1)=pp_prono_24tot;
pp_acum_wrf(:,:,2,1)=pp_prono_36tot;
pp_acum_wrf(:,:,3,1)=pp_prono_48tot;
pp_acum_wrf(:,:,4,1)=pp_prono_60tot;
pp_acum_wrf(:,:,5,1)=pp_prono_72tot;

pp_acum_wrf(:,:,1,2)=pp_prono_24tot;
pp_acum_wrf(:,:,2,2)=pp_prono_36tot;
pp_acum_wrf(:,:,3,2)=pp_prono_48tot;
pp_acum_wrf(:,:,4,2)=pp_prono_60tot;
pp_acum_wrf(:,:,5,2)=pp_prono_72tot;
fecha_archivo(:,1)=dia';
fecha_archivo(:,2)=dia';
save pp_wrf.mat pp_acum_wrf fecha_archivo -v6
end

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
save lat_lon_cmorph.mat latitud_regional longitud_regional

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
    
    % Hago una interpolacion por cajas. Defino los bordes de la caja.
    for i=1:ny
        for j=1:nx
            if (i>=2 & i<=ny-1)
                lat_s=lat(i)-(lat(i+1)-lat(i))/2;
                lat_n=-(lat(i-1)-lat(i))/2+lat(i);
            end
            if i==1 
                lat_s=lat(i)-(lat(i+1)-lat(i))/2;
                lat_n=lat(i)+1/4;
            end
            if i==ny
                lat_s=lat(i)-1/4;
                lat_n=-(lat(i-1)-lat(i))/2+lat(i);
            end
            lon_w=lon(j)-i_lon_med;
            lon_e=lon(j)+i_lon_med;
            %Busco las estaciones que están dentro de la caja.
            i_box_lat=find( latitud_regional <= lat_n & latitud_regional >= lat_s);
            i_box_lon=find( longitud_regional <= lon_e & longitud_regional >= lon_w);
            box_cmorph(i,j)=squeeze(nanmean(nanmean(pp_cmorph(i_box_lat,i_box_lon),2)));
        end
    end
else
    box_cmorph(:,:)=NaN(ny,nx);
end

%Guarda los datos nuevos si es q existe el archivo y sino crea el archivo
%para guardar los datos
file=fopen('pp_cmorph.mat');
if(file ~= -1)
%Va guardando todos los dias, el nuevo dato en la region de la region WRF
load pp_cmorph.mat
a=size(pp_acum_cmorph);
b=a(3)+1;
pp_acum_cmorph(:,:,b)=box_cmorph;
fecha_archivo(:,b)=dia';
save pp_cmorph.mat pp_acum_cmorph fecha_archivo -v6
end

if(file==-1)
%Genera el archivo q va ir guardando los datos de la region WRF
%Donde el primer lugar corresponde a el numero de variables, el segundo a
%los tiempos y el tercero a la cantidad de dias almacenado en el archivo
pp_acum_cmorph(:,:,1)=box_cmorph;
pp_acum_cmorph(:,:,2)=box_cmorph;
fecha_archivo(:,1)=dia';
fecha_archivo(:,2)=dia';
save pp_cmorph.mat pp_acum_cmorph fecha_archivo -v6
end
%*************************************************************************

%*****************************Datos de PLUVIOMETROS*************************
% Abro los archivos donde estan los pronosticos del WRF y las latitudes y
% longitudes del WRF
load pp_wrf.mat
load lat_lon_wrf.mat
tam_wrf=size(pp_acum_wrf,4);
% Abro el archivo con los datos de pluviometro
archivo_datos=strcat('/datos/cpc/','sa_12z.',tiempo);
datos_file=fopen(archivo_datos);
if(datos_file~= -1)              %Pregunto si existe el archivo de pluviomtro de ese dia.
    datos=load('-ascii',archivo_datos);
    tam_datos=size(datos,1);
    if(t==datos(1,1))            %Verifico que la fecha del archivo coincida con la fecha que tiene adentro. 
       datos(find(datos(:,6)==999),6)=NaN; %Convierto en 0 los undef (porque no hay 0)NO HACER!!
       for i=1:tam_datos
           lat_est=datos(i,3);
           lon_est=datos(i,4);
           dist_lat=abs(lat-lat_est); 
           dist_lon=abs(lon-lon_est);
           [r,ei]=min(dist_lat);
           [rr,ej]=min(dist_lon);
           % Me genreo un a matriz q en la primera columna tenga la fecha
           % enla segunda la latitud, en la tercera la longitud, en la
           % cuarta el dato de pluviometro y en las sucesivas columnas los
           % distintos pronosticos del WRF
           pp_es(i,1)=t;
           pp_es(i,2)=lat_est;
           pp_es(i,3)=lon_est;
           pp_es(i,4)=datos(i,6);
           pp_es(i,5)=pp_acum_wrf(ei,ej,1,tam_wrf);
           pp_es(i,6)=pp_acum_wrf(ei,ej,2,tam_wrf);
           pp_es(i,7)=pp_acum_wrf(ei,ej,3,tam_wrf);
           pp_es(i,8)=pp_acum_wrf(ei,ej,4,tam_wrf);
           pp_es(i,9)=pp_acum_wrf(ei,ej,5,tam_wrf);
       end
    end
end

%Guarda los datos nuevos si es q existe el archivo y sino crea el archivo
%para guardar los datos
file=fopen('pp_est.mat');
if(file ~= -1)
%Va guardando todos los dias, el nuevo dato en la region de la region WRF
load pp_est.mat
pp_acum_est=[pp_acum_est ; pp_es];
save pp_est.mat pp_acum_est
end

if(file==-1)
%Genera el archivo q va ir guardando los datos de la region WRF
%Donde el primer lugar corresponde a el numero de variables, el segundo a
%los tiempos y el tercero a la cantidad de dias almacenado en el archivo
pp_acum_est=pp_es;
save pp_est.mat pp_acum_est
end
%******************************VARIABLES**********************************
%1=hgt en 900
%2=hgt en 850
%3=hgt en 700
%4=hgt en 500
%5=hgt en 300
%6=hgt en 200
%7=Temp en 900
%8=Temp en 850
%9=Temp en 700
%10=Temp en 500
%11=Temp en 300
%12=Temp en 200
%13=V Zonal en 900
%14=V Zonal en 850
%15=V Zonal en 700
%16=V Zonal en 500
%17=V Zonal en 300
%18=V Zonal en 200
%19=V Meridional en 900
%20=V Meridional en 850
%21=V Meridional en 700
%22=V Meridional en 500
%23=V Meridional en 300
%24=V Meridional en 200
%25=Veloc. Vert. en 900
%26=Veloc. Vert. en 850
%27=Veloc. Vert. en 700
%28=Veloc. Vert. en 500
%29=Veloc. Vert. en 300
%30=Veloc. Vert. en 200
%31=Vapor en 900
%32=Vapor en 850
%33=Vapor en 700
%34=Vapor en 500
%35=Vapor en 300
%36=Vapor en 200
%37=Pp
%**************************************************************************