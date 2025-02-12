clear all
close all

ncload('../data/wrfinput_d01','XLONG','XLAT');
XLONG(XLONG < 0)=XLONG( XLONG < 0 )+360;

%caracter??sticas del CMORPH
delta_t=3;  %Intervalo de tiempo entre los datos en horas.
nx_cmorph=4948;  %N??mero de puntos en x en el archivo original
ny_cmorph=1649;   %N??mero de puntos en y en el archivo origianl
dx_cmorph=0.072756669;  %Resoluci??n en grados de los datos originales.(x)
dy_cmorph=0.072771377;  %Resolucion en grados de los datos originales (y)
lat_ini=-59.963614; %valor de latitud de donde arrancan los datos.
lon_ini=0.036378335;  %valor de longitud de donde arrancan los datos.
hora_ini=0; %hora inicial de cada archivo. El primer dato del archivo es el
%acumulado en las 3 horas previas correspondiente a las 00 UTC.
undef=-9999;

%Definimos la matriz de latitud y la matriz de longitud necesaria para
%recortar nuestro dominio.
tic
for j=1:nx_cmorph
   lon(j)=lon_ini+dx_cmorph*(j-1);
end
for i=1:ny_cmorph
    lat(i)=lat_ini+dy_cmorph*(i-1);
end
[longitud latitud]=meshgrid(lon,lat);
toc

%Get files

tmp=dir('../data/*.dat');

nfiles=size(tmp,1);

for ii=1:nfiles
 tmpstr=tmp(ii).name;
 fecha=[tmpstr(22:25) tmpstr(27:28) tmpstr(30:31) tmpstr(33:34) tmpstr(36:37)];

 %filename_1=['../data/CMORPH_V1.0_RAW_8km-30min_' fecha_1(1:8) ];
 %filename_2=['../data/CMORPH_V1.0_RAW_8km-30min_' fecha_2(1:8) ];
 %filename_3=['../data/CMORPH_V1.0_RAW_8km-30min_' fecha_3(1:8) ];


    for i=1:48 %for sobre los tiempos del archivo.
        fecha_2=datestr(datenum(fecha,'yyyymmddHH')+(i-1)/24,'yyyymmddHH');
        filename=['../data/CMORPH_V1.0_RAW_8km-30min_' fecha_2(1:10) ]
        nfile=fopen(filename,'r','l');
    auxiliar(:,:,2*i-1)=fread(nfile,[nx_cmorph ny_cmorph],'single')';     %Leo la estimacion CMORPH
    auxiliar(:,:,2*i)=fread(nfile,[nx_cmorph ny_cmorph],'single')';     %Leo la estimacion CMORPH

    fclose(nfile);
    end
  
  %Interpolo a la reticula del WRF y acumulo la precipitacion en 3 horas.
  for jj=1:16
      index=(3*2)*(jj-1)+1;
  lluvia_obs(:,:,jj,ii)=interp2(longitud,latitud,sum(auxiliar(:,:,index:index+(3*2-1)),3),XLONG,XLAT);
  end


end
lluvia_obs=lluvia_obs/2; %Como la lluvia esta en mm/h y viene cada media hora divido por 2.

save('lluvia_obs_hires.mat','lluvia_obs');
LAT=XLAT;
LON=XLONG;
save('lat_lon.mat','LAT','LON');
