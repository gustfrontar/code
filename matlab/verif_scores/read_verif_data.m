clear all
close all

ncload('../data/wrfinput_d01','XLONG','XLAT');
XLONG(XLONG < 0)=XLONG( XLONG < 0 )+360;

%caracter??sticas del CMORPH
delta_t=3;  %Intervalo de tiempo entre los datos en horas.
nx_cmorph=1440;  %N??mero de puntos en x en el archivo original
ny_cmorph=480;   %N??mero de puntos en y en el archivo origianl
dx_cmorph=0.25;  %Resoluci??n en grados de los datos originales.
lat_ini=-59.875; %valor de latitud de donde arrancan los datos.
lon_ini=0.125 ;  %valor de longitud de donde arrancan los datos.
hora_ini=0; %hora inicial de cada archivo. El primer dato del archivo es el
%acumulado en las 3 horas previas correspondiente a las 00 UTC.
undef=-9999;

%Definimos la matriz de latitud y la matriz de longitud necesaria para
%recortar nuestro dominio.
for j=1:ny_cmorph
   for i=1:nx_cmorph
   latitud(j,i)=lat_ini+dx_cmorph*(j-1);
   longitud(j,i)=lon_ini+dx_cmorph*(i-1);
   end
end

%Get files

tmp=dir('../data/*.dat');

nfiles=size(tmp,1);

for ii=1:nfiles
 tmpstr=tmp(ii).name;
 fecha_1=[tmpstr(22:25) tmpstr(27:28) tmpstr(30:31) tmpstr(33:34) ];
 fecha_2=datestr(datenum(fecha_1,'yyyymmddHH')+1,'yyyymmddHH');
 fecha_3=datestr(datenum(fecha_1,'yyyymmddHH')+2,'yyyymmddHH');

 filename_1=['../data/CMORPH_V1.0_RAW_0.25deg-3HLY_' fecha_1(1:8) ];
 filename_2=['../data/CMORPH_V1.0_RAW_0.25deg-3HLY_' fecha_2(1:8) ];
 filename_3=['../data/CMORPH_V1.0_RAW_0.25deg-3HLY_' fecha_3(1:8) ];

 nfile_1=fopen(filename_1,'r','l');
 nfile_2=fopen(filename_2,'r','l');
 nfile_3=fopen(filename_3,'r','l');

    for i=1:8 %for sobre los tiempos del archivo.
    %nada=fread(nfile_1,[nx_cmorph ny_cmorph],'single')';         %Leo la estimacion sin CMORPHING
    auxiliar(:,:,i)=fread(nfile_1,[nx_cmorph ny_cmorph],'single')';     %Leo la estimacion CMORPH
    %nada=fread(nfile_2,[nx_cmorph ny_cmorph],'single')';     %Leo la estimacion sin CMORPHING
    auxiliar(:,:,8+i)=fread(nfile_2,[nx_cmorph ny_cmorph],'single')';%Leo la estimacion CMORPH
    %nada=fread(nfile_3,[nx_cmorph ny_cmorph],'single')';     %Leo la estimacion sin CMORPHING
    auxiliar(:,:,16+i)=fread(nfile_3,[nx_cmorph ny_cmorph],'single')';%Leo la estimacion CMORPH
    end
  
  %Interpolo a la reticula del WRF.  
  for jj=5:20
  lluvia_obs(:,:,jj-4,ii)=interp2(longitud,latitud,auxiliar(:,:,jj),XLONG,XLAT);
  end

 fclose(nfile_1)
 fclose(nfile_2)
 fclose(nfile_3)

end
lluvia_obs=lluvia_obs;

save('lluvia_obs.mat','lluvia_obs');
LAT=XLAT;
LON=XLONG;
save('lat_lon.mat','LAT','LON');
