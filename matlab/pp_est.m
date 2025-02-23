clear all
close all

load pp_wrf.mat
load lat_lon_wrf.mat

b=datenum(date);
c=b-5;
d=datevec(c);
tiempo=datestr(d, 'yyyymmdd');
t=str2num(tiempo);

tam_wrf=size(pp_acum_wrf,4);

archivo_datos=strcat('/datos/cpc/','sa_12z.',tiempo);
datos_file=fopen(archivo_datos);
if(datos_file~= -1)              %Pregunto si existe el archivo GTS de ese d�a.
    datos=load('-ascii',archivo_datos);
    tam_datos=size(datos,1);
    if(t==datos(1,1))            %Verifico que la fecha del archivo coincida con la fecha que tiene adentro. 
       datos(find(datos(:,6)==999),6)=0; %Conviernto en 0 los undef (porque no hay 0) (Solo para el GTS).
       for i=1:tam_datos
           lat_est=datos(i,3);
           lon_est=datos(i,4);
           dist_lat=abs(lat-lat_est); 
           dist_lon=abs(lon-lon_est);
           [r,ei]=min(dist_lat);
           [rr,ej]=min(dist_lon);
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
