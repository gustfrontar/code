%   Funcion q abre los archivos de salida del WRF del dia, los archivos
%   generados ese dia. Se le debe introducir la fecha de hoy, la variable q
%   quiero q lea q es solo una variable, ruta de acceso a los
%   datos. Usa la funcion read_file
function [fecha prono_24 prono_36 prono_48 prono_60 prono_72]=read_fun(fecha, vari, archivo_wrf)

%archivo_wrf='/WRFV2/wrfsi/domains/operativo/archivo/3d/';
n=12; % numero de archivos q quiero q abra
nx=101; %Cantidad de ptos en x
ny=110; %Cantidad de ptos en y
nlevs=6; %Cantidad de niveles q tiene
b=datenum(fecha,'yyyymmdd');
dd=datevec(b);
ee=datevec(b+1);
ff=datevec(b+2);
gg=datevec(b+3);

tiempo=datestr(dd, 'yyyymmdd'); %Fecha para el pronostico WRF



manpas_12=gg(1)*1e6+gg(2)*1e4+gg(3)*1e2+12*1e0;
manpas_6=gg(1)*1e6+gg(2)*1e4+gg(3)*1e2+6*1e0;
manpas_0=gg(1)*1e6+gg(2)*1e4+gg(3)*1e2+00*1e0;
pas_18=ff(1)*1e6+ff(2)*1e4+ff(3)*1e2+18*1e0;
pas_12=ff(1)*1e6+ff(2)*1e4+ff(3)*1e2+12*1e0;
pas_6=ff(1)*1e6+ff(2)*1e4+ff(3)*1e2+6*1e0;
pas_0=ff(1)*1e6+ff(2)*1e4+ff(3)*1e2+00*1e0;
man_18=ee(1)*1e6+ee(2)*1e4+ee(3)*1e2+18*1e0;
man_12=ee(1)*1e6+ee(2)*1e4+ee(3)*1e2+12*1e0;
man_6=ee(1)*1e6+ee(2)*1e4+ee(3)*1e2+6*1e0;
man_0=ee(1)*1e6+ee(2)*1e4+ee(3)*1e2+00*1e0;
hoy_18=dd(1)*1e6+dd(2)*1e4+dd(3)*1e2+18*1e0;


time_72=num2str(manpas_12);
time_66=num2str(manpas_6);
time_60=num2str(manpas_0);
time_54=num2str(pas_18);
time_48=num2str(pas_12);
time_42=num2str(pas_6);
time_36=num2str(pas_0);
time_30=num2str(man_18);
time_24=num2str(man_12);
time_18=num2str(man_6);
time_12=num2str(man_0);
time_6=num2str(hoy_18);



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
arch_wrf7272=strcat(archivo_wrf,time_72,'f',T_72(1:2),'.3d.dat');
arch_wrf7266=strcat(archivo_wrf,time_66,'f',T_72(5:6),'.3d.dat');
arch_wrf7260=strcat(archivo_wrf,time_60,'f',T_72(9:10),'.3d.dat');
arch_wrf7254=strcat(archivo_wrf,time_54,'f',T_72(13:14),'.3d.dat');
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf7272);
precipitacion(:,:,1)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf7266);
precipitacion(:,:,2)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf7260);
precipitacion(:,:,3)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf7254);
precipitacion(:,:,4)=variable(:,:,37);
%Pronostico a 60 horas
arch_wrf6060=strcat(archivo_wrf,time_60,'f',T_60(1:2),'.3d.dat');
arch_wrf6054=strcat(archivo_wrf,time_54,'f',T_60(5:6),'.3d.dat');
arch_wrf6048=strcat(archivo_wrf,time_48,'f',T_60(9:10),'.3d.dat');
arch_wrf6042=strcat(archivo_wrf,time_42,'f',T_60(13:14),'.3d.dat');
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf6060);
precipitacion(:,:,5)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf6054);
precipitacion(:,:,6)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf6048);
precipitacion(:,:,7)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf6042);
precipitacion(:,:,8)=variable(:,:,37);
%Pronostico a 48 horas
arch_wrf4848=strcat(archivo_wrf,time_48,'f',T_48(1:2),'.3d.dat');
arch_wrf4842=strcat(archivo_wrf,time_42,'f',T_48(5:6),'.3d.dat');
arch_wrf4836=strcat(archivo_wrf,time_36,'f',T_48(9:10),'.3d.dat');
arch_wrf4830=strcat(archivo_wrf,time_30,'f',T_48(13:14),'.3d.dat');
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf4848);
precipitacion(:,:,9)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf4842);
precipitacion(:,:,10)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf4836);
precipitacion(:,:,11)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf4830);
precipitacion(:,:,12)=variable(:,:,37);
%Pronostico a 36 horas
arch_wrf3636=strcat(archivo_wrf,time_36,'f',T_36(1:2),'.3d.dat');
arch_wrf3630=strcat(archivo_wrf,time_30,'f',T_36(5:6),'.3d.dat');
arch_wrf3624=strcat(archivo_wrf,time_24,'f',T_36(9:10),'.3d.dat');
arch_wrf3618=strcat(archivo_wrf,time_18,'f',T_36(13:14),'.3d.dat');
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf3636);
precipitacion(:,:,13)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf3630);
precipitacion(:,:,14)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf3624);
precipitacion(:,:,15)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf3618);
precipitacion(:,:,16)=variable(:,:,37);
%Pronostico a 24 horas
arch_wrf2424=strcat(archivo_wrf,time_24,'f',T_24(1:2),'.3d.dat');
arch_wrf2418=strcat(archivo_wrf,time_18,'f',T_24(5:6),'.3d.dat');
arch_wrf2412=strcat(archivo_wrf,time_12,'f',T_24(9:10),'.3d.dat');
arch_wrf2406=strcat(archivo_wrf,time_6,'f',T_24(14),'.3d.dat');
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf2424);
precipitacion(:,:,17)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf2418);
precipitacion(:,:,18)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf2412);
precipitacion(:,:,19)=variable(:,:,37);
variable(:,:,:)=read_file(nx,ny,nlevs,arch_wrf2406);
precipitacion(:,:,20)=variable(:,:,37);

%*************************Acumulo la pp de 24 hs***********************
prono_72=(nanmean(precipitacion(:,:,1:4),3))*4; %Pp acumuluda 
%desde las 12UTC del primer dia hasta las 12UTC del segundo dia de pronostico
prono_60=(nanmean(precipitacion(:,:,5:8),3))*4; 
prono_48=(nanmean(precipitacion(:,:,9:12),3))*4; 
prono_36=(nanmean(precipitacion(:,:,13:16),3))*4; 
prono_24=(nanmean(precipitacion(:,:,17:20),3))*4; 










