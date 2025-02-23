% Calculo del RMSE para todo el dominio y para una region especifica. 
% Ademas genera un archivo con los datos de cada region 
% Calcula tmb el LLJ para Mariscal Estigarribia y lo gaurda en un archivo
% punto mat, para el analisis y para el pronostico a las 24, a las 48 y a las 72
clear all
close all

nx=101;
ny=110;
nvar=7;
nlevs=6;
ntimes=7;

%Defino el area de la Cuenca del Plata
lat_min=-38;
lat_max=-16;
lon_min=-64;
lon_max=-40;

%Defino el area como todo el domino de los mapas de WRF
lat_mintot=-60;
lat_maxtot=-18;
lon_mintot=-95;
lon_maxtot=-35;

%Defino la malla de longitudes
for j=1:nx
lon(j)=-99.2366 +  0.6847*(j-1);
end

%Defino la malla de latitudes
lat=[-61.46607
  -61.13726
  -60.80500
  -60.46925
  -60.13000
  -59.78720
  -59.44086
  -59.09093
  -58.73739
  -58.38022
  -58.01940
  -57.65490
  -57.28671
  -56.91479
  -56.53913
  -56.15970
  -55.77649
  -55.38947
  -54.99863
  -54.60395
  -54.20540
  -53.80297
  -53.39664
  -52.98639
  -52.57221
  -52.15408
  -51.73199
  -51.30591
  -50.87584
  -50.44177
  -50.00368
  -49.56156
  -49.11541
  -48.66520
  -48.21093
  -47.75259
  -47.29019
  -46.82371
  -46.35314
  -45.87849
  -45.39974
  -44.91691
  -44.42998
  -43.93896
  -43.44386
  -42.94467
  -42.44140
  -41.93405
  -41.42263
  -40.90716
  -40.38764
  -39.86407
  -39.33649
  -38.80489
  -38.26929
  -37.72972
  -37.18618
  -36.63870
  -36.08731
  -35.53202
  -34.97287
  -34.40986
  -33.84304
  -33.27244
  -32.69808
  -32.12001
  -31.53825
  -30.95284
  -30.36382
  -29.77123
  -29.17512
  -28.57553
  -27.97249
  -27.36607
  -26.75631
  -26.14325
  -25.52696
  -24.90749
  -24.28489
  -23.65923
  -23.03056
  -22.39893
  -21.76443
  -21.12711
  -20.48705
  -19.84429
  -19.19892
  -18.55102
  -17.90064
  -17.24787
  -16.59278
  -15.93546
  -15.27598
  -14.61441
  -13.95084
  -13.28537
  -12.61806
  -11.94901
  -11.27831
  -10.60602
   -9.93227
   -9.25712
   -8.58067
   -7.90302
   -7.22424
   -6.54445
   -5.86373
   -5.18218
   -4.49989
   -3.81697];

%Nombre del archivo.
%Primero generamos la fecha correspondiente al archivo que queremos abrir
                                                                                                      
S=load('-ascii', 'fecha.txt');
                                                                                                      
tiempo=num2str(S);    %Pasamos la fecha que leimos a un string.
year=tiempo(1:4);
month=tiempo(5:6);
day=tiempo(7:8);
hour=tiempo(9:10);
dia=[ str2num(year) str2num(month) str2num(day) str2num(hour) ]; %Genera la fecha como un vector para guardar en formato yymmddhh
                                                                                                      
arch00=strcat('/WRFV2/wrfsi/domains/operativo/archivo/3d/',tiempo,'f0.3d.dat')
arch12=strcat('/WRFV2/wrfsi/domains/operativo/archivo/3d/',tiempo,'f12.3d.dat')
arch24=strcat('/WRFV2/wrfsi/domains/operativo/archivo/3d/',tiempo,'f24.3d.dat')
arch36=strcat('/WRFV2/wrfsi/domains/operativo/archivo/3d/',tiempo,'f36.3d.dat')
arch48=strcat('/WRFV2/wrfsi/domains/operativo/archivo/3d/',tiempo,'f48.3d.dat')
arch60=strcat('/WRFV2/wrfsi/domains/operativo/archivo/3d/',tiempo,'f60.3d.dat')
arch72=strcat('/WRFV2/wrfsi/domains/operativo/archivo/3d/',tiempo,'f72.3d.dat')
                                                                                                      
%Leo la topografia para poder enmascarar los campos.
archtopo='/WRFV2/wrfsi/domains/operativo/matlab/topografia.bin'
ntopo=fopen(archtopo,'r','l');
nada=fread(ntopo,[2 1],'single')';
topografia(:,:)=fread(ntopo,[nx ny],'single')';

variable(:,:,:,1)=read_file(nx,ny,nlevs,arch00);
variable(:,:,:,2)=read_file(nx,ny,nlevs,arch12);
variable(:,:,:,3)=read_file(nx,ny,nlevs,arch24);
variable(:,:,:,4)=read_file(nx,ny,nlevs,arch36);
variable(:,:,:,5)=read_file(nx,ny,nlevs,arch48);
variable(:,:,:,6)=read_file(nx,ny,nlevs,arch60);
variable(:,:,:,7)=read_file(nx,ny,nlevs,arch72);

%*****************************CUENCA DEL PLATA*****************************
%Diagramo el area para el calculo del RMSE en la region de la Cuenca del
%Plata
puntos_x=find(lon>=lon_min & lon <= lon_max);
puntos_y=find(lat>=lat_min & lat <= lat_max);

for i=1:ntimes
    for j=1:37
      topo(:,:,j,i)=topografia(puntos_x,puntos_y);
    end
end

i_topo=find(topo >= 1000);

%Calculo del RMSE para la Cuenca del Plata
for i=1:ntimes
diff_cua(:,:,:,i)=(variable(puntos_x,puntos_y,:,i)-variable(puntos_x,puntos_y,:,1)).^2;
end

diff_cua(i_topo)=NaN;

ecm=(squeeze(nanmean(nanmean(diff_cua,1),2))).^0.5;
%*************************************************************************
%*********************************REGION WRF*******************************
%Diagramo el area para el calculo del RMSE en la el WRF
puntos_xtot=find(lon>=lon_mintot & lon <= lon_maxtot);
puntos_ytot=find(lat>=lat_mintot & lat <= lat_maxtot);

for i=1:ntimes
    for j=1:37
      topotot(:,:,j,i)=topografia(puntos_xtot,puntos_ytot);
    end
end

i_topotot=find(topotot >= 1000);

%Calculo del RMSE para la region WRF
for i=1:ntimes
diff_cuatot(:,:,:,i)=(variable(puntos_xtot,puntos_ytot,:,i)-variable(puntos_xtot,puntos_ytot,:,1)).^2;
end

diff_cuatot(i_topotot)=NaN;

ecmtot=(squeeze(nanmean(nanmean(diff_cuatot,1),2))).^0.5;
%**************************************************************************
%*****************************************
%Guarda los datos nuevos si es q existe el archivo y sino crea el archivo
%para guardar los datos
file=fopen('rmset.mat')
if(file ~= -1)
%Va guardando todos los dias, el nuevo dato en la region de la region WRF
load rmset.mat
a=size(ecm_archivo_total);
b=a(3)+1;
ecm_archivo_total(:,:,b)=ecmtot;
ecm_archivo_centro(:,:,b)=ecm;
fecha_archivo(:,b)=dia';
save rmset.mat ecm_archivo_total ecm_archivo_centro fecha_archivo -v6
end

if(file==-1)
%Genera el archivo q va ir guardando los datos de la region WRF
%Donde el primer lugar corresponde a el numero de variables, el segundo a
%los tiempos y el tercero a la cantidad de dias almacenado en el archivo
ecm_archivo_total(:,:,1)=ecmtot;
ecm_archivo_total(:,:,2)=ecmtot;
ecm_archivo_centro(:,:,1)=ecm;
ecm_archivo_centro(:,:,2)=ecm;
fecha_archivo(:,1)=dia';
fecha_archivo(:,2)=dia';
save rmset.mat ecm_archivo_total ecm_archivo_centro fecha_archivo -v6
end
%****************************************

%****************************CALCULO DE LLJ*******************************
llj_00=(variable(57,83,14,1)*cos(pi/2))-(variable(57,83,20,1)*sin(pi/2));
llj_24=(variable(57,83,14,3)*cos(pi/2))-(variable(57,83,20,3)*sin(pi/2));
llj_48=(variable(57,83,14,5)*cos(pi/2))-(variable(57,83,20,5)*sin(pi/2));
llj_72=(variable(57,83,14,7)*cos(pi/2))-(variable(57,83,20,7)*sin(pi/2));
%*****************************************
%Guarda los datos nuevos si es q existe el archivo y sino crea el archivo
%para guardar los datos
file=fopen('llj.mat')
if(file ~= -1)
%Va guardando todos los dias, el nuevo dato en la region de la region WRF
load llj.mat
tam_llj=size(llj);
ta=tam_llj(1)+1;
llj(ta,1)=llj_00;
llj(ta,2)=llj_24;
llj(ta,3)=llj_48;
llj(ta,4)=llj_72;
fecha_ar(:,ta)=dia';
save llj.mat llj fecha_ar
end

if(file==-1)
%Genera el archivo q va ir guardando los datos de la region WRF
%Donde el primer lugar corresponde a el numero de variables, el segundo a
%los tiempos y el tercero a la cantidad de dias almacenado en el archivo
llj(1,1)=llj_00;
llj(1,2)=llj_24;
llj(1,3)=llj_48;
llj(1,4)=llj_72;
llj(2,1)=llj_00;
llj(2,2)=llj_24;
llj(2,3)=llj_48;
llj(2,4)=llj_72;
fecha_ar(:,1)=dia';
fecha_ar(:,2)=dia';
save llj.mat llj fecha_ar
end
%****************************************
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
