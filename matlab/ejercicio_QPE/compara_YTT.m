% Este programa Grafica dos campos de reflectividad, su diferencia y
% calcula el RMSE, BIAS entre las dos matrices
clear all; close all; clc


% VARIABLES A MODIFICAR
% filename  Nombre del archivo disponible
%filename='inta_parana_cscan_0350_18Nov2009_total.nc';
%filename='inta_parana_cscan_0510_18Nov2009_total.nc';
%filename='inta_parana_cscan_1020_18Nov2009_total.nc';


filename='inta_parana_cscan_0350_18Nov2009_total.nc';


% Elevacion del Radar
% de 1 a 10
% Las elevaciones disponibles son:
%elev=1 => 0.500
%elev=2 => 1.3000
%elev=3 => 2.3000
%elev=4 => 3.5000
%elev=5 => 5.0000
%elev=6 => 6.9000
%elev=7 => 9.1000
%elev=8 =>11.8000
%elev=9 =>15.1000
%elev=10 =>19.2000

elev=1;
% Es el rango (radio maximo) que se utiliza para graficar en km
Rmax=120;               



%##########################################################################
%##########################################################################
% OPCIONES DEL GRAFICO SOLO PARA CONOCEDORES EN MATLAB ;)
% Las lineas siguientes leen estructuras

VARIABLE1='REFLECTIVITY';

VARIABLE2='CORRECTED_REFLECTIVITY';



% Tener en cuenta que la diferencia se calcula como VARIABLE1 - VARIABLE2

ch4 = netcdfvar(filename,VARIABLE1);
ch4.data(ch4.data==-9999)=NaN;


ch5 = netcdfvar(filename,VARIABLE2);
ch5.data(ch5.data==-9999)=NaN;

% Para ver el contenido de la estructura basta con tipear el nombre de la
% variable de salida, por ejemplo, ch4.

load('mapas.mat')


% figure
% pcolor(ch4.data(:,:,1))
% shading flat;


% Coordenadas del sitio del radar (Parana):
lat_radar=-31.85;
lon_radar=-60.54;

%##########################################################################
% Variables a definir para luego usar dentro del for

R=12742/2;              % Radio medio de la Tierra.
m=2*pi*R/360;
alfa=-pi:0.0001:pi;

xx=(-119.5:119.5);
yy=(-119.5:119.5);

nazim=360;
nbins=480;
binres=0.5;

v=[5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80];
vcol=[152 153 154 155 156 157 158 159 160 161 162 163 164 165 166];

%##########################################################################

%##########################################################################
% A continuacion calculo las lats y lons alrededor del radar
 azimuth=repmat(((1:nazim)*360/nazim)',[1 nbins]);           %Matriz de azimuth desde el norte al punto de reticula en cuestion.
 rangos=repmat((1:nbins)*binres,[nazim 1]);                  %Matriz de rango del centro del radar al punto de reticula en cuestion.

% Coordenadas de los datos de reflectividad
lat=lat_radar+(rangos./m).*cos((azimuth).*pi/180);
lon=lon_radar+(rangos./m).*sin((azimuth).*pi/180)./cos(lat.*pi/180);

% Coordenadas del anillo correspondiente al rango maximo de alcance del
% radar (Rmax)
 lat_Rmax=lat_radar+(Rmax/m)*sin(alfa);
 lon_Rmax=lon_radar+((Rmax/m)*cos(alfa)./cos(lat_Rmax*pi/180));

%##########################################################################

v=[5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80];
vcol=[152 153 154 155 156 157 158 159 160 161 162 163 164 165 166];
  
figure
hold('all')
box('on')
pcolor(lon,lat,ch4.data(:,:,elev)')
plot_jrcol_2010(v,vcol);
shading flat
plot(lon_Rmax,lat_Rmax,'k-')
plot(provincias(:,1),provincias(:,2),'k-')
axis([min(lon_Rmax) max(lon_Rmax) min(lat_Rmax) max(lat_Rmax)]);
axis square
title('Reflectivity dBZ')




v2=v/(10*5);


Z_1=10.^(ch4.data(:,:,elev)./10);
att=7.2*0.00001*Z_1.^0.77;

Total_att=cumtrapz(att,1)*binres;

figure
hold('all')
box('on')
pcolor(lon,lat,att')
plot_jrcol_2010(v2,vcol);
shading flat
plot(lon_Rmax,lat_Rmax,'k-')
plot(provincias(:,1),provincias(:,2),'k-')
axis([min(lon_Rmax) max(lon_Rmax) min(lat_Rmax) max(lat_Rmax)]);
axis square
title('Specific Attenuation Yh dB/km')

figure
hold('all')
box('on')
pcolor(lon,lat,Total_att')
plot_jrcol_2010(v,vcol);
shading flat
plot(lon_Rmax,lat_Rmax,'k-')
plot(provincias(:,1),provincias(:,2),'k-')
axis([min(lon_Rmax) max(lon_Rmax) min(lat_Rmax) max(lat_Rmax)]);
axis square
title('Total Specific Attenuation dB')

Z_cor=ch4.data(:,:,elev)+Total_att;

figure
hold('all')
box('on')
pcolor(lon,lat,Z_cor');
plot_jrcol_2010(v,vcol);
shading flat
plot(lon_Rmax,lat_Rmax,'k-')
plot(provincias(:,1),provincias(:,2),'k-')
axis([min(lon_Rmax) max(lon_Rmax) min(lat_Rmax) max(lat_Rmax)]);
axis square
title('Reflectivity corrected by Atenuation from equation dBZ')

ch6=Z_cor-ch5.data(:,:,elev);
 
 
RMSE_1=ch6.^2;

RMSE_1(isnan(RMSE_1))=0;
NNan=0;

%Falta contar los Nans de la muestra !!!!

RMSE_2=sum(RMSE_1,'double');
RMSE_3=sum(RMSE_2','double');
RMSE=(RMSE_3/(nazim*nbins-NNan))^0.5;


BIAS_1=ch6;
BIAS_1(isnan(BIAS_1))=0;

BIAS_2=sum(BIAS_1,'double');
BIAS_3=sum(BIAS_2','double');
BIAS=(BIAS_3/(nazim*nbins-NNan));

str = num2str(BIAS);
disp(['BIAS ---->  ' str ' dBZ ' ])

str = num2str(RMSE);
disp(['RMSE ---->  ' str ' dBZ ' ])

MAX=max(max(ch6));
MIN=min(min(ch6));

str = num2str(MAX);
disp(['MAX DIFFERENCE ---->  ' str ' dBZ ' ])

str = num2str(MIN);
disp(['MIN DIFFERENCE ---->  ' str ' dBZ ' ])

figure
hold('all') 
box('on')
pcolor(lon,lat,ch6(:,:)')
shading flat
plot(lon_Rmax,lat_Rmax,'k-')
plot(provincias(:,1),provincias(:,2),'k-')
axis([min(lon_Rmax) max(lon_Rmax) min(lat_Rmax) max(lat_Rmax)]);
axis square
colorbar
title('Reflectivity Corrected by Ah-Z - Corrected Reflectivity dBZ')





