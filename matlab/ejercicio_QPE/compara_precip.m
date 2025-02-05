
% Este programa Grafica dos campos de precipitacion calculados 
% a partir de una relacion Z-R, su diferencia y
% calcula el RMSE, BIAS entre las dos matrices

clear all; close all; clc


% VARIABLES A MODIFICAR
% filename  Nombre del archivo disponible
%filename='inta_parana_cscan_0350_18Nov2009_total.nc';
%filename='inta_parana_cscan_0510_18Nov2009_total.nc';
%filename='inta_parana_cscan_1020_18Nov2009_total.nc';


filename='inta_parana_cscan_0350_18Nov2009_total.nc';

% VARIABLE1 = Reflectividad 1
VARIABLE1='REFLECTIVITY';

% VARIABLE2 = Reflectividad 2
VARIABLE2='CORRECTED_REFLECTIVITY';
% Tener en cuenta que la diferencia se calcula como VARIABLE1 - VARIABLE2

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

% Parametros a y b de la relacion Z=a*R^b
% Estos parametros seran aplicados la VARIABLE  1
a1=300;
b1=1.4;

% Estos parametros seran aplicados la VARIABLE 2
a2=300;
b2=1.4;


%##########################################################################
%##########################################################################
% OPCIONES DEL GRAFICO SOLO PARA EXPERTOS EN MATLAB ;)

% Las lineas siguientes leen estructuras
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

v=[0.05 0.1 0.5 1 2 5 10 20 30 50 60 70 80 100 200 300];

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


Z_1=10.^(ch4.data(:,:,elev)./10);
R_1= (Z_1./a1).^(1/b1);


Z_2=10.^(ch5.data(:,:,elev)./10);
R_2= (Z_2./a2).^(1/b2);

  
figure
hold('all')
box('on')
pcolor(lon,lat,R_1')
plot_jrcol_2010(v,vcol);
shading flat
plot(lon_Rmax,lat_Rmax,'k-')
plot(provincias(:,1),provincias(:,2),'k-')
axis([min(lon_Rmax) max(lon_Rmax) min(lat_Rmax) max(lat_Rmax)]);
axis square
title('Rainfall from Uncorrected reflectivity Z=a*R^b ')


figure
hold('all')
box('on')
pcolor(lon,lat,R_2')
plot_jrcol_2010(v,vcol);
shading flat
plot(lon_Rmax,lat_Rmax,'k-')
plot(provincias(:,1),provincias(:,2),'k-')
axis([min(lon_Rmax) max(lon_Rmax) min(lat_Rmax) max(lat_Rmax)]);
axis square
title('Rainfall from Corrected reflectivity ZCOR=a*RCOR^b ')


ch6=R_1-R_2;
ch6(isnan(R_1))=NaN;
ch6(isnan(R_2))=NaN;


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
disp(['BIAS ---->  ' str ' mm/h ' ])
str = num2str(RMSE);

disp(['RMSE ---->  ' str ' mm/h ' ])

MAX=max(max(ch6));
MIN=min(min(ch6));

str = num2str(MAX);
disp(['MAX DIFFERENCE ---->  ' str ' mm/h ' ])

str = num2str(MIN);
disp(['MIN DIFFERENCE ---->  ' str ' mm/h ' ])


figure
hold('all') 
box('on')
pcolor(lon,lat,ch6')
shading flat
plot(lon_Rmax,lat_Rmax,'k-')
plot(provincias(:,1),provincias(:,2),'k-')
axis([min(lon_Rmax) max(lon_Rmax) min(lat_Rmax) max(lat_Rmax)]);
axis square
colorbar
title('Rainfall Difference between Z=a*R^b - ZCOR=a*RCOR^b')



% sacar para el curso
%VARIABLES DISPONIBLES EN EL NC
%         float ELEVATION(ELEV) ;
%                 ELEVATION:UNITS = "deg" ;
%                 ELEVATION:NAME = "elevation" ;
%         float REFLECTIVITY(ELEV, AZIMUTH, RANGE) ;
%                 REFLECTIVITY:UNITS = "[dbz]" ;
%         float PHIDP(ELEV, AZIMUTH, RANGE) ;
%                 PHIDP:UNITS = "[deg]" ;
%         float RHOHV(ELEV, AZIMUTH, RANGE) ;
%                 RHOHV:UNITS = "[unitless]" ;
%         float KDP(ELEV, AZIMUTH, RANGE) ;
%                 KDP:UNITS = "[deg/km]" ;
%         float CORRECTED_PHIDP(ELEV, AZIMUTH, RANGE) ;
%                 CORRECTED_PHIDP:UNITS = "[deg]" ;
%         float CORRECTED_KDP(ELEV, AZIMUTH, RANGE) ;
%                 CORRECTED_KDP:UNITS = "[deg/km]" ;
%         float _2_WAY_SPECIFIC_ATTENUATION(ELEV, AZIMUTH, RANGE) ;
%                 _2_WAY_SPECIFIC_ATTENUATION:UNITS = "[db/km]" ;
%         float _2_WAY_TOTAL_ATTENUATION(ELEV, AZIMUTH, RANGE) ;
%                 _2_WAY_TOTAL_ATTENUATION:UNITS = "[db]" ;
%         float CORRECTED_REFLECTIVITY(ELEV, AZIMUTH, RANGE) ;
%                 CORRECTED_REFLECTIVITY:UNITS = "[dbz]" ;




