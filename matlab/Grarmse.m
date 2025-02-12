%Genera los graficos de RMSE
clear all
close all
load rmset.mat

%Me genera el intervalo de dias q va a graficar
a=size(ecm_archivo_total);
tmax=a(3);
tmin=a(3)-30;

%*************************************Region WRF************************************************
%*************************************GRAFICOS DE LINEAS*******************
ecm_archivo=real(ecm_archivo_total);
graflinea_fun(squeeze(ecm_archivo(4,1:7,tmin:tmax)),'Geopotencial','Evolucion del error de Geopotencial en 500 hPa','500_T')
graflinea_fun(squeeze(ecm_archivo(8,1:7,tmin:tmax)),'Temperatura','Evolucion del error de Temperatura en 850 hPa','850_T')
graflinea_fun(squeeze(ecm_archivo(14,1:7,tmin:tmax)),'U','Evolucion del error del Viento Zonal en 850 hPa','850_T')
graflinea_fun(squeeze(ecm_archivo(18,1:7,tmin:tmax)),'U','Evolucion del error del Viento Zonal en 200 hPa','200_T')
graflinea_fun(squeeze(ecm_archivo(20,1:7,tmin:tmax)),'V','Evolucion del error del Viento Meridional en 850 hPa','850_T')
graflinea_fun(squeeze(ecm_archivo(24,1:7,tmin:tmax)),'V','Evolucion del error del Viento Meridional en 200 hPa','200_T')
graflinea_fun(squeeze(ecm_archivo(31,1:7,tmin:tmax)),'Humedad_especifica','Evolucion del error de la Humedad especifica en 900 hPa','900_T')
graflinea_fun(squeeze(ecm_archivo(32,1:7,tmin:tmax)),'Humedad_especifica','Evolucion del error de la Humedad especifica en 850 hPa','850_T')
%**************************************************************************
%******************************GRAFICOS DE CONTORNOS***********************
ecm_archivo=real(ecm_archivo_total);
grafcon_fun(squeeze(ecm_archivo(7:12,3,tmin:tmax)),'Temperatura','Estructura vertical de los errores cuadraticos medios','f24_T',[0.5 0.75 1.0 1.3 1.6 1.9 2.5 3.0 4.0],100)
grafcon_fun(squeeze(ecm_archivo(7:12,5,tmin:tmax)),'Temperatura','Estructura vertical de los errores cuadraticos medios','f48_T',[0.5 0.75 1.0 1.3 1.6 1.9 2.5 3.0 4.0],100)
grafcon_fun(squeeze(ecm_archivo(7:12,7,tmin:tmax)),'Temperatura','Estructura vertical de los errores cuadraticos medios','f72_T',[0.5 0.75 1.0 1.3 1.6 1.9 2.5 3.0 4.0],100)

grafcon_fun(squeeze(ecm_archivo(31:36,3,tmin:tmax)*1000),'Humedad_especifica','Estructura vertical de los errores cuadraticos medios','f24_T',[0.3 0.5 0.7 0.9 1.10 1.35 1.55 1.75 1.95 2.15 3.0],100)
grafcon_fun(squeeze(ecm_archivo(31:36,5,tmin:tmax)*1000),'Humedad_especifica','Estructura vertical de los errores cuadraticos medios','f48_T',[0.3 0.5 0.7 0.9 1.10 1.35 1.55 1.75 1.95 2.15 3.0],100)
grafcon_fun(squeeze(ecm_archivo(31:36,7,tmin:tmax)*1000),'Humedad_especifica','Estructura vertical de los errores cuadraticos medios','f72_T',[0.3 0.5 0.7 0.9 1.10 1.35 1.55 1.75 1.95 2.15 3.0],100)
%**************************************************************************
%*************************************Region Cuenca del Plata*************************************
%*************************************GRAFICOS DE LINEAS*******************
ecm_archivo=real(ecm_archivo_centro);
graflinea_fun(squeeze(ecm_archivo(4,1:7,tmin:tmax)),'Geopotencial','Evolucion del error de Geopotencial en 500 hPa','500_C')
graflinea_fun(squeeze(ecm_archivo(8,1:7,tmin:tmax)),'Temperatura','Evolucion del error de Temperatura en 850 hPa','850_C')
graflinea_fun(squeeze(ecm_archivo(14,1:7,tmin:tmax)),'U','Evolucion del error del Viento Zonal en 850 hPa','850_C')
graflinea_fun(squeeze(ecm_archivo(18,1:7,tmin:tmax)),'U','Evolucion del error del Viento Zonal en 200 hPa','200_C')
graflinea_fun(squeeze(ecm_archivo(20,1:7,tmin:tmax)),'V','Evolucion del error del Viento Meridional en 850 hPa','850_C')
graflinea_fun(squeeze(ecm_archivo(24,1:7,tmin:tmax)),'V','Evolucion del error del Viento Meridional en 200 hPa','200_C')
graflinea_fun(squeeze(ecm_archivo(31,1:7,tmin:tmax)),'Humedad_especifica','Evolucion del error de la Humedad especifica en 900 hPa','900_C')
graflinea_fun(squeeze(ecm_archivo(32,1:7,tmin:tmax)),'Humedad_especifica','Evolucion del error de la Humedad especifica en 850 hPa','850_C')
%**************************************************************************
%********************************GRAFICOS DE BARRAS************************
ecm_archivo=real(ecm_archivo_centro);
grafbar_fun(squeeze(ecm_archivo(4,1:7,tmin:tmax)),'Geopotencial','500_C',[0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30],100)
%**************************************************************************
%******************************GRAFICOS DE CONTORNOS***********************
grafcon_fun(squeeze(ecm_archivo(7:12,3,tmin:tmax)),'Temperatura','Estructura vertical de los errores cuadraticos medios','f24_C',[0.5 0.75 1.0 1.3 1.6 1.9 2.5 3.0 4.0],100)
grafcon_fun(squeeze(ecm_archivo(7:12,5,tmin:tmax)),'Temperatura','Estructura vertical de los errores cuadraticos medios','f48_C',[0.5 0.75 1.0 1.3 1.6 1.9 2.5 3.0 4.0],100)
grafcon_fun(squeeze(ecm_archivo(7:12,7,tmin:tmax)),'Temperatura','Estructura vertical de los errores cuadraticos medios','f72_C',[0.5 0.75 1.0 1.3 1.6 1.9 2.5 3.0 4.0],100)

grafcon_fun(squeeze(ecm_archivo(31:36,3,tmin:tmax)*1000),'Humedad_especifica','Estructura vertical de los errores cuadraticos medios','f24_C',[0.3 0.5 0.7 0.9 1.10 1.35 1.55 1.75 1.95 2.15 3.0],100)
grafcon_fun(squeeze(ecm_archivo(31:36,5,tmin:tmax)*1000),'Humedad_especifica','Estructura vertical de los errores cuadraticos medios','f48_C',[0.3 0.5 0.7 0.9 1.10 1.35 1.55 1.75 1.95 2.15 3.0],100)
grafcon_fun(squeeze(ecm_archivo(31:36,7,tmin:tmax)*1000),'Humedad_especifica','Estructura vertical de los errores cuadraticos medios','f72_C',[0.3 0.5 0.7 0.9 1.10 1.35 1.55 1.75 1.95 2.15 3.0],100)
%**************************************************************************
