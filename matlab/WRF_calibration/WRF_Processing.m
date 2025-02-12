clear all
close all

% startDate=[datestr(now,'yyyymmdd') '00'];
% endDate=startDate;
startDate='2015010100';
endDate='2015010100';
option='Mean'; % Mean,Mode,Median,Parametric,Prct
turbines=[1,2,43]; % Las turbinas apagadas
output_name='Pronostico_potencia';

%% Variables
% ============================
WRF.Date=[]; % OJO CON ESTO!!
% ============================
WRF.Name='PER';
% WRF.Path='$HOME/automatizacionCAMMESA/output/';
% WRF.Scripts='$HOME/automatizacionCAMMESA/';
WRF.Path='D:\CAMMESA\Salidas_WRF\';
WRF.Scripts='D:\CAMMESA\Salidas_WRF\';
WRF.FrequencyForecast=1; %frequencyForecast= frecuencia de los datos, si es 1 es un dia, sino 0,5 es cada 12 horas y asi siguiendo
WRF.LengthForecast=51; % Largo del pronostico 
WRF.Accumulation=600; % Plazo de pronostico en segundos
WRF.cosalpha=0.9988;
WRF.sinalpha=0.0489;

%% Trae los datos de WRF
[WRF] = Read_WRF_function(startDate,endDate,WRF);

%% Trae los datos del Parque
load 'datos_Parque.mat'

%% Calculos
[F_WRF]=interpolation_function(Parque,WRF,turbines,option);

%% Guarda la salida
save_data_function(output_name,WRF.Date,WRF.Accumulation/60,F_WRF);
 
