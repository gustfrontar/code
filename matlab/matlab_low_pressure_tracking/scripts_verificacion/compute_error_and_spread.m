close all
clear all

%INCLUDE SPECIFIC FUNCTIONS DIRECTORY.
addpath('../common_functions_vpo/');

%La idea de este script es calcular utilizando una funcion el error y el
%spread de diferentes variables en cada uno de los grupos obtenidos.
%Tambien se calcula la probabilidad de existencia del sistema y los rank
%histograms.

config.date_ini='2007040112';          
config.date_end='2007053112';

config.model='kwbc';
config.datadateformat='yyyymmddHH';
config.resultfrec=24; 
config.grouppath=['../RESULTS/' config.model '/GROUP_2/'];
config.outpath=['../RESULTS/' config.model '/ERRORSPREAD/'];
config.outfile=[ config.outpath '/ERROR_SPREAD_' config.date_ini '_' config.date_end '.mat'];
config.forecastlength=7;

mkdir(config.outpath);
%Will read all the groups comming from the selected model during the
%selected period.

tic
[group]=read_groups_fun(config);
tiempo=toc;
fprintf('Time to read the data : %f \n',tiempo);

tic
%Calculo probabilidades, error, spread, etc.
[group]=group_error_spread_fun(group);
tiempo=toc;
fprintf('Time to compute error, spread, probability, etc : %f \n',tiempo);

%Utilizamos el spread solo donde tiene sentido es decir donde la cantidad
%de miembros es mayor a un determinado umbral y ademas donde ha un analisis
%con le cual comparar, los demas lugares los fijamos como NaN.

%mascara=group.sistemexistance==0 | group.nummembers < 4;

%group.distspread(mascara)=NaN;
%group.distlatspread(mascara)=NaN;
%group.distlonspread(mascara)=NaN;
%group.distlonerror(mascara)=NaN;
%group.distlaterror(mascara)=NaN;
%group.disterror(mascara)=NaN;
%group.nummembers(mascara)=NaN;

save(config.outfile,'group');



plot_diagnostic_fun(group);







