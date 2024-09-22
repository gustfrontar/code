close all
clear all

%INCLUDE SPECIFIC FUNCTIONS DIRECTORY.
addpath('../common_functions_vpo/');

%La idea de este script es calcular utilizando una funcion el error y el
%spread de diferentes variables en cada uno de los grupos obtenidos.
%Tambien se calcula la probabilidad de existencia del sistema y los rank
%histograms.

config.date_ini='2007040112';          
config.date_end='2010040112';

config.model='egrr';
config.datadateformat='yyyymmddHH';
config.resultfrec=24; 
config.grouppath=['../RESULTS/' config.model '/GROUP_2/'];
config.outpath=['../RESULTS/' config.model '/ERRORSPREAD/'];
config.outfile=[ config.outpath '/ERROR_SPREAD_' config.date_ini '_' config.date_end '.mat'];
config.forecastlength=7;
config.mindist=2000e3;     %Minima distancia recorrida por el sistema.
config.minanom=-100;        %Pido que el sistema al menos haya alcanzado esta intensidad de anomalia en algun putno de su vida.

mkdir(config.outpath);
%Will read all the groups comming from the selected model during the
%selected period.

tic
[group]=read_groups_fun_2(config);
[group]=group_error_spread_fun_2(group);
tiempo=toc;
fprintf('Time to read the data : %f \n',tiempo);
config.outfile=[ config.outpath '/ERROR_SPREAD_' config.date_ini '_' config.date_end '.mat'];
save('-v7.3',config.outfile,'group');

