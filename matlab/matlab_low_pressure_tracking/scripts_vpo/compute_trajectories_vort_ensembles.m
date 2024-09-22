clear all
close all

%INCLUDE SPECIFIC FUNCTIONS DIRECTORY.
addpath('../common_functions_vpo/');
%This script computes the trajectories corresponding to the NCEP reanalysis
%starting every 12 hours and for a period of 7 days.
startdate='2007070112';     %Fecha inicial del calculo.
enddate  ='2007070112';     %Fecha final del calculo.
config.model='kwbc';
config.enssize=20;        %Cual es el numero maximo de miembros en el ensamble.

%CONFIGURATION.............................................................

config.leadtime=168;      %Requested lead time.      
config.data_path=['/media/GUSTFRONT/data/TIGGE/' config.model];
config.data_path=['/media/GUSTFRONT/data/TIGGE/' config.model '/SMALLGRIB/'];
config.result_path=['../RESULTS/' config.model '/'];
config.clim_path='../RESULTS/CLIMATOLOGY/NCEP500HPA_2/';
config.analysistrajpath='../RESULTS/ANALISIS/TRAJECTORIES/';
config.climprefix='HGT';
config.climsufix='.grd';
config.climdateformat='mmddHH';
config.dataformat='GRIB';
config.gribversion=1;
config.dataprefix='HGT_500';
config.datasufix='.grib';
config.datadateformat='yyyymmddHH';
config.isforecast=true;
config.timefrec=6;   
config.timebetweenforecast=24;           %Cada cuanto estan inicializados los pronosticos.

config.mintrajlength=12;                 %Minima longitud que debe tener una trayectoria para iniciar un grupo.

mkdir([config.result_path '/TRAJECTORIESVORT/']);
mkdir([config.result_path '/MINIMOS/']);
mkdir([config.result_path '/GROUPVORT/']);

%==========================================================================
% START LOOPS.

startdaten=datenum(startdate,'yyyymmddHH');
enddaten  =datenum(enddate  ,'yyyymmddHH');
currentdate=startdaten;

while ( currentdate <= enddaten )
fprintf('CURRENTLY WE ARE COMPUTING TRAJECTORIES FOR DATE=%s \n',datestr(currentdate,'yyyymmddHH'));

%Vacio el contenido de EnsTrajStruct (sino pueden quedar trayectorias
%viejas de otros dias).
EnsTrajStruct=struct([]);

for iens=1:config.enssize    
fprintf('PROCESING ENSEMBLE MEMBER=%f \n',iens);
config.ensemblemember=iens;
config.date_ini=datestr(currentdate,'yyyymmddHH');          
config.date_end=datestr(currentdate+config.leadtime/24,'yyyymmddHH');
EnsTrajStruct(iens).trajectories=calc_trayectory_vorticity_fun(config); 


end

save([config.result_path '/TRAJECTORIESVORT/TRAJ_' config.date_ini '_L' num2str(int32(config.leadtime/24)) '.mat'],'EnsTrajStruct');

%==========================================================================
% COMPUTE GROUPS
%==========================================================================

%Load analysis trayectoris for this date. (se podrian calcular pero es mas
%rapido si las cargamos).

load([config.analysistrajpath '/TRAJ_' config.date_ini '_L' num2str(int32(config.leadtime/24)) '.mat']); 


[group]=group_trajectories_fun(EnsTrajStruct,AnalysisTrajStruct,config);

save([config.result_path '/GROUPVORT/GROUP_' config.date_ini '_L' num2str(int32(config.leadtime/24)) '.mat'],'group');


currentdate=currentdate+config.timebetweenforecast/24;


end



 
 