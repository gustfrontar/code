clear all
close all

%matlabpool

%INCLUDE SPECIFIC FUNCTIONS DIRECTORY.
addpath('../common_functions_vpo/');
%This script computes the trajectories corresponding to the NCEP reanalysis
%starting every 12 hours and for a period of 7 days.
startdate='2010122512';     %Fecha inicial del calculo.
enddate  ='2010123112';     %Fecha final del calculo.
deltat   =7;                %Periodo de las trayectorias en dias.
frec     =24/24;            %Cada cuanto se reinicializa el calculo de las trayectorias.

%CONFIGURATION.............................................................
config.data_path='../DATA/CFSR/HGTVPO/';
config.clim_path='../RESULTS/CLIMATOLOGY/NCEP500HPA_2_CFSR/';
config.result_path='../RESULTS/ANALISISCFSR/';
config.climprefix='HGT';
config.climsufix='.grd';
config.climdateformat='mmddHH';
config.dataformat='BIN';
config.gribversion=1;
config.dataprefix='CFSR_HGT_';
config.datasufix='.bin';
config.datadateformat='yyyymmddHH';
config.isforecast=false;
config.enssize=1;
config.timefrec=6;   


%==========================================================================
% CREATE PATHS.

mkdir([config.result_path '/TRAJECTORIES_2/']);
mkdir([config.result_path '/MINIMOS_2/']);

%==========================================================================
% START LOOPS.

startdaten=datenum(startdate,'yyyymmddHH');
enddaten  =datenum(enddate  ,'yyyymmddHH');
currentdate=startdaten;

while ( currentdate <= enddaten )
     
fprintf('CURRENTLY WE ARE COMPUTING TRAJECTORIES FOR DATE=%s\n',datestr(currentdate,'yyyymmddHH'));
config.date_ini=datestr(currentdate,'yyyymmddHH');          
config.date_end=datestr(currentdate+deltat,'yyyymmddHH');
AnalysisTrajStruct=calc_trayectory_fun_2(config);
save([config.result_path '/TRAJECTORIES_2/TRAJ_' config.date_ini '_L' num2str(deltat) '.mat'],'AnalysisTrajStruct');

currentdate=currentdate+frec;

end

%matlabpool close
 
 
