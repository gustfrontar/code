clear all
close all

%This script computes the trajectories corresponding to the NCEP reanalysis
%starting every 12 hours and for a period of 7 days.
startdate='2007042312';     %Fecha inicial del calculo.
enddate  ='2007042712';     %Fecha final del calculo.
config.model='kwbc';
config.enssize=21;        %Cual es el numero maximo de miembros en el ensamble.

%CONFIGURATION.............................................................

config.leadtime=168;      %Requested lead time.      
config.data_path=['/home/jruiz/TIGGE/' config.model];
config.data_path=['/home/jruiz/TIGGE/' config.model '/SMALLGRIB/'];
config.result_path=['../../RESULTS/' config.model '/'];
config.clim_path='../../RESULTS/CLIMATOLOGY/NCEP500HPA_2_CFSR/';
config.analysistrajpath='../../RESULTS/ANALISISCFSR/TRAJECTORIES_2/';
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
config.timebetweenforecast=24; %Cada cuanto estan inicializados los pronosticos.
config.mintrajlength=12;       %Minima longitud que debe tener una trayectoria para iniciar un grupo.

%--------------------------------------------------------------------------
%Generate random work directory (to support simultaneous computations).
rand('state',100*sum(clock));
rand_number=rand;
tmp=pwd;
config.randomwork=[pwd '/' num2str(rand_number)];
mkdir(config.randomwork);
%Go to the randomwork.
cd(config.randomwork);

mkdir([config.result_path '/TRAJECTORIES_2/']);
mkdir([config.result_path '/MINIMOS_2/']);
mkdir([config.result_path '/GROUP_2/']);

%INCLUDE SPECIFIC FUNCTIONS DIRECTORY.
addpath('../../common_functions_vpo/');

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

 config.date_ini=datestr(currentdate,'yyyymmddHH');          
 config.date_end=datestr(currentdate+config.leadtime/24,'yyyymmddHH');
% % 
 for iens=1:config.enssize    
 fprintf('PROCESING ENSEMBLE MEMBER=%f \n',iens);
 config.ensemblemember=iens;  %Al incluir el control el miembro 0 representa el control que se guardara en el elemento 1 de los arrays.
 EnsTrajStruct(iens).trajectories=calc_trayectory_fun_2(config); 
 end


%load([config.result_path '/TRAJECTORIES_2/TRAJ_' config.date_ini '_L' num2str(int32(config.leadtime/24)) '.mat'],'EnsTrajStruct');
save([config.result_path '/TRAJECTORIES_2/TRAJ_' config.date_ini '_L' num2str(int32(config.leadtime/24)) '.mat'],'EnsTrajStruct');

%==========================================================================
% COMPUTE GROUPS
%==========================================================================

%Load analysis trayectoris for this date. (se podrian calcular pero es mas
%rapido si las cargamos).

load([config.analysistrajpath '/TRAJ_' config.date_ini '_L' num2str(int32(config.leadtime/24)) '.mat']); 

[group]=group_trajectories_fun_2(EnsTrajStruct,AnalysisTrajStruct,config);

save([config.result_path '/GROUP_2/GROUP_' config.date_ini '_L' num2str(int32(config.leadtime/24)) '.mat'],'group');

currentdate=currentdate+config.timebetweenforecast/24;


end


cd('../');

 
