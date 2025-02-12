%==========================================================================
% TRAJECTORIES FOR THE HFX EXPERIMENT
%==========================================================================
clear all
close all

startdate='2008090900';
leadtime=120;
timefrec=3;
path='/data/letkf01/jruiz/SENSITIVITY_FLUX_TYPHOON/';

addpath(genpath('/homes/metofac/jruiz/mexnc/'))
addpath(genpath('/homes/metofac/jruiz/netcdf_toolbox/'))

%Compute FNL trajectory
[TrajStructFNL]=calc_trajectory_fun_fnl(path,startdate,leadtime,timefrec);

% count=1;
% for iifactor=0.5:0.1:1.5 
% filename=[path 'wrfout_HFX_' num2str(iifactor) '.nc'];
% fprintf('Reading and computing trajectories for experiment %s',filename)
% [TrajStruct(count).trajectories]=calc_trajectory_fun_wrf(filename,startdate,leadtime,timefrec);
% count=count+1;
% end
% 
% %Agrupo las trayectorias de los diferentes experimentos y las trayectorias
% %del FNL.
% [group]=group_trajectories_fun_wrf(TrajStruct,TrajStructFNL,startdate,leadtime,timefrec);
% 
% 
% save('TRAJECTORIES_HFX.mat','group','TrajStructFNL','TrajStruct');
% 
% %==========================================================================
% % TRAJECTORIES FOR THE QFX EXPERIMENT
% %==========================================================================
% clear all
% close all
% 
% startdate='2008090900';
% leadtime=120;
% timefrec=3;
% path='../';
% 
% addpath(genpath('/homes/metofac/jruiz/mexnc/'))
% addpath(genpath('/homes/metofac/jruiz/netcdf_toolbox/'))
% 
% %Compute FNL trajectory
% [TrajStructFNL]=calc_trajectory_fun_fnl(path,startdate,leadtime,timefrec);
% 
% count=1;
% for iifactor=0.5:0.1:1.5 
% filename=[path 'wrfout_QFX_' num2str(iifactor) '.nc'];
% fprintf('Reading and computing trajectories for experiment %s',filename)
% [TrajStruct(count).trajectories]=calc_trajectory_fun_wrf(filename,startdate,leadtime,timefrec);
% count=count+1;
% end
% 
% %Agrupo las trayectorias de los diferentes experimentos y las trayectorias
% %del FNL.
% [group]=group_trajectories_fun_wrf(TrajStruct,TrajStructFNL,startdate,leadtime,timefrec);
% 
% 
% save('TRAJECTORIES_QFX.mat','group','TrajStructFNL','TrajStruct');

%==========================================================================
% TRAJECTORIES FOR THE UST EXPERIMENT
%==========================================================================
clear all
close all

startdate='2008090900';
leadtime=120;
timefrec=3;
path='../';

addpath(genpath('/homes/metofac/jruiz/mexnc/'))
addpath(genpath('/homes/metofac/jruiz/netcdf_toolbox/'))

%Compute FNL trajectory
[TrajStructFNL]=calc_trajectory_fun_fnl(path,startdate,leadtime,timefrec);

count=1;
for iifactor=0.5:0.1:1.2 
filename=[path 'wrfout_UST_' num2str(iifactor) '.nc'];
fprintf('Reading and computing trajectories for experiment %s',filename)
[TrajStruct(count).trajectories]=calc_trajectory_fun_wrf(filename,startdate,leadtime,timefrec);
count=count+1;
end

%Agrupo las trayectorias de los diferentes experimentos y las trayectorias
%del FNL.
[group]=group_trajectories_fun_wrf(TrajStruct,TrajStructFNL,startdate,leadtime,timefrec);


save('TRAJECTORIES_UST.mat','group','TrajStructFNL','TrajStruct');

