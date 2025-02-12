%==========================================================================
% TRAJECTORIES FOR THE HFX EXPERIMENT
%==========================================================================
clear all
close all

startdate='2008082100';
enddate='2008093000';
traj_start_frec=12;

leadtime=72;
timefrec=1;
path_exp='/data/letkf02/jruiz/FORECAST_EXP_40mem_sfflux_ntpar_smooth_fixparinf0.05_qfxhfx_loc/';
unix(['mkdir -p ' path_exp  '/trajectories/']);

addpath(genpath('/homes/metofac/jruiz/mexnc/'))
addpath(genpath('/homes/metofac/jruiz/netcdf_toolbox/'))

ednum=datenum(enddate,'yyyymmddHH');
sdnum=datenum(startdate,'yyyymmddHH');

cdnum=sdnum;

while (cdnum <= ednum)
%Compute ANAL trajectory
currentdate=datestr(cdnum,'yyyymmddHHMM');
[TrajStruct]=calc_trajectory_fun_forecast_sfflux(path_exp,currentdate,leadtime,timefrec);

unix(['mkdir -p ' path_exp  '/trajectories/'  datestr(cdnum,'yyyymmddHH') ]);
save([path_exp '/trajectories/' datestr(cdnum,'yyyymmddHH') '/trajectories.mat'],'TrajStruct');


cdnum=cdnum+traj_start_frec/24;
end


