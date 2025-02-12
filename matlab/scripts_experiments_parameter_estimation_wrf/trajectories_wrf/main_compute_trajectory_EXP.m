%==========================================================================
% TRAJECTORIES
%==========================================================================
clear all
close all

startdate='2008082012';
enddate='2008093012';
traj_start_frec=24;
ens_size=40;

leadtime=72;
timefrec=3;
path_exp='/home/jruiz/datos/EXPERIMENTS/FORECAST_CONTROL40M_MEMNC/';
unix(['mkdir -p ' path_exp  '/trajectories/']);

%addpath(genpath('/homes/metofac/jruiz/mexnc/'))
%addpath(genpath('/homes/metofac/jruiz/netcdf_toolbox/'))

ednum=datenum(enddate,'yyyymmddHH');
sdnum=datenum(startdate,'yyyymmddHH');

cdnum=sdnum;

while (cdnum <= ednum)

for iens=1:ens_size
%Compute ANAL trajectory
currentdate=datestr(cdnum,'yyyymmddHHMM');

clear TrajStruct

[TrajStruct]=calc_trajectory_fun_forecast(path_exp,currentdate,leadtime,timefrec,iens);

unix(['mkdir -p ' path_exp  '/trajectories/'  datestr(cdnum,'yyyymmddHH') ]);
save([path_exp '/trajectories/' datestr(cdnum,'yyyymmddHH') '/trajectories_' num2str(iens) '.mat'],'TrajStruct');

end

cdnum=cdnum+traj_start_frec/24;
end


