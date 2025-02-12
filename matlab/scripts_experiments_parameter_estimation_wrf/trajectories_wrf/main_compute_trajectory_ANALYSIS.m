%==========================================================================
% TRAJECTORIES
%==========================================================================
clear all
close all

startdate='2008082012';
enddate='2008093012';
%traj_start_frec=24;
ens_size=40;

%leadtime=72;
timefrec=6;
path_exp='/home/jruiz/datos/EXPERIMENTS/QFX0DNOZLOC40M_MEMNC/';
unix(['mkdir -p ' path_exp  '/trajectories/']);


%ednum=datenum(enddate,'yyyymmddHH');
%sdnum=datenum(startdate,'yyyymmddHH');

%cdnum=sdnum;

%while (cdnum <= ednum)

for iens=1:ens_size
%Compute ANAL trajectory
%currentdate=datestr(cdnum,'yyyymmddHHMM');

clear TrajStruct

[TrajStruct]=calc_trajectory_fun_anal(path_exp,startdate,enddate,iens,timefrec);

unix(['mkdir -p ' path_exp  '/trajectories/']);
save([path_exp '/trajectories/trajectories_' startdate '' num2str(iens) '.mat'],'TrajStruct');

end

%cdnum=cdnum+traj_start_frec/24;

%end


