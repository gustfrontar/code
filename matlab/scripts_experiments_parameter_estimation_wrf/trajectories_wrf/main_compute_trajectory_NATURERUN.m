%==========================================================================
% TRAJECTORIES
%==========================================================================
clear all
close all

startdate='2008080700';
enddate='2008092900';
%traj_start_frec=24;
ens_size=1;

%leadtime=72;
timefrec=1;
path_exp='/home/jruiz/datos/EXPERIMENTS/TRUE_RUN_SINLAKU_CONSTANT_QFX0.8_60KM/';
unix(['mkdir -p ' path_exp  '/trajectories/']);


%ednum=datenum(enddate,'yyyymmddHH');
%sdnum=datenum(startdate,'yyyymmddHH');

%cdnum=sdnum;

%while (cdnum <= ednum)

for iens=1:ens_size
%Compute ANAL trajectory
%currentdate=datestr(cdnum,'yyyymmddHHMM');

clear TrajStruct

[TrajStruct MinStruct]=calc_trajectory_fun_nature(path_exp,startdate,enddate,iens,timefrec);

unix(['mkdir -p ' path_exp  '/trajectories/']);

ntracks=size( TrajStruct , 2 );

ibt=0

for itr=1:ntracks;

  %Add some additional conditions to keep only tropical storms.

  if( TrajStruct(itr).length > 48 & TrajStruct(itr).minlat(1) < 30.0 & nanmin(TrajStruct(itr).minanomf) < 990.0  )

   ibt=ibt+1;

   BestTrack(ibt)=TrajStruct(itr);

  end

end

save([path_exp '/trajectories/BestTrackNatureRun.mat'],'BestTrack');

end

%cdnum=cdnum+traj_start_frec/24;

%end


