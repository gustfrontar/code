clear all
close all

startdate='2008082100';
enddate='2008093000';
traj_start_frec=12;

best_track_file='./JMA_BESTTRACK.mat';
PATH_GFS='../../copy/FORECAST_GFS/trajectories/';
PATH_LETKFCONTROL='../../copy/FORECAST_EXP_40mem_CONTROL/trajectories/';
PATH_LETKFQFX='../../copy/FORECAST_EXP_40mem_sfflux_ntpar_smooth_fixparinf0.05_qfx/trajectories/';
PATH_LETKFQFXUST='../../copy/FORECAST_EXP_40mem_sfflux_ntpar_smooth_fixparinf0.05_qfxust/trajectories/';
PATH_LETKFQFXHFX='../../copy/FORECAST_EXP_40mem_sfflux_ntpar_smooth_fixparinf0.05_qfxhfx/trajectories/';
PATH_FIGURE='./FIGURES/';
mkdir(PATH_FIGURE);

leadtime=72;
timefrec=1;
maxdist=500e3;    %Max distance in meters.

ednum=datenum(enddate,'yyyymmddHH');
sdnum=datenum(startdate,'yyyymmddHH');

cdnum=sdnum;

%Load besttrack data.
load(best_track_file);

while (cdnum <= ednum)

sf=cdnum;
ef=cdnum+leadtime/24;

%Get for the current forecast period the observed typhoons tracks.
[CurrentTracks]=get_current_tracks_fun(sf,ef,BestTrack);

nbt=size(CurrentTracks,2); %Number of best tracks for this date.

%For each CurrentTrack we will get the forecasted track.
for it=1:nbt
   %Comparison with GFS FORECASTED TRACK ----------------------------------
   file_track=[ PATH_GFS '/' datestr(sf,'yyyymmddHH') '/trajectories.mat'];
    GfsBestTrack.minlon=NaN;
    GfsBestTrack.minlat=NaN;
    GfsBestTrack.daten=NaN;
    GfsBestTrack.minanomf=NaN;
    GfsBestTrack.maxwind=NaN;
    GfsBestTrack.uvelf=NaN;
    GfsBestTrack.vvelf=NaN;
    GfsBestTrack.trajnumber=NaN;
    GfsBestTrack.dist_track=NaN;

   fid=fopen(file_track);if(fid>0);fclose(fid);end
   if(fid > 0)
   load(file_track);
   GfsTrack=TrajStruct;
   ngt=size(GfsTrack,2);
    BestDist=1e10;
    for jj=1:ngt
    %Find the best GFS track for the current best track.
     lon1=CurrentTracks(it).minlon;
     lat1=CurrentTracks(it).minlat;
     times1=CurrentTracks(it).daten;
     lon2=GfsTrack(jj).minlon;
     lat2=GfsTrack(jj).minlat;
     times2=GfsTrack(jj).daten;
     [dist_track npuntos ntimes index1 index2]...
     =compare_track_fun(lon1,lat1,times1,lon2,lat2,times2,maxdist);
     if(mean(dist_track)/maxdist < 3 && mean(dist_track) < BestDist & length(index2) > 3)
      GfsBestTrack.minlon=GfsTrack(jj).minlon(index2);
      GfsBestTrack.minlat=GfsTrack(jj).minlat(index2);
      GfsBestTrack.daten=GfsTrack(jj).daten(index2);
      GfsBestTrack.minanomf=GfsTrack(jj).minanomf(index2); 
      GfsBestTrack.maxwind=GfsTrack(jj).maxwind(index2);
      GfsBestTrack.uvelf=GfsTrack(jj).uvelf(index2);
      GfsBestTrack.vvelf=GfsTrack(jj).vvelf(index2);
      GfsBestTrack.trajnumber=jj;
      GfsBestTrack.dist_track=dist_track;
      BestDist=mean(dist_track);
     end 
    end
    end

   
   %End comparison with GFS FORECASTED TRACK -----------------------------

   %Comparison with EXP 40 MEM CONTROL FORECASTED TRACK ------------------
   file_track=[ PATH_LETKFCONTROL '/' datestr(sf,'yyyymmddHH') '/trajectories.mat'];
      LetkfcontrolBestTrack.minlon=NaN;
      LetkfcontrolBestTrack.minlat=NaN;
      LetkfcontrolBestTrack.daten=NaN;
      LetkfcontrolBestTrack.minanomf=NaN;
      LetkfcontrolBestTrack.maxwind=NaN;
      LetkfcontrolBestTrack.uvelf=NaN;
      LetkfcontrolBestTrack.vvelf=NaN;
      LetkfcontrolBestTrack.trajnumber=NaN;
      LetkfcontrolBestTrack.dist_track=NaN;
   fid=fopen(file_track);if(fid>0);fclose(fid);end
   if(fid > 0)
   load(file_track);
   LetkfcontrolTrack=TrajStruct;
   ngt=size(LetkfcontrolTrack,2);
    BestDist=1e10;
    for jj=1:ngt
    %Find the best GFS track for the current best track.
     lon1=CurrentTracks(it).minlon;
     lat1=CurrentTracks(it).minlat;
     times1=CurrentTracks(it).daten;
     lon2=LetkfcontrolTrack(jj).minlon;
     lat2=LetkfcontrolTrack(jj).minlat;
     times2=LetkfcontrolTrack(jj).daten;
     [dist_track npuntos ntimes index1 index2]...
     =compare_track_fun(lon1,lat1,times1,lon2,lat2,times2,maxdist);
     if(mean(dist_track)/maxdist < 3 && mean(dist_track) < BestDist  & length(index2) > 3)
      LetkfcontrolBestTrack.minlon=LetkfcontrolTrack(jj).minlon(index2);
      LetkfcontrolBestTrack.minlat=LetkfcontrolTrack(jj).minlat(index2);
      LetkfcontrolBestTrack.daten=LetkfcontrolTrack(jj).daten(index2);
      LetkfcontrolBestTrack.minanomf=LetkfcontrolTrack(jj).minanomf(index2);
      LetkfcontrolBestTrack.maxwind=LetkfcontrolTrack(jj).maxwind(index2);
      LetkfcontrolBestTrack.uvelf=LetkfcontrolTrack(jj).uvelf(index2);
      LetkfcontrolBestTrack.vvelf=LetkfcontrolTrack(jj).vvelf(index2);
      LetkfcontrolBestTrack.trajnumber=jj;
      LetkfcontrolBestTrack.dist_track=dist_track;
      BestDist=mean(dist_track);
     end
    end
    end


   %End comparison with LETKF CONTROL FORECASTED TRACK -----------------------------

   %Comparison with EXP 40 MEM QFX FORECASTED TRACK ------------------
   file_track=[ PATH_LETKFQFX '/' datestr(sf,'yyyymmddHH') '/trajectories.mat'];
   fid=fopen(file_track);if(fid>0);fclose(fid);end
      LetkfqfxBestTrack.minlon=NaN;
      LetkfqfxBestTrack.minlat=NaN;
      LetkfqfxBestTrack.daten=NaN;
      LetkfqfxBestTrack.minanomf=NaN;
      LetkfqfxBestTrack.maxwind=NaN;
      LetkfqfxBestTrack.uvelf=NaN;
      LetkfqfxBestTrack.vvelf=NaN;
      LetkfqfxBestTrack.trajnumber=NaN;
      LetkfqfxBestTrack.dist_track=NaN;
   if(fid > 0)
   load(file_track);
   LetkfqfxTrack=TrajStruct;
   ngt=size(LetkfqfxTrack,2);

    BestDist=1e10;
    for jj=1:ngt
    %Find the best GFS track for the current best track.
     lon1=CurrentTracks(it).minlon;
     lat1=CurrentTracks(it).minlat;
     times1=CurrentTracks(it).daten;
     lon2=LetkfqfxTrack(jj).minlon;
     lat2=LetkfqfxTrack(jj).minlat;
     times2=LetkfqfxTrack(jj).daten;
     [dist_track npuntos ntimes index1 index2]...
     =compare_track_fun(lon1,lat1,times1,lon2,lat2,times2,maxdist);
     if(mean(dist_track)/maxdist < 3 && mean(dist_track) < BestDist  & length(index2) > 3)
      LetkfqfxBestTrack.minlon=LetkfqfxTrack(jj).minlon(index2);
      LetkfqfxBestTrack.minlat=LetkfqfxTrack(jj).minlat(index2);
      LetkfqfxBestTrack.daten=LetkfqfxTrack(jj).daten(index2);
      LetkfqfxBestTrack.minanomf=LetkfqfxTrack(jj).minanomf(index2);
      LetkfqfxBestTrack.maxwind=LetkfqfxTrack(jj).maxwind(index2);
      LetkfqfxBestTrack.uvelf=LetkfqfxTrack(jj).uvelf(index2);
      LetkfqfxBestTrack.vvelf=LetkfqfxTrack(jj).vvelf(index2);
      LetkfqfxBestTrack.trajnumber=jj;
      LetkfqfxBestTrack.dist_track=dist_track;
      BestDist=mean(dist_track);
     end
    end
    end


   %End comparison with LETKF CONTROL FORECASTED TRACK -----------------------------

   %Comparison with EXP 40 MEM QFX UST FORECASTED TRACK ------------------
%    file_track=[ PATH_LETKFQFXUST '/' datestr(sf,'yyyymmddHH') '/trajectories.mat'];
%       LetkfqfxustBestTrack.minlon=NaN;
%       LetkfqfxustBestTrack.minlat=NaN;
%       LetkfqfxustBestTrack.daten=NaN;
%       LetkfqfxustBestTrack.minanomf=NaN;
%       LetkfqfxustBestTrack.maxwind=NaN;
%       LetkfqfxustBestTrack.uvelf=NaN;
%       LetkfqfxustBestTrack.vvelf=NaN;
%       LetkfqfxustBestTrack.trajnumber=NaN;
%       LetkfqfxustBestTrack.dist_track=NaN;
%    fid=fopen(file_track);if(fid>0);fclose(fid);end
%    if(fid > 0)
%    load(file_track);
%    LetkfqfxustTrack=TrajStruct;
%    ngt=size(LetkfqfxustTrack,2);
%     BestDist=1e10;
%     for jj=1:ngt
%     %Find the best GFS track for the current best track.
%      lon1=CurrentTracks(it).minlon;
%      lat1=CurrentTracks(it).minlat;
%      times1=CurrentTracks(it).daten;
%      lon2=LetkfqfxustTrack(jj).minlon;
%      lat2=LetkfqfxustTrack(jj).minlat;
%      times2=LetkfqfxustTrack(jj).daten;
%      [dist_track npuntos ntimes index1 index2]...
%      =compare_track_fun(lon1,lat1,times1,lon2,lat2,times2,maxdist);
%      if(mean(dist_track)/maxdist < 3 && mean(dist_track) < BestDist  & length(index2) > 3)
%       LetkfqfxustBestTrack.minlon=LetkfqfxustTrack(jj).minlon(index2);
%       LetkfqfxustBestTrack.minlat=LetkfqfxustTrack(jj).minlat(index2);
%       LetkfqfxustBestTrack.daten=LetkfqfxustTrack(jj).daten(index2);
%       LetkfqfxustBestTrack.minanomf=LetkfqfxustTrack(jj).minanomf(index2);
%       LetkfqfxustBestTrack.maxwind=LetkfqfxustTrack(jj).maxwind(index2);
%       LetkfqfxustBestTrack.uvelf=LetkfqfxustTrack(jj).uvelf(index2);
%       LetkfqfxustBestTrack.vvelf=LetkfqfxustTrack(jj).vvelf(index2);
%       LetkfqfxustBestTrack.trajnumber=jj;
%       LetkfqfxustBestTrack.dist_track=dist_track;
%       BestDist=mean(dist_track);
%      end
%     end
%    end
    

   %End comparison with LETKF CONTROL FORECASTED TRACK -----------------------------
   
   
   %Comparison with EXP 40 MEM QFX HFX FORECASTED TRACK ------------------
   file_track=[ PATH_LETKFQFXHFX '/' datestr(sf,'yyyymmddHH') '/trajectories.mat'];
      LetkfqfxhfxBestTrack.minlon=NaN;
      LetkfqfxhfxBestTrack.minlat=NaN;
      LetkfqfxhfxBestTrack.daten=NaN;
      LetkfqfxhfxBestTrack.minanomf=NaN;
      LetkfqfxhfxBestTrack.maxwind=NaN;
      LetkfqfxhfxBestTrack.uvelf=NaN;
      LetkfqfxhfxBestTrack.vvelf=NaN;
      LetkfqfxhfxBestTrack.trajnumber=NaN;
      LetkfqfxhfxBestTrack.dist_track=NaN;
   fid=fopen(file_track);if(fid>0);fclose(fid);end
   if(fid > 0)
   load(file_track);
   LetkfqfxhfxTrack=TrajStruct;
   ngt=size(LetkfqfxhfxTrack,2);
    BestDist=1e10;
    for jj=1:ngt
    %Find the best GFS track for the current best track.
     lon1=CurrentTracks(it).minlon;
     lat1=CurrentTracks(it).minlat;
     times1=CurrentTracks(it).daten;
     lon2=LetkfqfxhfxTrack(jj).minlon;
     lat2=LetkfqfxhfxTrack(jj).minlat;
     times2=LetkfqfxhfxTrack(jj).daten;
     [dist_track npuntos ntimes index1 index2]...
     =compare_track_fun(lon1,lat1,times1,lon2,lat2,times2,maxdist);
     if(mean(dist_track)/maxdist < 3 && mean(dist_track) < BestDist  & length(index2) > 3)
      LetkfqfxhfxBestTrack.minlon=LetkfqfxhfxTrack(jj).minlon(index2);
      LetkfqfxhfxBestTrack.minlat=LetkfqfxhfxTrack(jj).minlat(index2);
      LetkfqfxhfxBestTrack.daten=LetkfqfxhfxTrack(jj).daten(index2);
      LetkfqfxhfxBestTrack.minanomf=LetkfqfxhfxTrack(jj).minanomf(index2);
      LetkfqfxhfxBestTrack.maxwind=LetkfqfxhfxTrack(jj).maxwind(index2);
      LetkfqfxhfxBestTrack.uvelf=LetkfqfxhfxTrack(jj).uvelf(index2);
      LetkfqfxhfxBestTrack.vvelf=LetkfqfxhfxTrack(jj).vvelf(index2);
      LetkfqfxhfxBestTrack.trajnumber=jj;
      LetkfqfxhfxBestTrack.dist_track=dist_track;
      BestDist=mean(dist_track);
     end
    end
    end



   %Plot traj comparison.
   figure
   subplot(2,2,1)
   %Trajectory plot.
     hold on
     title([datestr(sf) ' - ' CurrentTracks(it).name ' Track'])
     xlabel('Longitude');ylabel('Latitude')
     plot(CurrentTracks(it).minlon,CurrentTracks(it).minlat,'-ko','LineWidth',2,'MarkerSize',2);
     %plot(GfsBestTrack.minlon,GfsBestTrack.minlat,'-go','LineWidth',2,'MarkerSize',2);
     plot(LetkfcontrolBestTrack.minlon,LetkfcontrolBestTrack.minlat,'-bo','LineWidth',2,'MarkerSize',2);
     plot(LetkfqfxBestTrack.minlon,LetkfqfxBestTrack.minlat,'-go','LineWidth',2,'MarkerSize',2);
     %plot(LetkfqfxustBestTrack.minlon,LetkfqfxustBestTrack.minlat,'-mo','LineWidth',2,'MarkerSize',2);
     plot(LetkfqfxhfxBestTrack.minlon,LetkfqfxhfxBestTrack.minlat,'-ro','LineWidth',2,'MarkerSize',2);
     
     %legend('JMA Best Track','GFS','CTRL','QFX','QFX-UST','Location','SouthWest')
     load coast
     plot(long,lat);
     maxlon=max(CurrentTracks(it).minlon);
     minlon=min(CurrentTracks(it).minlon);
     maxlat=max(CurrentTracks(it).minlat);
     minlat=min(CurrentTracks(it).minlat);
     axis([minlon-5 maxlon+5 minlat-5 maxlat+5])

   subplot(2,2,2)
   %Min pressure comparison.
     hold on
     title([datestr(sf) ' - ' strtrim(CurrentTracks(it).name) ' Min. Press.'])
     xlabel('Time (hours)');ylabel('Min. Press. (hPa.)')
 
     plot((CurrentTracks(it).daten-sf)*24,CurrentTracks(it).minanomf,'-ko','LineWidth',2);
     %plot((GfsBestTrack.daten-sf)*24, GfsBestTrack.minanomf,'-go','LineWidth',2);
     plot((LetkfcontrolBestTrack.daten-sf)*24, LetkfcontrolBestTrack.minanomf,'-bo','LineWidth',2);
     plot((LetkfqfxBestTrack.daten-sf)*24, LetkfqfxBestTrack.minanomf,'-go','LineWidth',2); 
     %plot((LetkfqfxustBestTrack.daten-sf)*24, LetkfqfxustBestTrack.minanomf,'-mo','LineWidth',2);
     plot((LetkfqfxhfxBestTrack.daten-sf)*24, LetkfqfxhfxBestTrack.minanomf,'-ro','LineWidth',2);

     axis([0 72 900 1010])
     %legend('JMA Best Track','GFS','CTRL','QFX','QFX-UST') 

   subplot(2,2,3)
   %Min pressure comparison.
     hold on
     title([datestr(sf) ' - ' strtrim(CurrentTracks(it).name) ' Max. Wind'])
     xlabel('Time (hours)');ylabel('Wind (m/s)')

     plot((CurrentTracks(it).daten-sf)*24,CurrentTracks(it).maxwind,'-ko','LineWidth',2);
     %plot((GfsBestTrack.daten-sf)*24, GfsBestTrack.maxwind,'-go','LineWidth',2);
     plot((LetkfcontrolBestTrack.daten-sf)*24, LetkfcontrolBestTrack.maxwind,'-bo','LineWidth',2);
     plot((LetkfqfxBestTrack.daten-sf)*24, LetkfqfxBestTrack.maxwind,'-go','LineWidth',2);
     %plot((LetkfqfxustBestTrack.daten-sf)*24, LetkfqfxustBestTrack.maxwind,'-mo','LineWidth',2);
     plot((LetkfqfxhfxBestTrack.daten-sf)*24, LetkfqfxhfxBestTrack.maxwind,'-ro','LineWidth',2);


     axis([0 72 0 60])
     %legend('JMA Best Track','GFS','CTRL')

    %Dist error.
    subplot(2,2,4)
     title([datestr(sf) ' - ' strtrim(CurrentTracks(it).name) ' Dist. error'])
     xlabel('Time (hours)');ylabel('Error (km)')
     hold on
     %plot((GfsBestTrack.daten-sf)*24,GfsBestTrack.dist_track/1e3,'-go','LineWidth',2);
     plot((LetkfcontrolBestTrack.daten-sf)*24, LetkfcontrolBestTrack.dist_track/1e3,'-bo','LineWidth',2);
     plot((LetkfqfxBestTrack.daten-sf)*24, LetkfqfxBestTrack.dist_track/1e3,'-go','LineWidth',2);
     %plot((LetkfqfxustBestTrack.daten-sf)*24, LetkfqfxustBestTrack.dist_track/1e3,'-mo','LineWidth',2);
     plot((LetkfqfxhfxBestTrack.daten-sf)*24, LetkfqfxhfxBestTrack.dist_track/1e3,'-ro','LineWidth',2);



     %legend('GFS','CTRL','QFX','QFX-UST')
     
     print('-dpng',[PATH_FIGURE '/' datestr(sf,'yyyymmddHH') '_' strtrim(CurrentTracks(it).name) '_BestTrackComp.png'])

end

cdnum=cdnum+traj_start_frec/24;
end

