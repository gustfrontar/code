clear all
close all

startdate='2008082012';
enddate='2008093012';
traj_start_frec=24;
ens_size=40;

best_track_file='./JMA_BESTTRACK.mat';
PATH_EXPERIMENT='/home/jruiz/datos/EXPERIMENTS/FORECAST_QFX0DNOZLOC40M_MEMNC/';

PATH_FIGURE='./FIGURES/';
mkdir(PATH_FIGURE);

leadtime=72;
timefrec=3;
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

nbt=size(CurrentTracks,2) %Number of best tracks for this date.

%For each CurrentTrack we will get the forecasted track.
for it=1:nbt

for iens=1:ens_size
   %Comparison with EXP 40 MEM CONTROL FORECASTED TRACK ------------------
   strens=num2str(iens);
   
      ExperimentBestTrack(iens).minlon=NaN;
      ExperimentBestTrack(iens).minlat=NaN;
      ExperimentBestTrack(iens).daten=NaN;
      ExperimentBestTrack(iens).minanom=NaN;
      ExperimentBestTrack(iens).maxwind=NaN;
      ExperimentBestTrack(iens).uvelf=NaN;
      ExperimentBestTrack(iens).vvelf=NaN;
      ExperimentBestTrack(iens).trajnumber=NaN;
      ExperimentBestTrack(iens).dist_track=NaN;
      
   file_track=[ PATH_EXPERIMENT '/trajectories/' datestr(sf,'yyyymmddHH') '/trajectories_' strens '.mat'];
   fid=fopen(file_track);if(fid>0);fclose(fid);end
   if(fid > 0)
   load(file_track);
   clear ExperimentTrack;
   ExperimentTrack=TrajStruct;
   ngt=size(ExperimentTrack,2);
    BestDist=1e10;
    for jj=1:ngt
    
     lon1=CurrentTracks(it).minlon;
     lat1=CurrentTracks(it).minlat;
     times1=CurrentTracks(it).daten;
     lon2=ExperimentTrack(jj).minlon;
     lat2=ExperimentTrack(jj).minlat;
     times2=ExperimentTrack(jj).daten;
     [dist_track npuntos ntimes index1 index2]...
     =compare_track_fun(lon1,lat1,times1,lon2,lat2,times2,maxdist);
 
     %Calculo la distancia pesada por el tiempo (la distancia de los
     %primeros tiempos es la mas importante y la de los tiempos posteriores
     %la menos importante).
     tmp=1:length(dist_track);
     wdist=sum(dist_track.*(1./tmp))/sum(1./tmp);
 
 
     %if(mean(dist_track)/maxdist < 3 && mean(dist_track) < BestDist  & length(index2) > 2)
     if(wdist/maxdist < 1 && wdist < BestDist  & length(index2) > 2)
      ExperimentBestTrack(iens).minlon=ExperimentTrack(jj).minlon;
      ExperimentBestTrack(iens).minlat=ExperimentTrack(jj).minlat;
      ExperimentBestTrack(iens).daten=ExperimentTrack(jj).daten;
      ExperimentBestTrack(iens).minanom=ExperimentTrack(jj).minanomf;
      ExperimentBestTrack(iens).maxwind=ExperimentTrack(jj).maxwind;
      ExperimentBestTrack(iens).uvelf=ExperimentTrack(jj).uvelf;
      ExperimentBestTrack(iens).vvelf=ExperimentTrack(jj).vvelf;
      ExperimentBestTrack(iens).trajnumber=jj;
      ExperimentBestTrack(iens).dist_track=NaN(1,length(ExperimentBestTrack(iens).minlon));
      ExperimentBestTrack(iens).dist_track(index2)=dist_track;
      
      BestDist=wdist;
     end
    end
   end
    
end

%
MeanBestTrack.daten=sf:timefrec/24:ef;
nelements=length(sf:timefrec/24:ef);
MeanBestTrack.minlon=zeros(1,nelements);
MeanBestTrack.minlat=zeros(1,nelements);
MeanBestTrack.minanom=zeros(1,nelements);
MeanBestTrack.maxwind=zeros(1,nelements);
MeanBestTrack.ntraj=zeros(1,nelements);

for iens=1:ens_size
    
[intersection,index1,index2]=intersect(MeanBestTrack.daten,ExperimentBestTrack(iens).daten);

MeanBestTrack.minlon(index1)=MeanBestTrack.minlon(index1)+ExperimentBestTrack(iens).minlon(index2);
MeanBestTrack.minlat(index1)=MeanBestTrack.minlat(index1)+ExperimentBestTrack(iens).minlat(index2);
MeanBestTrack.minanom(index1)=MeanBestTrack.minanom(index1)+ExperimentBestTrack(iens).minanom(index2);
MeanBestTrack.maxwind(index1)=MeanBestTrack.maxwind(index1)+ExperimentBestTrack(iens).maxwind(index2);
MeanBestTrack.ntraj(index1)=MeanBestTrack.ntraj(index1)+1;

end

%CALCULO LA MEDIA DE LAS POSICIONES DE LOS MIEMBROS DEL ENSAMBLE.

MeanBestTrack.ntraj(MeanBestTrack.ntraj == 0)=NaN;

MeanBestTrack.minlon=MeanBestTrack.minlon./MeanBestTrack.ntraj;
MeanBestTrack.minlat=MeanBestTrack.minlat./MeanBestTrack.ntraj;
MeanBestTrack.minanom=MeanBestTrack.minanom./MeanBestTrack.ntraj;
MeanBestTrack.maxwind=MeanBestTrack.maxwind./MeanBestTrack.ntraj;

isnanntraj=isnan(MeanBestTrack.ntraj);
MeanBestTrack.minlon(isnanntraj)=[];
MeanBestTrack.minlat(isnanntraj)=[];
MeanBestTrack.minanom(isnanntraj)=[];
MeanBestTrack.maxwind(isnanntraj)=[];
MeanBestTrack.daten(isnanntraj)=[];
MeanBestTrack.ntraj(isnanntraj)=[];

lon1=CurrentTracks(it).minlon;
lat1=CurrentTracks(it).minlat;
times1=CurrentTracks(it).daten;
lon2=MeanBestTrack.minlon;
lat2=MeanBestTrack.minlat;
times2=MeanBestTrack.daten;
     
%CALCULO EL ERROR DE POSICION PARA LA MEDIA DEL ENSAMBLE.
[dist_track npuntos ntimes index1 index2]...
    =compare_track_fun(lon1,lat1,times1,lon2,lat2,times2,maxdist);
 
MeanBestTrack.dist_track=NaN(1,length(MeanBestTrack.minlon));
MeanBestTrack.dist_track(index2)=dist_track;


%-----------------------------


   %Plot traj comparison.
   figure
   subplot(2,2,1)
   %Trajectory plot.
     hold on
     title([datestr(sf) ' - ' CurrentTracks(it).name ' Track'])
     xlabel('Longitude');ylabel('Latitude')
     plot(CurrentTracks(it).minlon,CurrentTracks(it).minlat,'-ko','LineWidth',3,'MarkerSize',2);
     for iens=1:ens_size
     plot(ExperimentBestTrack(iens).minlon,ExperimentBestTrack(iens).minlat,'-r','LineWidth',0.5,'MarkerSize',2);
     end
     plot(MeanBestTrack.minlon,MeanBestTrack.minlat,'-bo','LineWidth',3,'MarkerSize',2);

 
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
 
     plot((CurrentTracks(it).daten-sf)*24,CurrentTracks(it).minanomf,'-ko','LineWidth',3);
     for iens=1:ens_size
     plot((ExperimentBestTrack(iens).daten-sf)*24, ExperimentBestTrack(iens).minanom,'-r','LineWidth',0.5);
     end
     plot((MeanBestTrack.daten-sf)*24,MeanBestTrack.minanom,'-bo','LineWidth',3);
     axis([0 72 900 1010])
     

   subplot(2,2,3)
   %Min pressure comparison.
     hold on
     title([datestr(sf) ' - ' strtrim(CurrentTracks(it).name) ' Max. Wind'])
     xlabel('Time (hours)');ylabel('Wind (m/s)')

     plot((CurrentTracks(it).daten-sf)*24,CurrentTracks(it).maxwind,'-ko','LineWidth',2);
     for iens=1:ens_size
     plot((ExperimentBestTrack(iens).daten-sf)*24, ExperimentBestTrack(iens).maxwind,'-r','LineWidth',0.5);
     end
     plot((MeanBestTrack.daten-sf)*24,MeanBestTrack.maxwind,'-bo','LineWidth',3);


     axis([0 72 0 60])
     %legend('JMA Best Track','GFS','CTRL')

    %Dist error.
    subplot(2,2,4)
     title([datestr(sf) ' - ' strtrim(CurrentTracks(it).name) ' Dist. error'])
     xlabel('Time (hours)');ylabel('Error (km)')
     hold on
     for iens=1:ens_size
     plot((ExperimentBestTrack(iens).daten-sf)*24, ExperimentBestTrack(iens).dist_track/1e3,'-or','LineWidth',1);
     end
     plot((MeanBestTrack.daten-sf)*24, MeanBestTrack.dist_track/1e3,'-ob','LineWidth',2);

     
     print('-dpng',[PATH_FIGURE '/QFX0DNOZLOC' datestr(sf,'yyyymmddHH') '_' strtrim(CurrentTracks(it).name) '_BestTrackComp.png'])

end

cdnum=cdnum+traj_start_frec/24;
end

