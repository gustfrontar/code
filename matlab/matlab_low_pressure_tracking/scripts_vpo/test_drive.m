clear all
close all

%matlabpool

%INCLUDE SPECIFIC FUNCTIONS DIRECTORY.
addpath('../common_functions_vpo/');
%This script computes the trajectories corresponding to the NCEP reanalysis
%starting every 12 hours and for a period of 7 days.
startdate='2008052512';     %Fecha inicial del calculo.
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

fprintf('CURRENTLY WE ARE COMPUTING TRAJECTORIES FOR DATE=%s\n',datestr(startdaten,'yyyymmddHH'));
config.date_ini=datestr(startdaten,'yyyymmddHH');          
config.date_end=datestr(startdaten+deltat,'yyyymmddHH');
AnalysisTrajStruct=calc_trayectory_fun_2(config);
TrajStruct=AnalysisTrajStruct;
save([config.result_path '/TRAJECTORIES_2/TRAJ_' config.date_ini '_L' num2str(deltat) '.mat'],'AnalysisTrajStruct');




%SOME PLOTS TO CONTROL THE SYSTEM BEHAVIOUR
figure
load coast   
long(long<0)=long(long<0)+360;
for ii=1:(length(long)-1)
    if(abs(long(ii) - long(ii+1)) > 180)
        long(ii)=NaN;
        lat(ii)=NaN;
    end
end
plot(long,lat)
hold on
a=jet(20);
icolor=1;
for ii=1:size(TrajStruct,2)
  if(length(TrajStruct(ii).minlat) >0)
   tmplon=TrajStruct(ii).minlonf;
   tmplon2=TrajStruct(ii).minlon;
   for jj=1:(length(tmplon)-1)
       if(abs(tmplon(jj)-tmplon(jj+1)) > 180)
           tmplon(jj)=NaN;
       end
       if(abs(tmplon2(jj)-tmplon2(jj+1))>180)
           tmplon2(jj)=NaN;
       end
   end
    
   %plot(tmplon,TrajStruct(ii).minlatf,'-o','Color',a(icolor,:),'LineWidth',2)
   plot(tmplon2,TrajStruct(ii).minlat,'--o','Color',a(icolor,:),'LineWidth',2);
   icolor=icolor+3;
   if(icolor > size(a,1))
       icolor=1;
   end
  end
end

figure
icolor=1
for ii=1:size(TrajStruct,2)
   if(length(TrajStruct(ii).minlat) >0)
   subplot(2,2,1)
   hold on
   %plot(TrajStruct(ii).uvel,'-o','Color',a(icolor,:))
   plot(TrajStruct(ii).uvelf,'--o','Color',a(icolor,:))
   title('West-East Velocity')
   subplot(2,2,2)
   hold on
   %plot(TrajStruct(ii).vvel,'-o','Color',a(icolor,:))
   plot(TrajStruct(ii).vvelf,'--o','Color',a(icolor,:))
   title('South-North Velocity')
   subplot(2,2,3)
   hold on
   plot(TrajStruct(ii).minareaf,'-o','Color',a(icolor,:))
   %plot(TrajStruct(ii).minlap,'--o','Color',a(icolor,:))
   title('System size')
   subplot(2,2,4)
   hold on
   %plot(TrajStruct(ii).minanomsis,'-o','Color',a(icolor,:))
   plot(TrajStruct(ii).meananomf,'--o','Color',a(icolor,:))
   title('Min Anomaly')
   icolor=icolor+3;
   if(icolor > size(a,1))
       icolor=1;
   end
   end
end
% 
 trajectory_plot_fun(TrajStruct,config);

 
 