%Este script sirve para probar el funcionamiento del codigo de calculo de
%las trayectorias. 

clear all
close all

%INCLUDE SPECIFIC FUNCTIONS DIRECTORY.
addpath('../common_functions_vpo/');


config.model='kwbc';
config.enssize=20;        %Cual es el numero maximo de miembros en el ensamble.
config.maxleadtime=168;   %Cual es el maximo lead time de los pronosticos.
config.leadtime=168;      %Requested lead time.
config.ensemblemember=1;

config.date_ini='2008020112';   
config.date_end=datestr(datenum(config.date_ini,'yyyymmddHH')+config.leadtime/24,'yyyymmddHH');
config.data_path=['/media/GUSTFRONT/data/TIGGE/' config.model '/SMALLGRIB/'];
config.clim_path='../RESULTS/CLIMATOLOGY/NCEP500HPA_2/';
config.result_path=['../RESULTS/' config.model '/'];
config.climprefix='HGT';
config.climsufix='.grd';
config.climdateformat='mmddHH';
config.dataformat='GRIB';
config.gribversion=1;
config.dataprefix=['HGT_500'];
config.datasufix='.grib';
config.datadateformat='yyyymmddHH';
config.isforecast=true;
config.timefrec=6;   
config.timebetweenforecast=24;           %Cada cuanto estan inicializados los pronosticos.

mkdir([config.result_path '/TRAJECTORIES/']);
mkdir([config.result_path '/MINIMOS/']);


TrajStruct=calc_trayectory_vorticity_fun(config);


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
  if(length(TrajStruct(ii).minlatf) >= 10)
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
   if(length(TrajStruct(ii).minlat) > 10)
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
   %plot(TrajStruct(ii).minarea,'-o','Color',a(icolor,:))
   plot(TrajStruct(ii).minareaf,'--o','Color',a(icolor,:))
   title('System size')
   subplot(2,2,4)
   hold on
   %plot(TrajStruct(ii).minanomsis,'-o','Color',a(icolor,:))
   plot(TrajStruct(ii).minanomsisf,'--o','Color',a(icolor,:))
   title('Min Anomaly')
   icolor=icolor+3;
   if(icolor > size(a,1))
       icolor=1;
   end
   end
end

%trajectory_plot_forecast_fun(TrajStruct,config);

 
 