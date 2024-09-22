%Este script sirve para probar el funcionamiento del codigo de calculo de
%las trayectorias. 

clear all
close all

%INCLUDE SPECIFIC FUNCTIONS DIRECTORY.
addpath('../common_functions_vpo/');


config.date_ini='2008020112';          
config.date_end='2008020812';
config.data_path='../DATA/REA2NCEP/HGTVPO/';
config.clim_path='../RESULTS/CLIMATOLOGY/NCEP500HPA_2.5/';
config.result_path='../RESULTS/ANALISIS/';
config.climprefix='HGT';
config.climsufix='.grd';
config.climdateformat='mmddHH';
config.dataformat='GRIB';
config.gribversion=1;
config.dataprefix='REA2NCEP_HGT_';
config.datasufix='.grib';
config.datadateformat='yyyymmddHH';
config.isforecast=false;
config.enssize=1;
config.timefrec=6;                                     



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
   if(length(TrajStruct(ii).minlat) > 15)
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
% 
 trajectory_plot_fun(TrajStruct,config);

 
 