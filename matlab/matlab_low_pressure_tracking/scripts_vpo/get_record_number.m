function fast_read_tigge_grib(gribfile,config,cvar,cleadtime,cens,cdate,lonlatflag,tmpfile)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
wgrib='wgrib2';   

%==========================================================================
% As TIGGE files are very large then look through the inventory takes a lot
% of time. The fast way in this case is to get the grib record number based
% on the TIGGE file order.
%==========================================================================

%==========================================================================
%COMPUTE RECORD NUMBER.
%==========================================================================

%cvar, cleadtime and cens are the current variable number, current leadtime
%in hours and current ensemble member number.

nleadtime=cleadtime/config.timefrec+1;

nmaxleadtime=config.maxleadtime/config.timefrec+1;

NRegsPerInitialization=config.nvars*nmaxleadtime*config.enssize; %Number of registers per initialization time.

PreviousInitialization=floor(24*(datenum(cdate,'yyyymmddHH')-datenum(cdate(1:6),'yyyymm'))/config.timebetweenforecast);

NReg=NRegsPerInitialization*PreviousInitialization+ ...
     (nleadtime-1)*config.enssize*config.nvars+...
     (cens-1)*config.nvars+...
     cvar;



 
%==========================================================================
% DECODE DATA TO A TEMPORARY FILE
%==========================================================================
%[wgrib ' -d ' num2str(NReg) ...
%           ' -bin '  tmpfile ' ' gribfile]
%[~,~]=
unix([wgrib ' -d ' num2str(NReg) ...
           ' -bin '  tmpfile ' ' gribfile]);

%==========================================================================
% WRITE LAT AND LON GRIDS TO A TEMPORARY FILE
%==========================================================================

if(lonlatflag)
 [~, info]=unix([wgrib ' ' gribfile ' -d ' num2str(NReg) ' -V']);
% 
 info=strread(info,'%s','delimiter','\n','whitespace','');
 [tmp]=strread(info{5},'%s');
% 
 
 lat_min=str2double(tmp{2});
 lat_max=str2double(tmp{4});

% 
 coef=1;
 if(lat_min > lat_max)
     coef=-1; %El orden de las latitudes esta invertido.
 end
 delta_lat=str2double(tmp{6});
 grid_lat=lat_min:(coef)*delta_lat:lat_max;
% 
 [tmp]=strread(info{6},'%s');
% 
 lon_min=str2double(tmp{2});
 lon_max=str2double(tmp{4});
 delta_lon=str2double(tmp{6});
 if(lon_max < lon_min)
     lon_max=lon_max+360;
 end

 grid_lon=lon_min:delta_lon:lon_max;

 [longitudes latitudes]=meshgrid(grid_lon,grid_lat);

 fidlat=fopen('DataLatGribOut.bin','w');
 fwrite(fidlat,latitudes','single');
 fclose(fidlat);
 fidlon=fopen('DataLonGribOut.bin','w');
 fwrite(fidlon,longitudes','single');
 fclose(fidlon);
end
 
 
