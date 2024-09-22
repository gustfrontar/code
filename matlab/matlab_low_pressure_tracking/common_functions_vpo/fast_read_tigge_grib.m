function [error]=fast_read_tigge_grib(gribfile,lonlatflag,tmpfile)
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

NReg=1;

%==========================================================================
% DECODE DATA TO A TEMPORARY FILE
%==========================================================================
%[wgrib ' -d ' num2str(NReg) ...
%           ' -bin '  tmpfile ' ' gribfile]

[error,record_info]=unix([wgrib ' -order raw -d ' num2str(NReg) ...
           ' -bin '  tmpfile ' ' gribfile]);
      
%Before proceeding we should check if the record info is consistent with
%the desired information.

if(error ~=0)
    return    %Do not continue executing the function
end

% record_info=strread(record_info,'%s','delimiter',':');
% record_date=record_info{3};
% record_date=record_date(3:end);



%==========================================================================
% WRITE LAT AND LON GRIDS TO A TEMPORARY FILE
%==========================================================================


if(lonlatflag)
 [nada info]=unix([wgrib ' ' gribfile ' -d ' num2str(NReg) ' -V']);
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
 
 
