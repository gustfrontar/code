function fast_read_grib(gribfile,matchinput,lonlatflag,tmpfile)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
wgrib='wgrib';     %If not in the PATH of the system then have to use the 
                  %complete path for wgrib
%==========================================================================
%GET INVENTORY USING WGRIB AND LOAD RESULT INTO A MATLAB STRUCTURE.
%==========================================================================

%==========================================================================
%GET INVENTORY OF RECORDS THAT MATCH THE INPUT EXPRESION
%==========================================================================

[status tmpinv]=unix([wgrib ' ' gribfile ' -s | egrep ' matchinput]);
if(status ~=0)
    disp(['Error: ' ])
    disp(tmpinv)
    data=NaN;
    inventory=NaN;
    return
end

invload = strread(tmpinv,'%s','delimiter','\n','whitespace','');

nrecords=length(invload);

 for i=1:nrecords
 str=strread(invload{i},'%s','delimiter',':');
 %GET RECORD NUMBER.
 inventory.recnum(i)=str2double(str{1});
 end
 
 
%==========================================================================
% DECODE DATA TO A TEMPORARY FILE
%==========================================================================

 for ii=1:nrecords
  [nada nada]=unix([wgrib ' ' gribfile ' -d ' num2str(inventory.recnum(ii)) ...
           '-bin -o '  tmpfile]);
 end

%==========================================================================
% WRITE LAT AND LON GRIDS TO A TEMPORARY FILE
%==========================================================================

if(lonlatflag)
 [nada info]=unix([wgrib ' ' gribfile ' -d 1 -V']);
% 
 info=strread(info,'%s','delimiter','\n','whitespace','');
 [tmp]=strread(info{5},'%s');
% 
 
 lat_min=str2double(tmp{3});
 lat_max=str2double(tmp{5});

% 
 coef=1;
 if(lat_min > lat_max)
     coef=-1; %El orden de las latitudes esta invertido.
 end
 delta_lat=str2double(tmp{7});
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
 
 
