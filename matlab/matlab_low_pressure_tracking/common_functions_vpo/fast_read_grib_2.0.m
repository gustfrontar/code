function fast_read_grib_2(gribfile,matchinput,tmpfile)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
wgrib='wgrib';     %If not in the PATH of the system then have to use the 
                  %complete path for wgrib
%==========================================================================
%GET INVENTORY USING WGRIB AND LOAD RESULT INTO A MATLAB STRUCTURE.
%==========================================================================

%DEFINE PRIOR GRID
original_resol=2.5;
lat_original=-90:original_resol:90;
lon_original=0:original_resol:360-original_resol;

%DEFINE FINAL GRID
final_resol=0.5;

lat_final=-90:final_resol:90;
lon_final=0:final_resol:360-final_resol;

[lon_final lat_final]=meshgrid(lon_final,lat_final);

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
% REGRID DATA TO A HIGHER RESOLUTION USING CUBIC SPLINES AND 
% AND COMPUTE SYSTEM INTENSITY WITH AN APROXIMATION TO THE
% LAPLACIAN
%==========================================================================
 
 fid=fopen(tmpfile,'r');
 fread(fid,[1],'single');
 data=fread(fid,[size(lat_original')],'single')';
 fclose(fid);
 
 data_final=regrid2(lon_original,lat_original,data,lon_final,lat_final,'splines');
 
 fid=fopen(tmpfile,'w');
 
 [tmp1,tmp2]=gradient(data_final);
 laplaciano=-divergence(tmp1,tmp2)/(final_resol^2);
 
 %THE REGRIDED DATA AND THE INTENSITY DATA ARE STORED INTO THE SAME FILE.
 fwrite(fid,data_final','single');
 fwrite(fid,laplaciano','single');
 fclose(fid);
 
 
%==========================================================================
% WRITE LAT AND LON GRIDS TO A TEMPORARY FILE
%==========================================================================

 fidlat=fopen('DataLatGribOut.bin','w');
 fwrite(fidlat,lat_final','single');
 fclose(fidlat);
 fidlon=fopen('DataLonGribOut.bin','w');
 fwrite(fidlon,lon_final','single');
 fclose(fidlon);


end
 
 
