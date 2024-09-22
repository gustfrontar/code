function [data inventory] = read_grib(gribfile,gribversion,matchinput,wgribpath)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here

%INITIALIZE OUTPUT.

%==========================================================================
%CHECK WGRIB VERSION INPUT

if(gribversion==1)
    exe='wgrib';
elseif(gribversion==2)
    exe='wgrib2';
else
    disp('Error: Bad gribversion option!');
    return
end

%==========================================================================

%CHECK WHETER WGRIB IS IN THE SYSTEM PATH, IF NOT USE THE PATH PROVIDED BY
%THE USER.

[ ~, tmp]=unix(['which ' exe]); %Buscamos wgrib en el path.
if (isempty(tmp))
    disp(['WARNING: ' exe ' in not in the machine path']);
    disp('I will try to use the path given to this function');
    wgrib=[wgribpath '/' exe];
else
    wgrib=exe;
end
wgrib=strtrim(wgrib);

%==========================================================================
%GET INVENTORY USING WGRIB AND LOAD RESULT INTO A MATLAB STRUCTURE.
%==========================================================================

%==========================================================================
%GET INVENTORY OF RECORDS THAT MATCH THE INPUT EXPRESION
%==========================================================================

[status tmpinv]=unix([wgrib ' ' gribfile ' -s | egrep ' matchinput]);
if(status ~=0)
    disp(['Error: ' exe ])
    disp(tmpinv)
    data=NaN;
    inventory=NaN;
    return
end

invload = strread(tmpinv,'%s','delimiter','\n','whitespace','');

nrecords=length(invload);

%==========================================================================
%SAVE INVENTORY INTO A STRUCTURED ARRAY CALLED INV.
%==========================================================================


 for i=1:nrecords
 str=strread(invload{i},'%s','delimiter',':');
 nelements=length(str);
 
 %GET RECORD NUMBER.
 inventory.recnum(i)=str2double(str{1});
% %GET VARNAME
 inventory.varname{i}=(str{4});
% 
% %SEARCH FOR THE DATE RECORD.  
tmpdate=str{3}(3:end);
%     %Estamos frente al registro de la fecha.
         if(gribversion==1)
             inventory.year(i)=str2double(tmpdate(1:2));
             inventory.month(i)=str2double(tmpdate(3:4));
             inventory.day(i)=str2double(tmpdate(5:6));
             inventory.hour(i)=str2double(tmpdate(7:8));
             inventory.datenum(i)=datenum(tmpdate,'yymmddHH');
         elseif(gribversion==2)
             inventory.year(i)=str2double(tmpdate(1:4));
             inventory.month(i)=str2double(tmpdate(5:6));
             inventory.day(i)=str2double(tmpdate(7:8));
             inventory.hour(i)=str2double(tmpdate(9:10));
             inventory.datenum(i)=datenum(tmpdate,'yyyymmddHH');
         end

% %SEARCH FOR THE LEVEL RECORD
 if(nelements >= 5)
 inventory.lev(i)=str2num(str{5}(1:4)); %#ok<ST2NM>
 
 else
 inventory.lev(i)=NaN;
 end
 if(nelements >=6)
% %SEARCH FOR THE FORECAST HOUR.
 if(strcmp(str{6},'anl'))
     inventory.frcst(i)=0;
 else
     inventory.frcst(i)=str2num(str{6}(1:3)); %#ok<ST2NM>
 end
 else
   inventory.frcst(i)=NaN;
 end
 if(nelements >= 7 && strcmp(str{7}(1:3),'ENS'))
% %SEARCH FOR THE ENSEMBLE MEMBER NUMBER.
 inventory.ens(i)=str2num(str{7}(5:end)); %#ok<ST2NM>
 else
 inventory.ens(i)=NaN;
 end
% 
 end
 
% 
% %==========================================================================
% %GET GRID DIMENSIONS AND ADD THIS INFORMATION TO THE STRUCTURE INV
% %==========================================================================
% 
% %Assume that all records has the same size...
 [~, info]=unix([wgrib ' ' gribfile ' -d 1 -V']);
% 
 info=strread(info,'%s','delimiter','\n','whitespace','');
 [tmp]=strread(info{5},'%s');
% 
 inventory.yrev=false;
 inventory.lat_min=str2double(tmp{3});
 inventory.lat_max=str2double(tmp{5});
 if(inventory.lat_min > inventory.lat_max)
     inventory.yrev=true;
     tmplat=inventory.lat_min;
     inventory.lat_min=inventory.lat_max;
     inventory.lat_max=tmplat;
 end
 
% 
 inventory.delta_lat=str2double(tmp{7});
 inventory.grid_lat=inventory.lat_min:inventory.delta_lat:inventory.lat_max;
 inventory.nlat=size(inventory.grid_lat,2);
% 
 [tmp]=strread(info{6},'%s');
% 
 inventory.lon_min=str2double(tmp{2});
 inventory.lon_max=str2double(tmp{4});
 inventory.delta_lon=str2double(tmp{6});
 if(inventory.lon_max < inventory.lon_min)
    inventory.lon_max=inventory.lon_max+360;
 end
 inventory.grid_lon=inventory.lon_min:inventory.delta_lon:inventory.lon_max;
 inventory.nlon=size(inventory.grid_lon,2);

% 
% %==========================================================================
% % READ DATA FROM TEMPORARY FILE
% %==========================================================================
% 

% 
 tmpfile='tmpgriboutAaBbTmP.bin';
 data=NaN(inventory.nlat,inventory.nlon,nrecords);
 for ii=1:nrecords
  [~, ~,]=unix([wgrib ' ' gribfile ' -d ' num2str(inventory.recnum(ii)) ...
           '-bin -o '  tmpfile]);
%  
%             
% %READ THE TMPFILE 
 fid=fopen(tmpfile);
 tmp=fread(fid,1,'int32');
% %GET INFO ABOUT DATA PRECISION (CONTAINDE IN THE FILE HEADER).
 tmp=tmp/(inventory.nlon*inventory.nlat); %
 if(tmp==4)
     precision='float32';
 elseif(tmp==8)
     precision='float64';
 else
 disp('Error: la precision de los datos no pudo ser determinada')
 return
 end
% 
 data(:,:,ii)=fread(fid,[inventory.nlon inventory.nlat],precision)';
% 
 fclose(fid);
%                       
 end
% 
% unix(['rm -f ' tmpfile]);
% 
 if(inventory.yrev)
     data=flipdim(data,1);
 end

end
