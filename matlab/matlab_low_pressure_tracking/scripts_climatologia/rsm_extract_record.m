
function [grid_data grid_lon grid_lat] = rsm_extract_record(gribfile,record,wgribpath)
% function [FIELD, S] = rsm_extract_record(gribfile, record);
%	Extract field corresponding to record number RECORD 
%     from GRIBFILE into FIELD. S returns the inventory of
%     the selected record. To get a full inventory of the GRIBFILE
%     use  INV = rsm_get_inv(GRIBFILE)
% Manu - (edl@gatech.edu)
% Juan Ruiz, MODIFIED 2011.

% generate a tmp file for local dump

file='./tmpgribout';

[~, wgribpath]=unix('which wgrib'); %Buscamos wgrib en el path.
if (isempty(wgribpath))
    disp('WARNING: El WGRIB no esta en el path de la maquina!');
    disp('Uso el path ingresado por el usuario');
    wgribpath=[wgribpath2 '/wgrib'];
end
wgribpath=strtrim(wgribpath);

[~, info]=unix([wgribpath ' ' gribfile ' -d ' num2str(record) ' -bin -V -o '  file]);

info=strread(info,'%s','delimiter','\n','whitespace','');
[tmp]=strread(info{5},'%s');

yrev=false;
lat_min=str2double(tmp{3});
lat_max=str2double(tmp{5});
if(lat_min > lat_max)
    yrev=true;
    tmplat=lat_min;
    lat_min=lat_max;
    lat_max=tmplat;
end

delta_lat=str2double(tmp{7});
grid_lat=lat_min:delta_lat:lat_max;
nlat=size(grid_lat,2);

[tmp]=strread(info{6},'%s');

lon_min=str2double(tmp{2});
lon_max=str2double(tmp{4});
delta_lon=str2double(tmp{6});
if(lon_max < lon_min)
   lon_max=lon_max+360;
end
grid_lon=lon_min:delta_lon:lon_max;
nlon=size(grid_lon,2);

% read the tmp file and assign record
fid=fopen(file);
tmp=fread(fid,1,'int32');
%Tmp es float_size*nx*ny. Float size asumo que es 4 (precision de 32 bits).
tmp=tmp/(nlon*nlat); %El header tiene info sobre la precision.
if(tmp==4)
    precision='float32';
elseif(tmp==8)
    precision='float64';
else
    disp('Error: la precision de los datos no pudo ser determinada')
    return
end

grid_data=fread(fid,[nlon nlat],precision);
if(yrev)
    grid_data=flipdim(grid_data,2)';
end


fclose(fid);


end


