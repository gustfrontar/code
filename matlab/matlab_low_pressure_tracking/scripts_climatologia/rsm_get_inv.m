
function inv = rsm_get_inv(gribfile,gribversion,wgribpath);
%==========================================================================
% function [inv] = rsm_get_inv(gribfile,wgribpath);
%	Load the invertory of the GRIBFILE into
%	a structure array.
%   gribfile is the name of the gribfile.
%   gribversion is the version of the grib file (1 or 2).
%   wgribpath is the alternative path to be used (in case that wgrib is not
%   in the PATH variable).
%   Manu - (edl@gatech.edu) -----------------------original code for wgrib
%   Juan Ruiz -------------------------------------MODIFIED January 2011.
%   RESTRICCIONES IMPORTANTES A TENER EN CUENTA:
%   Esta funcion asume que todos los registros tienen el mismo tamanio.
%   (nx * ny)
%   La forma en la que esta funcion obtiene las caracteristicas de la
%   reticula se aplica a reticulas regulares (no se aplica a reticulas
%   gaussianas o proyecciones mas complejas).
%==========================================================================

% extract record
if(gribversion==1)
    exe='wgrib';
elseif(gribversion==2)
    exe='wgrib2';
else
    disp('Error: Bad gribversion option!');
    inv=NaN;
    return
end

[status wgribpath]=unix(['which ' exe]); %Buscamos wgrib en el path.
if (isempty(wgribpath))
    disp(['WARNING: ' exe ' in not in the machine path']);
    disp('I will try to use the path given to this function');
    wgribpath=[wgribpath '/' exe];
end

%==========================================================================
%GET INVENTORY USING WGRIB AND LOAD RESULT INTO A MATLAB STRUCTURE.
%==========================================================================

%GET INVENTORY.
wgribpath=strtrim(wgribpath);
[status inventario]=unix([wgribpath ' ' gribfile ' -s ']);
if(status ~=0)
    disp(['Error: ' exe ])
    disp(inventario)
    inv=NaN;
    return
end


invload = strread(inventario,'%s','delimiter','\n','whitespace','');

%SAVE INVENTORY INTO A STRUCTURED ARRAY CALLED INV.
for i=1:length(invload);
str=invload{i};
n=strfind(str, ':');
inv.string{i}=str;

irec=1;
ind=1:n(irec)-1;
inv.recnum(i)=str2double( str(ind));

irec=3;
ind=n(irec-1)+1:n(irec)-1;
inv.datestr{i}=( str(ind));
if(gribversion==1)
inv.year(i)=str2double(inv.datestr{i}(3:4));
inv.month(i)=str2double(inv.datestr{i}(4:6));
inv.day(i)=str2double(inv.datestr{i}(7:8));
inv.hour(i)=str2double(inv.datestr{i}(9:10));
elseif(gribversion==2)
inv.year(i)=str2double(inv.datestr{i}(3:6));
inv.month(i)=str2double(inv.datestr{i}(7:8));
inv.day(i)=str2double(inv.datestr{i}(9:10));
inv.hour(i)=str2double(inv.datestr{i}(11:12));   
end


%Matlab datenum
if(gribversion==1)
inv.datenum(i)=datenum(inv.datestr{i}(3:end),'yymmddHH');
elseif(gribversion==2)
inv.datenum(i)=datenum(inv.datestr{i}(3:end),'yyyymmddHH');
end


irec=4;
ind=n(irec-1)+1:n(irec)-1;
inv.varname{i}=( str(ind));

irec=5;
%Output level without 
ind=n(irec-1)+1:n(irec)-1;
tmplevel=str(ind);
inv.level(i)=str2double(tmplevel(1:4));

end

%==========================================================================
%GET FIELD DIMENSIONS AND ADD THIS INFORMATION TO THE STRUCTURE INV
%==========================================================================

%Assume that all records has the same size...
[~, info]=unix([wgribpath ' ' gribfile ' -d 1 -V']);

info=strread(info,'%s','delimiter','\n','whitespace','');
[tmp]=strread(info{5},'%s');

inv.yrev=false;
inv.lat_min=str2double(tmp{3});
inv.lat_max=str2double(tmp{5});
if(inv.lat_min > inv.lat_max)
    inv.yrev=true;
    tmplat=inv.lat_min;
    inv.lat_min=inv.lat_max;
    inv.lat_max=tmplat;
end

inv.delta_lat=str2double(tmp{7});
inv.grid_lat=inv.lat_min:inv.delta_lat:inv.lat_max;
inv.nlat=size(inv.grid_lat,2);

[tmp]=strread(info{6},'%s');

inv.lon_min=str2double(tmp{2});
inv.lon_max=str2double(tmp{4});
inv.delta_lon=str2double(tmp{6});
if(inv.lon_max < inv.lon_min)
   inv.lon_max=inv.lon_max+360;
end
inv.grid_lon=inv.lon_min:inv.delta_lon:inv.lon_max;
inv.nlon=size(inv.grid_lon,2);




end

