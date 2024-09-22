function fast_grib_decode(gribfile,matchinput,wgribpath,lonlatflag)
%Esta funcion recibe un criterio de busqueda y genera un archivo temporal
%con el campo solicitado. Opcionalmente con las longitudes y latitudes.
%Si lonlatflat es true genera un output para eso tambien, sino escribe solo
%el campo.
%IMPORTANTE: ESTA FUNCION SIRVE SI EL MATCHINPUT (criterio de busqueda) ES
%LO SUFICIENTEMENTE ESPECIFICO PARA GENERAR UN SOLO CAMPO Y NO MUCHOS
%CAMPOS AL MISMO TIEMPO.
%==========================================================================
%CHECK WGRIB VERSION INPUT


exe='wgrib2';

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

%tmpfile='./DataGribOut.bin';

unix([wgrib ' ' gribfile   ' wgrib -bin -d 2'])

%SI EL USUARIO LO PIDE LA FUNCION OBTIENE TAMBIEN INFORMACION SOBRE LAS
%RETICULAS.
if(lonlatflag)
 [~, info]=unix([wgrib ' ' gribfile ' -d 1 -V']);
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
 tmp
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
 
end
