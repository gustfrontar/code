function [minstruct]=fast_read_tigge_grib_2(gribfile,climfile,tmpfile)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
wgrib='wgrib2';     %If not in the PATH of the system then have to use the 
                  %complete path for wgrib
%==========================================================================
%GET INVENTORY USING WGRIB AND LOAD RESULT INTO A MATLAB STRUCTURE.
%ASUMO QUE LOS DATOS SON YREV Y POR ESO DEFINO LAS LATITUDES AL REVES.
%==========================================================================

%DEFINE PRIOR GRID
original_resol=2;
lat_original=-90:original_resol:90;
lon_original=0:original_resol:360-original_resol;

[lon_original lat_original]=meshgrid(lon_original,lat_original);

%DEFINE FINAL GRID
final_resol=0.5;

%lat_final=90:-final_resol:-90;
%lon_final=0:final_resol:360-final_resol;

%[lon_final lat_final]=meshgrid(lon_final,lat_final);

%==========================================================================
%COMPUTE RECORD NUMBER.
%==========================================================================

NReg=1;

%==========================================================================
% DECODE DATA TO A TEMPORARY FILE
%==========================================================================
%[wgrib ' -d ' num2str(NReg) ...
%           ' -bin '  tmpfile ' ' gribfile]

%[error,record_info]=unix([wgrib ' -order raw -d ' num2str(NReg) ...
%Si no se incluye la opcion -order raw, los datos se decodifican a una reticula
%con el orden correcto de latitud y longitud.
[error,record_info]=unix([wgrib ' -d ' num2str(NReg) ...
           ' -bin '  tmpfile ' ' gribfile])
      
%Before proceeding we should check if the record info is consistent with
%the desired information.

if(error ~=0)
    return    %Do not continue executing the function
end

 
 fid=fopen(tmpfile,'r');
 fread(fid,[1],'single');
 FIELD=fread(fid,[size(lat_original')],'single')';
 fclose(fid);
 
 
 fid=fopen(climfile,'r');
 CLIMATOLOGY=fread(fid,[size(lat_original)],'single');
 %CLIMATOLOGY=flipdim(CLIMATOLOGY,1);
 fclose(fid);

 %figure
 %pcolor(CLIMATOLOGY)
 %figure
 %pcolor(FIELD)
 
 %COMPUTE ANOMALY

 FIELD=FIELD-CLIMATOLOGY;
 
 [tmp1,tmp2]=gradient(FIELD);


 LAPLACIAN=-divergence(tmp1,tmp2)/(final_resol^2);
 
%=========================================================================
% USE THE COMPUTED DATA TO GET THE LOCAL MINIMUN OF THE FIELD.
%=========================================================================


minstruct=min_fun(FIELD,LAPLACIAN,lon_original,lat_original,false);

end
 
 
