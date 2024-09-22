function [minstruct]=fast_read_bin_2(binfile,climfile)
%READ BINARI FILE AND GET MINIMUM INFORMATION.


%DEFINE PRIOR GRID
original_resol=2;
lat_original=-90:original_resol:90;
lon_original=0:original_resol:360-original_resol;

[lon_original lat_original]=meshgrid(lon_original,lat_original);

binfile

%==========================================================================
% READ DATA
%==========================================================================
 
 fid=fopen(binfile,'r');
 %fread(fid,[1],'single');
 FIELD=fread(fid,[size(lat_original)],'single');
 fclose(fid);

 fid=fopen(climfile,'r');
 CLIMATOLOGY=fread(fid,[size(lat_original)],'single');
 fclose(fid);

 
 %COMPUTE ANOMALY

 FIELD=FIELD-CLIMATOLOGY;
 
 [tmp1,tmp2]=gradient(FIELD);
 LAPLACIAN=-divergence(tmp1,tmp2)/(original_resol^2);

%==========================================================================
% GET MINIMUM INFORMATION
%==========================================================================


minstruct=min_fun(FIELD,LAPLACIAN,lon_original,lat_original,false);

end
 
 
