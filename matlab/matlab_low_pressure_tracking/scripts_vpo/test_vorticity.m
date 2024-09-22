
close all
clear all

%INCLUDE SPECIFIC FUNCTIONS DIRECTORY.
addpath('../common_functions_vpo/');


ini_date_num=datenum('2008073118','yyyymmddHH');
end_date_num=datenum('2008073118','yyyymmddHH');
cur_date_num=ini_date_num;

%WHILE SOBRE LOS TIEMPOS.
count=1;
  
    %READ DATA...
    
    %Define climatology file corresponding to the current date.
    ClimFile=['../RESULTS/CLIMATOLOGY/NCEP500HPA_2.5/HGT' datestr(cur_date_num,'mmddHH') '.grd'];
    %Define temporary output for decoded geopotential fields.
    DataTmpFile='./analysistmpgribout.bin';
    
    
    
    %Define grib file corresponding to the current date.
    GribFile=['../DATA/REA2NCEP/HGTVPO/REA2NCEP_HGT_'  datestr(cur_date_num,'yyyymmddHH') '.grib'];
  

    search='HGT:500'; 
    
    %Decode fields.
    fast_read_grib(GribFile,search,true,DataTmpFile);
    %Call system identification routines.
    
   
    
     fid=fopen('./DataLatGribOut.bin');
     lat=fread(fid,[144 73],'single')';
     
     fid=fopen('./DataLonGribOut.bin');
     lon=fread(fid,[144 73],'single')';

    
    

 
     fid=fopen(ClimFile);
     clim=fread(fid,[73 144],'single');
     figure
     pcolor(clim)
     fclose(fid)
%     
%     

     fid=fopen('./analysistmpgribout.bin');
     fread(fid,1,'single')
     analysis=fread(fid,[144 73],'single')';
     figure
     analysis=flipdim(analysis,1);
     pcolor(analysis-clim)
     shading flat
     fclose(fid)

     

    [auxx auxy]=gradient(analysis);
    pseudovorticity=divergence(auxx,auxy);
    [pseudovorticity]=spherical_harmonic_filter(pseudovorticity,lat(:,1),lon(1,:),42,lat(:,1),lon(1,:));
    [tmp]=spherical_harmonic_filter(pseudovorticity,lat(:,1),lon(1,:),5,lat(:,1),lon(1,:));
    pseudovorticity=pseudovorticity-tmp;
     figure
     pcolor(pseudovorticity)
     title('pseudovorticity')
     shading flat
     
     
     figure
     pcolor(analysis-clim)
     shading flat
     
     hold on
     pseudovorticity(pseudovorticity<0)=0;
     contour(pseudovorticity,'k')
     
     figure
     pcolor(analysis)
     shading flat
     
     hold on

     contour(pseudovorticity,'k')



