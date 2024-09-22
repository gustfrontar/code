
close all
clear all

ini_date_num=datenum('2008020112','yyyymmddHH');
end_date_num=datenum('2008020112','yyyymmddHH');
cur_date_num=ini_date_num;

%WHILE SOBRE LOS TIEMPOS.
count=1;
  
    %READ DATA...
    
    %Define climatology file corresponding to the current date.
    ClimFile=['../RESULTS/CLIMATOLOGY/NCEP500HPA_2/HGT' datestr(cur_date_num,'mmddHH') '.grd'];
    %Define temporary output for decoded geopotential fields.
    DataTmpFile='./analysistmpgribout.bin';
    
    
    
    %Define grib file corresponding to the current date.
    GribFile=['../DATA/REA2NCEP/HGTVPO/REA2NCEP_HGT_'  datestr(cur_date_num,'yyyymmddHH') '.grib'];
  

    search='HGT:500'; 
    
    %Decode fields.
    fast_read_grib(GribFile,search,true,DataTmpFile);
    %Call system identification routines.
    
   
    %Lets consider the case where it is a forecast.
    %Define grib file corresponding to the current date.
    GribFile=['/media/GUSTFRONT/data/TIGGE/kwbc/SMALLGRIB/' datestr(ini_date_num,'yyyymmddHH') '/HGT_500' datestr(ini_date_num,'yyyymmddHH') '_F' num2str(int32((count-1)*6)) '_M' num2str(int32(1)) '.grib'];
 
    
    DataTmpFile='./forecasttmpgribout.bin';
 
    GenLatLon=true;
    read_error=fast_read_tigge_grib(GribFile,GenLatLon,DataTmpFile);
    
     fid=fopen('./DataLatGribOut.bin');
     lat=fread(fid,[180 91],'single')';
     figure
     pcolor(lat)
     fclose(fid)
     
     fid=fopen('./DataLonGribOut.bin');
     lon=fread(fid,[180 91],'single')';
     figure
     pcolor(lat)
     fclose(fid)
    
    

 
     fid=fopen(ClimFile);
     aclim=fread(fid,[91 180],'single');
     figure
     pcolor(aclim)
     fclose(fid)
%     
%     
     fid=fopen('./forecasttmpgribout.bin');
     fread(fid,1,'single')
     a=fread(fid,[180 91],'single')';
     a=flipdim(a,1);
     figure
     pcolor(a-aclim)
     fclose(fid)

     fid=fopen('./analysistmpgribout.bin');
     fread(fid,1,'single')
     b=fread(fid,[144 72],'single')';
     figure
     b=flipdim(b,1);
     pcolor(b-bclim)
     fclose(fid)

     

     
     
     




