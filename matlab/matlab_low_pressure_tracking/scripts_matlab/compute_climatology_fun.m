

%Script to read a plot 1D parameter ensemble evolution.

function [clim_mean clim_std]=compute_climatology_fun(INI_DATE,END_DATE,DATA_PATH)


%DATA_PATH='../DATA/nature/'      %Path to data.
%INI_DATE='1982010112';
%END_DATE='1982030100';

EST_FREC=6;                      %Analysis frequency. (hours)

%Definiciones de la reticula del SPEEDY.

nvars=37; 
nx_s=96;
ny_s=48;
%nz_s=7;

C_DATE_NUM=datenum(INI_DATE,'yyyymmddhh');
END_DATE_NUM=datenum(END_DATE,'yyyymmddhh');

nmean=0;
clim_mean=zeros(ny_s,nx_s,nvars);
clim_std=zeros(ny_s,nx_s,nvars);

while ( C_DATE_NUM <= END_DATE_NUM )
   
   input_file=strcat(DATA_PATH,datestr(C_DATE_NUM,'yyyymmddHH'),'_p.grd');
   
   input_file_n=fopen(input_file,'r','b');


   if(input_file_n ~= -1)
     for ivars=1:nvars
     tmp(:,:,ivars)=fread(input_file_n,[nx_s ny_s],'single')';
     end
     fclose(input_file_n);
     
     clim_mean=clim_mean+tmp;
     clim_std=clim_std+tmp.^2;
     nmean=nmean+1;
     
   end
   
C_DATE_NUM=C_DATE_NUM + EST_FREC/24;   
end

clim_mean=clim_mean/nmean;
clim_std=sqrt(clim_std/nmean-clim_mean.^2);


















