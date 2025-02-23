function [minstruct]=fast_read_grib_2(gribfile,climfile,matchinput,tmpfile)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
wgrib='wgrib';     %If not in the PATH of the system then have to use the 
                  %complete path for wgrib
%==========================================================================
%GET INVENTORY USING WGRIB AND LOAD RESULT INTO A MATLAB STRUCTURE.
%ASUMO QUE LOS DATOS SON YREV Y POR ESO DEFINO LAS LATITUDES AL REVES.
%==========================================================================

%DEFINE PRIOR GRID
original_resol=2.5;
lat_original=90:-original_resol:-90;
lon_original=0:original_resol:360-original_resol;

[lon_original lat_original]=meshgrid(lon_original,lat_original);

%DEFINE FINAL GRID
final_resol=0.5;

%lat_final=90:-final_resol:-90;
%lon_final=0:final_resol:360-final_resol;

%[lon_final lat_final]=meshgrid(lon_final,lat_final);

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
 FIELD=fread(fid,[size(lat_original')],'single')';
 fclose(fid);
 
 %FIELD=interp2(lon_original,lat_original,data,lon_final,lat_final,'splines');
 
 fid=fopen(climfile,'r');
 CLIMATOLOGY=fread(fid,[size(lat_original)],'single');
 CLIMATOLOGY=flipdim(CLIMATOLOGY,1);
 fclose(fid);
 
 %COMPUTE ANOMALY

 FIELD=FIELD-CLIMATOLOGY;
 
 [tmp1,tmp2]=gradient(FIELD);
 LAPLACIAN=-divergence(tmp1,tmp2)/(final_resol^2);
 
 %LAPLACIAN=interp2(lon_original,lat_original,LAPLACIAN,lon_final,lat_final,'splines');
 

 
 %=========================================================================
 % USE THE COMPUTED DATA TO GET THE LOCAL MINIMUN OF THE FIELD.
 %=========================================================================
[ny nx]=size(FIELD);
 
 
NMINIMOS=0;
MIN_I=0;
MIN_J=0;
UMB_INTENSIDAD=-0.5;
UMB_ANOMALY=0;
%pcolor(FIELD)

for j=1:nx
    for i=2:ny-1

       if(LAPLACIAN(i,j) < UMB_INTENSIDAD && FIELD(i,j) < UMB_ANOMALY)
       testmin=0;
       for ii=-1:1
           for jj=-1:1
                   indexi=i+ii;
                   indexj=j+jj;
                   
                   [indexi,indexj]=global_boundary_fun(indexi,indexj,nx,ny);
                   if(FIELD(indexi,indexj) < FIELD(i,j) )
                   testmin=1;
                   end

           end
       end
       

       if(testmin==0)

       NMINIMOS=NMINIMOS+1;
       MINLAP(NMINIMOS)=LAPLACIAN(i,j);
       MIN_ANOM(NMINIMOS)=FIELD(i,j);
       %Para encontrar la posicion del minimo interpolo el campo usando
       %splines cubicos.
       %Primero genero una reticula alrededor del minimo.
       LOCALFIELD=NaN(3,3);
       LOCALLAT  =NaN(3,3);
       LOCALLON  =NaN(3,3);
       for ii=-1:1
           for jj=-1:1
                   indexi=i+ii;
                   indexj=j+jj;
                   
                   [indexi,indexj]=global_boundary_fun(indexi,indexj,nx,ny);
                   LOCALFIELD(ii+2,jj+2)=FIELD(indexi,indexj);
                   
                   LOCALLAT(ii+2,jj+2)  =lat_original(indexi,indexj);
                   if( i==1 && ii==-1)
                   LOCALLAT(ii+2,jj+2)=lat_original(1,indexj)+original_resol;    
                   end
                   if( i==ny && ii==1)
                   LOCALLAT(ii+2,jj+2)=lat_original(ny,indexj)-original_resol;
                   end
                   LOCALLON(ii+2,jj+2)  =lon_original(indexi,indexj);
                   if( j == 1 && jj==-1)
                   LOCALLON(ii+2,jj+2)=lon_original(indexi,indexj)-360;
                   end
                   if( j == nx && jj==1)
                   LOCALLON(ii+2,jj+2)=lon_original(indexi,indexj)+360;
                   end
           end
       end
       LOCALLATHIRES=LOCALLAT(1,1):-final_resol:LOCALLAT(3,1);
       LOCALLONHIRES=LOCALLON(1,1):final_resol:LOCALLON(1,3);
       [LOCALLONHIRES LOCALLATHIRES] = meshgrid(LOCALLONHIRES,LOCALLATHIRES);

       
       LOCALFIELDHIRES=interp2(LOCALLON,LOCALLAT,LOCALFIELD,LOCALLONHIRES,LOCALLATHIRES,'splines');
       [localny localnx]=size(LOCALFIELDHIRES);
       LOCALFIELDHIRES=reshape(LOCALFIELDHIRES,[1 localny*localnx]);
       LOCALLONHIRES=reshape(LOCALLONHIRES,[1 localny*localnx]);
       LOCALLATHIRES=reshape(LOCALLATHIRES,[1 localny*localnx]);
       [minimo minpos]=min(LOCALFIELDHIRES);
       
       MIN_LAT(NMINIMOS)=LOCALLATHIRES(minpos);
       MIN_LON(NMINIMOS)=LOCALLONHIRES(minpos);

       end

       end
    end
end

%Vamos a testear los minimos en el polo.
testmin=0;
for ii=1:nx
   if(FIELD(1,1) > FIELD(2,ii))
   testmin=1;
   end
end
if(testmin==0)
   NMINIMOS=NMINIMOS+1;
   MIN_I(NMINIMOS)=1;
   MIN_J(NMINIMOS)=1;
   MINLAP(NMINIMOS)=LAPLACIAN(1,1);
   MIN_LAT(NMINIMOS)=lat_original(1,1);
   MIN_LON(NMINIMOS)=lon_original(1,1);
   MIN_ANOM(NMINIMOS)=FIELD(i,j);
end
testmin=0;
for ii=1:nx
   if(FIELD(ny,1) > FIELD(ny-1,ii))
   testmin=1;
   end
end
if(testmin==0)
   NMINIMOS=NMINIMOS+1;
   MIN_I(NMINIMOS)=ny;
   MIN_J(NMINIMOS)=1;
   MINLAP(NMINIMOS)=LAPLACIAN(1,1);
   MIN_LAT(NMINIMOS)=lat_original(1,1);
   MIN_LON(NMINIMOS)=lon_original(1,1);
   MIN_ANOM(NMINIMOS)=FIELD(i,j);

end

for ii=1:NMINIMOS
MIN_ID(ii)=ii;
end


minstruct.minlat=MIN_LAT;
minstruct.minlon=MIN_LON;
minstruct.minlap=MINLAP;
minstruct.nminimos=NMINIMOS;
minstruct.minanom=MIN_ANOM;

end
 
 
