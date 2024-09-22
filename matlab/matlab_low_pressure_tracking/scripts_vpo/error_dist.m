clear all
close all

%Script to read a plot 1D parameter ensemble evolution.

EXPERIMENTM='../DATA/CONTROL/anal/mean/'        %Experiment name.
TRUTH='../DATA/CONTROL/gues/mean/'             %True experiment

ENS_SIZE=20;                       %Ensemble size.
EXP_INI_DATE=1982020112;
EXP_END_DATE=1982021512;
EST_FREC=6;                        %Analysis frequency. (hours)

BIAS_OFFSET=100;                   %How many times before bias computation starts.

%Definiciones de la reticula del SPEEDY.

nvars=29; 
nx_s=96;
ny_s=48;
nz_s=7;

lat_s=[-87.159 -83.479 -79.777 -76.070 -72.362 -68.652 -64.942 -61.232 -57.521 ...
      -53.810 -50.099 -46.389 -42.678 -38.967 -35.256 -31.545 -27.833 -24.122  ...
      -20.411 -16.700 -12.989  -9.278  -5.567  -1.856   1.856   5.567   9.278  ...
      12.989  16.700  20.411  24.122  27.833  31.545  35.256  38.967  42.678   ...
      46.389  50.099  53.810  57.521  61.232  64.942  68.652  72.362  76.070   ...
      79.777  83.479  87.159];
  
lon_s=0:3.75:360-3.75;

%Read parameter ensemble

INI_DATE_NUM=datenum(num2str(EXP_INI_DATE),'yyyymmddHH');
END_DATE_NUM=datenum(num2str(EXP_END_DATE),'yyyymmddHH');
C_DATE_NUM=INI_DATE_NUM;
NTIMES=(END_DATE_NUM-INI_DATE_NUM)*24/EST_FREC+1;

ERROR=NaN(ny_s,nx_s,nvars,NTIMES);
TRUTHF=NaN(ny_s,nx_s,nvars,NTIMES);


nbias=0;

%Primero calculamos el error para todas las variables sobre un periodo.
while ( C_DATE_NUM <= END_DATE_NUM )
  ITIME=(C_DATE_NUM-INI_DATE_NUM)*24/EST_FREC+1;
   
   anamean=strcat(EXPERIMENTM,datestr(C_DATE_NUM,'yyyymmddHH'),'.grd');
   truenat=strcat(TRUTH,datestr(C_DATE_NUM,'yyyymmddHH'),'.grd');
   
   nanam=fopen(anamean,'r','b');
   ntrue=fopen(truenat,'r','b');
   
   tmpmean=NaN(ny_s,nx_s,nvars);
   tmptrue=NaN(ny_s,nx_s,nvars);
   
   if(nanam ~= -1)
     for ivars=1:nvars
     tmpmean(:,:,ivars)=fread(nanam,[nx_s ny_s],'single')';
     end
     fclose(nanam);
   end
   
   
   if(ntrue ~= -1)
     for ivars=1:nvars
     tmptrue(:,:,ivars)=fread(ntrue,[nx_s ny_s],'single')';
     end
     fclose(ntrue); 
   end
   
   
  
   
 ERROR(:,:,:,ITIME)=(tmpmean-tmptrue);
 TRUTHF(:,:,:,ITIME)=(tmptrue);
 
 

 
C_DATE_NUM=C_DATE_NUM + EST_FREC/24;
end


BIN_CENTER=[-2:0.5:2];

%Temperatura

minlev=14
for ilev=1:7
    
    level=minlev+ilev;
    tmp=ERROR(:,:,level,:);
    aux=reshape(tmp,1,nx_s*ny_s*NTIMES);
    histograma_t(:,ilev)=hist(aux,BIN_CENTER);
    %calculo la asimetria.
    asimetria_t(ilev)=nanmean((aux-nanmean(aux)).^3)/(std(aux)^3);
    media_t(ilev)=nanmean(aux);
    
end

minlev=7
for ilev=1:7
    
    level=minlev+ilev;
    tmp=ERROR(:,:,level,:);
    aux=reshape(tmp,1,nx_s*ny_s*NTIMES);
    histograma_v(:,ilev)=hist(aux,BIN_CENTER);
    %calculo la asimetria.
    asimetria_v(ilev)=nanmean((aux-nanmean(aux)).^3)/(std(aux)^3);
    media_v(ilev)=nanmean(aux);
    
end

minlev=0
for ilev=1:7
    
    level=minlev+ilev;
    tmp=ERROR(:,:,level,:);
    aux=reshape(tmp,1,nx_s*ny_s*NTIMES);
    histograma_u(:,ilev)=hist(aux,BIN_CENTER);
    %calculo la asimetria.
    asimetria_u(ilev)=nanmean((aux-nanmean(aux)).^3)/(std(aux)^3);
    media_u(ilev)=nanmean(aux);
    
end

minlev=21
for ilev=1:7
    
    level=minlev+ilev;
    tmp=ERROR(:,:,level,:);
    aux=reshape(tmp,1,nx_s*ny_s*NTIMES);
    histograma_q(:,ilev)=hist(aux,BIN_CENTER);
    %calculo la asimetria.
    asimetria_q(ilev)=nanmean((aux-nanmean(aux)).^3)/(std(aux)^3);
    media_q(ilev)=nanmean(aux);
    
end








