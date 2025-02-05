clear all
close all

%Script to read a plot 1D parameter ensemble evolution.

EXPERIMENT='/home/jruiz/DATA/EXP_SPEEDY/EXPERIMENTO_SIM100_CONV_FIXINF_FIXPARINF/';
EXP_INI_DATE=1982010106;
EXP_END_DATE=1982060100;
EST_FREC=6;                                    %Analysis frequency. (hours)

EXPPAR=[EXPERIMENT '/gues/'];        
EXPPAR2=[EXPERIMENT '/parameter/'];
system(['mkdir -p ' EXPERIMENT '/correlation/'])
table=[EXPERIMENT '/parameters.tbl'];

ENSSIZE=100;

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

%Read parameters.tbl to get the number of estimated parameters
[read_in(1,:) read_in(2,:) read_in(3,:) read_in(4,:) read_in(5,:) read_in(6,:) read_in(7,:) read_in(8,:) read_in(9,:) read_in(10,:) read_in(11,:) ]=textread(table,'%s%s%s%s%s%s%s%s%s%s%s');
aux_name=read_in(1,2:end);
est_par=read_in(3,2:end);
par1d=read_in(4,2:end);
n1d=0;
for i=1:length(est_par);
   if(est_par{i}(1)=='T' & par1d{i}(1) == 'T')
       n1d=n1d+1;
       par_name{n1d}=aux_name(i);
       par_max(n1d)=str2num(read_in{6,i+1});
       par_min(n1d)=str2num(read_in{7,i+1});
   end
end
np1d=n1d;


INI_DATE_NUM=datenum(num2str(EXP_INI_DATE),'yyyymmddHH');
END_DATE_NUM=datenum(num2str(EXP_END_DATE),'yyyymmddHH');
C_DATE_NUM=INI_DATE_NUM;
NTIMES=(END_DATE_NUM-INI_DATE_NUM)*24/EST_FREC+1;


%For error / spread correlation computation.
COVARPARSTATE=zeros(ny_s,nx_s,nvars,np1d);
CORRPARSTATE=zeros(ny_s,nx_s,nvars,np1d);
TMPENS=NaN(ny_s,nx_s,nvars,ENSSIZE);
TMPPAR=NaN(np1d,ENSSIZE);

while ( C_DATE_NUM <= END_DATE_NUM )
  ITIME=(C_DATE_NUM-INI_DATE_NUM)*24/EST_FREC+1;
  datestr(C_DATE_NUM,'yyyymmddHH')

   %Read state variable ensemble
   for iens=1:ENSSIZE   
     striens=num2str(iens+1e4);striens=striens(end-2:end);
     file=strcat(EXPPAR,'/',striens,'/',datestr(C_DATE_NUM,'yyyymmddHH'),'F06.grd');
     filen=fopen(file,'r','b');
     if(filen ~= -1)
       for ivars=1:nvars
        TMPENS(:,:,ivars,iens)=fread(filen,[nx_s ny_s],'single')';
       end
       fclose(filen);
     end
   %Read parameter ensemble.
   file=strcat(EXPPAR2,striens,'/gp1d',datestr(C_DATE_NUM,'yyyymmddHH'),'.grd');
   filen=fopen(file,'r','b');
    if(filen ~= -1)
      TMPPAR(:,iens)=fread(filen,np1d,'single');
      fclose(filen);
     end
   end

   %Compute the correlation between the parameter and the state variables.
   for ip=1:np1d
     for ivar=1:nvars
        for ii=1:nx_s
           for jj=1:ny_s
            tmp=cov(squeeze(TMPPAR(ip,:)),...
            squeeze(TMPENS(jj,ii,ivar,:)));
            COVARPARSTATE(jj,ii,ivar,ip)=tmp(1,2);
            tmp=corrcoef(squeeze(TMPPAR(ip,:)),...
            squeeze(TMPENS(jj,ii,ivar,:)));
            CORRPARSTATE(jj,ii,ivar,ip)=tmp(1,2);
           end
        end
     end
   end

   %Write the results.
   file=strcat(EXPERIMENT,'/correlation/',datestr(C_DATE_NUM,'yyyymmddHH'),'.grd');
   filen=fopen(file,'w','b');
   for ip=1:np1d
     for ivar=1:nvars
     fwrite(filen,COVARPARSTATE(:,:,ivar,ip)','single');
     end
   end
   for ip=1:np1d
     for ivar=1:nvars
     fwrite(filen,CORRPARSTATE(:,:,ivar,ip)','single');
     end
   end


  
C_DATE_NUM=C_DATE_NUM + EST_FREC/24;
end













