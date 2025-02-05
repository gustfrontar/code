clear all
close all

%Speedy dimensions
nvars=29;
nx_s=96;
ny_s=48;
nz_s=7;

NPAR=41   %Numero de valores diferentes que toman los parametros para cada fecha.
EXPPATH=['/home/jruiz/DATA/EXP_SPEEDY/COST_FUNCTION/IMPERFECTMOD_TRCNV/']
NATUREPATH=['/home/jruiz/datos/Data_assimilation/speedy_new/speedy/DATA/nature/']

EXP_INI_DATE=1982030300;
EXP_END_DATE=1982033100;
FORECASTL=12;                                 %Forecast length
NFOR=FORECASTL/6;

error_norm=[ones(1,21) 1e-3*ones(1,7) 100];

%Read parameter ensemble (which is fixed in time)
for iens=1:ENS_SIZE

   ENS_MEM='000';
   if(iens < 10);ENS_MEM(3:3)=num2str(iens);end
   if(iens >=10 & iens <=100);ENS_MEM(2:3)=num2str(iens);end
   if(iens >=100);ENS_MEM=num2str(iens);end
       
   filep1d=[EXPERIMENTPER '/parameters/par' ENS_MEM '.dat'];
   nfile=fopen(filep1d,'r','b');
   if(nfile ~= -1)
     par(iens)=fread(nfile,1,'single');
     fclose(nfile); 
   end
 
end


INI_DATE_NUM=datenum(num2str(EXP_INI_DATE),'yyyymmddHH');
END_DATE_NUM=datenum(num2str(EXP_END_DATE),'yyyymmddHH');
C_DATE_NUM=INI_DATE_NUM;
NTIMES=(END_DATE_NUM-INI_DATE_NUM)*4+1;

%Read J

J=NaN(nvar,ENS_SIZE,NTIMES,NFOR,2);

itime=0;
while ( C_DATE_NUM <= END_DATE_NUM )
itime=itime+1;
   
   nature=[NATUREPATH datestr(C_DATE_NUM,'yyyymmddHH') '.grd'];
   %Read nature file
   nt=fopen(nature,'r','b');
   for ivars=1:nvars
     tmpn(:,:,ivars)=fread(nt,[nx_s ny_s],'single')';
   end
   fclose(nt);

 for ifor=1:NFOR
   FOR=num2str(ifor*6)
   if(ifor < 10);FOR=['0' FOR];end
   for ipar=1:NPAR
       PAR=num2str(ipar);
       if(ipar < 10);PAR=['0' PAR];end
       if(ipar < 100);PAR=['0' PAR];end
       forecast=[EXPPATH '/ensemble/' datestr(C_DATE_NUM,'yyyymmddHH') 'F' FORECAST 'P' PAR  '.grd'];
 
       nf=fopen(forecast,'r','b');

       if(nf > 0)       
       %Read forecast file
       for ivars=1:nvars
       tmpf(:,:,ivars)=fread(nf,[nx_s ny_s],'single')';
       end
       fclose(nf);
    
       J(:,ipar,itime,ifor)=sqrt(squeeze(nanmean(nanmean((tmpf-tmpn).^2,2),1)))/error_norm;
       end
       
   end
 end

C_DATE_NUM=C_DATE_NUM + 6/24;
end


%Diagnostics.

%Compute mean J in time.
MEANJ=squeeze(nanmean(J,3));


%==========================================================================
% Compute and plot total J and the position of the minimun as a function of
% time for total J. The spread in the minimun position can be used as a
% estimation of parameter uncertainty. 

TOTALMEANJ=squeeze(sum(MEANJ));

UMEANJ=squeeze(sum(MEANJ(1:7,:,:)));

VMEANJ=squeeze(sum(MEANJ(8:14,:,:)));

TMEANJ=squeeze(sum(MEANJ(15:21,:,:)));

QMEANJ=squeeze(sum(MEANJ(22:28,:,:)));

PSMEANJ=squeeze(sum(MEANJ(22:28,:,:)));

TOTALJ=squeeze(sum(J,1));
UJ=squeeze(sum(J(1:7,:,:,:,:),1));
VJ=squeeze(sum(J(8:14,:,:,:,:),1));
TJ=squeeze(sum(J(15:21,:,:,:,:),1));
QJ=squeeze(sum(J(22:28,:,:,:,:),1));
PSJ=squeeze(J(29,:,:,:,:));

[nparj ntj nfj]=size(TOTALJ);
%Compute parameter value at minimun of J

 for it=1:ntj
   for ifor=1:nfj
     for iexp=1:nexp
             [nada index]=min(squeeze(TOTALJ(:,it,ifor)));
             TOTALMINPAR(it,ifor)=par(index);    
             [nada index]=min(squeeze(UJ(:,it,ifor)));
             UMINPAR(it,ifor)=par(index);
             [nada index]=min(squeeze(VJ(:,it,ifor)));
             VMINPAR(it,ifor)=par(index);
             [nada index]=min(squeeze(TJ(:,it,ifor)));
             TMINPAR(it,ifor)=par(index);
             [nada index]=min(squeeze(QJ(:,it,ifor)));
             QMINPAR(it,ifor)=par(index);
             [nada index]=min(squeeze(PSJ(:,it,ifor)));
             PSMINPAR(it,ifor)=par(index);
    end
  end
 end

save('cost_function.mat','J','MEANJ','TOTMEANJ','UMEANJ','VMEANJ','TMEANJ','QMEANJ','PSMEANJ',TOTAL_J',...
                        'UJ','VJ','TJ','QJ','PSJ','TOTALMINPAR','UMINPAR','VMINPAR','TMINPAR','QMINPAR','PSMINPAR')














