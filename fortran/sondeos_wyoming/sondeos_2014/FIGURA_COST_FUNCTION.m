clear all
close all

%Speedy dimensions

nvar=29;

%Parameter information
true_par=0.16;


%Script to read an plot the parameter sensitivity
DATAPATH='/home/jruiz/DATA/EXP_SPEEDY/COST_FUNCTION/';

EXPERIMENTIMP=[DATAPATH 'RHBL_IMPERFECTMOD/'];     %Experiment name.
EXPERIMENTPER=[DATAPATH 'RHBL_PERFECTMOD/'  ];
ENS_SIZE=41;                                  %Ensemble size.
EXP_INI_DATE=1982030300;
EXP_END_DATE=1982033100;
FORECASTL=12;                                 %Forecast length

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

J=NaN(nvar,ENS_SIZE,NTIMES,round(FORECASTL/6),2);

while ( C_DATE_NUM <= END_DATE_NUM )
  ITIME=(C_DATE_NUM-INI_DATE_NUM)*4+1;
  datestr(C_DATE_NUM,'yyyymmddHH')
  
  forecast=6;

  while ( forecast <= FORECASTL )
  if(forecast > 10);charforecast=num2str(forecast);end
  if(forecast < 10);charforecast=strcat('0',num2str(forecast));end
   file=[EXPERIMENTPER 'cost_function/' datestr(C_DATE_NUM,'yyyymmddHH') 'F' charforecast '.dat'];
   nfile=fopen(file,'r','b');
   if(nfile ~= -1)
     J(:,:,ITIME,round(forecast/6),1)=load(file);
     fclose(nfile);
   end
   file=[EXPERIMENTIMP 'cost_function/' datestr(C_DATE_NUM,'yyyymmddHH') 'F' charforecast '.dat'];
   nfile=fopen(file,'r','b');
   if(nfile ~= -1)
     J(:,:,ITIME,round(forecast/6),2)=load(file);
     fclose(nfile);
   end


  forecast=forecast+6;
  
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
TOTALMINJ=squeeze(min(TOTALMEANJ,[],1));

UMEANJ=squeeze(sum(MEANJ(1:7,:,:,:)));
UMINJ=squeeze(min(UMEANJ,[],1));

VMEANJ=squeeze(sum(MEANJ(8:14,:,:,:)));
VMINJ=squeeze(min(VMEANJ,[],1));

TMEANJ=squeeze(sum(MEANJ(15:21,:,:,:)));
TMINJ=squeeze(min(TMEANJ,[],1));

QMEANJ=squeeze(sum(MEANJ(22:28,:,:,:)));
QMINJ=squeeze(min(QMEANJ,[],1));

PSMEANJ=squeeze(sum(MEANJ(22:28,:,:,:)));
PSMINJ=squeeze(min(PSMEANJ,[],1));



TOTALJ=squeeze(sum(J,1));
UJ=squeeze(sum(J(1:7,:,:,:,:),1));
VJ=squeeze(sum(J(8:14,:,:,:,:),1));
TJ=squeeze(sum(J(15:21,:,:,:,:),1));
QJ=squeeze(sum(J(22:28,:,:,:,:),1));
PSJ=squeeze(J(29,:,:,:,:));

[nparj ntj nfj nexp]=size(TOTALJ);
%Compute parameter value at minimun of J

     for it=1:ntj
         for ifor=1:nfj
            for iexp=1:nexp
             [nada index]=min(squeeze(TOTALJ(:,it,ifor,iexp)));
             TOTALMINPAR(it,ifor,iexp)=par(index);    
             [nada index]=min(squeeze(UJ(:,it,ifor,iexp)));
             UMINPAR(it,ifor,iexp)=par(index);
             [nada index]=min(squeeze(VJ(:,it,ifor,iexp)));
             VMINPAR(it,ifor,iexp)=par(index);
             [nada index]=min(squeeze(TJ(:,it,ifor,iexp)));
             TMINPAR(it,ifor,iexp)=par(index);
             [nada index]=min(squeeze(QJ(:,it,ifor,iexp)));
             QMINPAR(it,ifor,iexp)=par(index);
             [nada index]=min(squeeze(PSJ(:,it,ifor,iexp)));
             PSMINPAR(it,ifor,iexp)=par(index);
            end
         end
     end


%Comparo la funcion de costo para el modelo perfecto y apra el modelo imperfecto. 

figure

subplot(1,3,1)
plot(par,TOTALMEANJ(:,1,1)/TOTALMINJ(1,1),'b','LineWidth',2);
hold on
plot(par,TOTALMEANJ(:,1,2)/TOTALMINJ(1,2),'r','LineWidth',2);
axis([0.5 1.1 1 1.2])
title('Total cost function')
legend('PERFECT','IMPERFECT')

subplot(1,3,2)
plot(par,UMEANJ(:,1,1)/UMINJ(1,1),'b','LineWidth',2);
hold on
plot(par,UMEANJ(:,1,2)/UMINJ(1,2),'r','LineWidth',2);
axis([0.5 1.1 1 1.2])
title('U cost function')
legend('PERFECT','IMPERFECT')

subplot(1,3,3)
plot(par,TMEANJ(:,1,1)/TMINJ(1,1),'b','LineWidth',2);
hold on
plot(par,TMEANJ(:,1,2)/TMINJ(1,2),'r','LineWidth',2);
axis([0.5 1.1 1 1.2])
title('T cost function')
legend('PERFECT','IMPERFECT')

print('-dpng','FIGURE_COST_FUNCTION_1.png')

figure

plot(TOTALMINPAR(:,1,1),'b','LineWidth',2);
hold on
plot(TOTALMINPAR(:,1,2),'r','LineWidth',2);
plot(TMINPAR(:,1,2),'g','LineWidth',2);
title('Parameter minimun location (Total cost function)')
legend('PERFECT','IMPERFECT','IMPERFECT U')

print('-dpng','FIGURE_COST_FUNCTION_2.png')














