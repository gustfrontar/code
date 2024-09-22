clear all
close all

%Este script plotea la relacion entre el error y el spread para diferentes
%situaciones (segun hemisferio y estacion del anio).

EnsSizeThre=0.5;    %Ensambles mas chicos que este porcentaje del total no van a ser tenidos en cuenta en el calculo.

filein='../RESULTS/kwbc/ERRORSPREAD/ERROR_SPREAD_2007040112_2010040112.mat';
outfile='../RESULTS/kwbc/ERRORSPREAD/INTENSITYERROR_2007040112_2010040112.mat';

load(filein)

group.distlatpert=[];
group.distlonpert=[];
group.uvel=[];
group.vvel=[];
group.trajid=[];
group.minlat=[];
group.minlon=[];


enssize=size(group.minanom,1);
flength=size(group.minanom,2);

%Calculos previos
tmpmeananomerror=squeeze(nanmean(group.minanom,1))-group.minanomanalysis;
tmpctrlanomerror=squeeze(group.minanom(1,:,:))-group.minanomanalysis;
tmpmeanlaperror=squeeze(nanmean(group.minlap,1))-group.minlapanalysis;
tmpctrllaperror=squeeze(group.minlap(1,:,:))-group.minlapanalysis;

tmpsprdanom=squeeze(nanstd(group.minanom,1));
tmpsprdlap=squeeze(nanstd(group.minlap,1));


tmpmemb=group.nummembers;
mask=( isnan(tmpmeananomerror) | isnan(tmpctrlanomerror) | tmpmemb < enssize*EnsSizeThre );  %Esta mascara considera solo los puntos donde la media y el control estan presentes.

tmpmeananomerror(mask)=NaN;
tmpctrlanomerror(mask)=NaN;
tmpmeanlaperror(mask)=NaN;
tmpsprdanom(mask)=NaN;
tmpctrllaperror(mask)=NaN;

%Calculo para todos los grupos.
meananomerror_total=sqrt(nanmean(tmpmeananomerror.^2,2));
meanlaperror_total=sqrt(nanmean(tmpmeanlaperror.^2,2));
ctrlanomerror_total=sqrt(nanmean(tmpctrlanomerror.^2,2));
ctrllaperror_total=sqrt(nanmean(tmpctrllaperror.^2,2));

anomsprd_total=nanmean(tmpsprdanom,2);
lapsprd_total=nanmean(tmpsprdlap,2);

meananombias_total=nanmean(tmpmeananomerror,2);
meanlapbias_total=nanmean(tmpmeanlaperror,2);
ctrlanombias_total=nanmean(tmpctrlanomerror,2);
ctrllapbias_total=nanmean(tmpmeanlaperror,2);

tmp=repmat(meananombias_total,[1 size(tmpmeananomerror,2)]);
meananomerrordebias_total=sqrt(nanmean((tmpmeananomerror-tmp).^2,2));


%Calculo para el hemisferio sur.
index=group.shemi==1;
meananomerror_shemi=sqrt(nanmean(tmpmeananomerror(:,index).^2,2));
meanlaperror_shemi=sqrt(nanmean(tmpmeanlaperror(:,index).^2,2));
ctrlanomerror_shemi=sqrt(nanmean(tmpctrlanomerror(:,index).^2,2));
ctrllaperror_shemi=sqrt(nanmean(tmpctrllaperror(:,index).^2,2));

anomsprd_shemi=nanmean(tmpsprdanom(:,index),2);
lapsprd_shemi=nanmean(tmpsprdlap(:,index),2);

meananombias_shemi=nanmean(tmpmeananomerror(:,index),2);
meanlapbias_shemi=nanmean(tmpmeanlaperror(:,index),2);
ctrlanombias_shemi=nanmean(tmpctrlanomerror(:,index),2);
ctrllapbias_shemi=nanmean(tmpmeanlaperror(:,index),2);

%Calculo para el hemisferio norte.
index=group.nhemi==1;
meananomerror_nhemi=sqrt(nanmean(tmpmeananomerror(:,index).^2,2));
meanlaperror_nhemi=sqrt(nanmean(tmpmeanlaperror(:,index).^2,2));
ctrlanomerror_nhemi=sqrt(nanmean(tmpctrlanomerror(:,index).^2,2));
ctrllaperror_nhemi=sqrt(nanmean(tmpctrllaperror(:,index).^2,2));

anomsprd_nhemi=nanmean(tmpsprdanom(:,index),2);
lapsprd_nhemi=nanmean(tmpsprdlap(:,index),2);

meananombias_nhemi=nanmean(tmpmeananomerror(:,index),2);
meanlapbias_nhemi=nanmean(tmpmeanlaperror(:,index),2);
ctrlanombias_nhemi=nanmean(tmpctrlanomerror(:,index),2);
ctrllapbias_nhemi=nanmean(tmpmeanlaperror(:,index),2);

%Calculo para la estacion calida
index=group.calida==1;
meananomerror_calida=sqrt(nanmean(tmpmeananomerror(:,index).^2,2));
meanlaperror_calida=sqrt(nanmean(tmpmeanlaperror(:,index).^2,2));
ctrlanomerror_calida=sqrt(nanmean(tmpctrlanomerror(:,index).^2,2));
ctrllaperror_calida=sqrt(nanmean(tmpctrllaperror(:,index).^2,2));

anomsprd_calida=nanmean(tmpsprdanom(:,index),2);
lapsprd_calida=nanmean(tmpsprdlap(:,index),2);

meananombias_calida=nanmean(tmpmeananomerror(:,index),2);
meanlapbias_calida=nanmean(tmpmeanlaperror(:,index),2);
ctrlanombias_calida=nanmean(tmpctrlanomerror(:,index),2);
ctrllapbias_calida=nanmean(tmpmeanlaperror(:,index),2);

%Calculo para la estacion fria
index=group.fria==1;
meananomerror_fria=sqrt(nanmean(tmpmeananomerror(:,index).^2,2));
meanlaperror_fria=sqrt(nanmean(tmpmeanlaperror(:,index).^2,2));
ctrlanomerror_fria=sqrt(nanmean(tmpctrlanomerror(:,index).^2,2));
ctrllaperror_fria=sqrt(nanmean(tmpctrllaperror(:,index).^2,2));

anomsprd_fria=nanmean(tmpsprdanom(:,index),2);
lapsprd_fria=nanmean(tmpsprdlap(:,index),2);

meananombias_fria=nanmean(tmpmeananomerror(:,index),2);
meanlapbias_fria=nanmean(tmpmeanlaperror(:,index),2);
ctrlanombias_fria=nanmean(tmpctrlanomerror(:,index),2);
ctrllapbias_fria=nanmean(tmpmeanlaperror(:,index),2);


horas=((1:29)-1)*6;
%FIGURA ERRORES ANOM TOTAL, POR HEMISFERIO
figure
subplot(1,2,1)
hold on
plot(horas,meananomerror_total,'k-','LineWidth',2);
plot(horas,ctrlanomerror_total,'k--','LineWidth',2);
plot(horas,anomsprd_total,'k-.','LineWidth',2);

%plot(nanmean(tmp,2),'--','Color',[0.5 0.5 0.5],'LineWidth',2);

title('Total geopotential anomaly error');legend('MEAN ERROR','CTRL ERROR','SPREAD');

subplot(1,2,2)
hold on
plot(horas,meananomerror_shemi,'-','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(horas,meananomerror_nhemi,'-','Color',[0.6 0.6 0.6],'LineWidth',2)
plot(horas,ctrlanomerror_shemi,'--','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(horas,ctrlanomerror_nhemi,'--','Color',[0.6 0.6 0.6],'LineWidth',2)
plot(horas,anomsprd_shemi,'-.','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(horas,anomsprd_nhemi,'-.','Color',[0.6 0.6 0.6],'LineWidth',2)
xlabel('Lead time')
ylabel('Error')
title('Geopotential anomaly error by Hemisphere');legend('SH MEAN ERROR','NH MEAN ERROR','SH CTRL ERROR','NH CTRL ERROR','SH SPREAD','NH SPREAD');


figure
hold on
plot(horas,meananomerror_calida,'-','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(horas,meananomerror_fria,'-','Color',[0.6 0.6 0.6],'LineWidth',2)
plot(horas,ctrlanomerror_calida,'--','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(horas,ctrlanomerror_fria,'--','Color',[0.6 0.6 0.6],'LineWidth',2)
plot(horas,anomsprd_calida,'-.','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(horas,anomsprd_fria,'-.','Color',[0.6 0.6 0.6],'LineWidth',2)
xlabel('Lead time')
ylabel('Error')
title('Geopotential anomaly error by season');legend('WARM MEAN ERROR','COLD MEAN ERROR','WARM CTRL ERROR','COLD CTRL ERROR','WARM SPREAD','COLD SPREAD');

%FIGURA BIAS, POR HEMISFERIO Y POR ESTACION.

figure
subplot(1,2,1)
hold on
plot(horas,meananombias_total,'k-','LineWidth',2);
plot(horas,ctrlanombias_total,'k--','LineWidth',2);
xlabel('Lead time')
ylabel('Bias')
axis([0 168 -30 5])
%plot(nanmean(tmp,2),'--','Color',[0.5 0.5 0.5],'LineWidth',2);

title('Geopotential anomaly bias');legend('MEAN','CTRL');

subplot(1,2,2)
hold on
plot(horas,meananombias_shemi,'-','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(horas,meananombias_nhemi,'-','Color',[0.6 0.6 0.6],'LineWidth',2)
plot(horas,ctrlanombias_shemi,'--','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(horas,ctrlanombias_nhemi,'--','Color',[0.6 0.6 0.6],'LineWidth',2)
xlabel('Lead time')
ylabel('Bias')
axis([0 168 -30 5])
title('Geopotential anomaly bias');legend('SH MEAN','NH MEAN','SH CTRL','NH CTRL');




horas=((1:29)-1)*6;
%FIGURA ERRORES ANOM TOTAL, POR HEMISFERIO
figure
subplot(1,2,1)
hold on
plot(horas,meanlaperror_total,'k-','LineWidth',2);
plot(horas,ctrllaperror_total,'k--','LineWidth',2);
plot(horas,lapsprd_total,'k-.','LineWidth',2);

%plot(nanmean(tmp,2),'--','Color',[0.5 0.5 0.5],'LineWidth',2);

title('Total laplacian error');legend('MEAN ERROR','CTRL ERROR','SPREAD');

subplot(1,2,2)
hold on
plot(horas,meanlaperror_shemi,'-','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(horas,meanlaperror_nhemi,'-','Color',[0.6 0.6 0.6],'LineWidth',2)
plot(horas,ctrllaperror_shemi,'--','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(horas,ctrllaperror_nhemi,'--','Color',[0.6 0.6 0.6],'LineWidth',2)
plot(horas,lapsprd_shemi,'-.','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(horas,lapsprd_nhemi,'-.','Color',[0.6 0.6 0.6],'LineWidth',2)
xlabel('Lead time')
ylabel('Error')
title('Laplacian error by Hemisphere');legend('SH MEAN ERROR','NH MEAN ERROR','SH CTRL ERROR','NH CTRL ERROR','SH SPREAD','NH SPREAD');


figure
hold on
plot(horas,meanlaperror_calida,'-','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(horas,meanlaperror_fria,'-','Color',[0.6 0.6 0.6],'LineWidth',2)
plot(horas,ctrllaperror_calida,'--','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(horas,ctrllaperror_fria,'--','Color',[0.6 0.6 0.6],'LineWidth',2)
plot(horas,lapsprd_calida,'-.','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(horas,lapsprd_fria,'-.','Color',[0.6 0.6 0.6],'LineWidth',2)
xlabel('Lead time')
ylabel('Error')
title('Laplacian error by season');legend('WARM MEAN ERROR','COLD MEAN ERROR','WARM CTRL ERROR','COLD CTRL ERROR','WARM SPREAD','COLD SPREAD');

%FIGURA BIAS, POR HEMISFERIO Y POR ESTACION.

figure
subplot(1,2,1)
hold on
plot(horas,meanlapbias_total,'k-','LineWidth',2);
plot(horas,ctrllapbias_total,'k--','LineWidth',2);
xlabel('Lead time')
ylabel('Bias')
axis([0 168 -30 5])
%plot(nanmean(tmp,2),'--','Color',[0.5 0.5 0.5],'LineWidth',2);

title('Laplacian bias');legend('MEAN','CTRL');

subplot(1,2,2)
hold on
plot(horas,meanlapbias_shemi,'-','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(horas,meanlapbias_nhemi,'-','Color',[0.6 0.6 0.6],'LineWidth',2)
plot(horas,ctrllapbias_shemi,'--','Color',[0.3 0.3 0.3],'LineWidth',2)
plot(horas,ctrllapbias_nhemi,'--','Color',[0.6 0.6 0.6],'LineWidth',2)
xlabel('Lead time')
ylabel('Bias')
axis([0 168 -30 5])
title('Laplacian bias');legend('SH MEAN','NH MEAN','SH CTRL','NH CTRL');

clear group
clear tmpmean tmpctrl tmpmemb tmpsprd
save(outfile)

