clear all
close all

load datos_val.mat;

%TODO: Reconstruir los pronosticos corregidos para todos las fechas y
%calcular los scores.

NFor=size(Val.WRFVel,1);

for CurrentForecast=1:NFor

CurrentForecast/NFor;


CVelFor=Val.WRFVel(CurrentForecast,:);
CDirFor=Val.WRFDir(CurrentForecast,:);

CDate=Val.datetotal(CurrentForecast,:);

VelForDb=Val.WRFVel;
VelObsDb=Val.ObsVel;
DirForDb=Val.WRFDir;
DirObsDb=Val.ObsDir;

DatesDb=Val.datetotal;

minindex=CurrentForecast-2;
maxindex=CurrentForecast+2;
if(minindex < 1);minindex=1;end
if(maxindex > NFor);maxindex=NFor;end

VelForDb(minindex:maxindex,:)=[];
VelObsDb(minindex:maxindex,:)=[];
DirForDb(minindex:maxindex,:)=[];
DirObsDb(minindex:maxindex,:)=[];
DatesDb(minindex:maxindex,:)=[];

%TODO: Incorporar una cross validation sacando el dia del pronostico.

[ CVelMean(CurrentForecast,:) CDirMean(CurrentForecast,:) CVelMedian(CurrentForecast,:) CDirMedian(CurrentForecast,:) CVelStd(CurrentForecast,:) CDirStd(CurrentForecast,:) SampleSize(CurrentForecast,:) ]=calibration_fun( CVelFor , CDirFor , CDate , VelForDb , VelObsDb , DirForDb, DirObsDb , DatesDb );


end

%Ejemplo de performance para un pronostico.
PlotForecast=101;

figure
subplot(1,2,1)
hold on
plot(CVelMean(PlotForecast,:),'r');
plot(CVelMedian(PlotForecast,:),'r--');
plot(Val.WRFVel(PlotForecast,:),'b');
plot(Val.ObsVel(PlotForecast,:),'k');

subplot(1,2,2)
hold on
plot(CDirMean(PlotForecast,:),'r');
plot(CDirMedian(PlotForecast,:),'r--');
plot(Val.WRFDir(PlotForecast,:),'b');
plot(Val.ObsDir(PlotForecast,:),'k');


%Verificamos los pronosticos calibrados y los no calibrados.

[ VerificationWRF ]=verification_fun(Val.WRFVel,Val.ObsVel,Val.WRFDir,Val.ObsDir);
[ VerificationWRFCalibratedMean ]=verification_fun(CVelMean,Val.ObsVel,CDirMean,Val.ObsDir);
[ VerificationWRFCalibratedMedian ]=verification_fun(CVelMedian,Val.ObsVel,CDirMedian,Val.ObsDir);

fontsize=20;

figure

subplot(2,2,1)
set(gca,'FontSize',fontsize);
horas=[1:size(Val.WRFDir,2)]*10/60;


hold on
plot(horas,VerificationWRF.VelRmse,'b','LineWidth',3)
plot(horas,VerificationWRFCalibratedMean.VelRmse,'r','LineWidth',3)
plot(horas,VerificationWRFCalibratedMedian.VelRmse,'r--','LineWidth',3)
title('RMSE')
axis([0 max(horas) 0 max(VerificationWRF.VelRmse)]);

subplot(2,2,2)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationWRF.VelCorrelation,'b','LineWidth',3)
plot(horas,VerificationWRFCalibratedMean.VelCorrelation,'r','LineWidth',3)
plot(horas,VerificationWRFCalibratedMedian.VelCorrelation,'r--','LineWidth',3)
title('CORRELACION')
axis([0 max(horas) 0 1]);


subplot(2,2,3)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationWRF.VelBias,'b','LineWidth',3)
plot(horas,VerificationWRFCalibratedMean.VelBias,'r','LineWidth',3)
plot(horas,VerificationWRFCalibratedMedian.VelBias,'r--','LineWidth',3)
title('BIAS')

axis([0 max(horas) min(VerificationWRF.VelBias) max(VerificationWRF.VelBias)]);

subplot(2,2,4)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationWRF.VelStdFor,'b','LineWidth',3)
plot(horas,VerificationWRFCalibratedMean.VelStdFor,'r','LineWidth',3)
plot(horas,VerificationWRFCalibratedMedian.VelStdFor,'r--','LineWidth',3)
plot(horas,VerificationWRF.VelStdObs,'k','LineWidth',3)
title('VARIABILIDAD')
print('-dpng','ErroresDir.png');

axis([0 max(horas) 0 max(VerificationWRF.VelStdObs)]);

figure

subplot(2,2,1)

hold on
set(gca,'FontSize',fontsize);
plot(horas,VerificationWRF.DirRmse,'b','LineWidth',3)
plot(horas,VerificationWRFCalibratedMean.DirRmse,'r','LineWidth',3)
plot(horas,VerificationWRFCalibratedMedian.DirRmse,'r--','LineWidth',3)
title('RMSE')
axis([0 max(horas) 0 max(VerificationWRF.DirRmse)]);


subplot(2,2,2)
hold on
set(gca,'FontSize',fontsize);
plot(horas,VerificationWRF.DirCorrelation,'b','LineWidth',3)
plot(horas,VerificationWRFCalibratedMean.DirCorrelation,'r','LineWidth',3)
plot(horas,VerificationWRFCalibratedMedian.DirCorrelation,'r--','LineWidth',3)
title('CORRELACION')
axis([0 max(horas) 0 1 ]);


subplot(2,2,3)
hold on
set(gca,'FontSize',fontsize);
plot(horas,VerificationWRF.DirBias,'b','LineWidth',3)
plot(horas,VerificationWRFCalibratedMean.DirBias,'r','LineWidth',3)
plot(horas,VerificationWRFCalibratedMedian.DirBias,'r--','LineWidth',3)
title('BIAS')
axis([0 max(horas) min(VerificationWRF.DirBias) max(VerificationWRFCalibratedMean.DirBias) ]);

subplot(2,2,4)
hold on
set(gca,'FontSize',fontsize);
plot(horas,VerificationWRF.DirStdFor,'b','LineWidth',3)
plot(horas,VerificationWRFCalibratedMean.DirStdFor,'r','LineWidth',3)
plot(horas,VerificationWRFCalibratedMedian.DirStdFor,'r--','LineWidth',3)
plot(horas,VerificationWRF.DirStdObs,'k','LineWidth',3)
title('VARIABILIDAD')
print('-dpng','ErroresVel.png');
axis([0 max(horas) 0 max(VerificationWRF.DirStdObs) ]);


save('Calibrated_Forecasts.mat');


