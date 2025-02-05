clear all
close all

load datos_val.mat;

NFor=size(Val.WRFVel,1);

for CurrentForecast=1:NFor

CurrentForecast/NFor

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

ErrorModel=Gaussian_calibration_fun( CVelFor , CDirFor , CDate , VelForDb , VelObsDb , DirForDb, DirObsDb , DatesDb );

CVelCalibrated(CurrentForecast,:)=ErrorModel.VelCalibrated;
CDirCalibrated(CurrentForecast,:)=ErrorModel.DirCalibrated;

end

%Verificamos los pronosticos calibrados y los no calibrados.

[ VerificationWRF ]=verification_fun(Val.WRFVel,Val.ObsVel,Val.WRFDir,Val.ObsDir);
[ VerificationWRFCalibrated ]=verification_fun(CVelCalibrated,Val.ObsVel,CDirCalibrated,Val.ObsDir);

fontsize=20;

%Ejemplo de performance para un pronostico.
PlotForecast=100;

figure
subplot(1,2,1)
hold on
plot(CVelCalibrated(PlotForecast,:),'r','LineWidth',3);
plot(Val.WRFVel(PlotForecast,:),'b','LineWidth',3);
plot(Val.ObsVel(PlotForecast,:),'k','LineWidth',3);

subplot(1,2,2)
hold on
plot(CDirCalibrated(PlotForecast,:),'r','LineWidth',3);
plot(Val.WRFDir(PlotForecast,:),'b','LineWidth',3);
plot(Val.ObsDir(PlotForecast,:),'k','LineWidth',3);

figure

subplot(2,2,1)
set(gca,'FontSize',fontsize);
horas=[1:size(Val.WRFDir,2)]*10/60;

hold on
plot(horas,VerificationWRF.VelRmse,'b','LineWidth',3)
plot(horas,VerificationWRFCalibrated.VelRmse,'r','LineWidth',3)
title('RMSE')
axis([0 max(horas) 0 max(VerificationWRF.VelRmse)]);

subplot(2,2,2)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationWRF.VelCorrelation,'b','LineWidth',3)
plot(horas,VerificationWRFCalibrated.VelCorrelation,'r','LineWidth',3)
title('CORRELACION')
axis([0 max(horas) 0 1]);

subplot(2,2,3)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationWRF.VelBias,'b','LineWidth',3)
plot(horas,VerificationWRFCalibrated.VelBias,'r','LineWidth',3)
title('BIAS')

axis([0 max(horas) min(VerificationWRF.VelBias) max(VerificationWRF.VelBias)]);

subplot(2,2,4)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationWRF.VelStdFor,'b','LineWidth',3)
plot(horas,VerificationWRFCalibrated.VelStdFor,'r','LineWidth',3)
plot(horas,VerificationWRF.VelStdObs,'k','LineWidth',3)
title('VARIABILIDAD')
print('-dpng','GaussianErroresDir.png');

axis([0 max(horas) 0 max(VerificationWRF.VelStdObs)]);

figure

subplot(2,2,1)

hold on
set(gca,'FontSize',fontsize);
plot(horas,VerificationWRF.DirRmse,'b','LineWidth',3)
plot(horas,VerificationWRFCalibrated.DirRmse,'r','LineWidth',3)
title('RMSE')
axis([0 max(horas) 0 max(VerificationWRF.DirRmse)]);

subplot(2,2,2)
hold on
set(gca,'FontSize',fontsize);
plot(horas,VerificationWRF.DirCorrelation,'b','LineWidth',3)
plot(horas,VerificationWRFCalibrated.DirCorrelation,'r','LineWidth',3)
title('CORRELACION')
axis([0 max(horas) 0 1 ]);

subplot(2,2,3)
hold on
set(gca,'FontSize',fontsize);
plot(horas,VerificationWRF.DirBias,'b','LineWidth',3)
plot(horas,VerificationWRFCalibrated.DirBias,'r','LineWidth',3)
title('BIAS')
axis([0 max(horas) min(VerificationWRF.DirBias) max(VerificationWRFCalibrated.DirBias) ]);

subplot(2,2,4)
hold on
set(gca,'FontSize',fontsize);
plot(horas,VerificationWRF.DirStdFor,'b','LineWidth',3)
plot(horas,VerificationWRFCalibrated.DirStdFor,'r','LineWidth',3)
plot(horas,VerificationWRF.DirStdObs,'k','LineWidth',3)
title('VARIABILIDAD')
print('-dpng','GaussianErroresVel.png');
axis([0 max(horas) 0 max(VerificationWRF.DirStdObs) ]);

save('GaussianCalibratedForecasts.mat');


