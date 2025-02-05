clear all
close all

%==========================================================================
% Este script realiza una verificacion "Offline" de los pronosticos de
% viento y potencia y ademas compara los resultados con los obtenidos a
% partir de un pronostico calibrado estadisticamente.
%==========================================================================

%Cargamos un conjunto de datos de pronosticos y observaciones. Este archivo
%contiene una estructura Val que tiene 4 arrays (pronosticos del WRF para
%velocidad y direccion y observaciones del viento medio del parque para
%velocidad y direccion)

load datos_val.mat;

%Cargamos el archivo que contiene la estructura turbines en donde esta
%contenido la info de la relacion entre el viento medio del parque y la
%potencia producida por cada aerogenerador. 

% load datos_Parque.mat;
% %Anulo algunos de los elementos la estructura parque para hacer mas liviano
% %el resto del calculo.
% Parque.Dir=[];
% Parque.Vel=[];
% Parque.Speed=[];
% Parque.Date=[];
% for ii=1:length(Parque.Turbine);
% Parque.Turbine(ii).Angle=[];
% Parque.Turbine(ii).Power=[];
% Parque.Turbine(ii).VelQc=[];
% Parque.Turbine(ii).PowerQc=[];
% Parque.Turbine(ii).AngleQc=[];
% Parque.Turbine(ii).EffectiveDirection=[];
% Parque.Turbine(ii).VelQc=[];
% Parque.Turbine(ii).DirVel=[];
% Parque.Turbine(ii).DirAngle=[];
% Parque.Turbine(ii).DirPower=[];
% end

%Obtenemos la cantidad total de pronosticos que hay en la base de datos.
NFor=size(Val.WRFVel,1);

%Seteo los molinos como si estuvieran funcionando al 100 % todos.
% 
% MaskTurbine=ones(length(Parque.Turbine),1);

%TODO:

%Acumular los pronsoticos de potencia en horizontes de 1 hora, 3 horas, 6
%horas y 24 horas y verificar el pronostico para cada uno de esos
%horizontes.

%==========================================================================
% CALIBRACION DE LOS PRONOSTICOS Y TRADUCCION A POTENCIA.
%Iniciamos un loop sobre cada uno de los pronosticos. En este loop vamos a
%aplicar la correccion estadistica al pronostico y ademas vamos a hacer la
%traduccion a potencia del pronostico no calibrado, del pronostico
%calibrado y de las observaciones. 
%==========================================================================

for CurrentForecast=1:NFor

%Esto es para que imprima por pantalla el porcentaje de procesamiento
%completo.
CurrentForecast/NFor

%Me quedo con el pronostico que vamos a calibrar y obtengo su fecha.
CVelFor=Val.WRFVel(CurrentForecast,:);
CDirFor=Val.WRFDir(CurrentForecast,:);
CDate=Val.datetotal(CurrentForecast,:);

%Renombro el conjunto total de datos de vel, dir observada y pronosticada.
%Esto se hace porque el pronostico actual (CurrentForecast) lo tengo que
%sacar de la base de datos de entrenamiento. 

VelForDb=Val.WRFVel;
VelObsDb=Val.ObsVel;
DirForDb=Val.WRFDir;
DirObsDb=Val.ObsDir;

DatesDb=Val.datetotal;

%Voy a eliminar los dos dias previos y los dos dias posteriores al
%CurrentForecast de la base de datos de entrenamiento para garantizar
%independencia.

minindex=CurrentForecast-2;
maxindex=CurrentForecast+2;
if(minindex < 1);minindex=1;end
if(maxindex > NFor);maxindex=NFor;end

VelForDb(minindex:maxindex,:)=[];
VelObsDb(minindex:maxindex,:)=[];
DirForDb(minindex:maxindex,:)=[];
DirObsDb(minindex:maxindex,:)=[];
DatesDb(minindex:maxindex,:)=[];

%La Funcion CalibrationFun aplica una correccion estadistica de los
%pronosticos de Velocidad y Direccion. Tambien calcula estadisticos utiles
%para los errores que nos permitiran generar por ejemplo ensambles y
%fusionar los pronosticos y los datos observados.
ErrorModel=CalibrationFun( CVelFor , CDirFor , CDate , VelForDb , VelObsDb , DirForDb, DirObsDb , DatesDb );

%Guardo en este array las observaciones.
VelObs(CurrentForecast,:)=Val.ObsVel(CurrentForecast,:);
DirObs(CurrentForecast,:)=Val.ObsVel(CurrentForecast,:);

%Guardo en este array los pronosticos no calibrados.
VelUncalibrated(CurrentForecast,:)=ErrorModel.VelUncalibrated;
DirUncalibrated(CurrentForecast,:)=ErrorModel.DirUncalibrated;

%Guardo en este array los pronosticos calibrados.
VelCalibrated(CurrentForecast,:)=ErrorModel.VelCalibrated;
DirCalibrated(CurrentForecast,:)=ErrorModel.DirCalibrated;

% %Traduzco el pronostico sin calibrar a potencia.
% [ Null PowUncalibrated(CurrentForecast,:)]=VelDirToPower(VelUncalibrated(CurrentForecast,:),DirUncalibrated(CurrentForecast,:),Parque,MaskTurbine,'Mode');
% 
% %Traduzco el pronostico calibrado a potencia.
% [ Null PowCalibrated(CurrentForecast,:)]=VelDirToPower(VelCalibrated(CurrentForecast,:),DirCalibrated(CurrentForecast,:),Parque,MaskTurbine,'Mode');
% 
% %Traduzco el viento observado a potencia. 
% [ Null PowObs(CurrentForecast,:)]=VelDirToPower(VelObs(CurrentForecast,:),DirObs(CurrentForecast,:),Parque,MaskTurbine,'Mode');

end
PowObs=NaN(size(VelObs));
PowCalibrated=NaN(size(VelObs));
PowUncalibrated=NaN(size(VelObs));

%==========================================================================
% VERIFICACION DE LOS PRONOSTICOS
% calculo del error cuadratico medio (rmse), error sistematico (bias) y
% coeficiente de correlacion para la intensidad de viento, direccion y
% potencia total producida por el parque. 
%==========================================================================

[ VerificationUncalibrated ]=VerificationFun(VelUncalibrated,VelObs,DirUncalibrated,DirObs,PowUncalibrated,PowObs);
[ VerificationCalibrated ]=VerificationFun(VelCalibrated,VelObs,DirCalibrated,DirObs,PowCalibrated,PowObs);

%==========================================================================
% GUARDO LOS RESULTADOS
%==========================================================================

save('TestCalibrationOutput.mat');

%==========================================================================
% GRAFICAMOS LOS RESULTADOS
%==========================================================================

fontsize=20;

%==========================================================================
% GRAFICO QUE COMPARA EL PRONOSTICO CALIBRADO, SIN CALIBRAR Y LAS
% OBSERVACIONES PARA UN CASO EN PARTICULAR
% usar PlotForecast para elegir que caso se quiere graficar.
%==========================================================================

%Ejemplo de performance para un pronostico.
PlotForecast=150;

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

%==========================================================================
% GRAFICO QUE COMPARA LOS ERRORES DE LOS PRONOSTICOS CALIBRADOS Y SIN
% CALIBRAR PARA VELOCIDAD DE VIENTO, DIRECCION Y POTENCIA.
%==========================================================================

%==========================================================================
% VELOCIDAD DE VIENTO
%==========================================================================

figure

subplot(2,2,1)
set(gca,'FontSize',fontsize);
horas=[1:size(Val.WRFDir,2)]*10/60;

hold on
plot(horas,VerificationUncalibrated.VelRmse,'b','LineWidth',3)
plot(horas,VerificationCalibrated.VelRmse,'r','LineWidth',3)
title('RMSE')
axis([0 max(horas) 0 max(VerificationUncalibrated.VelRmse)]);
legend('Uncal','Cal');

subplot(2,2,2)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated.VelCorrelation,'b','LineWidth',3)
plot(horas,VerificationCalibrated.VelCorrelation,'r','LineWidth',3)
title('CORRELACION')
axis([0 max(horas) 0 1]);

subplot(2,2,3)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated.VelBias,'b','LineWidth',3)
plot(horas,VerificationCalibrated.VelBias,'r','LineWidth',3)
title('BIAS')

axis([0 max(horas) min(VerificationUncalibrated.VelBias) max(VerificationUncalibrated.VelBias)]);

subplot(2,2,4)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated.VelStdFor,'b','LineWidth',3)
plot(horas,VerificationCalibrated.VelStdFor,'r','LineWidth',3)
plot(horas,VerificationUncalibrated.VelStdObs,'k','LineWidth',3)
title('VARIABILIDAD')
axis([0 max(horas) 0 max(VerificationWRF.VelStdObs)]);

print('-dpng','ErroresVel.png');

%==========================================================================
% DIRECCION DEL VIENTO
%==========================================================================

figure

subplot(2,2,1)
set(gca,'FontSize',fontsize);
horas=[1:size(Val.WRFDir,2)]*10/60;

hold on
plot(horas,VerificationUncalibrated.DirRmse,'b','LineWidth',3)
plot(horas,VerificationCalibrated.DirRmse,'r','LineWidth',3)
title('RMSE')
axis([0 max(horas) 0 max(VerificationUncalibrated.DirRmse)]);
legend('Uncal','Cal');

subplot(2,2,2)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated.DirCorrelation,'b','LineWidth',3)
plot(horas,VerificationCalibrated.DirCorrelation,'r','LineWidth',3)
title('CORRELACION')
axis([0 max(horas) 0 1]);

subplot(2,2,3)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated.DirBias,'b','LineWidth',3)
plot(horas,VerificationCalibrated.DirBias,'r','LineWidth',3)
title('BIAS')

axis([0 max(horas) min(VerificationUncalibrated.DirBias) max(VerificationUncalibrated.DirBias)]);

subplot(2,2,4)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated.DirStdFor,'b','LineWidth',3)
plot(horas,VerificationCalibrated.DirStdFor,'r','LineWidth',3)
plot(horas,VerificationUncalibrated.DirStdObs,'k','LineWidth',3)
title('VARIABILIDAD')
axis([0 max(horas) 0 max(VerificationWRF.DirStdObs)]);

print('-dpng','ErroresDir.png');

%==========================================================================
% POTENCIA
%==========================================================================

figure

subplot(2,2,1)
set(gca,'FontSize',fontsize);
horas=[1:size(Val.WRFDir,2)]*10/60;

hold on
plot(horas,VerificationUncalibrated.PowRmse,'b','LineWidth',3)
plot(horas,VerificationCalibrated.PowRmse,'r','LineWidth',3)
title('RMSE')
axis([0 max(horas) 0 max(VerificationUncalibrated.PowRmse)]);
legend('Uncal','Cal');

subplot(2,2,2)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated.PowCorrelation,'b','LineWidth',3)
plot(horas,VerificationCalibrated.PowCorrelation,'r','LineWidth',3)
title('CORRELACION')
axis([0 max(horas) 0 1]);

subplot(2,2,3)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated.PowBias,'b','LineWidth',3)
plot(horas,VerificationCalibrated.PowBias,'r','LineWidth',3)
title('BIAS')

axis([0 max(horas) min(VerificationUncalibrated.PowBias) max(VerificationUncalibrated.PowBias)]);

subplot(2,2,4)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated.PowStdFor,'b','LineWidth',3)
plot(horas,VerificationCalibrated.PowStdFor,'r','LineWidth',3)
plot(horas,VerificationUncalibrated.PowStdObs,'k','LineWidth',3)
title('VARIABILIDAD')
axis([0 max(horas) 0 max(VerificationWRF.PowStdObs)]);

print('-dpng','ErroresDir.png');



