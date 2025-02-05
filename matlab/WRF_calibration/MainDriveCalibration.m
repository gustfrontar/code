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

load datos_Parque.mat;
%Anulo algunos de los elementos la estructura parque para hacer mas liviano
%el resto del calculo.
Parque.Dir=[];
Parque.Vel=[];
Parque.Speed=[];
Parque.Date=[];
for ii=1:length(Parque.Turbine);
Parque.Turbine(ii).Angle=[];
Parque.Turbine(ii).Power=[];
Parque.Turbine(ii).VelQc=[];
Parque.Turbine(ii).PowerQc=[];
Parque.Turbine(ii).AngleQc=[];
Parque.Turbine(ii).EffectiveDirection=[];
Parque.Turbine(ii).VelQc=[];
Parque.Turbine(ii).DirVel=[];
Parque.Turbine(ii).DirAngle=[];
Parque.Turbine(ii).DirPower=[];
end

%Obtenemos la cantidad total de pronosticos que hay en la base de datos.
NFor=size(Val.WRFVel,1);
NLead=size(Val.WRFVel,2);

%Seteo los molinos como si estuvieran funcionando al 100 % todos.
% 
MaskTurbine=ones(length(Parque.Turbine),1);


%==========================================================================
% PARAMETROS PARA EVALUAR UN PRONOSTICO SIMPLE BASADO EN LA PERSITENCIA
%==========================================================================
    
Order=6; %Cuantos datos vamos a promediar para calcular la persistencia.
Lead=6;  %Cuantos datos vamos a asumir que tenemos disponibles al momento de generar el pronostico por persistencia.

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


CVelObs=Val.ObsVel(CurrentForecast,:);
CDirObs=Val.ObsDir(CurrentForecast,:);

CVelObs(Lead+1:end)=NaN;
CDirObs(Lead+1:end)=NaN;

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

%Aplico la calibracion que usa la epoca del anio y el plazo de pronsotico.
[ MeanErrorVelDoyLead(CurrentForecast,:) MeanErrorDirDoyLead(CurrentForecast,:) StdErrorVelDoyLead(CurrentForecast,:) StdErrorDirDoyLead(CurrentForecast,:) SampleSizeDoyLead(CurrentForecast,:)]=DoyLeadCalibrationFun( CVelFor , CDirFor , CDate , VelForDb , VelObsDb , DirForDb, DirObsDb , DatesDb );
%Aplico la calibracion que usa la eopca del anio, el plazo de pronostico y
%la velocidad. 
[ MeanErrorVelDoyLeadVel(CurrentForecast,:) MeanErrorDirDoyLeadVel(CurrentForecast,:) StdErrorVelDoyLeadVel(CurrentForecast,:) StdErrorDirDoyLeadVel(CurrentForecast,:) SampleSizeDoyLeadVel(CurrentForecast,:)]=DoyLeadVelCalibrationFun( CVelFor , CDirFor , CDate , VelForDb , VelObsDb , DirForDb, DirObsDb , DatesDb );

%Aplico la calibracion que usa la eopca del anio, el plazo de pronostico y
%la velocidad y la direccion.

[ MeanErrorVelDoyLeadVelDir(CurrentForecast,:) MeanErrorDirDoyLeadVelDir(CurrentForecast,:) StdErrorVelDoyLeadVel(CurrentForecast,:) StdErrorDirDoyLeadVelDir(CurrentForecast,:) SampleSizeDoyLeadVelDir(CurrentForecast,:)]=DoyLeadVelDirCalibrationFun( CVelFor , CDirFor , CDate , VelForDb , VelObsDb , DirForDb, DirObsDb , DatesDb );

[ MeanErrorVelLeadVelDir(CurrentForecast,:) MeanErrorDirLeadVelDir(CurrentForecast,:) StdErrorDoyLeadVel(CurrentForecast,:) StdErrorDirLeadVelDir(CurrentForecast,:) SampleSizeLeadVelDir(CurrentForecast,:)]=LeadVelDirCalibrationFun( CVelFor , CDirFor , CDate , VelForDb , VelObsDb , DirForDb, DirObsDb , DatesDb );


[ VelClimatology(CurrentForecast,:) DirClimatology(CurrentForecast,:) ]=DoyLeadClimatologyFun( CDate , VelObsDb , DirObsDb , DatesDb  );

[ VelClimatologyPlus(CurrentForecast,:) DirClimatologyPlus(CurrentForecast,:) ]=DoyLeadVelDirClimatologyFun( CVelObs , CDirObs , CDate , VelObsDb , DirObsDb , DatesDb , Order );


VelPersistance(CurrentForecast,:)=CVelObs(Lead)*ones(NLead,1);
DirPersistance(CurrentForecast,:)=CDirObs(Lead)*ones(NLead,1);


end

%Calculo los pronosticos calibrados.

VelObs=Val.ObsVel;
DirObs=Val.ObsDir;

TotalPowObs=NaN(size(VelObs));
[PowObs TotalPowObs]=VelDirToPower(VelObs,DirObs,Parque,MaskTurbine,'Parametric');

VelUncalibrated=Val.WRFVel;
DirUncalibrated=Val.WRFDir;

TotalPowUncalibrated=NaN(size(VelUncalibrated));
[PowUncalibrated TotalPowUncalibrated]=VelDirToPower(VelUncalibrated,DirUncalibrated,Parque,MaskTurbine,'Parametric');

VelCalibratedDoyLead=Val.WRFVel-MeanErrorVelDoyLead;
DirCalibratedDoyLead=Val.WRFDir-MeanErrorDirDoyLead;

TotalPowCalibratedDoyLead=NaN(size(VelUncalibrated));
[PowCalibratedDoyLead TotalPowCalibratedDoyLead]=VelDirToPower(VelCalibratedDoyLead,DirCalibratedDoyLead,Parque,MaskTurbine,'Parametric');

VelCalibratedDoyLeadVel=Val.WRFVel-MeanErrorVelDoyLeadVel;
DirCalibratedDoyLeadVel=Val.WRFDir-MeanErrorDirDoyLeadVel;

TotalPowCalibratedDoyLeadVel=NaN(size(VelUncalibrated));
[PowCalibratedDoyLeadVel TotalPowCalibratedDoyLeadVel]=VelDirToPower(VelCalibratedDoyLeadVel,DirCalibratedDoyLeadVel,Parque,MaskTurbine,'Parametric');

VelCalibratedDoyLeadVelDir=Val.WRFVel-MeanErrorVelDoyLeadVelDir;
DirCalibratedDoyLeadVelDir=Val.WRFDir-MeanErrorDirDoyLeadVelDir;

TotalPowCalibratedDoyLeadVelDir=NaN(size(VelUncalibrated));
[PowCalibratedDoyLeadVelDir TotalPowCalibratedDoyLeadVelDir]=VelDirToPower(VelCalibratedDoyLeadVelDir,DirCalibratedDoyLeadVelDir,Parque,MaskTurbine,'Parametric');

VelCalibratedLeadVelDir=Val.WRFVel-MeanErrorVelLeadVelDir;
DirCalibratedLeadVelDir=Val.WRFDir-MeanErrorDirLeadVelDir;

TotalPowCalibratedLeadVelDir=NaN(size(VelUncalibrated));
[PowCalibratedLeadVelDir TotalPowCalibratedLeadVelDir]=VelDirToPower(VelCalibratedLeadVelDir,DirCalibratedLeadVelDir,Parque,MaskTurbine,'Parametric');

TotalPowPersistance=NaN(size(VelUncalibrated));
[PowPersistance TotalPowPersistance ]=VelDirToPower(VelPersistance,DirPersistance,Parque,MaskTurbine,'Parametric');
TotalPowClimatology=NaN(size(VelUncalibrated));
[PowClimatology TotalPowClimatology ]=VelDirToPower(VelClimatology,DirClimatology,Parque,MaskTurbine,'Parametric');
TotalPowClimatologyPlus=NaN(size(VelUncalibrated));
[PowClimatologyPlus TotalPowClimatologyPlus ]=VelDirToPower(VelClimatologyPlus,DirClimatologyPlus,Parque,MaskTurbine,'Parametric');


%==========================================================================
% IMPLEMENTAMOS UNA CALIBRACION PARA LA POTENCIA CONDICIONADA EN LA EPOCA
% DEL AÑO, PLAZO DE PRONOSTICO E INTENSIDAD DEL VIENTO.
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

VelForDb=VelUncalibrated;

TotalPowForDb=TotalPowUncalibrated;
TotalPowObsDb=TotalPowObs;

DatesDb=Val.datetotal;

%Voy a eliminar los dos dias previos y los dos dias posteriores al
%CurrentForecast de la base de datos de entrenamiento para garantizar
%independencia.

minindex=CurrentForecast-2;
maxindex=CurrentForecast+2;
if(minindex < 1);minindex=1;end
if(maxindex > NFor);maxindex=NFor;end

VelForDb(minindex:maxindex,:)=[];
TotalPowForDb(minindex:maxindex,:)=[];
TotalPowObsDb(minindex:maxindex,:)=[];

DatesDb(minindex:maxindex,:)=[];

[ MeanErrorPowDoyLeadVel(CurrentForecast,:) StdErrorPowDoyLeadVel(CurrentForecast,:) ]=DoyLeadVelPowCalibrationFun( CVelFor , CDate , VelForDb , TotalPowForDb , TotalPowObsDb , DatesDb );

VelCalibratedPow(CurrentForecast,:)=VelUncalibrated(CurrentForecast,:);
DirCalibratedPow(CurrentForecast,:)=DirUncalibrated(CurrentForecast,:);
TotalPowCalibratedPow(CurrentForecast,:)=TotalPowUncalibrated(CurrentForecast,:) - MeanErrorPowDoyLeadVel(CurrentForecast,:);

end


%==========================================================================
% VERIFICACION DE LOS PRONOSTICOS
% calculo del error cuadratico medio (rmse), error sistematico (bias) y
% coeficiente de correlacion para la intensidad de viento, direccion y
% potencia total producida por el parque. 
%==========================================================================

[ VerificationUncalibrated ]=VerificationFun(VelUncalibrated,VelObs,DirUncalibrated,DirObs,TotalPowUncalibrated,TotalPowObs);

[ VerificationCalibratedDoyLead ]=VerificationFun(VelCalibratedDoyLead,VelObs,DirCalibratedDoyLead,DirObs,TotalPowCalibratedDoyLead,TotalPowObs);

[ VerificationCalibratedDoyLeadVel ]=VerificationFun(VelCalibratedDoyLeadVel,VelObs,DirCalibratedDoyLeadVel,DirObs,TotalPowCalibratedDoyLeadVel,TotalPowObs);

[ VerificationCalibratedDoyLeadVelDir ]=VerificationFun(VelCalibratedDoyLeadVelDir,VelObs,DirCalibratedDoyLeadVelDir,DirObs,TotalPowCalibratedDoyLeadVelDir,TotalPowObs);

[ VerificationCalibratedLeadVelDir ]=VerificationFun(VelCalibratedLeadVelDir,VelObs,DirCalibratedLeadVelDir,DirObs,TotalPowCalibratedLeadVelDir,TotalPowObs);

[ VerificationPersistance ]=VerificationFun(VelPersistance,VelObs,DirPersistance,DirObs,TotalPowPersistance,TotalPowObs);

[ VerificationClimatologyPlus ]=VerificationFun(VelClimatologyPlus,VelObs,DirClimatologyPlus,DirObs,TotalPowClimatologyPlus,TotalPowObs);

[ VerificationClimatology ]=VerificationFun(VelClimatology,VelObs,DirClimatology,DirObs,TotalPowClimatology,TotalPowObs);

[ VerificationCalibratedPow ]=VerificationFun(VelCalibratedPow,VelObs,DirCalibratedPow,DirObs,TotalPowCalibratedPow,TotalPowObs);


%==========================================================================
% GRAFICAMOS LOS RESULTADOS
%==========================================================================

fontsize=12;

%==========================================================================
% GRAFICO QUE COMPARA EL PRONOSTICO CALIBRADO, SIN CALIBRAR Y LAS
% OBSERVACIONES PARA UN CASO EN PARTICULAR
% usar PlotForecast para elegir que caso se quiere graficar.
%==========================================================================
horas=[1:size(Val.WRFDir,2)]*10/60;
%Ejemplo de performance para un pronostico.
PlotForecast=12;

hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 40 20]);

subplot(1,2,1)
set(gca,'FontSize',fontsize)
hold on
plot(horas,VelCalibratedDoyLead(PlotForecast,:),'r','LineWidth',3);
plot(horas,VelCalibratedDoyLeadVel(PlotForecast,:),'g','LineWidth',3);
% plot(horas,VelCalibratedDoyLeadVelDir(PlotForecast,:),'m','LineWidth',3);
% plot(horas,VelCalibratedLeadVelDir(PlotForecast,:),'y','LineWidth',3);
plot(horas,VelUncalibrated(PlotForecast,:),'b','LineWidth',3);
plot(horas,VelObs(PlotForecast,:),'k','LineWidth',3);
axis([0 max(horas) 0 25])
xlabel('Time (horas)')
ylabel('Direccion (grados)')

subplot(1,2,2)
set(gca,'FontSize',fontsize)
hold on
plot(horas,DirCalibratedDoyLead(PlotForecast,:),'r','LineWidth',3);
plot(horas,DirCalibratedDoyLeadVel(PlotForecast,:),'g','LineWidth',3);
% plot(horas,DirCalibratedDoyLeadVelDir(PlotForecast,:),'m','LineWidth',3);
% plot(horas,DirCalibratedLeadVelDir(PlotForecast,:),'y','LineWidth',3);
plot(horas,DirUncalibrated(PlotForecast,:),'b','LineWidth',3);
plot(horas,DirObs(PlotForecast,:),'k','LineWidth',3);
axis([0 max(horas) 0 360])
xlabel('Time (horas)')
ylabel('Viento (m/s)')

print('ExampleForecast.png')

%==========================================================================
% GRAFICO QUE COMPARA LOS ERRORES DE LOS PRONOSTICOS CALIBRADOS Y SIN
% CALIBRAR PARA VELOCIDAD DE VIENTO, DIRECCION Y POTENCIA.
%==========================================================================

%==========================================================================
% VELOCIDAD DE VIENTO
%==========================================================================


hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20]);

subplot(2,2,1)
set(gca,'FontSize',fontsize);


hold on
plot(horas,VerificationUncalibrated.VelRmse,'b','LineWidth',2)
plot(horas,VerificationCalibratedDoyLead.VelRmse,'r','LineWidth',2)
plot(horas,VerificationCalibratedDoyLeadVel.VelRmse,'g','LineWidth',2)
% plot(horas,VerificationCalibratedDoyLeadVelDir.VelRmse,'m','LineWidth',2)
% plot(horas,VerificationCalibratedLeadVelDir.VelRmse,'y','LineWidth',2)
% plot(horas,VerificationPersistance.VelRmse,'k','LineWidth',2)
plot(horas,VerificationClimatologyPlus.VelRmse,'k-','LineWidth',2)
plot(horas,VerificationClimatology.VelRmse,'k:','LineWidth',2)
grid('On')
title('RMSE (a)')
axis([0 max(horas) 0 5]);


subplot(2,2,2)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated.VelCorrelation,'b','LineWidth',2)
plot(horas,VerificationCalibratedDoyLead.VelCorrelation,'r','LineWidth',2)
plot(horas,VerificationCalibratedDoyLeadVel.VelCorrelation,'g','LineWidth',2)
% plot(horas,VerificationCalibratedDoyLeadVelDir.VelCorrelation,'m','LineWidth',2)
% plot(horas,VerificationCalibratedLeadVelDir.VelCorrelation,'y','LineWidth',2)
% plot(horas,VerificationPersistance.VelCorrelation,'k','LineWidth',2)
plot(horas,VerificationClimatologyPlus.VelCorrelation,'k-','LineWidth',2)
plot(horas,VerificationClimatology.VelCorrelation,'k:','LineWidth',2)
grid('On')
title('CORRELACION (b)')
axis([0 max(horas) 0 1]);

subplot(2,2,3)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated.VelBias,'b','LineWidth',2)
plot(horas,VerificationCalibratedDoyLead.VelBias,'r','LineWidth',2)
plot(horas,VerificationCalibratedDoyLeadVel.VelBias,'g','LineWidth',2)
% plot(horas,VerificationCalibratedDoyLeadVelDir.VelBias,'m','LineWidth',2)
% plot(horas,VerificationCalibratedLeadVelDir.VelBias,'y','LineWidth',2)
% plot(horas,VerificationPersistance.VelBias,'k','LineWidth',2)
plot(horas,VerificationClimatologyPlus.VelBias,'k-','LineWidth',2)
plot(horas,VerificationClimatology.VelBias,'k:','LineWidth',2)
grid('On')
title('BIAS (c)')

axis([0 max(horas) -2 2]);

subplot(2,2,4)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated.VelStdFor,'b','LineWidth',2)
plot(horas,VerificationCalibratedDoyLead.VelStdFor,'r','LineWidth',2)
plot(horas,VerificationCalibratedDoyLeadVel.VelStdFor,'g','LineWidth',2)
% plot(horas,VerificationCalibratedDoyLeadVelDir.VelStdFor,'m','LineWidth',2)
% plot(horas,VerificationCalibratedLeadVelDir.VelStdFor,'y','LineWidth',2)
plot(horas,VerificationUncalibrated.VelStdObs,'k','LineWidth',2)
% plot(horas,VerificationPersistance.VelStdObs,'k','LineWidth',2)
%plot(horas,VerificationClimatologyPlus.VelStdFor,'k-','LineWidth',2)
%plot(horas,VerificationClimatology.VelStdFor,'k:','LineWidth',2)
grid('On')
title('VARIABILIDAD (d)')
axis([0 max(horas) 0 4]);

print('-dpng','ErroresVel.png');

%==========================================================================
% DIRECCION DEL VIENTO
%==========================================================================


hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20]);

subplot(2,2,1)
set(gca,'FontSize',fontsize);
horas=[1:size(Val.WRFDir,2)]*10/60;

hold on
plot(horas,VerificationUncalibrated.DirRmse,'b','LineWidth',2)
plot(horas,VerificationCalibratedDoyLead.DirRmse,'r','LineWidth',2)
plot(horas,VerificationCalibratedDoyLeadVel.DirRmse,'g','LineWidth',2)
% plot(horas,VerificationCalibratedDoyLeadVelDir.DirRmse,'m','LineWidth',2)
% plot(horas,VerificationCalibratedLeadVelDir.DirRmse,'y','LineWidth',2)
% plot(horas,VerificationPersistance.DirRmse,'k','LineWidth',2)
plot(horas,VerificationClimatologyPlus.DirRmse,'k-','LineWidth',2)
plot(horas,VerificationClimatology.DirRmse,'k:','LineWidth',2)
grid('On')
title('RMSE (a)')
axis([0 max(horas) 0 90]);


subplot(2,2,2)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated.DirCorrelation,'b','LineWidth',2)
plot(horas,VerificationCalibratedDoyLead.DirCorrelation,'r','LineWidth',2)
plot(horas,VerificationCalibratedDoyLeadVel.DirCorrelation,'g','LineWidth',2)
% plot(horas,VerificationCalibratedDoyLeadVelDir.DirCorrelation,'m','LineWidth',2)
% plot(horas,VerificationCalibratedLeadVelDir.DirCorrelation,'y','LineWidth',2)
% plot(horas,VerificationPersistance.DirCorrelation,'k','LineWidth',2)
plot(horas,VerificationClimatologyPlus.DirCorrelation,'k-','LineWidth',2)
plot(horas,VerificationClimatology.DirCorrelation,'k:','LineWidth',2)
grid('On')
title('CORRELACION (b)')
axis([0 max(horas) 0 1]);

subplot(2,2,3)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated.DirBias,'b','LineWidth',2)
plot(horas,VerificationCalibratedDoyLead.DirBias,'r','LineWidth',2)
plot(horas,VerificationCalibratedDoyLeadVel.DirBias,'g','LineWidth',2)
% plot(horas,VerificationCalibratedDoyLeadVelDir.DirBias,'m','LineWidth',2)
% plot(horas,VerificationCalibratedLeadVelDir.DirBias,'y','LineWidth',2)
% plot(horas,VerificationPersistance.DirBias,'k','LineWidth',2)
plot(horas,VerificationClimatologyPlus.DirBias,'k-','LineWidth',2)
plot(horas,VerificationClimatology.DirBias,'k:','LineWidth',2)
grid('On')
title('BIAS (c)')

axis([0 max(horas) -15 15]);

subplot(2,2,4)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated.DirStdFor,'b','LineWidth',2)
plot(horas,VerificationCalibratedDoyLead.DirStdFor,'r','LineWidth',2)
plot(horas,VerificationCalibratedDoyLeadVel.DirStdFor,'g','LineWidth',2)
% plot(horas,VerificationCalibratedDoyLeadVelDir.DirStdFor,'m','LineWidth',2)
% plot(horas,VerificationCalibratedLeadVelDir.DirStdFor,'y','LineWidth',2)
plot(horas,VerificationUncalibrated.DirStdObs,'k','LineWidth',2)
% plot(horas,VerificationPersistance.DirStdObs,'k','LineWidth',2)
% plot(horas,VerificationClimatologyPlus.DirStdFor,'k-','LineWidth',2)
% plot(horas,VerificationClimatology.DirStdFor,'k:','LineWidth',2)
grid('On')
title('VARIABILIDAD (d)')
axis([0 max(horas) 0 150]);

print('-dpng','ErroresDir.png');

%==========================================================================
% POTENCIA
%==========================================================================
horas=[1:size(Val.WRFDir,2)]*10/60;

hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20]);

subplot(2,2,1)
set(gca,'FontSize',fontsize);


hold on
plot(horas,VerificationUncalibrated.PowRmse,'b','LineWidth',2)
plot(horas,VerificationCalibratedDoyLead.PowRmse,'r','LineWidth',2)
plot(horas,VerificationCalibratedDoyLeadVel.PowRmse,'g','LineWidth',2)
plot(horas,VerificationCalibratedPow.PowRmse,'m','LineWidth',2)
% plot(horas,VerificationCalibratedDoyLeadVelDir.PowRmse,'m','LineWidth',2)
% plot(horas,VerificationCalibratedLeadVelDir.PowRmse,'y','LineWidth',2)
% plot(horas,VerificationPersistance.PowRmse,'k','LineWidth',2)
plot(horas,VerificationClimatologyPlus.PowRmse,'k-','LineWidth',2)
plot(horas,VerificationClimatology.PowRmse,'k:','LineWidth',2)
grid('On')
title('RMSE (a)')
xlabel('Tiempo (horas)')
ylabel('Potencia (MW)')
axis([0 max(horas) 0 30000]);
%legend('Uncal','Cal');

subplot(2,2,2)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated.PowCorrelation,'b','LineWidth',2)
plot(horas,VerificationCalibratedDoyLead.PowCorrelation,'r','LineWidth',2)
plot(horas,VerificationCalibratedDoyLeadVel.PowCorrelation,'g','LineWidth',2)
plot(horas,VerificationCalibratedPow.PowCorrelation,'m','LineWidth',2)
% plot(horas,VerificationCalibratedDoyLeadVelDir.PowCorrelation,'m','LineWidth',2)
% plot(horas,VerificationCalibratedLeadVelDir.PowCorrelation,'y','LineWidth',2)
% plot(horas,VerificationPersistance.PowCorrelation,'k','LineWidth',2)
plot(horas,VerificationClimatologyPlus.PowCorrelation,'k-','LineWidth',2)
plot(horas,VerificationClimatology.PowCorrelation,'k:','LineWidth',2)
grid('On')
title('CORRELACION (b)')
axis([0 max(horas) 0 1]);

subplot(2,2,3)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated.PowBias,'b','LineWidth',2)
plot(horas,VerificationCalibratedDoyLead.PowBias,'r','LineWidth',2)
plot(horas,VerificationCalibratedDoyLeadVel.PowBias,'g','LineWidth',2)
plot(horas,VerificationCalibratedPow.PowBias,'m','LineWidth',2)
% plot(horas,VerificationCalibratedDoyLeadVelDir.PowBias,'m','LineWidth',2)
% plot(horas,VerificationCalibratedLeadVelDir.PowBias,'y','LineWidth',2)
% plot(horas,VerificationPersistance.PowBias,'k','LineWidth',2)
plot(horas,VerificationClimatologyPlus.PowBias,'k-','LineWidth',2)
plot(horas,VerificationClimatology.PowBias,'k:','LineWidth',2)
xlabel('Tiempo (horas)')
ylabel('Potencia (MW)')
grid('On')
title('BIAS (c)')

axis([0 max(horas) -20000 20000]);

subplot(2,2,4)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated.PowStdFor,'b','LineWidth',2)
plot(horas,VerificationCalibratedDoyLead.PowStdFor,'r','LineWidth',2)
plot(horas,VerificationCalibratedDoyLeadVel.PowStdFor,'g','LineWidth',2)
plot(horas,VerificationCalibratedPow.PowStdFor,'m','LineWidth',2)
% plot(horas,VerificationCalibratedDoyLeadVelDir.PowStdObs,'m','LineWidth',2)
% plot(horas,VerificationCalibratedLeadVelDir.PowStdObs,'y','LineWidth',2)
plot(horas,VerificationPersistance.PowStdObs,'k','LineWidth',2)
% plot(horas,VerificationClimatologyPlus.PowStdFor,'k-','LineWidth',2)
% plot(horas,VerificationClimatology.PowStdFor,'k:','LineWidth',2)
xlabel('Tiempo (horas)')
ylabel('Potencia (MW)')
grid('On')
title('VARIABILIDAD (d)')
axis([0 max(horas) 0 30000]);

print('-dpng','ErroresPow.png');


%==========================================================================
% ERROR EN POTENCIA COMO FUNCION DE LA POTENCIA
%==========================================================================

%Uncalibrated
hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 40 20]);

subplot(1,2,1)
set(gca,'FontSize',fontsize)
pcolor(horas,VerificationUncalibrated.PowConditionalPow,VerificationUncalibrated.PowConditionalRmse);shading flat
caxis([ 0 40000]);
colorbar
title('RMSE (a)')

subplot(1,2,2)
set(gca,'FontSize',fontsize)
pcolor(horas,VerificationUncalibrated.PowConditionalPow,VerificationUncalibrated.PowConditionalBias);shading flat
caxis([ -30000 30000]);
colorbar
title('BIAS (b)')
print('-dpng','ErroresPowConditionalUncalibrated.png');

%Calibrated DoyLead
hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 40 20]);

subplot(1,2,1)
set(gca,'FontSize',fontsize)
pcolor(horas,VerificationCalibratedDoyLead.PowConditionalPow,VerificationCalibratedDoyLead.PowConditionalRmse);shading flat
caxis([ 0 40000]);
colorbar
title('RMSE (a)')

subplot(1,2,2)
set(gca,'FontSize',fontsize)
pcolor(horas,VerificationCalibratedDoyLead.PowConditionalPow,VerificationCalibratedDoyLead.PowConditionalBias);shading flat
caxis([ -30000 30000]);
colorbar
title('BIAS (b)')
print('-dpng','ErroresPowConditionalCalibratedDoyLead.png');

%Calibrated DoyLeadVel
hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 40 20]);

subplot(1,2,1)
set(gca,'FontSize',fontsize)
pcolor(horas,VerificationCalibratedDoyLeadVel.PowConditionalPow,VerificationCalibratedDoyLeadVel.PowConditionalRmse);shading flat
caxis([ 0 40000]);
colorbar
title('RMSE (a)')

subplot(1,2,2)
set(gca,'FontSize',fontsize)
pcolor(horas,VerificationCalibratedDoyLeadVel.PowConditionalPow,VerificationCalibratedDoyLeadVel.PowConditionalBias);shading flat
caxis([ -30000 30000]);
colorbar
title('BIAS (b)')
print('-dpng','ErroresPowConditionalCalibratedDoyLeadVel.png');

%Calibrated Pow
hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 40 20]);

subplot(1,2,1)
set(gca,'FontSize',fontsize)
pcolor(horas,VerificationCalibratedPow.PowConditionalPow,VerificationCalibratedPow.PowConditionalRmse);shading flat
caxis([ 0 40000]);
colorbar
title('RMSE (a)')

subplot(1,2,2)
set(gca,'FontSize',fontsize)
pcolor(horas,VerificationCalibratedPow.PowConditionalPow,VerificationCalibratedPow.PowConditionalBias);shading flat
caxis([ -30000 30000]);
colorbar
title('BIAS (b)')
print('-dpng','ErroresPowConditionalCalibratedPow.png');



%Todos
hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 40 20]);

subplot(1,2,1)
set(gca,'FontSize',fontsize)
hold on
plot(VerificationUncalibrated.PowConditionalPow,nanmean(VerificationUncalibrated.PowConditionalRmse,2),'b','LineWidth',2);
plot(VerificationCalibratedDoyLead.PowConditionalPow,nanmean(VerificationCalibratedDoyLead.PowConditionalRmse,2),'r','LineWidth',2);
plot(VerificationCalibratedDoyLeadVel.PowConditionalPow,nanmean(VerificationCalibratedDoyLeadVel.PowConditionalRmse,2),'g','LineWidth',2);
plot(VerificationCalibratedPow.PowConditionalPow,nanmean(VerificationCalibratedPow.PowConditionalRmse,2),'m','LineWidth',2);
xlabel('Potencia (MW)')
ylabel('RMSE (MW)')
axis([0 max( VerificationUncalibrated.PowConditionalPow ) 0 30000 ])
grid on

title('RMSE (a)')

subplot(1,2,2)
set(gca,'FontSize',fontsize)
hold on
plot(VerificationUncalibrated.PowConditionalPow,nanmean(VerificationUncalibrated.PowConditionalBias,2),'b','LineWidth',2);
plot(VerificationCalibratedDoyLead.PowConditionalPow,nanmean(VerificationCalibratedDoyLead.PowConditionalBias,2),'r','LineWidth',2);
plot(VerificationCalibratedDoyLeadVel.PowConditionalPow,nanmean(VerificationCalibratedDoyLeadVel.PowConditionalBias,2),'g','LineWidth',2);
plot(VerificationCalibratedPow.PowConditionalPow,nanmean(VerificationCalibratedPow.PowConditionalBias,2),'m','LineWidth',2);
xlabel('Potencia (MW)')
ylabel('BIAS (MW)')
axis([0 max( VerificationUncalibrated.PowConditionalPow ) -20000 20000 ])
grid on

title('BIAS (b)')
print('-dpng','ErroresPowConditional.png');

%========================================================================================================
% HAGO LA VERIFICACION PARA LOS PRONOSTICOS ACUMULADOS EN PLAZOS DE 1 HORA.
%========================================================================================================

[ VelObsHourly ] = AccumulateDataFun( VelObs , 6 , 'Mean' );
[ DirObsHourly ] = AccumulateDataFun( DirObs , 6 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowObsHourly ] = AccumulateDataFun( TotalPowObs , 6 , 'Mean' );

[ VelUncalibratedHourly ] = AccumulateDataFun( VelUncalibrated , 6 , 'Mean' );
[ DirUncalibratedHourly ] = AccumulateDataFun( DirUncalibrated , 6 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowUncalibratedHourly ] = AccumulateDataFun( TotalPowUncalibrated , 6 , 'Mean' );

[ VelCalibratedDoyLeadHourly ] = AccumulateDataFun( VelCalibratedDoyLead , 6 , 'Mean' );
[ DirCalibratedDoyLeadHourly ] = AccumulateDataFun( DirCalibratedDoyLead , 6 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowCalibratedDoyLeadHourly ] = AccumulateDataFun( TotalPowCalibratedDoyLead , 6 , 'Mean' );

[ VelCalibratedDoyLeadVelHourly ] = AccumulateDataFun( VelCalibratedDoyLeadVel , 6 , 'Mean' );
[ DirCalibratedDoyLeadVelHourly ] = AccumulateDataFun( DirCalibratedDoyLeadVel , 6 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowCalibratedDoyLeadVelHourly ] = AccumulateDataFun( TotalPowCalibratedDoyLeadVel , 6 , 'Mean' );

[ VelCalibratedDoyLeadVelDirHourly ] = AccumulateDataFun( VelCalibratedDoyLeadVelDir , 6 , 'Mean' );
[ DirCalibratedDoyLeadVelDirHourly ] = AccumulateDataFun( DirCalibratedDoyLeadVelDir , 6 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowCalibratedDoyLeadVelDirHourly ] = AccumulateDataFun( TotalPowCalibratedDoyLeadVelDir , 6 , 'Mean' );

[ VelCalibratedLeadVelDirHourly ] = AccumulateDataFun( VelCalibratedLeadVelDir , 6 , 'Mean' );
[ DirCalibratedLeadVelDirHourly ] = AccumulateDataFun( DirCalibratedLeadVelDir , 6 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowCalibratedLeadVelDirHourly ] = AccumulateDataFun( TotalPowCalibratedLeadVelDir , 6 , 'Mean' );

[ VelPersistanceHourly ] = AccumulateDataFun( VelPersistance , 6 , 'Mean' );
[ DirPersistanceHourly ] = AccumulateDataFun( DirPersistance , 6 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowPersistanceHourly ] = AccumulateDataFun( TotalPowPersistance , 6 , 'Mean' );

[ VelClimatologyHourly ] = AccumulateDataFun( VelClimatology , 6 , 'Mean' );
[ DirClimatologyHourly ] = AccumulateDataFun( DirClimatology , 6 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowClimatologyHourly ] = AccumulateDataFun( TotalPowClimatology , 6 , 'Mean' );

[ VelClimatologyPlusHourly ] = AccumulateDataFun( VelClimatologyPlus , 6 , 'Mean' );
[ DirClimatologyPlusHourly ] = AccumulateDataFun( DirClimatologyPlus , 6 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowClimatologyPlusHourly ] = AccumulateDataFun( TotalPowClimatologyPlus , 6 , 'Mean' );

[ VelCalibratedPowHourly ] = AccumulateDataFun( VelCalibratedPow , 6 , 'Mean' );
[ DirCalibratedPowHourly ] = AccumulateDataFun( DirCalibratedPow , 6 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowCalibratedPowHourly ] = AccumulateDataFun( TotalPowCalibratedPow , 6 , 'Mean' );

%==========================================================================
% VERIFICACION DE LOS PRONOSTICOS
% calculo del error cuadratico medio (rmse), error sistematico (bias) y
% coeficiente de correlacion para la intensidad de viento, direccion y
% potencia total producida por el parque. 
%==========================================================================

[ VerificationUncalibratedHourly ]=VerificationFun(VelUncalibratedHourly,VelObsHourly,DirUncalibratedHourly,DirObsHourly,TotalPowUncalibratedHourly,TotalPowObsHourly);

[ VerificationCalibratedDoyLeadHourly ]=VerificationFun(VelCalibratedDoyLeadHourly,VelObsHourly,DirCalibratedDoyLeadHourly,DirObsHourly,TotalPowCalibratedDoyLeadHourly,TotalPowObsHourly);

[ VerificationCalibratedDoyLeadVelHourly ]=VerificationFun(VelCalibratedDoyLeadVelHourly,VelObsHourly,DirCalibratedDoyLeadVelHourly,DirObsHourly,TotalPowCalibratedDoyLeadVelHourly,TotalPowObsHourly);

[ VerificationCalibratedDoyLeadVelDirHourly ]=VerificationFun(VelCalibratedDoyLeadVelDirHourly,VelObsHourly,DirCalibratedDoyLeadVelDirHourly,DirObsHourly,TotalPowCalibratedDoyLeadVelDirHourly,TotalPowObsHourly);

[ VerificationCalibratedLeadVelDirHourly ]=VerificationFun(VelCalibratedLeadVelDirHourly,VelObsHourly,DirCalibratedLeadVelDirHourly,DirObsHourly,TotalPowCalibratedLeadVelDirHourly,TotalPowObsHourly);

[ VerificationPersistanceHourly ]=VerificationFun(VelPersistanceHourly,VelObsHourly,DirPersistanceHourly,DirObsHourly,TotalPowPersistanceHourly,TotalPowObsHourly);

[ VerificationClimatologyHourly ]=VerificationFun(VelClimatologyHourly,VelObsHourly,DirClimatologyHourly,DirObsHourly,TotalPowClimatologyHourly,TotalPowObsHourly);

[ VerificationClimatologyPlusHourly ]=VerificationFun(VelClimatologyPlusHourly,VelObsHourly,DirClimatologyPlusHourly,DirObsHourly,TotalPowClimatologyPlusHourly,TotalPowObsHourly);

[ VerificationCalibratedPowHourly ]=VerificationFun(VelCalibratedPowHourly,VelObsHourly,DirCalibratedPowHourly,DirObsHourly,TotalPowCalibratedPowHourly,TotalPowObsHourly);

%==========================================================================
% GRAFICAMOS LOS RESULTADOS
%==========================================================================

fontsize=12;

%==========================================================================
% GRAFICO QUE COMPARA EL PRONOSTICO CALIBRADO, SIN CALIBRAR Y LAS
% OBSERVACIONES PARA UN CASO EN PARTICULAR
% usar PlotForecast para elegir que caso se quiere graficar.
%==========================================================================
horas=1:length(VerificationUncalibratedHourly.VelRmse);
%Ejemplo de performance para un pronostico.
PlotForecast=100;


hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 40 20]);

subplot(1,2,1)
hold on
plot(horas,VelCalibratedDoyLeadHourly(PlotForecast,:),'r','LineWidth',3);
plot(horas,VelCalibratedDoyLeadVelHourly(PlotForecast,:),'g','LineWidth',3);
plot(horas,VelCalibratedDoyLeadVelDirHourly(PlotForecast,:),'m','LineWidth',3);
plot(horas,VelCalibratedLeadVelDirHourly(PlotForecast,:),'y','LineWidth',3);
plot(horas,VelUncalibratedHourly(PlotForecast,:),'b','LineWidth',3);
plot(horas,VelObsHourly(PlotForecast,:),'k','LineWidth',3);

subplot(1,2,2)
hold on
plot(horas,DirCalibratedDoyLeadHourly(PlotForecast,:),'r','LineWidth',3);
plot(horas,DirCalibratedDoyLeadVelHourly(PlotForecast,:),'r','LineWidth',3);
plot(horas,DirCalibratedDoyLeadVelDirHourly(PlotForecast,:),'m','LineWidth',3);
plot(horas,DirCalibratedLeadVelDirHourly(PlotForecast,:),'y','LineWidth',3);
plot(horas,DirUncalibratedHourly(PlotForecast,:),'b','LineWidth',3);
plot(horas,DirObsHourly(PlotForecast,:),'k','LineWidth',3);

%==========================================================================
% GRAFICO QUE COMPARA LOS ERRORES DE LOS PRONOSTICOS CALIBRADOS Y SIN
% CALIBRAR PARA VELOCIDAD DE VIENTO, DIRECCION Y POTENCIA.
%==========================================================================

%==========================================================================
% VELOCIDAD DE VIENTO
%==========================================================================


hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20]);

subplot(2,2,1)
set(gca,'FontSize',fontsize);


hold on
plot(horas,VerificationUncalibratedHourly.VelRmse,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadHourly.VelRmse,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVelHourly.VelRmse,'g','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDirHourly.VelRmse,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDirHourly.VelRmse,'y','LineWidth',3)
% plot(horas,VerificationPersistanceHourly.VelRmse,'k','LineWidth',3)
plot(horas,VerificationClimatologyPlusHourly.VelRmse,'k--','LineWidth',3)
plot(horas,VerificationClimatologyHourly.VelRmse,'k:','LineWidth',3)
grid('On')
title('RMSE (a)')
axis([0 max(horas) 0 4]);

subplot(2,2,2)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibratedHourly.VelCorrelation,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadHourly.VelCorrelation,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVelHourly.VelCorrelation,'g','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDirHourly.VelCorrelation,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDirHourly.VelCorrelation,'y','LineWidth',3)
% plot(horas,VerificationPersistanceHourly.VelCorrelation,'k','LineWidth',3)
plot(horas,VerificationClimatologyPlusHourly.VelCorrelation,'k--','LineWidth',3)
plot(horas,VerificationClimatologyHourly.VelCorrelation,'k:','LineWidth',3)
grid('On')
title('CORRELACION (b)')
axis([0 max(horas) 0 1]);

subplot(2,2,3)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibratedHourly.VelBias,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadHourly.VelBias,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVelHourly.VelBias,'g','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDirHourly.VelBias,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDirHourly.VelBias,'y','LineWidth',3)
% plot(horas,VerificationPersistanceHourly.VelBias,'k','LineWidth',3)
plot(horas,VerificationClimatologyPlusHourly.VelBias,'k--','LineWidth',3)
plot(horas,VerificationClimatologyHourly.VelBias,'k:','LineWidth',3)
grid('On')
title('BIAS (c)')

axis([0 max(horas) -2 2]);

subplot(2,2,4)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibratedHourly.VelStdFor,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadHourly.VelStdFor,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVelHourly.VelStdFor,'g','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDirHourly.VelStdFor,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDirHourly.VelStdFor,'y','LineWidth',3)
plot(horas,VerificationUncalibratedHourly.VelStdObs,'k','LineWidth',3)
% plot(horas,VerificationPersistanceHourly.VelStdObs,'k','LineWidth',3)
% plot(horas,VerificationClimatologyPlusHourly.VelStdObs,'k--','LineWidth',3)
% plot(horas,VerificationClimatologyHourly.VelStdObs,'k:','LineWidth',3)
grid('On')
title('VARIABILIDAD (d)')
axis([0 max(horas) 0 4]);

print('-dpng','ErroresVelHourly.png');

%==========================================================================
% DIRECCION DEL VIENTO
%==========================================================================


hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20]);

subplot(2,2,1)
set(gca,'FontSize',fontsize);

hold on
plot(horas,VerificationUncalibratedHourly.DirRmse,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadHourly.DirRmse,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVelHourly.DirRmse,'g','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDirHourly.DirRmse,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDirHourly.DirRmse,'y','LineWidth',3)
% plot(horas,VerificationPersistanceHourly.DirRmse,'k','LineWidth',3)
plot(horas,VerificationClimatologyPlusHourly.DirRmse,'k--','LineWidth',3)
plot(horas,VerificationClimatologyHourly.DirRmse,'k:','LineWidth',3)
grid('On')
title('RMSE (a)')
axis([0 max(horas) 0 70]);

subplot(2,2,2)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibratedHourly.DirCorrelation,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadHourly.DirCorrelation,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVelHourly.DirCorrelation,'g','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDirHourly.DirCorrelation,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDirHourly.DirCorrelation,'y','LineWidth',3)
% plot(horas,VerificationPersistanceHourly.DirCorrelation,'k','LineWidth',3)
plot(horas,VerificationClimatologyPlusHourly.DirCorrelation,'k--','LineWidth',3)
plot(horas,VerificationClimatologyHourly.DirCorrelation,'k:','LineWidth',3)
grid('On')
title('CORRELACION (b)')
axis([0 max(horas) 0 1]);

subplot(2,2,3)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibratedHourly.DirBias,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadHourly.DirBias,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVelHourly.DirBias,'g','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDirHourly.DirBias,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDirHourly.DirBias,'y','LineWidth',3)
% plot(horas,VerificationPersistanceHourly.DirBias,'k','LineWidth',3)
plot(horas,VerificationClimatologyPlusHourly.DirBias,'k--','LineWidth',3)
plot(horas,VerificationClimatologyHourly.DirBias,'k:','LineWidth',3)
grid('On')
title('BIAS (c)')

axis([0 max(horas) -15 15]);

subplot(2,2,4)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibratedHourly.DirStdFor,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadHourly.DirStdFor,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVelHourly.DirStdFor,'g','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDirHourly.DirStdFor,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDirHourly.DirStdFor,'y','LineWidth',3)
plot(horas,VerificationUncalibratedHourly.DirStdObs,'k','LineWidth',3)
% plot(horas,VerificationPersistanceHourly.DirStdObs,'k','LineWidth',3)
% plot(horas,VerificationClimatologyPlusHourly.DirStdObs,'k--','LineWidth',3)
% plot(horas,VerificationClimatologyHourly.DirStdObs,'k:','LineWidth',3)
grid('On')
title('VARIABILIDAD (d)')
axis([0 max(horas) 0 150]);

print('-dpng','ErroresDirHourly.png');

%==========================================================================
% POTENCIA
%==========================================================================

hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20]);

subplot(2,2,1)
set(gca,'FontSize',fontsize);


hold on
plot(horas,VerificationUncalibratedHourly.PowRmse,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadHourly.PowRmse,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVelHourly.PowRmse,'g','LineWidth',3)
plot(horas,VerificationCalibratedPowHourly.PowRmse,'m','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDirHourly.PowRmse,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDirHourly.PowRmse,'y','LineWidth',3)
% plot(horas,VerificationPersistanceHourly.PowRmse,'k','LineWidth',3)
plot(horas,VerificationClimatologyPlusHourly.PowRmse,'k--','LineWidth',3)
plot(horas,VerificationClimatologyHourly.PowRmse,'k:','LineWidth',3)
grid('On')
title('RMSE (a)')
xlabel('Tiempo (horas)')
ylabel('Potencia (MW)')
axis([0 max(horas) 0 30000]);
%legend('Uncal','Cal');

subplot(2,2,2)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibratedHourly.PowCorrelation,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadHourly.PowCorrelation,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVelHourly.PowCorrelation,'g','LineWidth',3)
plot(horas,VerificationCalibratedPowHourly.PowCorrelation,'m','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDirHourly.PowCorrelation,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDirHourly.PowCorrelation,'y','LineWidth',3)
% plot(horas,VerificationPersistanceHourly.PowCorrelation,'k','LineWidth',3)
plot(horas,VerificationClimatologyPlusHourly.PowCorrelation,'k--','LineWidth',3)
plot(horas,VerificationClimatologyHourly.PowCorrelation,'k:','LineWidth',3)
grid('On')
title('CORRELACION (b)')
axis([0 max(horas) 0 1]);

subplot(2,2,3)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibratedHourly.PowBias,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadHourly.PowBias,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVelHourly.PowBias,'g','LineWidth',3)
plot(horas,VerificationCalibratedPowHourly.PowBias,'m','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDirHourly.PowBias,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDirHourly.PowBias,'y','LineWidth',3)
% plot(horas,VerificationPersistanceHourly.PowBias,'k','LineWidth',3)
plot(horas,VerificationClimatologyPlusHourly.PowBias,'k--','LineWidth',3)
plot(horas,VerificationClimatologyHourly.PowBias,'k:','LineWidth',3)
xlabel('Tiempo (horas)')
ylabel('Potencia (MW)')
grid('On')
title('BIAS (c)')

axis([0 max(horas) -20000 20000]);

subplot(2,2,4)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibratedHourly.PowStdFor,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadHourly.PowStdFor,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVelHourly.PowStdFor,'g','LineWidth',3)
plot(horas,VerificationCalibratedPowHourly.PowStdFor,'m','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDirHourly.PowStdFor,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDirHourly.PowStdFor,'y','LineWidth',3)
plot(horas,VerificationUncalibratedHourly.PowStdObs,'k','LineWidth',3)
% plot(horas,VerificationPersistanceHourly.PowStdObs,'k','LineWidth',3)
% plot(horas,VerificationClimatologyPlusHourly.PowStdObs,'k--','LineWidth',3)
% plot(horas,VerificationClimatologyHourly.PowStdObs,'k:','LineWidth',3)
xlabel('Tiempo (horas)')
ylabel('Potencia (MW)')
grid('On')
title('VARIABILIDAD (d)')
axis([0 max(horas) 0 30000]);

print('-dpng','ErroresPowHourly.png');



%==========================================================================
% ERROR EN POTENCIA COMO FUNCION DE LA POTENCIA
%==========================================================================

%Uncalibrated
hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 40 20]);

subplot(1,2,1)
set(gca,'FontSize',fontsize)
pcolor(horas,VerificationUncalibratedHourly.PowConditionalPow,VerificationUncalibratedHourly.PowConditionalRmse);shading flat
caxis([ 0 40000]);
colorbar
title('RMSE (a)')

subplot(1,2,2)
set(gca,'FontSize',fontsize)
pcolor(horas,VerificationUncalibratedHourly.PowConditionalPow,VerificationUncalibratedHourly.PowConditionalBias);shading flat
caxis([ -30000 30000]);
colorbar
title('BIAS (b)')
print('-dpng','ErroresPowConditionalUncalibratedHourly.png');

%Calibrated DoyLead
hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 40 20]);

subplot(1,2,1)
set(gca,'FontSize',fontsize)
pcolor(horas,VerificationCalibratedDoyLeadHourly.PowConditionalPow,VerificationCalibratedDoyLeadHourly.PowConditionalRmse);shading flat
caxis([ 0 40000]);
colorbar
title('RMSE (a)')

subplot(1,2,2)
set(gca,'FontSize',fontsize)
pcolor(horas,VerificationCalibratedDoyLeadHourly.PowConditionalPow,VerificationCalibratedDoyLeadHourly.PowConditionalBias);shading flat
caxis([ -30000 30000]);
colorbar
title('BIAS (b)')
print('-dpng','ErroresPowConditionalCalibratedDoyLeadHourly.png');

%Calibrated DoyLeadVel
hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 40 20]);

subplot(1,2,1)
set(gca,'FontSize',fontsize)
pcolor(horas,VerificationCalibratedDoyLeadVelHourly.PowConditionalPow,VerificationCalibratedDoyLeadVelHourly.PowConditionalRmse);shading flat
caxis([ 0 40000]);
colorbar
title('RMSE (a)')

subplot(1,2,2)
set(gca,'FontSize',fontsize)
pcolor(horas,VerificationCalibratedDoyLeadVelHourly.PowConditionalPow,VerificationCalibratedDoyLeadVelHourly.PowConditionalBias);shading flat
caxis([ -30000 30000]);
colorbar
title('BIAS (b)')
print('-dpng','ErroresPowConditionalCalibratedDoyLeadVelHourly.png');

%Todos
hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 40 20]);

subplot(1,2,1)
set(gca,'FontSize',fontsize)
hold on
plot(VerificationUncalibratedHourly.PowConditionalPow,nanmean(VerificationUncalibratedHourly.PowConditionalRmse,2),'b','LineWidth',2);
plot(VerificationCalibratedDoyLeadHourly.PowConditionalPow,nanmean(VerificationCalibratedDoyLeadHourly.PowConditionalRmse,2),'r','LineWidth',2);
plot(VerificationCalibratedDoyLeadVelHourly.PowConditionalPow,nanmean(VerificationCalibratedDoyLeadVelHourly.PowConditionalRmse,2),'g','LineWidth',2);
xlabel('Potencia (MW)')
ylabel('RMSE (MW)')
axis([0 max( VerificationUncalibratedHourly.PowConditionalPow ) 0 30000 ])
grid on

title('RMSE (a)')

subplot(1,2,2)
set(gca,'FontSize',fontsize)
hold on
plot(VerificationUncalibratedHourly.PowConditionalPow,nanmean(VerificationUncalibratedHourly.PowConditionalBias,2),'b','LineWidth',2);
plot(VerificationCalibratedDoyLeadHourly.PowConditionalPow,nanmean(VerificationCalibratedDoyLeadHourly.PowConditionalBias,2),'r','LineWidth',2);
plot(VerificationCalibratedDoyLeadVelHourly.PowConditionalPow,nanmean(VerificationCalibratedDoyLeadVelHourly.PowConditionalBias,2),'g','LineWidth',2);
xlabel('Potencia (MW)')
ylabel('BIAS (MW)')
axis([0 max( VerificationUncalibratedHourly.PowConditionalPow ) -20000 20000 ])
grid on

title('BIAS (b)')
print('-dpng','ErroresPowConditionalHourly.png');



%========================================================================================================
% HAGO LA VERIFICACION PARA LOS PRONOSTICOS ACUMULADOS EN PLAZOS DE 12 HORAS.
%========================================================================================================

[ VelObs12Hourly ] = AccumulateDataFun( VelObs , 6*12 , 'Mean' );
[ DirObs12Hourly ] = AccumulateDataFun( DirObs , 6*12 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowObs12Hourly ] = AccumulateDataFun( TotalPowObs , 6*12 , 'Mean' );

[ VelUncalibrated12Hourly ] = AccumulateDataFun( VelUncalibrated , 6*12 , 'Mean' );
[ DirUncalibrated12Hourly ] = AccumulateDataFun( DirUncalibrated , 6*12 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowUncalibrated12Hourly ] = AccumulateDataFun( TotalPowUncalibrated , 6*12 , 'Mean' );

[ VelCalibratedDoyLead12Hourly ] = AccumulateDataFun( VelCalibratedDoyLead , 6*12 , 'Mean' );
[ DirCalibratedDoyLead12Hourly ] = AccumulateDataFun( DirCalibratedDoyLead , 6*12 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowCalibratedDoyLead12Hourly ] = AccumulateDataFun( TotalPowCalibratedDoyLead , 6*12 , 'Mean' );

[ VelCalibratedDoyLeadVel12Hourly ] = AccumulateDataFun( VelCalibratedDoyLeadVel , 6*12 , 'Mean' );
[ DirCalibratedDoyLeadVel12Hourly ] = AccumulateDataFun( DirCalibratedDoyLeadVel , 6*12 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowCalibratedDoyLeadVel12Hourly ] = AccumulateDataFun( TotalPowCalibratedDoyLeadVel , 6*12 , 'Mean' );

[ VelCalibratedDoyLeadVelDir12Hourly ] = AccumulateDataFun( VelCalibratedDoyLeadVelDir , 6*12 , 'Mean' );
[ DirCalibratedDoyLeadVelDir12Hourly ] = AccumulateDataFun( DirCalibratedDoyLeadVelDir , 6*12 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowCalibratedDoyLeadVelDir12Hourly ] = AccumulateDataFun( TotalPowCalibratedDoyLeadVelDir , 6*12 , 'Mean' );

[ VelCalibratedLeadVelDir12Hourly ] = AccumulateDataFun( VelCalibratedLeadVelDir , 6*12 , 'Mean' );
[ DirCalibratedLeadVelDir12Hourly ] = AccumulateDataFun( DirCalibratedLeadVelDir , 6*12 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowCalibratedLeadVelDir12Hourly ] = AccumulateDataFun( TotalPowCalibratedLeadVelDir , 6*12 , 'Mean' );

[ VelPersistance12Hourly ] = AccumulateDataFun( VelPersistance , 6*12 , 'Mean' );
[ DirPersistance12Hourly ] = AccumulateDataFun( DirPersistance , 6*12 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowPersistance12Hourly ] = AccumulateDataFun( TotalPowPersistance , 6*12 , 'Mean' );

[ VelClimatology12Hourly ] = AccumulateDataFun( VelClimatology , 6*12 , 'Mean' );
[ DirClimatology12Hourly ] = AccumulateDataFun( DirClimatology , 6*12 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowClimatology12Hourly ] = AccumulateDataFun( TotalPowClimatology , 6*12 , 'Mean' );

[ VelClimatologyPlus12Hourly ] = AccumulateDataFun( VelClimatologyPlus , 6*12 , 'Mean' );
[ DirClimatologyPlus12Hourly ] = AccumulateDataFun( DirClimatologyPlus , 6*12 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowClimatologyPlus12Hourly ] = AccumulateDataFun( TotalPowClimatologyPlus , 6*12 , 'Mean' );

[ VelCalibratedPow12Hourly ] = AccumulateDataFun( VelCalibratedPow , 6*12, 'Mean' );
[ DirCalibratedPow12Hourly ] = AccumulateDataFun( DirCalibratedPow , 6*12 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowCalibratedPow12Hourly ] = AccumulateDataFun( TotalPowCalibratedPow , 6*12 , 'Mean' );

%==========================================================================
% VERIFICACION DE LOS PRONOSTICOS
% calculo del error cuadratico medio (rmse), error sistematico (bias) y
% coeficiente de correlacion para la intensidad de viento, direccion y
% potencia total producida por el parque. 
%==========================================================================

[ VerificationUncalibrated12Hourly ]=VerificationFun(VelUncalibrated12Hourly,VelObs12Hourly,DirUncalibrated12Hourly,DirObs12Hourly,TotalPowUncalibrated12Hourly,TotalPowObs12Hourly);

[ VerificationCalibratedDoyLead12Hourly ]=VerificationFun(VelCalibratedDoyLead12Hourly,VelObs12Hourly,DirCalibratedDoyLead12Hourly,DirObs12Hourly,TotalPowCalibratedDoyLead12Hourly,TotalPowObs12Hourly);

[ VerificationCalibratedDoyLeadVel12Hourly ]=VerificationFun(VelCalibratedDoyLeadVel12Hourly,VelObs12Hourly,DirCalibratedDoyLeadVel12Hourly,DirObs12Hourly,TotalPowCalibratedDoyLeadVel12Hourly,TotalPowObs12Hourly);

[ VerificationCalibratedDoyLeadVelDir12Hourly ]=VerificationFun(VelCalibratedDoyLeadVelDir12Hourly,VelObs12Hourly,DirCalibratedDoyLeadVelDir12Hourly,DirObs12Hourly,TotalPowCalibratedDoyLeadVelDir12Hourly,TotalPowObs12Hourly);

[ VerificationCalibratedLeadVelDir12Hourly ]=VerificationFun(VelCalibratedLeadVelDir12Hourly,VelObs12Hourly,DirCalibratedLeadVelDir12Hourly,DirObs12Hourly,TotalPowCalibratedLeadVelDir12Hourly,TotalPowObs12Hourly);

[ VerificationPersistance12Hourly ]=VerificationFun(VelPersistance12Hourly,VelObs12Hourly,DirPersistance12Hourly,DirObs12Hourly,TotalPowPersistance12Hourly,TotalPowObs12Hourly);

[ VerificationClimatology12Hourly ]=VerificationFun(VelClimatology12Hourly,VelObs12Hourly,DirClimatology12Hourly,DirObs12Hourly,TotalPowClimatology12Hourly,TotalPowObs12Hourly);

[ VerificationClimatologyPlus12Hourly ]=VerificationFun(VelClimatologyPlus12Hourly,VelObs12Hourly,DirClimatologyPlus12Hourly,DirObs12Hourly,TotalPowClimatologyPlus12Hourly,TotalPowObs12Hourly);

[ VerificationCalibratedPow12Hourly ]=VerificationFun(VelCalibratedPow12Hourly,VelObs12Hourly,DirCalibratedPow12Hourly,DirObs12Hourly,TotalPowCalibratedPow12Hourly,TotalPowObs12Hourly);

%==========================================================================
% GRAFICAMOS LOS RESULTADOS
%==========================================================================

fontsize=12;

%==========================================================================
% GRAFICO QUE COMPARA EL PRONOSTICO CALIBRADO, SIN CALIBRAR Y LAS
% OBSERVACIONES PARA UN CASO EN PARTICULAR
% usar PlotForecast para elegir que caso se quiere graficar.
%==========================================================================
horas=[1:length(VerificationUncalibrated12Hourly.VelRmse)]*12;
%Ejemplo de performance para un pronostico.
PlotForecast=100;


hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 40 20]);

subplot(1,2,1)
hold on
plot(horas,VelCalibratedDoyLead12Hourly(PlotForecast,:),'r','LineWidth',3);
plot(horas,VelCalibratedDoyLeadVel12Hourly(PlotForecast,:),'g','LineWidth',3);
plot(horas,VelCalibratedDoyLeadVelDir12Hourly(PlotForecast,:),'m','LineWidth',3);
plot(horas,VelCalibratedLeadVelDir12Hourly(PlotForecast,:),'y','LineWidth',3);
plot(horas,VelUncalibrated12Hourly(PlotForecast,:),'b','LineWidth',3);
plot(horas,VelObs12Hourly(PlotForecast,:),'k','LineWidth',3);

subplot(1,2,2)
hold on
plot(horas,DirCalibratedDoyLead12Hourly(PlotForecast,:),'r','LineWidth',3);
plot(horas,DirCalibratedDoyLeadVel12Hourly(PlotForecast,:),'r','LineWidth',3);
plot(horas,DirCalibratedDoyLeadVelDir12Hourly(PlotForecast,:),'m','LineWidth',3);
plot(horas,DirCalibratedLeadVelDir12Hourly(PlotForecast,:),'y','LineWidth',3);
plot(horas,DirUncalibrated12Hourly(PlotForecast,:),'b','LineWidth',3);
plot(horas,DirObs12Hourly(PlotForecast,:),'k','LineWidth',3);

%==========================================================================
% GRAFICO QUE COMPARA LOS ERRORES DE LOS PRONOSTICOS CALIBRADOS Y SIN
% CALIBRAR PARA VELOCIDAD DE VIENTO, DIRECCION Y POTENCIA.
%==========================================================================

%==========================================================================
% VELOCIDAD DE VIENTO
%==========================================================================


hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20]);

subplot(2,2,1)
set(gca,'FontSize',fontsize);


hold on
plot(horas,VerificationUncalibrated12Hourly.VelRmse,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLead12Hourly.VelRmse,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVel12Hourly.VelRmse,'g','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDir12Hourly.VelRmse,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDir12Hourly.VelRmse,'y','LineWidth',3)
% plot(horas,VerificationPersistance12Hourly.VelRmse,'k','LineWidth',3)
plot(horas,VerificationClimatologyPlus12Hourly.VelRmse,'k--','LineWidth',3)
plot(horas,VerificationClimatology12Hourly.VelRmse,'k:','LineWidth',3)
grid('On')
title('RMSE (a)')
axis([0 max(horas) 0 4]);

subplot(2,2,2)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated12Hourly.VelCorrelation,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLead12Hourly.VelCorrelation,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVel12Hourly.VelCorrelation,'g','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDir12Hourly.VelCorrelation,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDir12Hourly.VelCorrelation,'y','LineWidth',3)
% plot(horas,VerificationPersistance12Hourly.VelCorrelation,'k','LineWidth',3)
plot(horas,VerificationClimatologyPlus12Hourly.VelCorrelation,'k--','LineWidth',3)
plot(horas,VerificationClimatology12Hourly.VelCorrelation,'k:','LineWidth',3)
grid('On')
title('CORRELACION (b)')
axis([0 max(horas) 0 1]);

subplot(2,2,3)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated12Hourly.VelBias,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLead12Hourly.VelBias,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVel12Hourly.VelBias,'g','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDir12Hourly.VelBias,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDir12Hourly.VelBias,'y','LineWidth',3)
% plot(horas,VerificationPersistance12Hourly.VelBias,'k','LineWidth',3)
plot(horas,VerificationClimatologyPlus12Hourly.VelBias,'k--','LineWidth',3)
plot(horas,VerificationClimatology12Hourly.VelBias,'k:','LineWidth',3)
grid('On')

title('BIAS (c)')

axis([0 max(horas) -2 2]);

subplot(2,2,4)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated12Hourly.VelStdFor,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLead12Hourly.VelStdFor,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVel12Hourly.VelStdFor,'g','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDir12Hourly.VelStdFor,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDir12Hourly.VelStdFor,'y','LineWidth',3)
plot(horas,VerificationUncalibrated12Hourly.VelStdObs,'k','LineWidth',3)
% plot(horas,VerificationPersistance12Hourly.VelStdObs,'k','LineWidth',3)
% plot(horas,VerificationClimatologyPlus12Hourly.VelStdObs,'k--','LineWidth',3)
% plot(horas,VerificationClimatology12Hourly.VelStdObs,'k:','LineWidth',3)
grid('On')
title('VARIABILIDAD (d)')
axis([0 max(horas) 0 4]);

print('-dpng','ErroresVel12Hourly.png');

%==========================================================================
% DIRECCION DEL VIENTO
%==========================================================================


hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20]);

subplot(2,2,1)
set(gca,'FontSize',fontsize);

hold on
plot(horas,VerificationUncalibrated12Hourly.DirRmse,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLead12Hourly.DirRmse,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVel12Hourly.DirRmse,'g','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDir12Hourly.DirRmse,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDir12Hourly.DirRmse,'y','LineWidth',3)
% plot(horas,VerificationPersistance12Hourly.DirRmse,'k','LineWidth',3)
plot(horas,VerificationClimatologyPlus12Hourly.DirRmse,'k--','LineWidth',3)
plot(horas,VerificationClimatology12Hourly.DirRmse,'k:','LineWidth',3)
grid('On')
title('RMSE (a)')
axis([0 max(horas) 0 70]);


subplot(2,2,2)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated12Hourly.DirCorrelation,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLead12Hourly.DirCorrelation,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVel12Hourly.DirCorrelation,'g','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDir12Hourly.DirCorrelation,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDir12Hourly.DirCorrelation,'y','LineWidth',3)
% plot(horas,VerificationPersistance12Hourly.DirCorrelation,'k','LineWidth',3)
plot(horas,VerificationClimatologyPlus12Hourly.DirCorrelation,'k--','LineWidth',3)
plot(horas,VerificationClimatology12Hourly.DirCorrelation,'k:','LineWidth',3)
grid('On')
title('CORRELACION (b)')
axis([0 max(horas) 0 1]);

subplot(2,2,3)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated12Hourly.DirBias,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLead12Hourly.DirBias,'r','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVel12Hourly.DirBias,'g','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDir12Hourly.DirBias,'m','LineWidth',3)
plot(horas,VerificationCalibratedLeadVelDir12Hourly.DirBias,'y','LineWidth',3)
% plot(horas,VerificationPersistance12Hourly.DirBias,'k','LineWidth',3)
plot(horas,VerificationClimatologyPlus12Hourly.DirBias,'k--','LineWidth',3)
plot(horas,VerificationClimatology12Hourly.DirBias,'k:','LineWidth',3)
grid('On')
title('BIAS (c)')

axis([0 max(horas) -15 15]);

subplot(2,2,4)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated12Hourly.DirStdFor,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLead12Hourly.DirStdFor,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVel12Hourly.DirStdFor,'g','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDir12Hourly.DirStdFor,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDir12Hourly.DirStdFor,'y','LineWidth',3)
plot(horas,VerificationUncalibrated12Hourly.DirStdObs,'k','LineWidth',3)
% plot(horas,VerificationPersistance12Hourly.DirStdObs,'k','LineWidth',3)
% plot(horas,VerificationClimatologyPlus12Hourly.DirStdObs,'k--','LineWidth',3)
% plot(horas,VerificationClimatology12Hourly.DirStdObs,'k:','LineWidth',3)
grid('On')
title('VARIABILIDAD (d)')
axis([0 max(horas) 0 150]);

print('-dpng','ErroresDir12Hourly.png');

%==========================================================================
% POTENCIA
%==========================================================================

hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20]);

subplot(2,2,1)
set(gca,'FontSize',fontsize);


hold on
plot(horas,VerificationUncalibrated12Hourly.PowRmse,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLead12Hourly.PowRmse,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVel12Hourly.PowRmse,'g','LineWidth',3)
plot(horas,VerificationCalibratedPow12Hourly.PowRmse,'m','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDir12Hourly.PowRmse,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDir12Hourly.PowRmse,'y','LineWidth',3)
% plot(horas,VerificationPersistance12Hourly.PowRmse,'k','LineWidth',3)
plot(horas,VerificationClimatologyPlus12Hourly.PowRmse,'k--','LineWidth',3)
plot(horas,VerificationClimatology12Hourly.PowRmse,'k:','LineWidth',3)
grid('On')
title('RMSE (a)')
xlabel('Tiempo (horas)')
ylabel('Potencia (MW)')
axis([0 max(horas) 0 30000]);
%legend('Uncal','Cal');

subplot(2,2,2)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated12Hourly.PowCorrelation,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLead12Hourly.PowCorrelation,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVel12Hourly.PowCorrelation,'g','LineWidth',3)
plot(horas,VerificationCalibratedPow12Hourly.PowCorrelation,'m','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDir12Hourly.PowCorrelation,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDir12Hourly.PowCorrelation,'y','LineWidth',3)
% plot(horas,VerificationPersistance12Hourly.PowCorrelation,'k','LineWidth',3)
plot(horas,VerificationClimatologyPlus12Hourly.PowCorrelation,'k--','LineWidth',3)
plot(horas,VerificationClimatology12Hourly.PowCorrelation,'k:','LineWidth',3)
grid('On')
title('CORRELACION (b)')
axis([0 max(horas) 0 1]);

subplot(2,2,3)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated12Hourly.PowBias,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLead12Hourly.PowBias,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVel12Hourly.PowBias,'g','LineWidth',3)
plot(horas,VerificationCalibratedPow12Hourly.PowBias,'m','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDir12Hourly.PowBias,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDir12Hourly.PowBias,'y','LineWidth',3)
% plot(horas,VerificationPersistance12Hourly.PowBias,'k','LineWidth',3)
plot(horas,VerificationClimatologyPlus12Hourly.PowBias,'k--','LineWidth',3)
plot(horas,VerificationClimatology12Hourly.PowBias,'k:','LineWidth',3)
xlabel('Tiempo (horas)')
ylabel('Potencia (MW)')
grid('On')
title('BIAS (c)')

axis([0 max(horas) -20000 20000]);

subplot(2,2,4)
set(gca,'FontSize',fontsize);
hold on
plot(horas,VerificationUncalibrated12Hourly.PowStdFor,'b','LineWidth',3)
plot(horas,VerificationCalibratedDoyLead12Hourly.PowStdFor,'r','LineWidth',3)
plot(horas,VerificationCalibratedDoyLeadVel12Hourly.PowStdFor,'g','LineWidth',3)
plot(horas,VerificationCalibratedPow12Hourly.PowStdFor,'m','LineWidth',3)
% plot(horas,VerificationCalibratedDoyLeadVelDir12Hourly.PowStdFor,'m','LineWidth',3)
% plot(horas,VerificationCalibratedLeadVelDir12Hourly.PowStdFor,'y','LineWidth',3)
plot(horas,VerificationUncalibrated12Hourly.PowStdObs,'k','LineWidth',3)
% plot(horas,VerificationPersistance12Hourly.PowStdObs,'k','LineWidth',3)
% plot(horas,VerificationClimatologyPlus12Hourly.PowStdObs,'k--','LineWidth',3)
% plot(horas,VerificationClimatology12Hourly.PowStdObs,'k:','LineWidth',3)
xlabel('Tiempo (horas)')
ylabel('Potencia (MW)')
grid('On')
title('VARIABILIDAD (d)')
axis([0 max(horas) 0 30000]);

print('-dpng','ErroresPow12Hourly.png');


%==========================================================================
% ERROR EN POTENCIA COMO FUNCION DE LA POTENCIA
%==========================================================================

%Uncalibrated
hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 40 20]);

subplot(1,2,1)
set(gca,'FontSize',fontsize)
pcolor(horas,VerificationUncalibrated12Hourly.PowConditionalPow,VerificationUncalibrated12Hourly.PowConditionalRmse);shading flat
caxis([ 0 40000]);
colorbar
title('RMSE (a)')

subplot(1,2,2)
set(gca,'FontSize',fontsize)
pcolor(horas,VerificationUncalibrated12Hourly.PowConditionalPow,VerificationUncalibrated12Hourly.PowConditionalBias);shading flat
caxis([ -30000 30000]);
colorbar
title('BIAS (b)')
print('-dpng','ErroresPowConditionalUncalibrated12Hourly.png');

%Calibrated DoyLead
hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 40 20]);

subplot(1,2,1)
set(gca,'FontSize',fontsize)
pcolor(horas,VerificationCalibratedDoyLead12Hourly.PowConditionalPow,VerificationCalibratedDoyLead12Hourly.PowConditionalRmse);shading flat
caxis([ 0 40000]);
colorbar
title('RMSE (a)')

subplot(1,2,2)
set(gca,'FontSize',fontsize)
pcolor(horas,VerificationCalibratedDoyLead12Hourly.PowConditionalPow,VerificationCalibratedDoyLead12Hourly.PowConditionalBias);shading flat
caxis([ -30000 30000]);
colorbar
title('BIAS (b)')
print('-dpng','ErroresPowConditionalCalibratedDoyLead12Hourly.png');

%Calibrated DoyLeadVel
hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 40 20]);

subplot(1,2,1)
set(gca,'FontSize',fontsize)
pcolor(horas,VerificationCalibratedDoyLeadVel12Hourly.PowConditionalPow,VerificationCalibratedDoyLeadVel12Hourly.PowConditionalRmse);shading flat
caxis([ 0 40000]);
colorbar
title('RMSE (a)')

subplot(1,2,2)
set(gca,'FontSize',fontsize)
pcolor(horas,VerificationCalibratedDoyLeadVe12lHourly.PowConditionalPow,VerificationCalibratedDoyLeadVel12Hourly.PowConditionalBias);shading flat
caxis([ -30000 30000]);
colorbar
title('BIAS (b)')
print('-dpng','ErroresPowConditionalCalibratedDoyLeadVel12Hourly.png');

%Todos
hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 40 20]);

subplot(1,2,1)
set(gca,'FontSize',fontsize)
hold on
plot(VerificationUncalibrated12Hourly.PowConditionalPow,nanmean(VerificationUncalibrated12Hourly.PowConditionalRmse,2),'b','LineWidth',2);
plot(VerificationCalibratedDoyLead12Hourly.PowConditionalPow,nanmean(VerificationCalibratedDoyLead12Hourly.PowConditionalRmse,2),'r','LineWidth',2);
plot(VerificationCalibratedDoyLeadVel12Hourly.PowConditionalPow,nanmean(VerificationCalibratedDoyLeadVel12Hourly.PowConditionalRmse,2),'g','LineWidth',2);
xlabel('Potencia (MW)')
ylabel('RMSE (MW)')
axis([0 max( VerificationUncalibrated12Hourly.PowConditionalPow ) 0 30000 ])
grid on

title('RMSE (a)')

subplot(1,2,2)
set(gca,'FontSize',fontsize)
hold on
plot(VerificationUncalibrated12Hourly.PowConditionalPow,nanmean(VerificationUncalibrated12Hourly.PowConditionalBias,2),'b','LineWidth',2);
plot(VerificationCalibratedDoyLead12Hourly.PowConditionalPow,nanmean(VerificationCalibratedDoyLead12Hourly.PowConditionalBias,2),'r','LineWidth',2);
plot(VerificationCalibratedDoyLeadVel12Hourly.PowConditionalPow,nanmean(VerificationCalibratedDoyLeadVel12Hourly.PowConditionalBias,2),'g','LineWidth',2);
xlabel('Potencia (MW)')
ylabel('BIAS (MW)')
axis([0 max( VerificationUncalibrated12Hourly.PowConditionalPow ) -20000 20000 ])
grid on

title('BIAS (b)')
print('-dpng','ErroresPowConditional12Hourly.png');




%==========================================================================
% GUARDO LOS RESULTADOS
%==========================================================================

save('CalibrationOutput.mat');
