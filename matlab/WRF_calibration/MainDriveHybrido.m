clear all
close all

%==========================================================================
% Este script realiza una verificacion "Offline" de los pronosticos hybridos de
% viento y potencia y ademas compara los resultados con los obtenidos a
% partir de un pronostico calibrado estadisticamente.
%==========================================================================

%Cargo la base de datos de pronosticos calibrados y de observaciones.

load('CalibrationOutput.mat');

%Cargamos el archivo que contiene la estructura turbines en donde esta
%contenido la info de la relacion entre el viento medio del parque y la
%potencia producida por cada aerogenerador. 

%Obtenemos la cantidad total de pronosticos que hay en la base de datos.
NFor=size(VelObs,1);
NLead=size(VelObs,2);

%Seteo los molinos como si estuvieran funcionando al 100 % todos.
% 
MaskTurbine=ones(length(Parque.Turbine),1);

Order=4; %Numero de observaciones que vamos a usar en el hybrido.
Lead=6;  %En que plazo asumo que tengo disponible la ultima observacion.

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
CVelFor=VelCalibratedDoyLeadVel(CurrentForecast,:);
CDirFor=DirCalibratedDoyLeadVel(CurrentForecast,:);
CPowFor=TotalPowCalibratedDoyLeadVel(CurrentForecast,:);



CVelObs=VelObs(CurrentForecast,:);
CDirObs=DirObs(CurrentForecast,:);
CPowObs=TotalPowObs(CurrentForecast,:);

CVelObs(Lead+1:end)=NaN; %Asumo que las observaciones solo las tengo hasta el plazo Lead, luego son faltantes.
CPowObs(Lead+1:end)=NaN;

CDate=Val.datetotal(CurrentForecast,:);

%Renombro el conjunto total de datos de vel, dir observada y pronosticada.
%Esto se hace porque el pronostico actual (CurrentForecast) lo tengo que
%sacar de la base de datos de entrenamiento. 

VelForDb=VelCalibratedDoyLeadVel;
VelObsDb=VelObs;
DirForDb=DirCalibratedDoyLeadVel;
DirObsDb=DirObs;
PowForDb=TotalPowCalibratedDoyLeadVel;
PowObsDb=TotalPowObs;

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

[ VelCalibratedHybrid(CurrentForecast,:) RegressionCoefficients ]=HybridForecastFun( CVelFor , CVelObs , CDate , VelForDb , VelObsDb , DatesDb , Order  );
DirCalibratedHybrid(CurrentForecast,:)=CDirFor;

% [ VelCalibratedHybridError(CurrentForecast,:) RegressionCoefficients ]=HybridForecastErrorFun( CVelFor , CVelObs , CDate , VelForDb , VelObsDb , DatesDb , Order  );
% DirCalibratedHybridError(CurrentForecast,:)=CDirFor;

[ TotalPowCalibratedHybridPow(CurrentForecast,:) PowRegressionCoefficients ]=HybridPowForecastFun( CPowFor , CPowObs , CDate , PowForDb , PowObsDb , DatesDb , Order  );
VelCalibratedHybridPow(CurrentForecast,:)=CDirFor;
DirCalibratedHybridPow(CurrentForecast,:)=CVelFor;

[ TotalPowObsPow(CurrentForecast,:) ObsPowRegressionCoefficients ]=ObsPowForecastFun( CPowObs , CDate , PowObsDb , DatesDb , Order  );
VelObsPow(CurrentForecast,:)=CDirFor;
DirObsPow(CurrentForecast,:)=CVelFor;

[ VelObsVel(CurrentForecast,:) ObsVelRegressionCoefficients ]=ObsVelForecastFun( CVelObs , CDate , VelObsDb , DatesDb , Order  );
%DirObsVel(CurrentForecast,:)=CDirObs(1); %Para el pronostico autoregresivo de velocidad asumo que la direccion se basa en la persistencia. 


end

DirObsVel=repmat( DirObs(:,1),[ 1 NLead ]);

[ PowCalibratedHybrid TotalPowCalibratedHybrid ]=VelDirToPower(VelCalibratedHybrid,DirCalibratedHybrid,Parque,MaskTurbine,'Parametric');

[ PowObsVel TotalPowObsVel ]=VelDirToPower(VelObsVel,DirObsVel,Parque,MaskTurbine,'Parametric');

% [ PowCalibratedHybridError TotalPowCalibratedHybridError ]=VelDirToPower(VelCalibratedHybridError,DirCalibratedHybridError,Parque,MaskTurbine,'Parametric');

%==========================================================================
% VERIFICACION DE LOS PRONOSTICOS
% calculo del error cuadratico medio (rmse), error sistematico (bias) y
% coeficiente de correlacion para la intensidad de viento, direccion y
% potencia total producida por el parque. 
%==========================================================================


[ VerificationCalibratedHybrid ]=VerificationFun(VelCalibratedHybrid,VelObs,DirCalibratedHybrid,DirObs,TotalPowCalibratedHybrid,TotalPowObs);

[ VerificationCalibratedHybridError ]=VerificationFun(VelCalibratedHybridError,VelObs,DirCalibratedHybridError,DirObs,TotalPowCalibratedHybridError,TotalPowObs);

[ VerificationCalibratedHybridPow ]=VerificationFun(VelCalibratedHybridPow,VelObs,DirCalibratedHybridPow,DirObs,TotalPowCalibratedHybridPow,TotalPowObs);

[ VerificationObsPow ]=VerificationFun(VelObsPow,VelObs,DirObsPow,DirObs,TotalPowObsPow,TotalPowObs);

[ VerificationObsVel ]=VerificationFun(VelObsVel,VelObs,DirObsVel,DirObs,TotalPowObsVel,TotalPowObs);


%==========================================================================
% GRAFICO QUE COMPARA LOS ERRORES DE LOS PRONOSTICOS CALIBRADOS Y SIN
% CALIBRAR PARA VELOCIDAD DE VIENTO, DIRECCION Y POTENCIA.
%==========================================================================

MaxLead=100;
%==========================================================================
% VELOCIDAD DE VIENTO
%==========================================================================

horas=[1:size(Val.WRFDir,2)]*10/60;

hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20]);

subplot(2,2,1)
set(gca,'FontSize',fontsize);
Lead=6;


hold on
plot(horas(Lead:MaxLead),VerificationUncalibrated.VelRmse(Lead:MaxLead),'b','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedDoyLeadVel.VelRmse(Lead:MaxLead),'r','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybrid.VelRmse(Lead:MaxLead),'g','LineWidth',3)
%plot(horas,VerificationCalibratedHybridError.VelRmse,'m','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsVel.VelRmse(Lead:MaxLead),'k-','LineWidth',3)
grid('On')
title('RMSE (a)')
axis([0 max(horas(Lead:MaxLead)) 0 5]);

subplot(2,2,2)
set(gca,'FontSize',fontsize);
hold on
plot(horas(Lead:MaxLead),VerificationUncalibrated.VelCorrelation(Lead:MaxLead),'b','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedDoyLeadVel.VelCorrelation(Lead:MaxLead),'r','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybrid.VelCorrelation(Lead:MaxLead),'g','LineWidth',3)
%plot(horas,VerificationCalibratedHybridError.VelCorrelation,'m','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsVel.VelCorrelation(Lead:MaxLead),'k','LineWidth',3)
grid('On')
title('CORRELACION (b)')
axis([0 max(horas(Lead:MaxLead)) 0 1]);

subplot(2,2,3)
set(gca,'FontSize',fontsize);
hold on
plot(horas(Lead:MaxLead),VerificationUncalibrated.VelBias(Lead:MaxLead),'b','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedDoyLeadVel.VelBias(Lead:MaxLead),'r','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybrid.VelBias(Lead:MaxLead),'g','LineWidth',3)
%plot(horas,VerificationCalibratedHybridError.VelBias,'m','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsVel.VelBias(Lead:MaxLead),'k','LineWidth',3)
grid('On')
title('BIAS (c)')

axis([0 max(horas(Lead:MaxLead)) -2 2]);

subplot(2,2,4)
set(gca,'FontSize',fontsize);
hold on
plot(horas(Lead:MaxLead),VerificationUncalibrated.VelStdFor(Lead:MaxLead),'b','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedDoyLeadVel.VelStdFor(Lead:MaxLead),'r','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybrid.VelStdFor(Lead:MaxLead),'g','LineWidth',3)
%plot(horas,VerificationCalibratedHybridError.VelStdFor,'m','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsVel.VelStdFor(Lead:MaxLead),'k','LineWidth',3)
grid('On')
title('VARIABILIDAD (d)')
axis([0 max(horas(Lead:MaxLead)) 0 4]);

print('-dpng','ErroresHybridVel.png');

%==========================================================================
% DIRECCION DEL VIENTO
%==========================================================================


hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20]);

subplot(2,2,1)
set(gca,'FontSize',fontsize);
horas=[1:size(Val.WRFDir,2)]*10/60;

hold on
plot(horas(Lead:MaxLead),VerificationUncalibrated.DirRmse(Lead:MaxLead),'b','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedDoyLeadVel.DirRmse(Lead:MaxLead),'r','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybrid.DirRmse(Lead:MaxLead),'g','LineWidth',3)
%plot(horas,VerificationCalibratedHybridError.DirRmse,'m','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsVel.DirRmse(Lead:MaxLead),'k','LineWidth',3)
grid('On')
title('RMSE (a)')
axis([0 max(horas(Lead:MaxLead)) 0 70]);
legend('Uncal','Cal');

subplot(2,2,2)
set(gca,'FontSize',fontsize);
hold on
plot(horas(Lead:MaxLead),VerificationUncalibrated.DirCorrelation(Lead:MaxLead),'b','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedDoyLeadVel.DirCorrelation(Lead:MaxLead),'r','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybrid.DirCorrelation(Lead:MaxLead),'g','LineWidth',3)
%plot(horas,VerificationCalibratedHybridError.DirCorrelation,'m','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsVel.DirCorrelation(Lead:MaxLead),'k','LineWidth',3)
grid('On')
title('CORRELACION (b)')
axis([0 max(horas(Lead:MaxLead)) 0 1]);

subplot(2,2,3)
set(gca,'FontSize',fontsize);
hold on
plot(horas(Lead:MaxLead),VerificationUncalibrated.DirBias(Lead:MaxLead),'b','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedDoyLeadVel.DirBias(Lead:MaxLead),'r','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybrid.DirBias(Lead:MaxLead),'g','LineWidth',3)
%plot(horas,VerificationCalibratedHybridError.DirBias,'m','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsVel.DirBias(Lead:MaxLead),'k','LineWidth',3)
grid('On')
title('BIAS (c)')

axis([0 max(horas(Lead:MaxLead)) -15 15]);

subplot(2,2,4)
set(gca,'FontSize',fontsize);
hold on
plot(horas(Lead:MaxLead),VerificationUncalibrated.DirStdObs(Lead:MaxLead),'b','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedDoyLeadVel.DirStdObs(Lead:MaxLead),'r','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybrid.DirStdObs(Lead:MaxLead),'g','LineWidth',3)
%plot(horas,VerificationCalibratedHybridError.DirStdObs,'m','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsVel.DirStdObs(Lead:MaxLead),'k','LineWidth',3)
grid('On')
title('VARIABILIDAD (d)')
axis([0 max(horas(Lead:MaxLead)) 0 150]);

print('-dpng','ErroresHybridDir.png');

%==========================================================================
% POTENCIA
%==========================================================================
horas=[1:size(Val.WRFDir,2)]*10/60;

hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20]);

subplot(2,2,1)
set(gca,'FontSize',fontsize);

hold on
plot(horas(Lead:MaxLead),VerificationUncalibrated.PowRmse(Lead:MaxLead),'b','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedDoyLeadVel.PowRmse(Lead:MaxLead),'r','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybrid.PowRmse(Lead:MaxLead),'g','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybridPow.PowRmse(Lead:MaxLead),'g--','LineWidth',3)
%plot(horas,VerificationCalibratedHybridError.PowRmse,'m','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsVel.PowRmse(Lead:MaxLead),'k','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsPow.PowRmse(Lead:MaxLead),'k--','LineWidth',3)
grid('On')
title('RMSE (a)')
xlabel('Tiempo (horas)')
ylabel('Potencia (MW)')
axis([0 max(horas(Lead:MaxLead)) 0 30000]);
%legend('Uncal','Cal');

subplot(2,2,2)
set(gca,'FontSize',fontsize);
hold on
plot(horas(Lead:MaxLead),VerificationUncalibrated.PowCorrelation(Lead:MaxLead),'b','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedDoyLeadVel.PowCorrelation(Lead:MaxLead),'r','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybrid.PowCorrelation(Lead:MaxLead),'g','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybridPow.PowCorrelation(Lead:MaxLead),'g--','LineWidth',3)
%plot(horas,VerificationCalibratedHybridError.PowCorrelation,'m','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsVel.PowCorrelation(Lead:MaxLead),'k','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsPow.PowCorrelation(Lead:MaxLead),'k--','LineWidth',3)
grid('On')
title('CORRELACION (b)')
axis([0 max(horas(Lead:MaxLead)) 0 1]);

subplot(2,2,3)
set(gca,'FontSize',fontsize);
hold on
plot(horas(Lead:MaxLead),VerificationUncalibrated.PowBias(Lead:MaxLead),'b','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedDoyLeadVel.PowBias(Lead:MaxLead),'r','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybrid.PowBias(Lead:MaxLead),'g','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybridPow.PowBias(Lead:MaxLead),'g--','LineWidth',3)
%plot(horas,VerificationCalibratedHybridError.PowBias,'m','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsVel.PowBias(Lead:MaxLead),'k','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsPow.PowCorrelation(Lead:MaxLead),'k--','LineWidth',3)
xlabel('Tiempo (horas) ')
ylabel('Potencia (MW)')
grid('On')
title('BIAS (c)')

axis([0 max(horas(Lead:MaxLead)) -20000 20000]);

subplot(2,2,4)
set(gca,'FontSize',fontsize);
hold on
plot(horas(Lead:MaxLead),VerificationUncalibrated.PowStdFor(Lead:MaxLead),'b','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedDoyLeadVel.PowStdFor(Lead:MaxLead),'r','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybrid.PowStdFor(Lead:MaxLead),'g','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybridPow.PowStdFor(Lead:MaxLead),'g--','LineWidth',3)
%plot(horas,VerificationCalibratedHybridError.PowStdObs,'m','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsVel.PowStdFor(Lead:MaxLead),'k','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsPow.PowStdFor(Lead:MaxLead),'k--','LineWidth',3)
xlabel('Tiempo (horas)')
ylabel('Potencia (MW)')
grid('On')
title('VARIABILIDAD (d)')
axis([0 max(horas(Lead:MaxLead)) 0 30000]);

print('-dpng','ErroresHybridPow.png');

%========================================================================================================
% HAGO LA VERIFICACION PARA LOS PRONOSTICOS ACUMULADOS EN PLAZOS DE 1 HORA.
%========================================================================================================

[ VelCalibratedHybridHourly ] = AccumulateDataFun( VelCalibratedHybrid , 6 , 'Mean' );
[ DirCalibratedHybridHourly ] = AccumulateDataFun( DirCalibratedHybrid , 6 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowCalibratedHybridHourly ] = AccumulateDataFun( TotalPowCalibratedHybrid , 6 , 'Mean' );

[ VelObsVelHourly ] = AccumulateDataFun( VelObsVel , 6 , 'Mean' );
[ DirObsVelHourly ] = AccumulateDataFun( DirObsVel , 6 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowObsVelHourly ] = AccumulateDataFun( TotalPowObsVel , 6 , 'Mean' );

[ VelObsPowHourly ] = AccumulateDataFun( VelObsPow , 6 , 'Mean' );
[ DirObsPowHourly ] = AccumulateDataFun( DirObsPow , 6 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowObsPowHourly ] = AccumulateDataFun( TotalPowObsPow , 6 , 'Mean' );

[ VelCalibratedHybridPowHourly ] = AccumulateDataFun( VelCalibratedHybridPow , 6 , 'Mean' );
[ DirCalibratedHybridPowHourly ] = AccumulateDataFun( DirCalibratedHybridPow , 6 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowCalibratedHybridPowHourly ] = AccumulateDataFun( TotalPowCalibratedHybridPow , 6 , 'Mean' );

%==========================================================================
% VERIFICACION DE LOS PRONOSTICOS
% calculo del error cuadratico medio (rmse), error sistematico (bias) y
% coeficiente de correlacion para la intensidad de viento, direccion y
% potencia total producida por el parque. 
%==========================================================================


[ VerificationCalibratedHybridHourly ]=VerificationFun(VelCalibratedHybridHourly,VelObsHourly,DirCalibratedHybridHourly,DirObsHourly,TotalPowCalibratedHybridHourly,TotalPowObsHourly);

[ VerificationCalibratedHybridPowHourly ]=VerificationFun(VelCalibratedHybridPowHourly,VelObsHourly,DirCalibratedHybridPowHourly,DirObsHourly,TotalPowCalibratedHybridPowHourly,TotalPowObsHourly);

[ VerificationObsPowHourly ]=VerificationFun(VelObsPowHourly,VelObsHourly,DirObsPowHourly,DirObsHourly,TotalPowObsPowHourly,TotalPowObsHourly);

[ VerificationObsVelHourly ]=VerificationFun(VelObsVelHourly,VelObsHourly,DirObsVelHourly,DirObsHourly,TotalPowObsVelHourly,TotalPowObsHourly);



%==========================================================================
% POTENCIA
%==========================================================================

MaxLead=24;
Lead=2;
horas=1:length(VerificationUncalibratedHourly.VelRmse);

hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20]);

subplot(2,2,1)
set(gca,'FontSize',fontsize);

hold on
plot(horas(Lead:MaxLead),VerificationUncalibratedHourly.PowRmse(Lead:MaxLead),'b','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedDoyLeadVelHourly.PowRmse(Lead:MaxLead),'r','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybridHourly.PowRmse(Lead:MaxLead),'g','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybridPowHourly.PowRmse(Lead:MaxLead),'g--','LineWidth',3)
%plot(horas,VerificationCalibratedHybridError.PowRmse,'m','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsVelHourly.PowRmse(Lead:MaxLead),'k','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsPowHourly.PowRmse(Lead:MaxLead),'k--','LineWidth',3)
grid('On')
title('RMSE (a)')
xlabel('Tiempo (horas)')
ylabel('Potencia (MW)')
axis([0 max(horas(Lead:MaxLead)) 0 30000]);
%legend('Uncal','Cal');

subplot(2,2,2)
set(gca,'FontSize',fontsize);
hold on
plot(horas(Lead:MaxLead),VerificationUncalibratedHourly.PowCorrelation(Lead:MaxLead),'b','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedDoyLeadVelHourly.PowCorrelation(Lead:MaxLead),'r','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybridHourly.PowCorrelation(Lead:MaxLead),'g','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybridPowHourly.PowCorrelation(Lead:MaxLead),'g--','LineWidth',3)
%plot(horas,VerificationCalibratedHybridError.PowCorrelation,'m','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsVelHourly.PowCorrelation(Lead:MaxLead),'k','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsPowHourly.PowCorrelation(Lead:MaxLead),'k--','LineWidth',3)
grid('On')
title('CORRELACION (b)')
axis([0 max(horas(Lead:MaxLead)) 0 1]);

subplot(2,2,3)
set(gca,'FontSize',fontsize);
hold on
plot(horas(Lead:MaxLead),VerificationUncalibratedHourly.PowBias(Lead:MaxLead),'b','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedDoyLeadVelHourly.PowBias(Lead:MaxLead),'r','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybridHourly.PowBias(Lead:MaxLead),'g','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybridPowHourly.PowBias(Lead:MaxLead),'g--','LineWidth',3)
%plot(horas,VerificationCalibratedHybridError.PowBias,'m','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsVelHourly.PowBias(Lead:MaxLead),'k','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsPowHourly.PowCorrelation(Lead:MaxLead),'k--','LineWidth',3)
xlabel('Tiempo (horas)')
ylabel('Potencia (MW)')
grid('On')
title('BIAS (c)')

axis([0 max(horas(Lead:MaxLead)) -20000 20000]);

subplot(2,2,4)
set(gca,'FontSize',fontsize);
hold on
plot(horas(Lead:MaxLead),VerificationUncalibratedHourly.PowStdFor(Lead:MaxLead),'b','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedDoyLeadVelHourly.PowStdFor(Lead:MaxLead),'r','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybridHourly.PowStdFor(Lead:MaxLead),'g','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybridPowHourly.PowStdFor(Lead:MaxLead),'g--','LineWidth',3)
%plot(horas,VerificationCalibratedHybridError.PowStdObs,'m','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsVelHourly.PowStdFor(Lead:MaxLead),'k','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsPowHourly.PowStdFor(Lead:MaxLead),'k--','LineWidth',3)
xlabel('Tiempo (horas)')
ylabel('Potencia (MW)')
grid('On')
title('VARIABILIDAD (d)')
axis([0 max(horas(Lead:MaxLead)) 0 30000]);

print('-dpng','ErroresHybridPowHourly.png');



%========================================================================================================
% HAGO LA VERIFICACION PARA LOS PRONOSTICOS ACUMULADOS EN PLAZOS DE 12 HORAS.
%========================================================================================================

[ VelCalibratedHybrid12Hourly ] = AccumulateDataFun( VelCalibratedHybrid , 12*6 , 'Mean' );
[ DirCalibratedHybrid12Hourly ] = AccumulateDataFun( DirCalibratedHybrid , 12*6 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowCalibratedHybrid12Hourly ] = AccumulateDataFun( TotalPowCalibratedHybrid , 12*6 , 'Mean' );

[ VelObsVel12Hourly ] = AccumulateDataFun( VelObsVel , 12*6 , 'Mean' );
[ DirObsVel12Hourly ] = AccumulateDataFun( DirObsVel , 12*6 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowObsVel12Hourly ] = AccumulateDataFun( TotalPowObsVel , 12*6 , 'Mean' );

[ VelObsPow12Hourly ] = AccumulateDataFun( VelObsPow , 12*6 , 'Mean' );
[ DirObsPow12Hourly ] = AccumulateDataFun( DirObsPow , 12*6 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowObsPow12Hourly ] = AccumulateDataFun( TotalPowObsPow , 12*6 , 'Mean' );

[ VelCalibratedHybridPow12Hourly ] = AccumulateDataFun( VelCalibratedHybridPow , 12*6 , 'Mean' );
[ DirCalibratedHybridPow12Hourly ] = AccumulateDataFun( DirCalibratedHybridPow , 12*6 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowCalibratedHybridPow12Hourly ] = AccumulateDataFun( TotalPowCalibratedHybridPow , 12*6 , 'Mean' );

%==========================================================================
% VERIFICACION DE LOS PRONOSTICOS
% calculo del error cuadratico medio (rmse), error sistematico (bias) y
% coeficiente de correlacion para la intensidad de viento, direccion y
% potencia total producida por el parque. 
%==========================================================================


[ VerificationCalibratedHybrid12Hourly ]=VerificationFun(VelCalibratedHybrid12Hourly,VelObs12Hourly,DirCalibratedHybrid12Hourly,DirObs12Hourly,TotalPowCalibratedHybrid12Hourly,TotalPowObs12Hourly);

[ VerificationCalibratedHybridPow12Hourly ]=VerificationFun(VelCalibratedHybridPow12Hourly,VelObs12Hourly,DirCalibratedHybridPow12Hourly,DirObs12Hourly,TotalPowCalibratedHybridPow12Hourly,TotalPowObs12Hourly);

[ VerificationObsPow12Hourly ]=VerificationFun(VelObsPow12Hourly,VelObs12Hourly,DirObsPow12Hourly,DirObs12Hourly,TotalPowObsPow12Hourly,TotalPowObs12Hourly);

[ VerificationObsVel12Hourly ]=VerificationFun(VelObsVel12Hourly,VelObs12Hourly,DirObsVel12Hourly,DirObs12Hourly,TotalPowObsVel12Hourly,TotalPowObs12Hourly);

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


%==========================================================================
% POTENCIA
%==========================================================================
MaxLead=4;
Lead=2;

hFig = figure('Menubar','none'); %,'Visible','Off');
set(hFig, 'Units', 'centimeters', 'Position', [0 0 20 20]);

subplot(2,2,1)
set(gca,'FontSize',fontsize);

hold on
plot(horas(Lead:MaxLead),VerificationUncalibrated12Hourly.PowRmse(Lead:MaxLead),'b','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedDoyLeadVel12Hourly.PowRmse(Lead:MaxLead),'r','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybrid12Hourly.PowRmse(Lead:MaxLead),'g','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybridPow12Hourly.PowRmse(Lead:MaxLead),'g--','LineWidth',3)
%plot(horas,VerificationCalibratedHybridError.PowRmse,'m','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsVel12Hourly.PowRmse(Lead:MaxLead),'k','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsPow12Hourly.PowRmse(Lead:MaxLead),'k--','LineWidth',3)
grid('On')
title('RMSE (a)')
xlabel('Tiempo (horas)')
ylabel('Potencia (MW)')
axis([0 max(horas(Lead:MaxLead)) 0 30000]);
%legend('Uncal','Cal');

subplot(2,2,2)
set(gca,'FontSize',fontsize);
hold on
plot(horas(Lead:MaxLead),VerificationUncalibrated12Hourly.PowCorrelation(Lead:MaxLead),'b','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedDoyLeadVel12Hourly.PowCorrelation(Lead:MaxLead),'r','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybrid12Hourly.PowCorrelation(Lead:MaxLead),'g','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybridPow12Hourly.PowCorrelation(Lead:MaxLead),'g--','LineWidth',3)
%plot(horas,VerificationCalibratedHybridError.PowCorrelation,'m','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsVel12Hourly.PowCorrelation(Lead:MaxLead),'k','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsPow12Hourly.PowCorrelation(Lead:MaxLead),'k--','LineWidth',3)
grid('On')
title('CORRELACION (b)')
axis([0 max(horas(Lead:MaxLead)) 0 1]);

subplot(2,2,3)
set(gca,'FontSize',fontsize);
hold on
plot(horas(Lead:MaxLead),VerificationUncalibrated12Hourly.PowBias(Lead:MaxLead),'b','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedDoyLeadVel12Hourly.PowBias(Lead:MaxLead),'r','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybrid12Hourly.PowBias(Lead:MaxLead),'g','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybridPow12Hourly.PowBias(Lead:MaxLead),'g--','LineWidth',3)
%plot(horas,VerificationCalibratedHybridError.PowBias,'m','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsVel12Hourly.PowBias(Lead:MaxLead),'k','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsPow12Hourly.PowCorrelation(Lead:MaxLead),'k--','LineWidth',3)
xlabel('Tiempo (horas)')
ylabel('Potencia (MW)')
grid('On')
title('BIAS (c)')

axis([0 max(horas(Lead:MaxLead)) -20000 20000]);

subplot(2,2,4)
set(gca,'FontSize',fontsize);
hold on
plot(horas(Lead:MaxLead),VerificationUncalibrated12Hourly.PowStdFor(Lead:MaxLead),'b','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedDoyLeadVel12Hourly.PowStdFor(Lead:MaxLead),'r','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybrid12Hourly.PowStdFor(Lead:MaxLead),'g','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationCalibratedHybridPow12Hourly.PowStdFor(Lead:MaxLead),'g--','LineWidth',3)
%plot(horas,VerificationCalibratedHybridError.PowStdObs,'m','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsVel12Hourly.PowStdFor(Lead:MaxLead),'k','LineWidth',3)
plot(horas(Lead:MaxLead),VerificationObsPow12Hourly.PowStdFor(Lead:MaxLead),'k--','LineWidth',3)
xlabel('Tiempo (horas)')
ylabel('Potencia (MW)')
grid('On')
title('VARIABILIDAD (d)')
axis([0 max(horas(Lead:MaxLead)) 0 30000]);

print('-dpng','ErroresHybridPow12Hourly.png');

save('HybridCalibrationOutput.mat');
