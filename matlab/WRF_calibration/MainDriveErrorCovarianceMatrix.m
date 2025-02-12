clear all
close all

%==========================================================================
% Este script realiza una verificacion "Offline" de los pronosticos de
% viento y potencia y ademas compara los resultados con los obtenidos a
% partir de un pronostico calibrado estadisticamente.
%==========================================================================

%Cargo la base de datos de pronosticos calibrados y de observaciones.

load('CalibrationOutput.mat');

%==========================================================================
% CALCULO LOS ERRORES DEL PRONOSTICO CALIBRADO
%==========================================================================
NaNIndex=any( isnan(VelObs) ,2) | any( isnan(VelCalibratedDoyLeadVel),2 )  ;

VelObs(NaNIndex,:)=[];
VelCalibratedDoyLeadVel(NaNIndex,:)=[];

NaNIndex=any( isnan(DirObs) ,2) | any( isnan(DirCalibratedDoyLeadVel),2 )  ;

DirObs(NaNIndex,:)=[];
DirCalibratedDoyLeadVel(NaNIndex,:)=[];

ErrorVel= VelCalibratedDoyLeadVel - VelObs ;

ErrorDir= DirCalibratedDoyLeadVel - DirObs ;

ErrorDir( ErrorDir > 180 )=ErrorDir(ErrorDir >  180 )-360;
ErrorDir( ErrorDir < -180)=ErrorDir(ErrorDir < -180 )+360;


VelErrorCorrelation= corrcoef( ErrorVel );


DirErrorCorrelation= corrcoef( ErrorDir );


%========================================================================================================
% CALCULO LAS OBSERVACIONES Y LOS PRONOSTICOS CALIBRADOS EN INTERVALOS DE 1
% HORA.
%========================================================================================================

[ VelObsHourly ] = AccumulateDataFun( VelObs , 6 , 'Mean' );
[ DirObsHourly ] = AccumulateDataFun( DirObs , 6 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowObsHourly ] = AccumulateDataFun( TotalPowObs , 6 , 'Mean' );

[ VelCalibratedDoyLeadVelHourly ] = AccumulateDataFun( VelCalibratedDoyLeadVel , 6 , 'Mean' );
[ DirCalibratedDoyLeadVelHourly ] = AccumulateDataFun( DirCalibratedDoyLeadVel , 6 , 'Mean' );  %NOTA: Este promedio no esta pensado para direccion.
[ TotalPowCalibratedDoyLeadVelHourly ] = AccumulateDataFun( TotalPowCalibratedDoyLeadVel , 6 , 'Mean' );

%==========================================================================
% CALCULO LOS ERRORES DEL PRONOSTICO CALIBRADO
%==========================================================================
NaNIndex=any( isnan(VelObsHourly) ,2) | any( isnan(VelCalibratedDoyLeadVelHourly),2 )  ;

VelObsHourly(NaNIndex,:)=[];
VelCalibratedDoyLeadVelHourly(NaNIndex,:)=[];

NaNIndex=any( isnan(DirObsHourly) ,2) | any( isnan(DirCalibratedDoyLeadVelHourly),2 )  ;

DirObsHourly(NaNIndex,:)=[];
DirCalibratedDoyLeadVelHourly(NaNIndex,:)=[];

ErrorVelHourly= VelCalibratedDoyLeadVelHourly - VelObsHourly ;

ErrorDirHourly= DirCalibratedDoyLeadVelHourly - DirObsHourly ;

ErrorDirHourly( ErrorDirHourly > 180 )=ErrorDirHourly(ErrorDirHourly >  180 )-360;
ErrorDirHourly( ErrorDirHourly < -180)=ErrorDirHourly(ErrorDirHourly < -180 )+360;


VelErrorCorrelationHourly = corrcoef( ErrorVelHourly );


DirErrorCorrelationHourly = corrcoef( ErrorDirHourly );



save('CorrelationMatrix.mat');





