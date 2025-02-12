
function [ ErrorModel ]=Gaussian_calibration_fun( CVelFor , CDirFor , CDate , VelForDb , VelObsDb , DirForDb, DirObsDb , DatesDb )

%En esta funcion implementamos una variacion del algoritmo EMOS (Schuhen et
%al. 2012) en el cual se utilizan los pronosticos previos junto con sus
%respectivas observaciones para determinar la incertidumbre de los
%pronosticos.
%Esta metodologia permite para cada velocidad y direccion pronosticada,
%encontrar la media y el desvio estandard de U y V observados. A partir de
%la media se puede reconstruir un pronostico deterministico con menor error
%sistematico y utilizando los desvios estandard se puede generar un
%ensamble de pronosticos que traducido a potencia puede brindar informacion
%sobre el grado de incertidumbre en la produccion energetica del parque.

%INPUTS
%CVelFor: Un vector conteniendo N pronosticos de intensidad de viento.
%CDirFor: Un vector conteniendo N pronosticos de direccion de viento.
%CDate: La fecha de inicio del pronostico en formato 'yyyymmddhh')
%VelForDb: Un array conteniendo la base de datos historica de pronosticos
%de intensidad de viento. Cada fila corresponde a un pronostico y cada
%columna corresponde a un plazo de pronostico. La cantidad de columnas es
%igual a la longitud de los vectores CVelFor y CDirFor.
%VelObsDb: Contiene los valores de intensidad de viento observados que
%corresponden a cada uno de los pronosticos de viento contenidos en
%VelForDb.
%DirForDb y DirObsDb son analogos a VelForDb y VelObsDb pero para la
%direccion del viento.
%DatesDb es un vector contiendo las fechas de inicio de cada pronostico en
%el formato 'yyyymmddhh';

display(['Realizando la calibracion Gaussiana'])

%==========================================================================
% PARAMETROS DEL METODO (idealmente estos parametros formarian parte del
% input a la funcion y serian seteados en un archivo de configuracion que
% controle todo el proceso de correccion estadistica de los pronosticos)
%==========================================================================
%La idea de estos parametros es controlar el funcionamiento del metodo.

%La idea del metodo es tomar pronosticos pasados que esten en un rango
%similar de valores de direccion, intensidad, plazo de pronostico y epoca
%del anio. Los parametros que vienen a continuacion controlan lo que quiere
%decir "similar" en este sentido. La idea es centrarnos en el valor
%pronosticar pronosticos en +/- un cierto valor de velocidad, direccion,
%plazo y epoca.

DVel=2.5; %Cuanto sera el rango de velocidad que usaremos para corregir el pronostico.
%DDir=45; %Cuanto sera el rango de direcciones que usaremos para corregir el pronostico.
DDay=60; %Cuanto sera la ventana de tiempo en dias que usaremos para corregir el pronostico.
DLead=3; %Cuanto sera la ventana en plazo de pronostico que usaremos para corregir el pronostico.
MinSampleSize=10;  %Numero minimo de elementos en la muestra local a partir del cual la muestra local tendra peso en los valores corregidos y en la estimacion de la incertidumbre.

%Estos parametros controlan la aplicacion de un suavizado posterior a la
%calibracion (esto esta pensando tendiente a reducir el ruido que puede
%introducirse debido a lo pequenio de las muestras utilizadas). Una vez
%realizada la calibracion, la idea es suavizar las correcciones en U y V y
%suavizar las estimaciones de las desviaciones estandard y la covarianza de
%manera tal de que su evolucion temporal en el plazo de pronostico sea lo
%mas suave posible.
SmoothFlag=true; %Si aplico o no el suavizado (true aplico el suavizado)
SmoothDegree=7;  %Grado de suavizado que se aplicara en las correccioes.

CDateNum=datenum( CDate );
DateNum=datenum( DatesDb );

%El algoritmo tiene la posibilidad de generar ademas un conjunto de
%pronosticos basandose en el estudio de las propiedades estadisticas de los
%errores. Este conjunto o ensamble de pronsoticos representa posibles
%escenarios de U,V intensidad y direccion de viento para cada uno de los
%plazos de pronostico. En este caso la generacion del ensamble es puramente
%estadistica y no se basa en multiples simulaciones numericas.

%==========================================================================
% OBTENGO LAS DIMENSIONES DE LAS VARIABLES DE ENTRADA.
%==========================================================================

NLead=length(CVelFor);

%==========================================================================
%CALCULO U Y V PARA LOS VALORES PRONOSTICADOS Y PARA LA BASE DE DATOS. 
%Todo el algoritmo de correccion trabaja internamente con U y V. 
%==========================================================================

[CUFor,CVFor]=VelDirToUV(CVelFor,CDirFor);
[UForDb ,VForDb ]=VelDirToUV(VelForDb,DirForDb);
[UObsDb ,VObsDb ]=VelDirToUV(VelObsDb,DirObsDb);

%==========================================================================
%CALCULO VALORES GLOBALES DE MEDIA DEL ERROR EN U, V, SUS RESPECTIVOS
%SIGMAS Y LA COVARIANZA. 
%Estos valores seran utilizados cuando la muestra de los pronosticos
%cercanos al pronostico actual sea muy pequenia o incluso nula. Esto
%sucederia con casos en los que los valores pronosticados son poco
%frecuentes y por ende estos valores se usaran con relativamente poca
%frecuencia para calibrar los pronsoticos. A medida que la base de datos de
%entrenamiento crezca en tamanio estos valores se usaran cada vez menos.
%==========================================================================

ErrorU=UForDb-UObsDb;  %Error en U para los pronosticos pasados.
ErrorV=VForDb-VObsDb;  %Error en V para los pronosticos pasados.

ErrorModel.GlobalMeanErrorU=nanmean(ErrorU,1);   %Media del error de U.
ErrorModel.GlobalMeanErrorV=nanmean(ErrorV,1);   %Media del error de V.

ErrorModel.GlobalStdErrorU=nanstd(ErrorU,[],1);  %Desviacion estandard del error de U
ErrorModel.GlobalStdErrorV=nanstd(ErrorV,[],1);  %Desviacion estandard del error de V

for ii=1:NLead
   tmp=corrcoef( ErrorU(:,ii) , ErrorV(:,ii) );
   ErrorModel.GlobalErrorCorrUV(ii)=tmp(1,2);   %Correlacion entre el error de U y de V.
end

for ii=1:NLead
 [ErrorModel.GlobalMeanErrorVel(ii) ErrorModel.GlobalStdErrorVel(ii) ErrorModel.GlobalMeanErrorDir(ii) ErrorModel.GlobalStdErrorDir(ii) ]=StatisticsVelDirError( VelForDb(:,ii) , DirForDb(:,ii) , VelObsDb(:,ii) , DirObsDb(:,ii) );
end

%==========================================================================
% EN LAS SECCIONES SIGUIENTES VAMOS A ESTIMAR LOS PARAMETROS LOCALES DE LA
% DISTRIBUCION DEL ERROR, ES DECIR COMO SON LA MEDIA, STD Y CORRELACION DE
% LOS ERRORES CUANDO LOS VALORES PRONOSTICADOS SON SIMILARES A LOS DEL
% PRONSOTICO ACTUAL.
%==========================================================================

%==========================================================================
%RETENEMOS SOLO LOS PRONOSTICOS PASADOS QUE CORRESPONDEN A UNA MISMA EPOCA
%DEL ANIO (esto es debido a que el comportamiento de los errores
%sistematicos depende mucho de la epoca del anio).
%Calculo el dia del anio (esto es necesario para elegir los pronosticos
%pasados que se encuentren en un rango de fechas similar al pronostico
%actual).
%==========================================================================

doy = DateNum - datenum(year(DateNum),1,1) + 1;     %Day of year de cada pronostico en la base de datos
cdoy = CDateNum - datenum(year(CDateNum),1,1) + 1;  %Day of year del pronostico actual.

NLead=length(CVelFor);  %Cantidad de plazos de pronostico.

%Reducimos la base de datos y nos quedamos solo con los pronosticos que se inicializaron en dias "cercanos" a la fecha del pronostico actual.

        max_doy=cdoy + DDay ;
        min_doy=cdoy - DDay ;

        if( max_doy > 365)
            %El tope del perido se paso para el anio siguiente.
            max_doy = max_doy - 365;
            TimeWindowIndex = doy <= max_doy | doy >= min_doy;
        elseif( min_doy < 1)
            min_doy=min_doy + 365;
            TimeWindowIndex = doy >= min_doy | doy <= max_doy;
        else
            TimeWindowIndex = doy >= min_doy & doy <= max_doy;
        end

 %Filtro las fechas que no estan dentro del rango de fechas que esta
 %centrado en el pronostico actual (con esto busco retener la informacion
 %de los errores correspondiente a la misma epoca del anio.
 
 TimeLocalErrorU=ErrorU(TimeWindowIndex,:);
 TimeLocalErrorV=ErrorV(TimeWindowIndex,:);

 TimeLocalVelFor=VelForDb(TimeWindowIndex,:);
 TimeLocalDirFor=DirForDb(TimeWindowIndex,:);
 
 TimeLocalVelObs=VelObsDb(TimeWindowIndex,:);
 TimeLocalDirObs=DirObsDb(TimeWindowIndex,:);
 
 TimeLocalUFor=UForDb(TimeWindowIndex,:);
 TimeLocalVFor=VForDb(TimeWindowIndex,:);
 
 TimeLocalUObs=UObsDb(TimeWindowIndex,:);
 TimeLocalVObs=VObsDb(TimeWindowIndex,:);
 
 NForDb=size(TimeLocalErrorU,1);

%==========================================================================
%EN ESTE PUNTO ARRANCAMOS UN LOOP SOBRE LOS PLAZOS DE PRONOSTICO, PARA CADA PLAZO
%VAMOS A CONSIDERAR EL VALOR PRONOSTICADO Y BUSCAR TODOS LOS PRONOSTICOS 
%PASADOS QUE ESTEN EN UN RANGO DE VALORES CERCANO AL VALOR PRONOSTICADO.
%==========================================================================
 
for ii=1:NLead   %ii es el indice sobre los plazos de pronostico.

%==========================================================================
% VAMOS A QUEDARNOS PRIMERO CON AQUELLOS PRONOSTICOS QUE CORRESPONDAN A
% PLAZOS SIMILARES.
%==========================================================================
 
   max_lead=ii+DLead;
   min_lead=ii-DLead;

   if( max_lead > NLead); max_lead = NLead ; end
   if( min_lead < 1    ); min_lead = 1     ; end

   TmpNLead= max_lead - min_lead  + 1;

   LocalErrorU=reshape(TimeLocalErrorU(:,min_lead:max_lead),[TmpNLead*NForDb 1]);
   LocalErrorV=reshape(TimeLocalErrorV(:,min_lead:max_lead),[TmpNLead*NForDb 1]);
   
   LocalVelFor=reshape(TimeLocalVelFor(:,min_lead:max_lead),[TmpNLead*NForDb 1]);
   LocalDirFor=reshape(TimeLocalDirFor(:,min_lead:max_lead),[TmpNLead*NForDb 1]);
   
   LocalVelObs=reshape(TimeLocalVelObs(:,min_lead:max_lead),[TmpNLead*NForDb 1]);
   LocalDirObs=reshape(TimeLocalDirObs(:,min_lead:max_lead),[TmpNLead*NForDb 1]);

   LocalUFor=reshape(TimeLocalUFor(:,min_lead:max_lead),[TmpNLead*NForDb 1]);
   LocalVFor=reshape(TimeLocalVFor(:,min_lead:max_lead),[TmpNLead*NForDb 1]);
   
   LocalUObs=reshape(TimeLocalUObs(:,min_lead:max_lead),[TmpNLead*NForDb 1]);
   LocalVObs=reshape(TimeLocalVObs(:,min_lead:max_lead),[TmpNLead*NForDb 1]);
   
%==========================================================================
% VAMOS A QUEDARNOS PRIMERO CON AQUELLOS PRONOSTICOS CUYO VECTOR DIFERENCIA
% TENGA UN MODULO MENOR A DVel.
%==========================================================================

   Distance=sqrt( ( LocalUFor - CUFor(ii) ).^2  + ( LocalVFor - CVFor(ii) ).^2 );

   IndexDist = Distance <= DVel ;
   
   LocalErrorU=LocalErrorU(IndexDist);
   LocalErrorV=LocalErrorV(IndexDist);
   
   LocalVelFor=LocalVelFor(IndexDist);
   LocalDirFor=LocalDirFor(IndexDist);
   
   LocalVelObs=LocalVelObs(IndexDist);
   LocalDirObs=LocalDirObs(IndexDist);
   
   LocalUFor=LocalUFor(IndexDist);
   LocalVFor=LocalVFor(IndexDist);
   
   LocalUObs=LocalUObs(IndexDist);
   LocalVObs=LocalVObs(IndexDist);
   
[ LocalMeanErrorVel LocalStdErrorVel LocalMeanErrorDir LocalStdErrorDir ]=StatisticsVelDirError( LocalVelFor , LocalDirFor , LocalVelObs , LocalDirObs );
   
   SampleSize=sum(IndexDist);  %Obtenemos el numero de elementos en la muestra local.

   LocalMeanErrorU=nanmean(LocalErrorU);   %Media del error de U.
   LocalMeanErrorV=nanmean(LocalErrorV);   %Media del error de V.

   LocalStdErrorU=nanstd(LocalErrorU);  %Desviacion estandard del error de U
   LocalStdErrorV=nanstd(LocalErrorV);  %Desviacion estandard del error de V
   
   LocalMeanDir=MeanDir( LocalDirObs, ones(size(LocalDirObs) ) );

   
%    if(ii == 102)
%       figure
%       hold on
%       
%       plot(LocalUFor,LocalVFor,'ro')
%       plot(LocalUObs,LocalVObs,'go')
%       plot(CUFor(ii),CVFor(ii),'ko','MarkerSize',7)
%       title('U V')
%       
%       figure
%       hold on
%       plot(LocalVelFor,LocalDirFor,'ro')
%       plot(LocalVelObs,LocalDirObs,'go')
%       plot(CVelFor(ii),CDirFor(ii),'ko','MarkerSize',7)
%       title('Vel Dir')
%    end
   
   
   tmp=corrcoef( LocalErrorU , LocalErrorV );
   
   if( SampleSize >= 2)
     LocalErrorCorrUV=tmp(1,2);   %Correlacion entre el error de U y de V.
   else
     %Si la muestra tiene 0 o 1 elementos no podemos calcular la
     %correlacion. En este caso asumimos que la correlacion local es 0. De
     %todas formas esto no tiene efecto ya que en este caso se usa la
     %correlacion global calculada con la muestra completa.
     LocalErrorCorrUV=0.0;
   end
   
   %Si la muestra es muy pequenia o 0 y el resultado del promedio o los
   %desvios es 0, entonces asumo que el SampleSize es 0 para todo lo que
   %viene despues y ademas seteo en 0 los valores de los estadisiticos de
   %la muestra local. Estos valores no van a ser utilizados porque se le va
   %a dar todo el peso a los valores globales, pero de esta manera se evita
   %que el resultado final sea un NaN.
   
   if( any( isnan( [LocalMeanErrorU LocalMeanErrorV LocalStdErrorU LocalStdErrorV ] ) ) )
       SampleSize=0;
       LocalMeanErrorU=0;
       LocalMeanErrorV=0;
       LocalStdErrorU=0;
       LocalStdErrorV=0;
       LocalErrorCorrUV=0;
   end
     
%==========================================================================
% CALCULO UN PESO QUE VOY A ASIGNAR A LOS VALORES LOCALES Y UN PESO A LOS
% VALORES GLOBALES. CUANTO MAS GRANDE SEA EL TAMANIO DE LA MUESTRA MAS
% GRANDE SERA EL PESO.
%==========================================================================
   ErrorModel.SampleSize(ii)=SampleSize;  %Guardo la cantidad de elementos que 
                                          %intervinieron en la correccion
                                          %del pronostico para cada plazo
                                          %de pronostico.
                                          
   if( SampleSize <= MinSampleSize )
      Weight=1.0;
   else
      Weight=1.0/(SampleSize-MinSampleSize);
   end
   
%==========================================================================
% COMBINO LOS PARAMETROS GLOBALES Y LOCALES TENIENDO EN CUENTA EL PESO
% ASIGNADO A CADA UNO.
%==========================================================================
   
   ErrorModel.MeanErrorU(ii)=Weight*ErrorModel.GlobalMeanErrorU(ii) + (1-Weight)*LocalMeanErrorU;
   ErrorModel.MeanErrorV(ii)=Weight*ErrorModel.GlobalMeanErrorV(ii) + (1-Weight)*LocalMeanErrorV;
   
   ErrorModel.StdErrorU(ii)=Weight*ErrorModel.GlobalStdErrorU(ii) + (1-Weight)*LocalStdErrorU;
   ErrorModel.StdErrorV(ii)=Weight*ErrorModel.GlobalStdErrorV(ii) + (1-Weight)*LocalStdErrorV;
   
   ErrorModel.ErrorCorrUV(ii)=Weight*ErrorModel.GlobalErrorCorrUV(ii) + (1-Weight)*LocalErrorCorrUV;
   
   ErrorModel.MeanErrorVel(ii)=Weight*ErrorModel.GlobalMeanErrorVel(ii) + (1-Weight)*LocalMeanErrorVel;
   %ErrorModel.MeanErrorDir(ii)=Weight*ErrorModel.GlobalMeanErrorDir(ii) + (1-Weight)*LocalMeanErrorDir;
   ErrorModel.DirCalibrated(ii)=MeanDir([CDirFor(ii) LocalMeanDir],[Weight (1-Weight)] );
   
   ErrorModel.StdErrorVel(ii)=Weight*ErrorModel.GlobalStdErrorVel(ii) + (1-Weight)*LocalStdErrorVel;
   ErrorModel.StdErrorDir(ii)=Weight*ErrorModel.GlobalStdErrorDir(ii) + (1-Weight)*LocalStdErrorDir;
   
   ErrorModel.Weight(ii)=Weight;

   
end %Fin del loop sobre los plazos de pronostico.

%En este punto calculamos los parametros del modelo estadistico para
%representar los errores en U y V asumiendo una Gaussiana bivariada.
%Dichos parametros fueron obtenidos a partir de muestras correspondientes a
%valores pronsticados similares cuando eso fue posible o de lo contrario
%reemplazada por valores estimados a partir de la muestra global de
%errores.

%==========================================================================
%  APLICO EL SUAVIZADO EN TIEMPO (A LO LARGO DE LOS PLAZOS DE PRONSOTICOS)
%  DE LOS PARAMETROS ESTIMADOS. 
%  Esto se hace siempre y cuando el parametro SmoothFlag sea true.
%  El suavizado se aplica sobre los parametros de la distribucion de
%  errores, pero no sobre los valores pronosticados.
%==========================================================================

if( SmoothFlag )

    ErrorModel.MeanErrorU=smooth(ErrorModel.MeanErrorU,'rlowess',SmoothDegree)';
    ErrorModel.MeanErrorV=smooth(ErrorModel.MeanErrorV,'rlowess',SmoothDegree)';
    
    ErrorModel.StdErrorU=smooth(ErrorModel.StdErrorU,'rlowess',SmoothDegree)';
    ErrorModel.StdErrorV=smooth(ErrorModel.StdErrorV,'rlowess',SmoothDegree)';
    
    ErrorModel.ErrorCorrUV=smooth(ErrorModel.ErrorCorrUV,'rlowess',SmoothDegree)';
    
    ErrorModel.MeanErrorVel=smooth(ErrorModel.MeanErrorVel,'rlowess',SmoothDegree)';
    %ErrorModel.MeanErrorDir=smooth(ErrorModel.MeanErrorDir,'rlowess',SmoothDegree)';
    
    ErrorModel.StdErrorVel=smooth(ErrorModel.StdErrorVel,'rlowess',SmoothDegree)';
    ErrorModel.StdErrorDir=smooth(ErrorModel.StdErrorDir,'rlowess',SmoothDegree)';
    
end

%==========================================================================
% GENERO UN PRONOSTICO CORREGIDO EN TERMINOS DE U,V, VEL Y DIR
%==========================================================================

ErrorModel.VelUncalibrated=CVelFor;
ErrorModel.DirUncalibrated=CDirFor;

ErrorModel.UUncalibrated=CUFor;
ErrorModel.VUncalibrated=CVFor;

ErrorModel.UCalibrated=ErrorModel.UUncalibrated-ErrorModel.MeanErrorU;
ErrorModel.VCalibrated=ErrorModel.VUncalibrated-ErrorModel.MeanErrorV;

%[ErrorModel.VelCalibrated ErrorModel.DirCalibrated]= UVToVelDir(ErrorModel.UCalibrated,ErrorModel.VCalibrated);

ErrorModel.VelCalibrated=ErrorModel.VelUncalibrated-ErrorModel.MeanErrorVel;
%ErrorModel.DirCalibrated=ErrorModel.DirUncalibrated-ErrorModel.MeanErrorDir;

%La direccion corregida se puede ir eventualmente del rango 0 - 360, si ese
%es el caso corrijo la direccion. 

for ii=1:NLead
   if( ErrorModel.DirCalibrated(ii) > 360 );
       ErrorModel.DirCalibrated(ii) = ErrorModel.DirCalibrated(ii)-360;
   elseif( ErrorModel.DirCalibrated(ii) < 0 );
       ErrorModel.DirCalibrated(ii) = ErrorModel.DirCalibrated(ii)+360;
   end
end

%==========================================================================
% GENERO LAS ESTADISTICAS PARA REPRODUCIR LA VARIABILIDAD TEMPORAL DE LOS
% ERRORES (LA MATRIZ DE CORRELACION DE LOS ERRORES EN TIEMPO PARA PODER
% LUEGO MODELAR LA EVOLUCION TEMPORAL DE LOS ERRORES).
%==========================================================================

%==========================================================================
% CALCULAMOS LA MATRIZ DE COVARIANZA EN TIEMPO DE LOS ERRORES.
% esto nos permite modelar como estan correlacionados los errores del
% pronostico en diferentes plazos. De esta manera podemos modelar errores
% que sean realistas respecto de la variacion temporal de los errores.
%==========================================================================
  
%Lo primero que vamos a generar es una serie temporal de numeros aleatorios
%con distribucion N(0,1) para cada plazo, pero correlacionados en tiempo
%(es decir que los errores que asumimos en diferentes plazos no son
%independientes entre si). La matriz de correlacion de los errores en
%tiempo que estimamos a partir de la muestra de los errores de U y V nos
%dara la informacion de como estan correlacionados en tiempo los errores.
    
%ErrorModel.CorrelationStructU=GetCorrelationStruct( ErrorU );
%ErrorModel.CorrelationStructV=GetCorrelationStruct( ErrorV );
    
end














