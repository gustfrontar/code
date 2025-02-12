
function [ CHybridVel RegressionCoefficients ]=HybridForecastErrorFun( CVelFor , CVelObs , CDate , VelForDb , VelObsDb  , DatesDb , Order )


%Esta funcion estima el error del pronostico en un plazo determinado
%aplicando un modelo multiple lineal cuyos predictores son el error del
%modelo en tiempos anteriores.

%INPUTS
%CVelFor: Un vector conteniendo N pronosticos de intensidad de viento.
%CVelObs: Un vector conteniendo N observaciones de intensidad de viento
%(solamente vamos a usar las primeras Lead componentes de este vector).
%CDate: La fecha de inicio del pronostico en formato 'yyyymmddhh')
%VelForDb: Un array conteniendo la base de datos historica de pronosticos
%de intensidad de viento. Cada fila corresponde a un pronostico y cada
%columna corresponde a un plazo de pronostico. La cantidad de columnas es
%igual a la longitud de los vectores CVelFor y CDirFor.
%VelObsDb: Contiene los valores de intensidad de viento observados que
%corresponden a cada uno de los pronosticos de viento contenidos en
%VelForDb.
%DatesDb es un vector contiendo las fechas de inicio de cada pronostico en
%el formato 'yyyymmddhh';
%Order indica cuantas observaciones previas vamos a utilizar en el modelo
%multiple lineal.
%Hasta que plazo de pronostico asumo que tengo observaciones.

%OUTPUT
%MeanErrorVel Media del error en velocidad para cada plazo de pronostico.
%MeanErrorDir Media del error en direccion para cada plazo de pronsotico.
%StdErrorVel  Desviacion estandard del error en velocidad para cada plazo
%de pronostico.
%StdErrorDir  Desviacion estandard del error en direccion para cada plazo
%de pronostico.

display(['Realizando el pronostico hybrido'])

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

DDay=60; %Cuanto sera la ventana de tiempo en dias que usaremos para corregir el pronostico.
DLead=3; %Cuanto sera la ventana en plazo de pronostico que usaremos para corregir el pronostico.

CDateNum=datenum( CDate );
DateNum=datenum( DatesDb );

%==========================================================================
% OBTENGO LAS DIMENSIONES DE LAS VARIABLES DE ENTRADA.
%==========================================================================

NLead=length(CVelFor);

%==========================================================================
% DETECTO HASTA DONDE TENGO DATOS
%==========================================================================

tmp=find(isnan(CVelObs));

Lead=tmp(1)-1;  %Las observaciones llegan hasta este Lead time. Uso la ultima observacion disponible y las anteriores hasta el tiempo Order.  



%==========================================================================
%RETENEMOS SOLO LOS PRONOSTICOS PASADOS QUE CORRESPONDEN A UNA MISMA EPOCA
%DEL ANIO (esto es debido a que el comportamiento de los errores
%sistematicos depende mucho de la epoca del anio).
%Calculo el dia del anio (esto es necesario para elegir los pronosticos
%pasados que se encuentren en un rango de fechas similar al pronostico
%actual).
%==========================================================================

doy = DateNum - datenum(year(DateNum),1,1) + 1;  %Day of year de cada pronostico en la base de datos
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
 
 DoyVelError=VelForDb(TimeWindowIndex,:)-VelObsDb(TimeWindowIndex,:);
 
 NForDb=sum(TimeWindowIndex);
 
 CError=CVelFor - CVelObs;

%==========================================================================
%EN ESTE PUNTO ARRANCAMOS UN LOOP SOBRE LOS PLAZOS DE PRONOSTICO, PARA CADA PLAZO
%VAMOS A CONSIDERAR EL VALOR PRONOSTICADO Y BUSCAR TODOS LOS PRONOSTICOS 
%PASADOS QUE ESTEN EN UN RANGO DE VALORES CERCANO AL VALOR PRONOSTICADO.
%==========================================================================

 
for ii=1:NLead   %ii es el indice sobre los plazos de pronostico.

%==========================================================================
% ARMAMOS EL MODELO MULTIPLE LINEAL CON AQUELLOS PRONOSTICOS QUE
% CORRESPONDEN A PLAZOS SIMILARES.
%==========================================================================
 
 if( ii >= Lead )

   max_lead=ii+DLead;
   min_lead=ii-DLead;

   if( max_lead > NLead); max_lead = NLead ; end
   if( min_lead < 1    ); min_lead = 1     ; end

   TmpNLead= max_lead - min_lead  + 1;

   %Los predictores seran los errores del pronostico en los tiempos
   %anteriores a Lead (hasta el tiempo Lead-Order+1)
   
   X=repmat(squeeze(DoyVelError(:,Lead)),[TmpNLead 1]);
   for io=2:Order 
      X(:,io)=repmat(squeeze(DoyVelError(:,Lead-io+1)),[TmpNLead 1]);
   end
   
    %Usamos la funcion robust fit de matlab para estimar los coeficientes del
    %modelo multiple lineal que incluye Order observaciones y el pronostico
    %dado por el modelo. 
   
    Y=reshape(DoyVelError(:,min_lead:max_lead),[TmpNLead*NForDb 1]);
   
    RegressionCoefficients(ii,:) = robustfit(X,Y,'bisquare'); 
    
    ErrorVel(ii) = RegressionCoefficients(ii,1) ;

    for io=1:Order

        ErrorVel(ii) = ErrorVel(ii) + CError(Lead-io+1)*RegressionCoefficients(ii,1+io);
        
    end
    
    CHybridVel(ii)=CVelFor(ii) - ErrorVel(ii);

   
    SampleSize(ii)=TmpNLead*NForDb;
 
 else
     
   SampleSize(ii)=NaN;
   CHybridVel(ii)=NaN;
   RegressionCoeficcients(ii,:)=NaN(Order+1,1);
   
 end
   
   
end %Fin del loop sobre los plazos de pronostico.


    
end














