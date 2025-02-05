
function [ CHybridVel RegressionCoefficients ]=HybridForecastFun( CVelFor , CVelObs , CDate , VelForDb , VelObsDb  , DatesDb , Order )


%Esta funcion realiza una combinacion de los pronosticos de velocidad
%basados puramente en las ultimas potencias observadas disponibles y en las
%potencias derivadas del modelo numerico calibrado. 

%INPUTS
%CVelFor: Un vector conteniendo N pronosticos de velocidad producida por el
%parque.
%CVelObs: Un vector conteniendo N observaciones de velocidad producida por
%el parque.
%(solamente vamos a usar las primeras Lead componentes de este vector).
%CDate: La fecha de inicio del pronostico en formato 'yyyymmddhh')
%VelForDb: Un array conteniendo la base de datos historica de pronosticos
%de velocidad. Cada fila corresponde a un pronostico y cada
%columna corresponde a un plazo de pronostico. 
%VelObsDb: Contiene los valores de velocidad observados que
%corresponden a cada uno de los pronosticos de velocidad contenidos en
%VelForDb.
%DatesDb es un vector contiendo las fechas de inicio de cada pronostico en
%el formato 'yyyymmddhh';
%Order indica cuantas observaciones previas vamos a utilizar en el modelo
%multiple lineal.
%Hasta que plazo de pronostico asumo que tengo observaciones.

%OUTPUT
%CHybridVel un vector conteniendo los pronosticos de velocidad que resultan
%de la combinacion lineal de las ultimas observaciones de velocidad
%disponibles y del pronostico numerico. 
%RegressionCoefficients un arreglo que contiene tantas columnas como
%predictores se utilizaron (Order observaciones + el pronostico numerico y
%el termino independiente de la regresion) y tantas filas como plazos de
%pronostico hay. RegressionCoefficientes contiene los coeficientes del
%modelo multiple lineal ajustado para cada plazo de pronostico. Notar que
%el modelo tambien va cambiando segun la epoca del anio, ya que para
%entrenar los coeficientes se utilizan solo los pronosticos historicos 

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
 
 DoyVelFor=VelForDb(TimeWindowIndex,:);
 DoyVelObs=VelObsDb(TimeWindowIndex,:);
 
 NForDb=sum(TimeWindowIndex);

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

   %Estas son las velocidades y direcciones pronosticadas correspondientes
   %a un plazos cercanos al plazo actual y a la misma epoca del anio.
   DoyLeadVelFor=reshape(DoyVelFor(:,min_lead:max_lead),[TmpNLead*NForDb 1]);
   
   DoyLeadVelObs=repmat(squeeze(DoyVelObs(:,Lead)),[TmpNLead 1]);
   for io=2:Order 
      DoyLeadVelObs(:,io)=repmat(squeeze(DoyVelObs(:,Lead-io+1)),[TmpNLead 1]);
   end
    %Usamos la funcion robust fit de matlab para estimar los coeficientes del
    %modelo multiple lineal que incluye Order observaciones y el pronostico
    %dado por el modelo. 
   
    X=[ DoyLeadVelFor DoyLeadVelObs];
    
    Y=reshape(DoyVelObs(:,min_lead:max_lead),[TmpNLead*NForDb 1]);
   
    RegressionCoefficients(ii,:) = robustfit(X,Y,'bisquare');
   
    
    CHybridVel(ii) = RegressionCoefficients(ii,1) + CVelFor(ii)*RegressionCoefficients(ii,2);

    for io=1:Order

        CHybridVel(ii) = CHybridVel(ii) + CVelObs(Lead-io+1)*RegressionCoefficients(ii,2+io);
        
    end

   
    SampleSize(ii)=TmpNLead*NForDb;
 
 else
     
   SampleSize(ii)=NaN;
   CHybridVel(ii)=NaN;
   RegressionCoeficcients(ii,:)=NaN(Order+2,1);
   
 end
   
   
end %Fin del loop sobre los plazos de pronostico.


    
end














