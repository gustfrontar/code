
function [ CVelFor CDirFor ]=DoyLeadClimatologyFun( CDate , VelObsDb , DirObsDb , DatesDb )

%Esta funcion utiliza una climatologia segmentada solo por epoca del anio y
%hora del dia (no se usa ningun conocimiento adicional ya sea observado o
%modelado).

%INPUTS
%CDate: La fecha de inicio del pronostico en formato 'yyyymmddhh')
%VelObsDb: Contiene los valores de intensidad de viento observados que
%corresponden a cada uno de los pronosticos de viento contenidos en
%VelForDb.
%DirObsDb es analogo a VelForDb pero para la direccion del viento.
%DatesDb es un vector contiendo las fechas de inicio de cada pronostico en
%el formato 'yyyymmddhh';
%Order representa cuantos tiempos para atras vamos a promediar las ultimas
%observaciones disponibles. El pronostico y la segmentacion de la
%climatologia lo vamos a hacer en funcion de estos valores.

%OUTPUT
%CVelFor y CDirFor son las velocidades y las direcciones pronosticadas para
%las proximas horas.

display(['Realizando la calibracion en base a velocidad , epoca del anio y plazo de pronostico'])

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

DDir=45;  %Cuanto sera la ventana en direccion que usamos para corregir el pronostico.
DVel=2.5; %Cuanto sera la ventana en velocidad que usamos para corregir el pronostico.
DDay=60; %Cuanto sera la ventana de tiempo en dias que usaremos para corregir el pronostico.
DLead=3; %Cuanto sera la ventana en plazo de pronostico que usaremos para corregir el pronostico.

%Estos parametros controlan la aplicacion de un suavizado posterior a la
%calibracion (esto esta pensando tendiente a reducir el ruido que puede
%introducirse debido a lo pequenio de las muestras utilizadas). Una vez
%realizada la calibracion, la idea es suavizar las correcciones en U y V y
%suavizar las estimaciones de las desviaciones estandard y la covarianza de
%manera tal de que su evolucion temporal en el plazo de pronostico sea lo
%mas suave posible.
SmoothFlag=false; %Si aplico o no el suavizado (true aplico el suavizado)
SmoothDegree=5;  %Grado de suavizado que se aplicara en las correccioes.

CDateNum=datenum( CDate );
DateNum=datenum( DatesDb );

NLead=size(VelObsDb,2);

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
 
 DoyVelObs=VelObsDb(TimeWindowIndex,:);
 DoyDirObs=DirObsDb(TimeWindowIndex,:);
 


 NForDb=sum(TimeWindowIndex);

 
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
   
   %Estas son las velocidades y direcciones pronosticadas correspondientes
   %a un plazos cercanos al plazo actual y a la misma epoca del anio.
   DoyLeadVelObs=reshape(DoyVelObs(:,min_lead:max_lead),[TmpNLead*NForDb 1]);
   DoyLeadDirObs=reshape(DoyDirObs(:,min_lead:max_lead),[TmpNLead*NForDb 1]);
   
   
   
   CVelFor(ii)=nanmean(DoyLeadVelObs);
   CDirFor(ii)=MeanDir(DoyLeadDirObs,ones(length(DoyLeadVelObs),1));
   
  
end %Fin del loop sobre los plazos de pronostico.


    
end














