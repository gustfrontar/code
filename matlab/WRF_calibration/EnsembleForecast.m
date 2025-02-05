

function [ EnsembleForecast ]=EnsembleForecast( ErrorModel )

EnableEnsemble=true;  %Si activamos o no activamos el ensamble (true indica que se activa el ensamble).
EnsembleSize=1000;    %Numero de pronosticos por ensamble que vamos a generar. Cuantos mas miembros o pronosticos generemos,
                      %mas precisa sera la traduccion de la incertidumbre
                      %en viento a la incertidumbre en potencia.

                      
                   
%Lo primero que vamos a generar es una serie temporal de numeros aleatorios
%con distribucion N(0,1) para cada plazo, pero correlacionados en tiempo
%(es decir que los errores que asumimos en diferentes plazos no son
%independientes entre si). La matriz de correlacion de los errores en
%tiempo que estimamos a partir de la muestra de los errores de U y V nos
%dara la informacion de como estan correlacionados en tiempo los errores.



    %Calculo la raiz cuadrada de la matriz de correlacion lo cual me
    %permitira obtener series correlacionadas en tiempo a partir de series
    %de numeros aleatorios no correlacionadas en tiempo.
    SqrtCorrelationMatrixU=sqrt(CorrelationMatrixU);
    SqrtCorrelationMatrixV=sqrt(CorrelationMatrixV);
    
    %Inicializamos las perturbaciones del ensamble (que representan
    %posibles valores del error) como EnsembleSize series de largo NLead
    %que inicialmente no tienen correlacion en tiempo y su amplitud no
    %depende del pronostico de hoy (eso lo vamos a ir modificando a
    %continuacion para que estas perturbaciones sean consistentes con la
    %estadistica de los errores que encontramos en los calculos
    %anteriores).
    
    CUEnsPert=randn(EnsembleSize,NLead);
    CVEnsPert=randn(EnsembleSize,NLead);
    
    %Introduzco la correlacion temporal en las perturbaciones, para que su
    %correlacion temporal sea la misma que tienen los errores de U y V.
    for iens=1:EnsembleSize
        CUEnsPert(iens,:)=SqrtCorrelationMatrixU*CUEnsPert(iens,:)';
        CVEnsPert(iens,:)=SqrtCorrelationMatrixV*CVEnsPert(iens,:)';
        
        
    end
    

    %TODO: Generar las perturbaciones del ensamble. Generar el esnamble
    %centrado en el pronostico corregido. Traducir los miembros del
    %ensamble a Vel y Dir. 
    %Agregar los miembros del ensamble a la estrucutra de salida (solo en
    %terminos de U y V, Vel y Dir se pueden calcular luego on-demand.
    
end