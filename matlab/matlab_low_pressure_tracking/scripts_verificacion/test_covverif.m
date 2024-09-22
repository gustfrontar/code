clear all
close all

addpath('../common_functions_vpo/');
%Este script sirve para testear la rutina de verificacion de la covarianza.
%Vamos a generar muestras de variables aleatorias conjuntamente
%distribuidas para ver el comportamiento de los coeficientes de correlacion
%de los errores y de las perturbaciones del ensamble entre si en cada caso.


%PRIMER CASO EL ERROR Y EL ENSAMBLE VIENEN DE UNA DISTRIBUCION NORMAL
%CONJUNTA QUE NO TIENE CORRELACION ENTRE LAS VARIABLES (TODAS LAS
%CORRELACIONES SALEN SIMPLEMENTE DEL ERROR DE MUESTREO). 

SIGMA=1;
COVAR=0.5;
MEDIA=0;
ENSSIZE=20;
SAMPLESIZE=1e4;
NTIMES=3;

pertlon=MEDIA+SIGMA*randn(ENSSIZE,NTIMES,SAMPLESIZE);
pertlat=MEDIA+SIGMA*randn(ENSSIZE,NTIMES,SAMPLESIZE);

meanerrorlon=MEDIA+SIGMA*randn(NTIMES,SAMPLESIZE);
meanerrorlat=MEDIA+SIGMA*randn(NTIMES,SAMPLESIZE);


[correlacionerror correlacionerrorideal tercilesr]=covariance_verif_fun(pertlon,pertlat,meanerrorlon,meanerrorlat);

%En este caso como era de esperarse las correlaciones condicionales
%resultaron casi nulas. Es decir el error no esta mas correlacionado en los
%casos que la covarianza es grande. No obstante el error ideal sigue
%mostrando importantes correlaciones condicionales, esto es porque este
%error asume que el ensamble pronostica perfectamente el spread de la pdf
%por lo tanto asume que el error de muestreo es nulo. Esto tambien quiere
%decir que el error ideal asi definido no permite conocer el potencial del
%ensamble o el potencial de existencia de una relacion entre el spread y el
%error, porque aun en la ausencia total de dicha relacion el error ideal
%indica que la misma podria alcanzarse.


%CASO 2: LOS ERRORES VIENEN DE UNA DISTRIBUCION NORMAL CONJUNTA QUE TIENE
%UNA CIERTA COVARIANZA, CONSTANTE EN EL TIEMPO
tmp=mvnrnd([MEDIA MEDIA],[SIGMA COVAR;COVAR SIGMA],SAMPLESIZE*NTIMES*ENSSIZE);

pertlon=reshape(tmp(:,1),[ENSSIZE NTIMES SAMPLESIZE]);
pertlat=reshape(tmp(:,2),[ENSSIZE NTIMES SAMPLESIZE]);

tmp=mvnrnd([MEDIA MEDIA],[SIGMA COVAR;COVAR SIGMA],SAMPLESIZE*NTIMES);

meanerrorlon=reshape(tmp(:,1),[NTIMES SAMPLESIZE]);
meanerrorlat=reshape(tmp(:,2),[NTIMES SAMPLESIZE]);

[correlacionerror correlacionerrorideal tercilesr]=covariance_verif_fun(pertlon,pertlat,meanerrorlon,meanerrorlat);

%En este caso, los errores estan correlacionados entre si, pero no hay una
%sensibilidad a la covarianza del ensamble, porque las variaciones de dicha
%covarianza estan nuevamente dominadas solamente por el error de muestreo.

%CASO 3, EL ENSAMBLE Y LOS ERRORES PROVIENEN DE UNA MUESTRA EN DONDE LA
%COVARIANZA DEL ENSAMBLE VARIA CON EL TIEMPO EN FORMA ALEATORIA, PERO TANTO
%EL ENSAMBLE COMO LOS ERRORES PROVIENEN DE LA MISMA POBLACION EN CADA
%TIEMPO.

SIGMA2=0.5;
for ii=1:SAMPLESIZE
    COVAR=SIGMA2*randn;
    if( COVAR > 1)
        COVAR=1;
    end
    if( COVAR < -1)
        COVAR=-1
    end
    
tmp=mvnrnd([MEDIA MEDIA],[SIGMA COVAR;COVAR SIGMA],NTIMES*ENSSIZE);

pertlon(:,:,ii)=reshape(tmp(:,1),[ENSSIZE NTIMES]);
pertlat(:,:,ii)=reshape(tmp(:,2),[ENSSIZE NTIMES]);

tmp=mvnrnd([MEDIA MEDIA],[SIGMA COVAR;COVAR SIGMA],NTIMES);

meanerrorlon(:,ii)=tmp(:,1);
meanerrorlat(:,ii)=tmp(:,2);
end

[correlacionerror correlacionerrorideal tercilesr]=covariance_verif_fun(pertlon,pertlat,meanerrorlon,meanerrorlat);

%En este caso si hay sensibilidad de la correlacion del error a los cambios
%en la covarianza. Esta sensibilidad es mayor cuanto mayor es la
%variabilidad temporal de la covarianza (en el limite de esta variabilidad
%tendiendo a 0, esto se reduce al caso 1 y la sensibilidad se pierde
%totalmente.)
%Por otro lado, el error ideal marca el efecto del error de muestreo.
%cuando el error de muestreo es grande respecto de la sensibilidad hay
%mucha diferencia entre la sensibilidad del error ideal y la sensibilidad
%del error real (aun cuando error y ensamble provienen de la misma
%poblacion). Mientras que cuando la variablidad temporal de la covarianza
%es mayor entonces la relacion entre el error ideal y el error real se
%achica (el error de muestreo es relativamente menos importante que los
%cambios reales que ocurren en la covarianza, es un efecto del tipo signal
%to noise ratio). Es importante tener en cuenta que en este experimento el
%error de muestreo siempre fue el mismo, porque este es funcion del tamnio
%del ensamble.

%En resumen este analisis permite mostrar si existe alguna relacion entre
%la covarianza derivada del ensamble y el error. La sensibilidad de la
%correlacion de los errores a la covarianza pronosticada por el modelo es
%mayor cuanto mayor es la variabilidad temporal de la covarianza y tambien
%cuanto mejor el ensamble puede representar esa variabilidad.

%Otras cosas a explorar a futuro son tambien como impactan los errores de
%las observaciones en estas cuestiones.



