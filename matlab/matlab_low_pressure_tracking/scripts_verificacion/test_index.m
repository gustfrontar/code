clear all
close all
addpath('../common_functions_vpo/');
% Este script esta pensado para testear el funcionamiento del indice de
% resolucion en diferentes circunstancias.

%PRIMER CASO, EL ENSAMBLE Y LAS OBSERVACIONES VIENEN DE UNA DISTRIBUCION
%QUE ES SIEMPRE LA MISMA, ES DECIR QUE TODA FLUCTUACION EN EL SPREAD DEL
%ENSAMBLE SOLO PROVIENE DE UN PROBLEMA DE MUESTREO
SIGMA=1;
MEDIA=0;
ENSSIZE=20;
SAMPLESIZE=1e5;

ENSEMBLE=MEDIA+SIGMA*randn(SAMPLESIZE,ENSSIZE);
OBS=MEDIA+SIGMA*randn(SAMPLESIZE,1);

SPREAD=std(ENSEMBLE,[],2);
ERROR=(OBS-mean(ENSEMBLE,2)).^2;

[IR IRSS IP PDF U_SERIE2 RANGO_SERIE1]=resolution_index_fun(SPREAD,ERROR,5,5);

%En este caso como era de esperarse el indice marca un valor de 0, las
%lineas de la PDF son lineas rectas indicando que no hay relacion entre una
%y otra serie.

%SEGUNDO CASO, EN ESTE CASO VAMOS A HACER QUE EL ERROR PROVENGA DE UNA
%DISTRIBUCION QUE ES SIEMPRE LA MISMA, MIENTRAS QUE EL ENSAMBLE PROVIENE DE
%UNA DISTRIBUCION QUE VA CAMBIANDO CON EL TIEMPO. 

SIGMA=1;
MEDIA=0;
ENSSIZE=20;
SAMPLESIZE=1e5;

OBS=MEDIA+SIGMA*randn(SAMPLESIZE,1);

SIGMA2=0.2;     %SIGMA 2 MIDE CUANTO VARIA LA DISPERSION DEL ENSAMBLE DE UN DIA A OTRO.

for ii=1:SAMPLESIZE;
    
TMPSIGMA=SIGMA+SIGMA2*rand;
if(TMPSIGMA < 0.1)
    TMPSIGMA=0.1;
end
ENSEMBLE(ii,:)=MEDIA+TMPSIGMA*randn(1,ENSSIZE);
end


SPREAD=std(ENSEMBLE,[],2);
ERROR=(OBS-mean(ENSEMBLE,2)).^2;

[IR IRSS IP PDF U_SERIE2 RANGO_SERIE1]=resolution_index_fun(SPREAD,ERROR,25,25);

%Nuevamente en este caso, el indice da casi 0 y las curvas de PDF dan
%totalmente chatas.

%TERCER CASO, EN ESTE CASO TANTO EL ERROR COMO EL ENSAMBLE VIENEN DE UNA
%DISTRIBUCION CUYO SPREAD VARIA CON EL TIEMPO. PERO EN CADA TIEMPO AMBOS
%VIENEN DE LA MISMA DISTRIBUCION. NO OBSTANTE PERSISTE EL PROBLEMA DEL
%MUESTREO. 

SIGMA=1;
MEDIA=0;
ENSSIZE=20;
SAMPLESIZE=1e5;

%SIGMA2 EN ESTE CASO MIDE COMO VARIA EL SIGMA DE LA DISTRIBUCION DE DONDE
%VIENEN EL ENSAMBL Y EL ERROR. 
SIGMA2=2.0;     

for ii=1:SAMPLESIZE;
    
TMPSIGMA=SIGMA+SIGMA2*rand;

ENSEMBLE(ii,:)=MEDIA+TMPSIGMA*randn(1,ENSSIZE);
OBS(ii)=MEDIA+TMPSIGMA*randn;

end


SPREAD=std(ENSEMBLE,[],2);
ERROR=(OBS-mean(ENSEMBLE,2)).^2;

[IR IRSS IP PDF U_SERIE2 RANGO_SERIE1]=resolution_index_fun(SPREAD,ERROR,25,25);

%CUARTO CASO, ANALIZAMOS COMO ES LA DEPENDENCIA DEL INDICE RESPECTO DE LA
%RELACION A LA VARIACION DE LA INCERTIDUMBRE.

for jj=1:100
    jj
SIGMA=1;
MEDIA=0;
ENSSIZE=20;
SAMPLESIZE=1e5;

%SIGMA2 EN ESTE CASO MIDE COMO VARIA EL SIGMA DE LA DISTRIBUCION DE DONDE
%VIENEN EL ENSAMBL Y EL ERROR. 
SIGMA2=5.0*jj/10;     

TMPSIGMA=SIGMA+SIGMA2*rand(SAMPLESIZE,1);
TMPSIGMA=repmat(TMPSIGMA,[1 ENSSIZE]);
  

ENSEMBLE=MEDIA+TMPSIGMA.*randn(SAMPLESIZE,ENSSIZE);
OBS=MEDIA+TMPSIGMA(:,1).*randn(SAMPLESIZE,1);




SPREAD=std(ENSEMBLE,[],2);
ERROR=abs(OBS-mean(ENSEMBLE,2));

[IR(:,jj) IRSS(:,jj) IP PDF(:,:,jj)]=resolution_index_fun(SPREAD,ERROR,25,25);

end

%Hay dos propiedades poco deseables que salen a la luz con este
%experimento, por un lado el valor del indice depende de cuanto varie el
%spread o la impredictabilidad de la situacion con el tiempo. esto por un
%lado es logico porque en el caso limite en el que la variacion es nula es
%obvio que no puede haber una relacion coherente entre el error y el spread
%del ensemble porque toda variacion estaria dada simplemente por el error
%de muestreo. 
%la otra propiedad es que el valor del indice depende del umbral de error
%considerado. no es claro porque esta pasando esto.

%QUINTO CASO EL VALOR DE SPREAD Y EL DE ERROR SON EXACTAMENTE IGUALES. CASO
%DETERMINISTICO POCO REALISTA PERO SIRVE PARA EVALUAR EL CASO LIMITE DEL
%INDICE.

SIGMA=1;
MEDIA=0;
ENSSIZE=20;
SAMPLESIZE=1e5;

%SIGMA2 EN ESTE CASO MIDE COMO VARIA EL SIGMA DE LA DISTRIBUCION DE DONDE
%VIENEN EL ENSAMBL Y EL ERROR. 
SIGMA2=5.0;     

TMPSIGMA=SIGMA+SIGMA2*rand(SAMPLESIZE,1);
TMPSIGMA=repmat(TMPSIGMA,[1 ENSSIZE]);
  

%ENSEMBLE=MEDIA+TMPSIGMA.*randn(SAMPLESIZE,ENSSIZE);
OBS=MEDIA+TMPSIGMA(:,1).*randn(SAMPLESIZE,1);

SPREAD=std(ENSEMBLE,[],2);
ERROR=SPREAD;   

[IR IRSS IP PDF]=resolution_index_fun(SPREAD,ERROR,25,25);
%En este caso el indice da lo que se esperaba, el IR es 0 para todos los
%umbrales de error y el IRSS es 1. la forma de las PDF es una funcion
%escalon.


%SEXTO CASO TOMO 2 VARIABLES ALEATORIAS CON DISTRIBUCION NORMAL Y CALCULO
%EL INDICE PARA ELLAS. UNA DE LAS VARIABLES ES LA ANTERIOR MAS UN ERROR
%ALEATORIO.

SIGMA=1;
SIGMA2=0.1;
MEDIA=0;
ENSSIZE=20;
SAMPLESIZE=1e5;

SPREAD=MEDIA+SIGMA*randn(SAMPLESIZE,1);
ERROR=SPREAD+SIGMA2*randn(SAMPLESIZE,1);   

[IR IRSS IP PDF]=resolution_index_fun(SPREAD,ERROR,10,10);
%En este caso el indice da lo que se esperaba, el IR es 0 para todos los
%umbrales de error y el IRSS es 1. la forma de las PDF es una funcion
%escalon.
%Este experimento muestra que aun cuando ambas variables son normales y las
%diferencias entre ellas tambien lo son (i.e. una esta altamente
%correlacionada con la otra), el valor del indice es dependiente del valor
%del umbral.


%RESUMEN HASTA AHORA.
%pudimos mostrar que el indice de resolucion captura la relacion entre
%error y spread, y que en los casos limites se comporta como se espera.
%no obstante no se puede establecer un valor optimo para este indice ya que
%el valor limite no es realista en el sentido que la distribucion spread
%error nunca va a ser deterministica.
%una alternativa seria considerar que en cada caso el error sale de una
%distribucion con el mismo spread provisto por el ensamble. Y calcular el
%indice para ese caso. Esto tendria en cuenta el error de muestreo, ya que
%se asumiria que el valor de spread provisto por el ensamble es el
%verdadero valor que mide la incertidumbre del pronostico.



