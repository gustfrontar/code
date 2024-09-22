clear all
close all

%Este script sirve para testear la rutina de pit histogram nueva.

%Parametros de la distribucion desde donde voy a sacar los miembros del
%ensamble y la verificacion.
SIGMA=1;
MEDIA=0;
ENSSIZE=20;
SAMPLESIZE=1e5;

ENSEMBLE=MEDIA+SIGMA*randn(SAMPLESIZE,ENSSIZE);
OBS=MEDIA+SIGMA*randn(SAMPLESIZE,1);




[PitHist]=compute_pit_histogram_fun(ENSEMBLE,OBS);