clear all
close all
addpath('../common_functions_vpo/');

%PRUEBA EFICIENCIA CALCULO DISTANCIAS.

alon=1;
blon=359;
alat=-2;
blat=3;

tic
dist1=distll_fun(alon,alat,blon,blat);
toc

tic
dist2=distll_fast_fun(alon,alat,blon,blat);
toc

dist1
dist2