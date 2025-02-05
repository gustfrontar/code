clear all
close all

%Test minimizador.
x=randn(10,1);
x0=ones(10,1);
options = optimset('GradObj','on');
[minX] = fminunc('funcionfacil',x,options,x0);

