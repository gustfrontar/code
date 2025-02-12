clear all
close all

%Este script lee los archivos de salida del WRF interpolados a 1�x1� y genera campos de probabilidad de lluvia y diagramas para verificar la bondad del mismo.
%
%Juan Ruiz- 2006
%
%Cargo la variable con los nombres de archivo y las caracteristicas del archivo.

precip1d_wrf_common

%Defino los umbrales de precipitacion con los que voy a trabajar:

umb=[1 5 10 15 20 30 50 75 100];
numb=length(umb);

%Especifico si voy a tomar el archivo entero o a recortar una subregion.

%*******************************************************************
%Numero de puntos en x y en y de los campos presentes en el archivo.
%*******************************************************************

xmax=nxmax;
xmin=1;
ymax=nymax;
ymin=1;
nx=xmax-xmin+1;
ny=ymax-ymin+1;
forecast_time='f24'

forecast_ver_time='12';
offset=2;



narch=length(arch)-2;
%Aca comienza el ciclo en tiempo.

%for i_time=1+offset:narch-2+offset;
for i_time=2:3;

arch_verif=strcat('/mnt/windows/SLAFexperiment2/CPCprecip/',arch(i_time+offset,:),forecast_ver_time,'CMORPHdailyprecip')
arch_ensemble=strcat('/mnt/windows/SLAFexperiment2/CPCprecip/',arch(i_time+offset,:),forecast_ver_time,'wrf',forecast_time(2:3))
narch_verif=fopen(arch_verif,'r','l')
narch_ensemble=fopen(arch_ensemble,'r','l')
undef=1.E35; %Valor del undef en el ctl.

%Pregunto si no son -1 porque si son -1 quiere decir que no encontro el archivo.
if (narch_verif~=-1 & narch_ensemble ~=-1);


%Leemos las variables del control.
for i=1:nvar_verif
nada=fread(narch_verif,[2 1],'single')';
var_verif(:,:,i)=fread(narch_verif,record_size,'single')';
end

%Leemos las variables del ensemble.
for i=1:nvar_ensemble
nada=fread(narch_ensemble,[2 1],'single')';
var_ensemble(:,:,i)=fread(narch_ensemble,record_size,'single')';
end


var_ensemble(find(var_ensemble>=1000000))=NaN;
var_verif(find(var_verif>=1000000))=NaN;



%************************************************************************
%Vamos a calcular la probabilidad de precipitacion para distintos umbrales.
%************************************************************************
for i=1:numb %Este do barre sobre los diferentes umbrales de precipitacion.

aux=zeros(ny,nx);
probabilidad(:,:,i)=zeros(ny,nx);

    for k=1:nvar_ensemble %Este do barre sobre los distintos miembros del ensemble para calcular la probabilidad asociada
    %al umbral umb(i)
    aux(find(var_ensemble(ymin:ymax,xmin:xmax,k)>=umb(i)))=1;
    probabilidad(:,:,i)=probabilidad(:,:,i)+aux/nvar_ensemble;
    end
    
end

%************************************************************************
%Ya calculamos la probabilidad de lluvia para un dia.
%Ahora resta saber para cada rango de probabilidad cual es la probabilidad
%real de ocurrencia del fenomeno.
%************************************************************************

for i=1:numb

for k=1:nvar_ensemble %Vamos a tener tantos intervalos de probabilidad como miembros tiene el ensemble.
    aux2=probabilidad(:,:,i);
    clear j;
    clear j2;
    prob(k)=(k-0.5)/nvar_ensemble;
    j=find(aux2>=(k-1)/nvar_ensemble & aux2<(k/nvar_ensemble));
    j2=find(aux2>=(k-1)/nvar_ensemble & aux2<(k/nvar_ensemble) & var_verif(ymin:ymax,xmin:xmax,1)>= umb(i));
    n_verif(k,i)=length(j);      %Numero total de puntos donde la probabilidad del ensemble esta en un rango dado.
    lluvia_verif(k,i)=length(j2);%Numero de puntos dentro de dicho rango donde efectivamente llovio.
    %Aux es 1 donde la probabilidad esta dentro del rango seleccionado.
    %n_verif(k) es el numero total de puntos que tienen una probabilidad entre (k-1)/nvar_ensemble y k/nvar_ensemble.
    probabilidad_real(k,i)=lluvia_verif(k,i)/n_verif(k,i);
    
end


end
    plot(prob,probabilidad_real(:,2))


end %Aqui termina el if sobre narch_ensemble y narch_control
end %Aqui finaliza el horario de proteccion al menor (y el ciclo en tiempo)
