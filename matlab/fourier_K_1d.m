clear all
close all
%Este script lee los archivos de salida del WRF y calcula el espectro de la energia cinetica promediado en la vertical.
%El espectro que se calcula es en una sola dimension tal como esta en Skamarock (2004) MWR. Para comparar con los datos
%de Lindborg (1999) J. Fluid Mechanics.
%Para que todo ande bien usar siempre un numero par de puntos, al menos en la direccion W-E.
%Juan Ruiz- 2006
%

fourier2d_wrf_common



%*******************************************************************
%Numero de puntos en x y en y de los campos presentes en el archivo.
%*******************************************************************

xmax=89
xmin=10
ymax=40
ymin=10
nx=xmax-xmin+1;
ny=ymax-ymin+1;
forecast_time='f48'
if (str2num(forecast_time)==str2num('f48'))
forecast_ver_time='12';
offset=2;
end

narch=length(arch)-2;

archivos_encontrados=0
for i_time=1+offset:narch-2+offset;

arch_control=strcat('../control/',arch(i_time+offset,:),forecast_ver_time,forecast_time,'.dat')
arch_ensemble=strcat('../ensemble/',arch(i_time+offset,:),forecast_ver_time,forecast_time,'mean.dat')
narch_control=fopen(arch_control,'r','l');
narch_ensemble=fopen(arch_ensemble,'r','l');
undef=1.E35; %Valor del undef en el ctl.


undef=1.E35; %Valor del undef en el ctl.
nvar_control=15; %numero de variables en el archivo.
nvar_ensemble=13; %numero de variables en el archivo del ensemble.

%Pregunto si no son -1 porque si son -1 quiere decir que no encontro el archivo.
if (narch_control~=-1 & narch_ensemble ~=-1);
    
%nxmax y nymax controlan la dimension de las matrices dentro del archivo.


record_size=[nxmax nymax];
 

%Leemos las variables del control.
for i=1:nvar_control
nada=fread(narch_control,[2 1],'single')';
var_control(:,:,i)=fread(narch_control,record_size,'single')';
end

%Leemos las variables del ensemble.
for i=1:nvar_ensemble
nada=fread(narch_ensemble,[2 1],'single')';
var_ensemble(:,:,i)=fread(narch_ensemble,record_size,'single')';
end


var_ensemble(find(var_ensemble>=1000000))=inf;
var_control(find(var_control>=1000000))=inf;

%**************************************************************************************************************
%Vamos a trabajar con la energia cinetica en 500 hpa y 250 hPa.
%**************************************************************************************************************

k500.control=(var_control(ymin:ymax,xmin:xmax,8).^2+var_control(ymin:ymax,xmin:xmax,9).^2)*0.5;
k500.ensemble=(var_ensemble(ymin:ymax,xmin:xmax,8).^2+var_ensemble(ymin:ymax,xmin:xmax,9).^2)*0.5;
k250.control=(var_control(ymin:ymax,xmin:xmax,10).^2+var_control(ymin:ymax,xmin:xmax,11).^2)*0.5;
k250.ensemble=(var_ensemble(ymin:ymax,xmin:xmax,10).^2+var_ensemble(ymin:ymax,xmin:xmax,11).^2)*0.5;


%**************************************************************************************************************
%Removemos la tendencia.
%**************************************************************************************************************

%for i=1:ny
%    k500.control(i,:)=detrend(k500.control(i,:),'linear');
%    k500.ensemble(i,:)=detrend(k500.ensemble(i,:),'linear');
%    k250.control(i,:)=detrend(k250.control(i,:),'linear');
%    k250.ensemble(i,:)=detrend(k250.ensemble(i,:),'linear');
%end

%Para esta metodologia debemos sacar solo la tendencia en la direccion de x, la otra aportra ruido y molesta.
k500.control=detrend3(k500.control,2);
k500.ensemble=detrend3(k500.ensemble,2);
k250.control=detrend3(k250.control,2);
k250.ensemble=detrend3(k250.ensemble,2);

%**************************************************************************************************************
%Para cada latitud constante calculamos el espectro unidimensional del campo en la longitud W-E
%luego promediamos dichos espectros para todas las latitudes comprendidas en nuestro dominio.
%**************************************************************************************************************
aux=fft(k500.control(1,1:nx)); %Hago una primera transformacion para calcular la frecuencia nyquist.
nyquist=length(aux)/2;


%Espectro 500 control
clear aux2;
aux2=zeros(1,nyquist);
for i=1:ny %Calculo el espectro para distintas latitudes y lo voy sumando.
    aux=fft(k500.control(i,1:nx))/(nx);
    aux2(1:(nyquist-1))=aux2(1:(nyquist-1))+abs(aux(2:nyquist))*2;
    aux2(nyquist)=aux2(nyquist)+aux(nyquist+1);
end

espectra500.control=aux2/(ny);


%Espectro 250 contro
clear aux2;
aux2=zeros(1,nyquist);
for i=1:ny %Calculo el espectro para distintas latitudes y lo voy sumando.
    aux=fft(k250.control(i,1:nx))/(nx);
    aux2(1:(nyquist-1))=aux2(1:(nyquist-1))+abs(aux(2:nyquist))*2;
    aux2(nyquist)=aux2(nyquist)+aux(nyquist+1);
end

espectra250.control=aux2/(ny);

%Espectro total control

espectraTOT.control=(espectra250.control); 

%Espectro 500 ensemble
clear aux2;
aux2=zeros(1,nyquist);
for i=1:ny %Calculo el espectro para distintas latitudes y lo voy sumando.
    aux=fft(k500.ensemble(i,1:nx))/(nx);
    aux2(1:(nyquist-1))=aux2(1:(nyquist-1))+abs(aux(2:nyquist))*2;
    aux2(nyquist)=aux2(nyquist)+aux(nyquist+1);
end

espectra500.ensemble=aux2/(ny);

%Espectro 250 control
clear aux2;
aux2=zeros(1,nyquist);
for i=1:ny %Calculo el espectro para distintas latitudes y lo voy sumando.
    aux=fft(k250.ensemble(i,1:nx))/(nx);
    aux2(1:(nyquist-1))=aux2(1:(nyquist-1))+abs(aux(2:nyquist))*2;
    aux2(nyquist)=aux2(nyquist)+aux(nyquist+1);
end

espectra250.ensemble=aux2/(ny);

%Espectro total control

espectraTOT.ensemble=(espectra250.ensemble);

%Calculamos los k que corresponden a cada uno de las potencias espectrales
%K esta en radianes/metro

%Calculo la latitud media.





if (archivos_encontrados==0)
espectraTOT.ensembleacum=espectraTOT.ensemble;
espectraTOT.controlacum=espectraTOT.control;
else
espectraTOT.ensembleacum=espectraTOT.ensembleacum+espectraTOT.ensemble;
espectraTOT.controlacum=espectraTOT.controlacum+espectraTOT.control;
end

%Pendientes:

archivos_encontrados=archivos_encontrados+1;
end %End del if que pregunta si el archivo esta o no

end %End del for sobre los tiempos


%Ingresamos los valores de la curva de Lindborg

klin=[1e-4 9e-5 8e-5 7e-5 6e-5 5e-5 4e-5 3e-5 2e-5 1e-5 9e-6 8e-6 7e-6 6e-6 5e-6 4e-6 3e-6 2e-6];
elin=[5e3 6e3 7e3 1e4 1.2e4 2e4 3e4 4e4 1e5 6e5 7e5 9e5 1.2e6 2e6 3e6 6e6 1.5e7 4e7];

latmedia=mean(lat(ymin:ymax));
xresol_km=abs(cos(latmedia*pi/180))*111000*xresol; %Resolucion media de la banda considerada

L=(nx-1)*xresol_km; %Longitud del armonico fundamental
i=1:nyquist;
k(i)=2*pi*i/L;

%Calculamos la densidad energetica en m^3/s^2 del esepectro dividiendo el espectro obtenido por el numero de onda.

rok.controlacum=espectraTOT.controlacum./(k*archivos_encontrados);
rok.ensembleacum=espectraTOT.ensembleacum./(k*archivos_encontrados);


loglog(k,rok.controlacum,k,rok.ensembleacum,klin,elin)




