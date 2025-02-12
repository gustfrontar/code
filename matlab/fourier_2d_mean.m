clear all
close all

%Este script lee los archivos de salida del WRF y calcula el espectro de la energia cinetica en 500 y 250 hPa.
%
%Juan Ruiz- 2006
%
%Cargo la variable con los nombres de archivo y las caracteristicas del archivo.

fourier2d_wrf_common

%Especifico si voy a tomar el archivo entero o a recortar una subregion.

%*******************************************************************
%Numero de puntos en x y en y de los campos presentes en el archivo.
%*******************************************************************

xmax=97
xmin=2
ymax=97
ymin=2
nx=xmax-xmin+1;
ny=ymax-ymin+1;
forecast_time='f48'
if (str2num(forecast_time)==str2num('f48'))
forecast_ver_time='12';
offset=2;

end

narch=length(arch)-2;
%Aca comienza el ciclo en tiempo.

for i_time=1+offset:narch-2+offset;

arch_control=strcat('../control/',arch(i_time+offset,:),forecast_ver_time,forecast_time,'.dat')
arch_ensemble=strcat('../ensemble/',arch(i_time+offset,:),forecast_ver_time,forecast_time,'mean.dat')
narch_control=fopen(arch_control,'r','l')
narch_ensemble=fopen(arch_ensemble,'r','l')
undef=1.E35; %Valor del undef en el ctl.

%Pregunto si no son -1 porque si son -1 quiere decir que no encontro el archivo.
if (narch_control~=-1 & narch_ensemble ~=-1);


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

k500.control=0.5*(var_control(xmin:xmax,ymin:ymax,8).^2+var_control(xmin:xmax,ymin:ymax,9).^2);
k500.ensemble=0.5*(var_ensemble(xmin:xmax,ymin:ymax,8).^2+var_ensemble(xmin:xmax,ymin:ymax,9).^2);
k250.control=0.5*(var_control(xmin:xmax,ymin:ymax,10).^2+var_control(xmin:xmax,ymin:ymax,11).^2);
k250.ensemble=0.5*(var_ensemble(xmin:xmax,ymin:ymax,10).^2+var_ensemble(xmin:xmax,ymin:ymax,11).^2);

%**************************************************************************************************************
%Removemos la tendencia.
%**************************************************************************************************************

k500.control=detrend2(k500.control,2);
k500.ensemble=detrend2(k500.ensemble,2);
k250.control=detrend2(k250.control,2);
k250.ensemble=detrend2(k250.ensemble,2);

%**************************************************************************************************************
%Calculamos la transformada discreta bidimensional del campo de energia cinetica con la tendencia removida.
%**************************************************************************************************************

kspectra500.control=abs(fft2(k500.control));
kspectra500.ensemble=abs(fft2(k500.ensemble));
kspectra250.control=abs(fft2(k250.control));
kspectra250.ensemble=abs(fft2(k250.ensemble));


%**************************************************************************************************************
%Calculamos la frecuencia nyquist en x y en y.
%**************************************************************************************************************

size_spectra=size(kspectra500.control); %Asumo siempre que las dimensiones de los campos son iguales para el ensemble y el control.
nyquistx=size_spectra(2)/2;
nyquisty=size_spectra(1)/2;

%**************************************************************************************************************
%Definimos la matriz de numero de onda (a cada componente del espectro le asigna un numero de onda
%efectivo definido como (l^2+k^2)^0.5. En base a este numero de onda efectivo se construye el espectro
%unidimensional equivalente.
%**************************************************************************************************************

for i=1:nyquisty*2
    for j=1:nyquistx*2
        if(i<=(nyquisty+1) & j<=(nyquistx+1))
            p(i,j)=2*pi*(j-1)/(resolucion*(nyquistx*2-1));
            q(i,j)=2*pi*(i-1)/(resolucion*(nyquistx*2-1));
        end
        if(i<=(nyquisty+1) & j>(nyquistx+1))
            p(i,j)=2*pi*(nyquistx*2+1-j)/(resolucion*(nyquistx*2-1));
            q(i,j)=2*pi*(i-1)/(resolucion*(nyquistx*2-1));
        end
        if(i>(nyquisty+1) & j<=(nyquistx+1))
            p(i,j)=2*pi*(j-1)/(resolucion*(nyquistx*2-1));
            q(i,j)=2*pi*(nyquisty*2+1-i)/(resolucion*(nyquistx*2-1));
        end
        if(i>(nyquisty+1) & j>(nyquistx+1))
            p(i,j)=2*pi*(nyquistx*2+1-j)/(resolucion*(nyquistx*2-1));
            q(i,j)=2*pi*(nyquisty*2+1-i)/(resolucion*(nyquistx*2-1));   
        end
          wave_number(i,j)=(p(i,j)^2+q(i,j)^2)^0.5;
    end
end

%**************************************************************************************************************
%Ahora usamos la funcion find para sumar todas las componentes del espectro cuya longitud de onda efectiva cae 
%dentro de un determinado rango. La idea de esto es pasar de un espectro bidimensional a un espectro lineal
%en una dimension.
%**************************************************************************************************************

deltak=min([2*pi/(resolucion*(nyquistx*2-1)) 2*pi/(resolucion*(2*nyquisty-1))]);
index=0;
corte_frec=min([nyquistx*2*pi/(resolucion*(nyquistx*2-1)) nyquisty*2*pi/(resolucion*(2*nyquisty-1))]);
topi=round(max(max(wave_number))/deltak);
for i=0:1:topi
    index=index+1;
    kint=i*deltak;
    kspectra_one_d500.control(index)=sum(kspectra500.control(find(wave_number>kint & wave_number<=kint+deltak)))/(nyquistx*nyquisty*4);
    kspectra_one_d500.ensemble(index)=sum(kspectra500.ensemble(find(wave_number>kint & wave_number<=kint+deltak)))/(nyquistx*nyquisty*4);
    kspectra_one_d250.control(index)=sum(kspectra250.control(find(wave_number>kint & wave_number<=kint+deltak)))/(nyquistx*nyquisty*4);
    kspectra_one_d250.ensemble(index)=sum(kspectra250.ensemble(find(wave_number>kint & wave_number<=kint+deltak)))/(nyquistx*nyquisty*4);
    wave_number_one_d(index)=kint;
    lambda(index)=2*pi/kint;
    if(wave_number_one_d(index)>corte_frec & wave_number_one_d(index-1)<= corte_frec);
        corte_i=index; %Vamos a graficar el espectro solo hasta la minima frecuencia nyquist, porque para valores
        wave_number_one_d(index);
        wave_number_one_d(index-1);
        %mayores de wave_number el espectro se deforma. Igualmente hay que tener en cuenta que para que la varianza
        %del espectro sea igual a la varianza total de la serie, hay sumar todas las componentes.
    end
end


%Voy sumando sobre todos los espectros para despues poder calcular el espectro medio.

if (i_time==1+offset)
    kspectra_sum_500.control(1:topi+1)=0;
    kspectra_sum_500.ensemble(1:topi+1)=0;
    kspectra_sum_250.control(1:topi+1)=0;
    kspectra_sum_250.ensemble(1:topi+1)=0;
end

kspectra_sum_500.control=kspectra_sum_500.control+kspectra_one_d500.control;
kspectra_sum_500.ensemble=kspectra_sum_500.ensemble+kspectra_one_d500.ensemble;
kspectra_sum_250.control=kspectra_sum_250.control+kspectra_one_d250.control;
kspectra_sum_250.ensemble=kspectra_sum_250.ensemble+kspectra_one_d250.ensemble;


end %Aqui termina el if sobre narch_ensemble y narch_control
end %Aqui finaliza el horario de proteccion al menor (y el ciclo en tiempo)

%**************************************************************************************************************
%Ploteo los resultados.
%**************************************************************************************************************






[lon1 lat1]=meshgrid(lon(xmin:xmax),lat(ymin:ymax));

krange(1)=lambda(corte_i);
krange(2)=lambda(1); %Defino esto para usarlo como limite del eje de los numeros de onda.
ythick=10:10:100;
for i=1:30;
xthick(i)=10^(i-15);
end


figure
%Ploteo los resultados para 500 hPa.

subplot(211);
loglog(lambda(1:corte_i),(kspectra_sum_500.control(1:corte_i)),lambda(1:corte_i),(kspectra_sum_500.ensemble(1:corte_i)));
title ('Espectro unidimensional');
set(gca,'XTick',[xthick]);
set(gca,'YTick',[xthick]);
set(gca,'XLim',[krange]);
subplot(212);
semilogx(lambda(1:corte_i),100*kspectra_sum_500.ensemble(1:corte_i)./kspectra_sum_500.control(1:corte_i));
set(gca,'XLim',[krange]);
set(gca,'XTick',[xthick]);
set(gca,'YTick',[ythick]);
title ('Relacion ens/ctrl');
fn = strcat('k500sum_',arch(i_time+offset,:),forecast_ver_time,forecast_time);
print( gcf, '-dpng', fn )
close 1


figure
%Ploteo los resultados para 250 hPa.

subplot(211);
loglog(lambda(1:corte_i),(kspectra_sum_250.control(1:corte_i)),lambda(1:corte_i),(kspectra_sum_250.ensemble(1:corte_i)));
title ('Espectro unidimensional');
set(gca,'XLim',[krange]);
set(gca,'XTick',[xthick]);
set(gca,'YTick',[xthick]);

subplot(212);
semilogx(lambda(1:corte_i),100*kspectra_sum_250.ensemble(1:corte_i)./kspectra_sum_250.control(1:corte_i));
title ('Relacion ens/ctrl');
set(gca,'XLim',[krange]);
set(gca,'XTick',[xthick]);
set(gca,'YTick',[ythick]);

fn = strcat('k250sum_',arch(i_time+offset,:),forecast_ver_time,forecast_time);
print( gcf, '-dpng', fn )
close 1

