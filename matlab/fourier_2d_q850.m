clear all
close all
%Este script lee los archivos de salida del WRF y calcula el espectro de la energia cinetica en 500 y 250 hPa.
%
%Juan Ruiz- 2006
%

resolucion=50;                %Resolucion horizontal en Km.
arch_control='../control/2005112500f36.dat';
arch_ensemble='../ensemble/2005112500f36mean.dat';
narch_control=fopen(arch_control,'r','l');
narch_ensemble=fopen(arch_ensemble,'r','l');
undef=1.E35; %Valor del undef en el ctl.
nvar_control=15; %numero de variables en el archivo.
nvar_ensemble=13; %numero de variables en el archivo del ensemble.

%*******************************************************************
%Numero de puntos en x y en y de los campos presentes en el archivo.
%*******************************************************************

xmax=85
xmin=55
ymax=85
ymin=55
nx=xmax-xmin+1;
ny=ymax-ymin+1;

%nxmax y nymax controlan la dimension de las matrices dentro del archivo.

nxmax=99
nymax=99
record_size=[nxmax nymax];

lat=[-44.74818
  -44.39475
  -44.03917
  -43.68145
  -43.32158
  -42.95956
  -42.59541
  -42.22911
  -41.86067
  -41.49010
  -41.11740
  -40.74257
  -40.36562
  -39.98654
  -39.60535
  -39.22206
  -38.83665
  -38.44915
  -38.05955
  -37.66788
  -37.27412
  -36.87829
  -36.48040
  -36.08046
  -35.67846
  -35.27444
  -34.86839
  -34.46033
  -34.05025
  -33.63820
  -33.22416
  -32.80814
  -32.39017
  -31.97026
  -31.54843
  -31.12467
  -30.69901
  -30.27147
  -29.84205
  -29.41078
  -28.97768
  -28.54275
  -28.10602
  -27.66750
  -27.22723
  -26.78520
  -26.34143
  -25.89597
  -25.44882
  -25.00000
  -24.54954
  -24.09744
  -23.64375
  -23.18848
  -22.73166
  -22.27331
  -21.81345
  -21.35210
  -20.88930
  -20.42507
  -19.95943
  -19.49242
  -19.02406
  -18.55437
  -18.08337
  -17.61112
  -17.13763
  -16.66293
  -16.18705
  -15.71001
  -15.23186
  -14.75262
  -14.27232
  -13.79100
  -13.30869
  -12.82540
  -12.34119
  -11.85608
  -11.37010
  -10.88330
  -10.39571
   -9.90734
   -9.41825
   -8.92847
   -8.43803
   -7.94696
   -7.45531
   -6.96310
   -6.47038
   -5.97717
   -5.48353
   -4.98946
   -4.49503
   -4.00027
   -3.50520
   -3.00988
   -2.51432
   -2.01859
   -1.52269];
for j=1:nxmax
lon(j)=-89.3104 +  0.4961*(j-1);
end

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
%Vamos a trabajar con q y t en 850hPa. Sobre todo en la porcion norte del dominio.
%**************************************************************************************************************

k500.control=var_control(xmin:xmax,ymin:ymax,5)*1000;
k500.ensemble=var_ensemble(xmin:xmax,ymin:ymax,5)*1000;
k250.control=var_control(xmin:xmax,ymin:ymax,4);
k250.ensemble=var_ensemble(xmin:xmax,ymin:ymax,4);

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
    kint=i*deltak+0.5*deltak;
    kspectra_one_d500.control(index)=sum(kspectra500.control(find(wave_number>kint-0.5*deltak & wave_number<=kint+0.5*deltak)))/(nyquistx*nyquisty*4);
    kspectra_one_d500.ensemble(index)=sum(kspectra500.ensemble(find(wave_number>kint-0.5*deltak & wave_number<=kint+0.5*deltak)))/(nyquistx*nyquisty*4);
    kspectra_one_d250.control(index)=sum(kspectra250.control(find(wave_number>kint-0.5*deltak & wave_number<=kint+0.5*deltak)))/(nyquistx*nyquisty*4);
    kspectra_one_d250.ensemble(index)=sum(kspectra250.ensemble(find(wave_number>kint-0.5*deltak & wave_number<=kint+0.5*deltak)))/(nyquistx*nyquisty*4);
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

%**************************************************************************************************************
%Ploteo los resultados.
%**************************************************************************************************************

[lon1 lat1]=meshgrid(lon(xmin:xmax),lat(ymin:ymax));

krange(1)=lambda(corte_i);
krange(2)=lambda(1); %Defino esto para usarlo como limite del eje de los numeros de onda.
for i=1:30;
xthick(i)=10^(i-15);
end


figure
%Ploteo los resultados para 500 hPa.
subplot(231);
contourf(lon1,lat1,k500.control);
run colorbar
title ('q en 850. Control');
subplot(232);
contourf(log10(abs(kspectra500.control)));
run colorbar
title ('Transformada bidimensional');
subplot(233);
loglog(lambda(1:corte_i),(kspectra_one_d500.control(1:corte_i)),lambda(1:corte_i),(kspectra_one_d500.ensemble(1:corte_i)));
title ('Espectro unidimensional');
set(gca,'XTick',[xthick]);
set(gca,'YTick',[xthick]);
set(gca,'XLim',[krange]);
subplot(234);
contourf(lon1,lat1,k500.ensemble);
run colorbar
title ('q en 850 Ensemble');
subplot(235);
contourf(log10(abs(kspectra500.ensemble)));
run colorbar
title ('Transformada bidimensional');
subplot(236);
loglog(lambda(1:corte_i),(kspectra_one_d500.control(1:corte_i)),lambda(1:corte_i),(kspectra_one_d500.ensemble(1:corte_i)));
title ('Espectro unidimensional');
set(gca,'XLim',[krange]);
set(gca,'XTick',[xthick]);
set(gca,'YTick',[xthick]);

figure
%Ploteo los resultados para 250 hPa.
subplot(231);
contourf(lon1,lat1,k250.control);
title ('T en 850. Control');
run colorbar
subplot(232);
contourf(log10(abs(kspectra250.control)));
run colorbar
title ('Transformada bidimensional');
subplot(233);
loglog(lambda(1:corte_i),(kspectra_one_d250.control(1:corte_i)),lambda(1:corte_i),(kspectra_one_d250.ensemble(1:corte_i)));
title ('Espectro unidimensional');
set(gca,'XLim',[krange]);
set(gca,'XTick',[xthick]);
set(gca,'YTick',[xthick]);

subplot(234);
contourf(lon1,lat1,k250.ensemble);
run colorbar
title ('T en 850. Ensemble');
subplot(235);
contourf(log10(abs(kspectra250.ensemble)));
title ('Transformada bidimensional');
run colorbar
subplot(236);
loglog(lambda(1:corte_i),(kspectra_one_d250.control(1:corte_i)),lambda(1:corte_i),(kspectra_one_d250.ensemble(1:corte_i)));
title ('Espectro unidimensional');
set(gca,'XLim',[krange]);
set(gca,'XTick',[xthick]);
set(gca,'YTick',[xthick]);