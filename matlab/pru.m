clear all
%Prueba para la funcion detrend2 y de lectura de un archivo.
arch='./2005111912f48.dat';
narch=fopen(arch,'r','l');
undef=1.E35; %Valor del undef en el ctl.
nvar=15; %numero de variables en el archivo.

%Numero de puntos en x y en y de los campos presentes en el archivo.
xmax=97
xmin=2
ymax=97
ymin=2
nx=xmax-xmin+1;
ny=ymax-ymin+1;
nxmax=99
nymax=99
record_size=[nxmax nymax];

for i=1:nvar
nada=fread(narch,[2 1],'single')';
var(:,:,i)=fread(narch,record_size,'single')';
end


var(find(var>=1000000))=inf;

%Vamos a trabajar con la energia cinetica en 500 hpa y 250 hPa.

k500=0.5*(var(xmin:xmax,ymin:ymax,8).^2+var(xmin:xmax,ymin:ymax,9).^2);
k250=0.5*(var(xmin:xmax,ymin:ymax,10).^2+var(xmin:xmax,ymin:ymax,11).^2);

%Removemos la tendencia.

k500=detrend2(k500,2);
k250=detrend2(k250,2);


kspectra.lev500=abs(fft2(k500));
kspectra.lev250=abs(fft2(k250));

size_spectra=size(kspectra.lev500);
nyquistx=size_spectra(2)/2;
nyquisty=size_spectra(1)/2;

%Vamos a definir coeficientes para la matriz que me van a permitir calcular el numero de onda efectivo k.
for i=1:nyquisty*2
    for j=1:nyquistx*2
        if(i<=(nyquisty+1) & j<=(nyquistx+1))
            wave_number(i,j)=((i-1)^2+(j-1)^2)^0.5;
        end
        if(i<=(nyquisty+1) & j>(nyquistx+1))
            wave_number(i,j)=((i-1)^2+(nyquistx*2+1-j)^2)^0.5;
        end
        if(i>(nyquisty+1) & j<=(nyquistx+1))
            wave_number(i,j)=((nyquisty*2+1-i)^2+(j-1)^2)^0.5;
        end
        if(i>(nyquisty+1) & j>(nyquistx+1))
            wave_number(i,j)=((nyquisty*2+1-i)^2+(nyquistx*2+1-j)^2)^0.5;
        end
        
    end
end

%Ahora usamos la funcion find para sumar todas las componentes del espectro cuya longitud de onda efectiva cae dentro de un determinado rango.
%la idea de esto es pasar de un espectro bidimensional a un espectro lineal en una dimension.

deltak=0.5
index=0
corte_frec=min([nyquisty nyquistx])
for i=0:deltak:round(max(max(wave_number)))
    index=index+1;
    kspectra_one_d.lev500(index)=sum(kspectra.lev500(find(wave_number>i & wave_number<i+deltak)));
    kspectra_one_d.lev250(index)=sum(kspectra.lev250(find(wave_number>i & wave_number<i+deltak)));
    wave_number_one_d(index)=i;
    if(wave_number_one_d(index)>corte_frec & wave_number_one_d(index-1)<= corte_frec);
        corte_i=index %Vamos a graficar el espectro solo hasta la minima frecuencia nyquist, porque para valores
        wave_number_one_d(index);
        wave_number_one_d(index-1);
        %mayores de wave_number el espectro se deforma. Igualmente hay que tener en cuenta que para que la varianza
        %del espectro sea igual a la varianza total de la serie, hay sumar todas las componentes.
    end
end

subplot(231);
contourf(k500);
run colorbar
subplot(232);
contourf(log(abs(kspectra.lev500)));
run colorbar
subplot(233);
plot(wave_number_one_d(1:corte_i),log(kspectra_one_d.lev500(1:corte_i)));

subplot(234);
contourf(k250);
run colorbar
subplot(235);
contourf(log(abs(kspectra.lev250)));
run colorbar
subplot(236);
plot(wave_number_one_d(1:corte_i),log(kspectra_one_d.lev250(1:corte_i)));