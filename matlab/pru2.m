clear all
%Prueba para la funcion detrend2 y de lectura de un archivo.

arch='../control/2005112500f36.dat';
narch=fopen(arch,'r','l');
undef=1.E35; %Valor del undef en el ctl.
nvar=15; %numero de variables en el archivo.

%Numero de puntos en x y en y de los campos presentes en el archivo.
nx=99;
ny=99;
size=[nx ny];

for i=1:nvar
nada=fread(narch,[2 1],'single')';
var(:,:,i)=fread(narch,size,'single')';
variable(:,:,i)=var(2:ny-1,2:nx-1,i);
end
variable(find(variable>=1000000))=NaN;
for i=1:nvar
    variable(:,:,i)=detrend2(variable(:,:,i),2);
end
N=ny-3
L=nx-3
pow=fourier2d(variable(1:N,1:L,8));

%Vamos a intentar calcular un espectro unidimensional equivalente
%Para eso calculo un numero de onda equivalente como la hipotenusa de los numeros de onda en x y en y.
%Tomo rangos de frecuencias de 0.5 y sumo todas las frecuencias presentes en esa ventana.
%Hay que chequear si lo que se hace en la realidad es sumarlas o promediarlas...
index=1;
deltafrec=1;
for i=1:N/2+1
for j=1:N/2+1
   knumber(i,j)=sqrt((i-1)^2+(j-1)^2);
end
end

for i=1:deltafrec:round(max(max(knumber)))
    avepow(index)=sum(pow(find(knumber>i & knumber<i+deltafrec)));
    avek(index)=i;
    index=index+1;
end

