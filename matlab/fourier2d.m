%Este script utiliza la funcion fft2 de Matlab para calcular la descomposicion 
%en series exponenciales complejas de un campo bidimensional para luego obtener
%los coeficientes de las series trigonometricas reales.
%a son los coeficientes de los terminos cos(i)*cos(j)
%b son los coeficientes de los terminos sen(i)*sen(j)
%c son los coeficientes de los terminos sen(i)*cos(j)
%d son los coeficientes de los terminos cos(i)*sen(j)
%En todas las matrices, la frecuencia a la cual corresponden los coeficientes estan desfasadas
%en una unidad con respecto al subindice de la matriz. Por ejemplo, el coeficiente 1,1 de a representa
%el valor medio de la funcion es decir lo que tradicionalmente se denota como coeficiente 0,0
%El valor de a(2,3) corresponde a la frecuencia 1 en filas y 2 en columnas.
%Asi la frecuencia Nyquist corresponde al subindice N/2+1 de la matriz.
%Antes de realizar la descomposicion bidmensional seria bueno remover la tendencia lineal en 
%2 direcciones. Sino, la tendencia lineal se proyecta sobre los coeficientes 0 de la matriz.

function  pow=fourier2d(var)
%N es el numero de filas de la matriz y L es el numero de columnas.
%Este script parte del supuesto que N y L son pares.

sizev=size(var);
N=sizev(1);
L=sizev(2);

tvar=fft2(var);
max(max(tvar))
%Voy a convertir esto a los coeficientes a y b de una serie de Fourier
%a los que acompañan al coseno y b los que acompañan al seno.

ao=tvar(1,1)/N^2;
%Defino los coeficientes para el rango que va entre 2 y N/2, L/2 porque los coeficientes
%estan corridos con respecto a la definicion tradicional, el coeficiente 0 aparece en el 
%lugar 1 de la matriz, etc.
for i=2:N/2
    for j=2:L/2
       a(i,j)=real(tvar(i,j)+tvar(N-i+2,N-j+2)+tvar(N-i+2,j)+tvar(i,N-j+2))/(N*L);
       b(i,j)=real(-tvar(i,j)-tvar(N-i+2,N-j+2)+tvar(N-i+2,j)+tvar(i,N-j+2))/(N*L);
       c(i,j)=imag(-tvar(i,j)+tvar(N-i+2,N-j+2)+tvar(N-i+2,j)-tvar(i,N-j+2))/(N*L);
       d(i,j)=imag(-tvar(i,j)+tvar(N-i+2,N-j+2)-tvar(N-i+2,j)+tvar(i,N-j+2))/(N*L);
    end
end
%Definimos los coeficientes "0" (los bordes de la matriz).
for i=2:N/2
    a(i,1)=real(tvar(i,1)+tvar(N-i+2,1))/(N*L);
    b(i,1)=real(-tvar(i,1)+tvar(N-i+2,1))/(N*L); %Esto seguramente es 0
    c(i,1)=imag(-tvar(i,1)+tvar(N-i+2,1))/(N*L); %Esto seguramente tambien.
    d(i,1)=imag(-tvar(i,1)-tvar(N-i+2,1))/(N*L);
end
for j=2:L/2
    a(1,j)=real(tvar(1,j)+tvar(1,N-j+2))/(N*L);
    b(1,j)=real(-tvar(1,j)+tvar(1,N-j+2))/(N*L); %Esto seguramente es 0
    c(1,j)=imag(-tvar(1,j)-tvar(1,N-j+2))/(N*L); %Esto seguramente tambien.
    d(1,j)=imag(-tvar(1,j)+tvar(1,N-j+2))/(N*L);
end
%Tambien definimos los coeficientes correspondientes a la frecuencia Nyquist.
for j=2:L/2
    a(N/2+1,j)=real(tvar(N/2+1,j)+tvar(N/2+1,N-j+2))/(N*L);
    b(N/2+1,j)=real(-tvar(N/2+1,j)+tvar(N/2+1,N-j+2))/(N*L);
    c(N/2+1,j)=imag(-tvar(N/2+1,j)-tvar(N/2+1,N-j+2))/(N*L);
    d(N/2+1,j)=imag(-tvar(N/2+1,j)+tvar(N/2+1,N-j+2))/(N*L);
end
for i=2:N/2
    a(i,N/2+1)=real(tvar(i,N/2+1)+tvar(N-i+2,N/2+1))/(N*L);
    b(i,N/2+1)=real(-tvar(i,N/2+1)+tvar(N-i+2,N/2+1))/(N*L);
    c(i,N/2+1)=imag(-tvar(i,N/2+1)+tvar(N-i+2,N/2+1))/(N*L);
    d(i,N/2+1)=imag(-tvar(i,N/2+1)-tvar(N-i+2,N/2+1))/(N*L);
end
%Defino para i=N/2+1 j=N/2+1
    a(N/2+1,N/2+1)=real(tvar(N/2+1,N/2+1))/(N*L);
    b(N/2+1,N/2+1)=-a(N/2+1,N/2+1);
    c(N/2+1,N/2+1)=imag(-tvar(N/2+1,N/2+1))/(N*L);
    d(N/2+1,N/2+1)=c(N/2+1,N/2+1);
    a(1,N/2+1)=real(tvar(1,N/2+1))/(N*L);
    b(1,N/2+1)=-a(1,N/2+1);
    c(1,N/2+1)=imag(-tvar(1,N/2+1))/(N*L);
    d(1,N/2+1)=c(1,N/2+1);
    a(N/2+1,1)=real(tvar(N/2+1,1))/(N*L);
    b(N/2+1,1)=-a(N/2+1,1);
    c(N/2+1,1)=imag(-tvar(N/2+1,1))/(N*L);
    d(N/2+1,1)=c(N/2+1,1);
    

%Y finalmente la frutilla del postre el coeficiente 00.
    a(1,1)=real(tvar(1,1))/(N*L);
    b(1,1)=0;
    c(1,1)=0;
    d(1,1)=0;
    
%Definimos los coeficientes para N/2 L/2
a(N/2,L/2)=real(tvar(N/2,L/2))/(N*L);
b(N/2,L/2)=real(-tvar(N/2,L/2))/(N*L);
c(N/2,L/2)=imag(-tvar(N/2,L/2))/(N*L);
d(N/2,L/2)=imag(-tvar(N/2,L/2))/(N*L);

%Voy a calcular la varianza de la funcion original y a compararla con la de la serie segun teorema de Parseval.

apow=a.^2;
bpow=b.^2;
cpow=c.^2;
dpow=d.^2;

for i=2:N/2+1
    for j=2:L/2+1
       pow(i,j)=0.25*(apow(i,j)+bpow(i,j)+cpow(i,j)+dpow(i,j)); 
    end
end

for i=2:N/2+1
    pow(i,1)=0.5*(apow(i,1)+bpow(i,1)+cpow(i,1)+dpow(i,1));
    pow(i,N/2+1)=0.5*(apow(i,N/2+1)+bpow(i,N/2+1)+cpow(i,N/2+1)+dpow(i,N/2+1));
end
for i=2:L/2+1
    pow(1,i)=0.5*(apow(1,i)+bpow(1,i)+cpow(1,i)+dpow(1,i));
    pow(L/2+1,i)=0.5*(apow(L/2+1,i)+bpow(L/2+1,i)+cpow(L/2+1,i)+dpow(L/2+1,i));
end
pow(1,1)=apow(1,1);

%Calculo la varianza del campo para comparar:

varianza=0;

for i=1:N
    for j=1:L
        varianza=varianza+var(i,j)^2;
    end
end
varianza=varianza/(N*L);

%Calculo la potencia espectral total para comparar con la varianza.

powsum=sum(sum(pow))
varianza

for i=1:N/2
    for j=1:L/2
        pow(i,j)=pow(i,j)/powsum;
    end
end







