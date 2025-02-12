%prueba de fft en dos dimensiones.
clear all

N=10
L=10
for i=0:N-1
for j=0:L-1
var(i+1,j+1)=2*cos(j*2*pi/L) %*cos(i*2*pi/N); %zeros(n,n);

end
end
pcolor(var)


tvar=fft2(var)

%Voy a convertir esto a los coeficientes a y b de una serie de Fourier
%a los que acompañan al coseno y b los que acompañan al seno.

ao=tvar(1,1)/N^2;
%Defino los coeficientes para el rango que va entre 2 y N/2, L/2 porque los coeficientes
%estan corridos con respecto a la definicion tradicional, el coeficiente 0 aparece en el 
%lugar 1 de la matriz, etc.
for i=2:N/2
    for j=2:L/2
       a(i,j)=real(tvar(i,j)+tvar(N-i+2,N-j+2)+tvar(N-i+2,j)+tvar(i,N-j+1))/(N*L);
       b(i,j)=real(-tvar(i,j)-tvar(N-i+2,N-j+2)+tvar(N-i+2,j)+tvar(i,N-j+1))/(N*L);
       c(i,j)=imag(-tvar(i,j)+tvar(N-i+2,N-j+2)+tvar(N-i+2,j)-tvar(i,N-j+1))/(N*L);
       d(i,j)=imag(-tvar(i,j)+tvar(N-i+2,N-j+2)+tvar(N-i+2,j)-tvar(i,N-j+1))/(N*L);
    end
end
for i=2:N/2
    a(i,1)=real(tvar(i,1)+tvar(N-i+2,1))/(N*L);
    b(i,1)=real(-tvar(i,1)+tvar(N-i+2,1))/(N*L); %Esto seguramente es 0
    c(i,1)=imag(-tvar(i,1)+tvar(N-i+2,1))/(N*L); %Esto seguramente tambien.
    d(i,1)=imag(-tvar(i,1)+tvar(N-i+2,1))/(N*L);
end


%Definimos los coeficientes para N/2 L/2
a(N/2,L/2)=real(tvar(N/2,L/2));
b(N/2,L/2)=real(-tvar(N/2,L/2));
c(N/2,L/2)=imag(-tvar(N/2,L/2));
d(N/2,L/2)=imag(-tvar(N/2,L/2));





