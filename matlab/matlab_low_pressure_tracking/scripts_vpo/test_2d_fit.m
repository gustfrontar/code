clear all
close all

%y=[1:1:3]';
%x=[1:1:3];
%[x y]=meshgrid(x,y);

%x=reshape(x,[9 1]);
%y=reshape(y,[9 1]);

x=[340.0000       340.0000       340.0000       342.5000       342.5000    342.5000       345.0000       345.0000       345.0000 ];   
y=[-62.50000      -60.00000      -57.50000      -62.50000      -60.00000   -57.50000      -62.50000      -60.00000      -57.50000];   
z=[-240.9297      -301.3228      -299.2524      -243.3364      -303.6934   -297.7373      -237.5635      -292.9180      -281.9551];

xmean=mean(x);
ymean=mean(y);
zmean=mean(z);

x=x-xmean;
y=y-ymean;
z=z-zmean;

%z=x.^2+y.^2;

coeficientes=NaN(1:5);   %Es el array que va a contener a los coeficientes del ajuste por minimos cuadrados.

sumx=sum(x(:));
sumy=sum(y(:));
sumxsq=sum(x(:).^2);
sumysq=sum(y(:).^2);
sumz=sum(z(:));
N=numel(x);

b(1)=sum(z.*(x.^2));
b(2)=sum(z.*x);
b(3)=sum(z.*(y.^2));
b(4)=sum(z.*y);
b(5)=sum(z);

c(1,1)=sum(x.^4);
c(1,2)=sum(x.^3);
c(1,3)=sum((x.^2).*(y.^2));
c(1,4)=sum(y.*(x.^2));
c(1,5)=sum(x.^2);
c(2,1)=sum(x.^3);
c(2,2)=sum(x.^2);
c(2,3)=sum((y.^2).*x);
c(2,4)=sum(y.*x);
c(2,5)=sum(x);
c(3,1)=sum((x.^2).*(y.^2));
c(3,2)=sum(x.*(y.^2));
c(3,3)=sum(y.^4);
c(3,4)=sum(y.^3);
c(3,5)=sum(y.^2);
c(4,1)=sum((x.^2).*y);
c(4,2)=sum(x.*y);
c(4,3)=sum(y.^3);
c(4,4)=sum(y.^2);
c(4,5)=sum(y);
c(5,1)=sum(x.^2);
c(5,2)=sum(x);
c(5,3)=sum(y.^2);
c(5,4)=sum(y);
c(5,5)=N;

d=inv(c);
coeficientes=inv(c)*b';

%Posicion del minimo en lat y lon
xmin=-coeficientes(2)/(2*coeficientes(1))+xmean;
ymin=-coeficientes(4)/(2*coeficientes(3))+ymean;


