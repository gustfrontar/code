
%Esta funcion remueve la tendencia bilineal de una variable 2D.
function det=detrend2(var)

%Obtenemos la dimension de var.

dim=size(var);

ny=dim(1);
nx=dim(2);
n=nx*ny;
x=1:nx
y=1:ny

%Vamos a calcular el plano que mejora aproxima a "a" segun el metodo de los cuadrados minimos.

xmedio=mean(x);
ymedio=mean(y);

varmedio=mean(mean(var));

%Definimos las covarianzas.

covx=0;
covy=0;
dy=0;
dx=0;

for i=1:nx
    for j=1:ny
        covx=covx+(var(i,j)-varmedio)*(x(i)-xmedio);
        covy=covy+(var(i,j)-varmedio)*(y(j)-ymedio);
        dy=dy+(y(j)-ymedio)^2;
        dx=dx+(x(i)-xmedio)^2;
    end
end


%Definimos los coeficientes de la funcion bilineal que sera de la forma:
% z=alfa+beta*x+gamma*y

alfa=varmedio;
beta=covx/dx;
gamma=covy/dy;


for i=1:nx
    for j=1:ny
        det(i,j)=var(i,j)-(alfa+beta*(x(i)-xmedio)+gamma*(y(j)-ymedio));
    end
end
alfa
beta
gamma

%***********************************************************************************************************************
%FIN DE LA REMOCION DE LA TENDENCIA BILINEAL
%***********************************************************************************************************************)
