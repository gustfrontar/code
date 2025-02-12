
%Esta funcion remueve la tendencia bidimensional
%
%Juan Ruiz--- 2006
%
%Uso: detrend(var,flag)

%var es el campo en dos dimensiones al cual se le remover´a la tendencia. Para indicar valores undef se utiliza el valor inf.
%flag es el metodo que se utilizara para remover la tendencia.
%1- remueve la tendencia bilienal calculada segun el metodo de cuadrados minimos.
%2- remueve la tendencia bidimensional no lineal que hace la funcion periodica sobre el intervalo de definicion.
%como esta descrito en Errico 1985 (Monthly Weather Review).
%El metodo 2 no funciona bien con campos que tiene variables undef, de todas maneras en analisis armonico tampoco se puede llevar a cabo
%para esos campos.
function det=detrend2(var,flag)

%Obtenemos la dimension de var.

dim=size(var);

ny=dim(1);
nx=dim(2);
n=nx*ny;
x=1:nx;
y=1:ny;

if(flag==1)
%Vamos a calcular el plano que mejora aproxima a "a" segun el metodo de los cuadrados minimos.

xmedio=mean(x);
ymedio=mean(y);

varmedio=mean(mean(var(find(var~=inf))));
varmedio
%Definimos las covarianzas.

covx=0;
covy=0;
dy=0;
dx=0;

for i=1:ny
    for j=1:nx
        if(var(i,j)<=1000000)
        covx=covx+(var(i,j)-varmedio)*(x(j)-xmedio);
        covy=covy+(var(i,j)-varmedio)*(y(i)-ymedio);
        dy=dy+(y(i)-ymedio)^2;
        dx=dx+(x(j)-xmedio)^2;
        end
    end
end


%Definimos los coeficientes de la funcion bilineal que sera de la forma:
% z=alfa+beta*x+gamma*y

alfa=varmedio;
beta=covx/dx;
gamma=covy/dy;


for i=1:ny
    for j=1:nx
        det(i,j)=var(i,j)-(alfa+beta*(x(j)-xmedio)+gamma*(y(i)-ymedio));
    end
end
alfa;
beta;
gamma;

end

%***********************************************************************************************************************
%FIN DE LA REMOCION DE LA TENDENCIA BILINEAL POR CUADRADOS MINIMOS. 
%***********************************************************************************************************************

if(flag==2)
    %Removemos la tendencia para aplicar analisis armonico bidimensional segun el metodo de Errico (1985).
    det=var;
    for i=1:ny
       sj(i)=(var(i,nx)-var(i,1))/(nx-1);
       for j=1:nx
           if(var(i,j)~=inf)
          det(i,j)=var(i,j)-0.5*(2*j-nx-1)*sj(i);
           end
       end 
    end
    %Solo removi la tendencia en la direccion de x.
    
        %varmedio=mean(mean(det(find(var~=inf))));
        %det(:,:)=det(:,:)-varmedio;
    
    
end
%***********************************************************************************************************************
%FIN DE LA REMOCION DE LA TENDENCIA BIDIMENSIONAL POR EL METODO DE ERRICO.
%***********************************************************************************************************************