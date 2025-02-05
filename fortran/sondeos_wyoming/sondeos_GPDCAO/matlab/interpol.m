%Funcion interpoladora (asigna a cada datos del entorno medido y representado 
%en el sondeo, un dato de temperatura, humedad de la parcela)
%Los datos de la parcela estan cada 0.5 hPa (resolucion recomendada del metodo de la parcela) y 
%los datos del sondeo estan a intervalos irregulares de presion, entonces hay que ver si alguno coincide
% con los valores del sondeo y si no coinciden interpolarlos entre los valores mas cercanos
%los valores de la parcela interpolados a los niveles de presion incluidos en los datos observados 
%se llamaran "variable"parcelareversible2 para distinguirlos del perfil de la parcela total en donde 
%los datos estan cada 0.5 hPa.
%Tambien calcula las diferencias de temperatura entre la parcela y el entorno y las diferencias en la temperatura
%de densidad.
%
%Parametros de entrada Pparcela, Tparcela, rparcela, qparcela, Troparcela, wparcela que son los calculados
%por la funcion parcelita.m
%Pentorno y Tventorno son los leidos del sondeo.
%nP es el numero de iteraciones realizadas durante el calculo de los parametros de la parcela (es decir es el 
%largo de los vectores que continene la informacion de las variables de la parcela a lo largo del ascenso.)
%nivel es el nivel desde donde sale la parcela Esta variable nos permite crear vectores del mismo largo para luego
%graficar facilmente el diagrama de la estabilidad del sondeo.

%Programa realizado por Juan Jose Ruiz (2005)


function int=interpol(Pparcela,Tparcela,rparcela,Troparcela,Esparcela,wparcela,Pentorno,Tentorno,Tventorno,nivel,nivel2)
%***********************************************************************************************************************
%INICIO DE LA INTERPOLACION DE DATOS
%***********************************************************************************************************************
nlevels=length(Pentorno)-nivel+1;
nP=length(Tparcela);
nini=nivel;
nfin=nlevels;

k=nivel;


%Para el primer punto.
%Tparcela2(k)=Tparcela(1);
%Pparcela2(k)=Pparcela(1);
%rparcela2(k)=rparcela(1);

%Troparcela2(k)=Troparcela(1);
%Esparcela2(k)=Esparcela(1);
%wparcela2(k)=wparcela(1);
%Tdif(k)=0;
%Trodif(k)=0;
for i=nini:nfin;
for j=1:nP;
    
    if(Pentorno(i)==Pparcela(j))
    %en este caso no es necesario interpolar, el valor coincide exactamente con el del sondeo.
    Tparcela2(i)=Tparcela(j);
    Pparcela2(i)=Pentorno(i);
    rparcela2(i)=rparcela(j);
    
    Troparcela2(i)=Troparcela(j);
    Esparcela2(i)=Esparcela(j);
    wparcela2(i)=wparcela(j);
    Tdif(i)=Tparcela2(i)-Tentorno(i);
    Trodif(i)=Troparcela2(i)-Tventorno(i);
    end  
    if j<nP
    if(Pparcela(j)>Pentorno(i) & Pparcela(j+1)<Pentorno(i))
    
    %En caso de no coincidir en forma exacta hacemos una interpolacion lineal al valor de presion medido por el sondeo
    %esto puede resultar sobreabundante con resoluciones de 0.5 hPa que son las recomendadas, pero puede ser necesario
    %en caso de que se desee trabajar con menores resoluciones en el calculo del proceso reversible.
    %Con resoluciones menores a 0.5 hPa se observaron errores importantes en el calculo de Theta-E para procesos reversibles.
  
    Tparcela2(i)=(Tparcela(j+1)-Tparcela(j))/(Pparcela(j+1)-Pparcela(j))*(Pentorno(i)-Pparcela(j))+Tparcela(j);
    Pparcela2(i)=Pentorno(i);
    rparcela2(i)=(rparcela(j+1)-rparcela(j))/(Pparcela(j+1)-Pparcela(j))*(Pentorno(i)-Pparcela(j))+rparcela(j);

    Troparcela2(i)=(Troparcela(j+1)-Troparcela(j))/(Pparcela(j+1)-Pparcela(j))*(Pentorno(i)-Pparcela(j))+Troparcela(j);
    Esparcela2(i)=(Esparcela(j+1)-Esparcela(j))/(Pparcela(j+1)-Pparcela(j))*(Pentorno(i)-Pparcela(j))+Esparcela(j);
    wparcela2(i)=(wparcela(j+1)-wparcela(j))/(Pparcela(j+1)-Pparcela(j))*(Pentorno(i)-Pparcela(j))+wparcela(j);
    Tdif(i)=Tparcela2(i)-Tentorno(i);
    Trodif(i)=Troparcela2(i)-Tventorno(i);
    
end
end
end 
end 
Tparcela2(nivel2)=Tentorno(nivel2);
Pparcela2(nivel2)=Pentorno(nivel2);
Troparcela2(nivel2)=Tventorno(nivel2);
wparcela2(nivel2)=0;

for k=nivel:nfin
int(k,1)=Tparcela2(k);
int(k,2)=Pparcela2(k);
int(k,3)=rparcela2(k);

int(k,5)=Troparcela2(k);
int(k,6)=Esparcela2(k);
int(k,7)=wparcela2(k);
int(k,8)=Tdif(k);
int(k,9)=Trodif(k);
end
%***********************************************************************************************************************
%FIN DE LA INTERPOLACION
%***********************************************************************************************************************
