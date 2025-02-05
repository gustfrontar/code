%Funcion para calcular algunos procesos isoentalpicos
%En este caso calcula las variaciones de T en una parcela saturada y con agua liquida cuando la misma
%se congela.
%Programa realizado por Juan Jose Ruiz (2005)
%Los parametros que se ingresan son:
%Tini temperatura inicial
%Pini presion inicial que se mantiene constante durante el proceso.
%rt contenido total de agua en la parcela.
%LA ECUACION QUE SE USA EN ESTE PROGRAMA TIENE QUE SER VERIFICADA (NO ESTOY SEGURO SI LAS CUENTAS 
%ESTAN BIEN)

function int=isoentalp(Tini,Pini,rt)


%*********************************************************************************************
%Parametros iniciales que controlan el funcionamiento del programa
%Este modulo define los parametros iniciales, el valor de las constantes termodinamicas
%*********************************************************************************************

%Definicion de constantes necesarias para el calculo:

Cpd=1005.7; %Calor especifico del aire seco a presion constante
Cpv=1860.6; %Calor especifico del vapor de agua a presion constante
Cw=4217.8;  %Calor especifico del agua liquida
Ci=2089.0;  %Calor especifico del hielo a -10C

Rd=287.04; %Constante del gas para el aire seco.
Rv=461.50; %Constante del gas para el vapor de agua.

Epsilon=Rd/Rv; %Epsilon
%Estos valores estan expresados en J/(Kg*K)

P0=1000; %Presion de referencia en hPa.
L1=3.1447e6; %Constante utilizada en el calculo de Lv(T).

L2=2357.2;  %Constante utilizada en el calculo de Lv(T).
Lf=3.34e5;  %Calor latente de fusion.
%Calculo de algunos parametros que son funcion de las constantes anteriores para simplificar
%las expresiones.

beta=(Cpv/Cpd-Rv/Rd);
Kapa2=Rd/Cpd;

%***********************************************************************************************************************
%INICIO DEL CALCULO
%***********************************************************************************************************************
%Calculamos a que temperatura llega una parcela que inicialmente se encuentra saturada y con un cierto contenido de agua
%liquida cuando todo ese agua liquida se congela y la parcela vuelve a estar saturada. Hay que tener en cuenta que el aumento
%de temperatura conduce a un aumento de Es por lo que el sistema podria quedar subsaturado si no hacemos la suposicion de 
%que el estado de saturacion es alcanzado nuevamente por un proceso isoentalpico.
%por otra parte, el pasaje de hielo a agua, hace descender la Es lo cual podria conducir a sobresaturaciones.


Es=611.2*exp(17.67*(Tini-273.16)/(Tini+243.5-273.16)); %Es en pascales
rv=(Rd/Rv)*Es/(Pini-Es); %relacion de mezcla de saturacion(adimencional
Lvini=L1-L2*Tini;
rwini=rt-rv;
qv=rv/(1+rv); %Humedad especifica inicial de la parcela.

aux=Cpd+(rt)*(Ci);         %Variable auxiliar para hacer mas corto el denominador.
ini=Tini+(rv*(Lvini+Lf)+rwini*Lf)/aux; %Este es el valor que se conserva durante el proceso.

%Esta ecuacion no puede resolverse directamente, hay que iterar para aproximarla.
Difmin=10;
%Probamos con distintos valores de Tf con un intervalo de 0.5 C y vemos para que valor la igualdad
%entre la variable ini (conservativa) y la variable ini2 que corresponde al estado final se parecen mas
%segun la ecuacion para procesos isoentalpicos deberian ser iguales... aca buscamos el valor de T para 
%que sean lo mas parecidas posible con un error de 0.5 C.
for j=1:1:173*2
    T(j)=100+j/2;
    Es(j)=611.15*exp((23.036-(T(j)-273.15)/333.7)*(T(j)-273.15)/(279.82+(T(j)-273.15))); %Este es el Es con respecto al hielo
    rv2(j)=(Rd/Rv)*Es(j)/(Pini-Es(j)); %relacion de mezcla de saturacion.
    wi(j)=rt-rv2(j);                 %Supongo que todo el condensado es hielo.
    Lv=L1-L2*T(j);
    ini2=T(j)+(rv2(j)*(Lv+Lf))/aux; %este es el valor que deberia ser igual a ini en el estado final
    Dif(j)=abs(ini2-ini); %Calculo la diferencia entre ambas cantidades ya que estoy buscando el valor de Tfinal
    %que minimice esas diferencias.
    if (Dif(j) <= Difmin)
        Tfin=T(j);
        wfin=wi(j);
        Difmin=Dif(j);
    end
end

int(1)=Tfin;
int(2)=wfin;

%***********************************************************************************************************************
%FIN DE FUNCION
%***********************************************************************************************************************
