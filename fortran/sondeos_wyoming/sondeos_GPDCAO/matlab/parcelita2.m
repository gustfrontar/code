
%Esta funcion calcula el proceso pseudo adiabatico de descenso de una parcela.
%Primero calcula la Temperatura isobarica de bulbo humedo (a traves de la conservacion de la entalpia)
%esto es equivalente a suponer que primero consideramos las condiciones del entorno y que a partir de dichas
%condiciones tenemos agua disponible de forma tal que el aire del entorno alcanza la saturacion a presion 
%constante (Se llega a la Tiw). A partir de ese punto la parcela desciende segun un proceso que solo puede
%ser pseudoadiabatico, ya que necesitamos suponer que constantemente se le entrega agua al sistema durante
%el descenso, lo que permite que la parcela se mantenga saturada.
%Programa realizado por Juan Jose Ruiz (2005)


function out = parcelita2(Tini,Pini,rvini,Pmax,deltaP)
deltaP=deltaP*100;
%*********************************************************************************************
%Parametros iniciales que controlan el funcionamiento del programa
%Este modulo define los parametros iniciales, el valor de las constantes termodinamicas
%*********************************************************************************************

%Definicion de constantes necesarias para el calculo:

Cpd=1005.7; %Calor especifico del aire seco a presion constante
Cpv=1860.6; %Calor especifico del vapor de agua a presion constante
Cw=4217.8;  %Calor especifico del agua liquida

Rd=287.04; %Constante del gas para el aire seco.
Rv=461.50; %Constante del gas para el vapor de agua.

Epsilon=Rd/Rv; %Epsilon
%Estos valores estan expresados en J/(Kg*K)

P0=1000; %Presion de referencia en hPa.
L1=3.1447e6; %Constante utilizada en el calculo de Lv(T).
L2=2357.2;  %Constante utilizada en el calculo de Lv(T).

%Calculo de algunos parametros que son funcion de las constantes anteriores para simplificar
%las expresiones.

beta=(Cpv/Cpd-Rv/Rd);
Kapa2=Rd/Cpd;


%*********************************************************************************************
%Fin de los parametros iniciales
%*********************************************************************************************

%*********************************************************************************************
%CALCULO DE LA TEMPERATURA ISOBARICA DE BULBO HUMEDO.
%*********************************************************************************************
Es=611.2*exp(17.67*(Tini-273.16)/(Tini+243.5-273.16)); %Es en pascales
rvs=(Rd/Rv)*Es/(Pini-Es); %relacion de mezcla de saturacion(adimencional
Lvini=L1-L2*Tini;
eini=Tini+rvini*Lvini/Cpd; %Esta es la cantidad que se conserva durante el proceso.
Difmin=10;
%Probamos con distintos valores de Tf con un intervalo de 0.5 ºC y vemos para que valor la igualdad
%entre la variable ini (conservativa) y la variable ini2 que corresponde al estado final se parecen mas
%segun la ecuacion para procesos isoentalpicos deberian ser iguales... aca buscamos el valor de T para 
%que sean lo mas parecidas posible con un error de 0.5 ºC.
for j=1:1:646
    T(j)=j/2;
    Es(j)=611.2*exp(17.67*(T(j)-273.16)/(T(j)+243.5-273.16)); %Este es el Es con respecto al agua
    rv2(j)=(Rd/Rv)*Es(j)/(Pini-Es(j)); %relacion de mezcla de saturacion.
    Lv=L1-L2*T(j);
    efin=T(j)+rv2(j)*Lv/Cpd; %este es el valor que deberia ser igual a ini en el estado final
    Dif(j)=abs(eini-efin); %Calculo la diferencia entre ambas cantidades ya que estoy buscando el valor de Tfinal
    %que minimice esas diferencias.
    if (Dif(j) <= Difmin)
        Tfin=T(j);
        Difmin=Dif(j);
    end
end
clear T;
clear Es;
clear Lv;
clear rv2;

%*********************************************************************************************
%FIN DEL CALCULO DE LA TEMPERATURA ISOBARICA DE BULBO HUMEDO Y COMIENZO DE LA INTEGRACION
%DEL PROCESO PSEUDO-ADIABATICO SATURADO HACIA ABAJO.
%*********************************************************************************************

Tini=Tfin;
% Calculos iniciales
nP=round((Pmax-Pini)/deltaP)+2; %No de pasos de integracion a realizar.
if nP>1 %Hacemos la integracion solo si vale la pena, si estamos a 1 niveles por encima de superficie.
    %Primer paso con Euler

    T(nP)=Tini;
    P(nP)=Pini;
    Es(nP)=611.2*exp(17.67*(Tini-273.16)/(Tini+243.5-273.16));
    Pd(nP)=Pini-Es(nP);
    rs(nP)=(Rd/Rv)*Es(nP)/(Pini-Es(nP));  %relacion de mezcla de saturacion.
    alfa=1/(1+Rv*rs(nP)/Rd);
    Tro(nP)=T(nP)*(1+Rv*rs(nP)/Rd)/(1+rs(nP)); %Temperatura de densidad.
    Kapa(nP)=(Rd/Cpd)*(1+Rv/Rd*rs(nP))/(1+Cpv/Cpd*rs(nP)); %Kapa ahora es una funcion variable, porque r comienza a variar.
    Lv(nP)=L1-L2*Tini;     %Ecuacion para la dependencia a orden 1 de Lv con T basandose en la ley de Kirchhoff        
    A(nP)=Cpd*Kapa(nP)*Tini/P(nP)+Lv(nP)*alfa*rs(nP)/Pd(nP);
    B(nP)=(Cpd+ alfa*P(nP)*(Lv(nP)^2)*rs(nP)/(Pd(nP)*Rv*(Tini^2)) +(Cpd*rs(nP)*beta));   
    T(nP-1)=Tini+deltaP*A(nP)/B(nP); %Kapa permanece constante.
    P(nP-1)=P(nP)+deltaP;
    Es(nP-1)=611.2*exp(17.67*(T(nP-1)-273.16)/(T(nP-1)+243.5-273.16));
    Pd(nP-1)=P(nP-1)-Es(nP-1);
    rs(nP-1)=(Rd/Rv)*Es(nP-1)/(P(nP-1)-Es(nP-1));  %relacion de mezcla de saturacion.
    Tro(nP-1)=T(nP-1)*(1+Rv*rs(nP-1)/Rd)/(1+rs(nP-1)); %Temperatura de densidad.
 
    
if nP>2  %El ciclo solo tiene sentido si estamos a 2 niveles por encima de la superficie.  
for j=nP-1:-1:2
   
    alfa=1/(1+Rv*rs(j)/Rd);
    Kapa(j)=(Rd/Cpd)*(1+Rv/Rd*rs(j))/(1+Cpv/Cpd*rs(j)); %Kapa ahora es una funcion variable, porque r comienza a variar.
    Lv(j)=L1-L2*T(j);     %Ecuacion para la dependencia a orden 1 de Lv con T basandose en la ley de Kirchhoff        
    A(j)=Cpd*Kapa(j)*Tini/P(j)+Lv(j)*alfa*rs(j)/Pd(j);
    B(j)=(Cpd+ alfa*P(j)*(Lv(j)^2)*rs(j)/(Pd(j)*Rv*(T(j)^2)) +(Cpd*rs(j)*beta));   
    T(j-1)=T(j)+deltaP*A(j)/B(j); %Kapa permanece constante.
    P(j-1)=P(j)+deltaP;
    Es(j-1)=611.2*exp(17.67*(T(j-1)-273.16)/(T(j-1)+243.5-273.16));
    Pd(j-1)=Pini-Es(j-1);
    rs(j-1)=(Rd/Rv)*Es(j-1)/(P(j-1)-Es(j-1));  %relacion de mezcla de saturacion.
    Tro(j-1)=T(j-1)*(1+Rv*rs(j-1)/Rd)/(1+rs(j-1)); %Temperatura de densidad.
    
end

end
end

if(nP>1)%Si nP no es mas grande que 1 no genera salidas
for j=1:nP
out(j,1)=T(j);%Temperatura de la parcela
out(j,2)=P(j);%Presion de la parcela
out(j,3)=rs(j);%Relacion de mezcla de la parcela
out(j,4)=Es(j);%Presion de vapor de saturacion de la parcela
out(j,5)=Tro(j);%Temperatura de densidad de la parcela
end
end





