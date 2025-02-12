% Metodo de la parcela, utilizando un proceso reversible.
% Utilizando la ecuacion (8) y otras aproximaciones del Paper de Lipps y Hemler (1979)
% Another Look at the Thermodynamic Equation For Deep Convection. Monthly Weather Review Voulume 108 78-84
%
%Los parametros de entrada para esta funcion son la presion en el nivel de donde sale la parcela (Pinicial)
%la temperatura del nivel de donde proviene la parcela y la HR (Tinicial y HRinicial).
%La presion hasta la cual se integrara la ecuacion (Pmin), el paso de integracion (deltaP, se recomiendo utilizar
%un valor de 0.5 hPa.) 
%Otros parametros son: proceso (proceso=1 ecuacion completa, proceso=0 ecuacion sin el agua liquida)
%latente: Este parametro define como se va a considerar el calor latente de condensacion en las ecuaciones:
%latente=0 Lv constante e igual al valor para 0 grados
%latente=1 Lv constante e igual al valor en la base de la nube
%latente=2 Lv variable y funcion de la temperatura a lo largo del proceso (valor de default)
%Programa realizado por Juan Jose Ruiz (2005).

function out = parcelita(Pinicial,Tinicial,HRinicial,Pmin,deltaP)


%*********************************************************************************************
%Parametros iniciales que controlan el funcionamiento del programa
%Este modulo define los parametros iniciales, el valor de las constantes termodinamicas
%*********************************************************************************************

%Definicion de constantes necesarias para el calculo:

Cpd=1005.7; %Calor especifico del aire seco a presion constante
Cpv=1860.6; %Calor especifico del vapor de agua a presion constante
Cw=4217.8;  %Calor especifico del agua liquida
Ci=2089.0;  %Calor especifico del hielo a -10 C

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


%*********************************************************************************************
%Fin de los parametros iniciales
%*********************************************************************************************
% Condiciones iniciales de la parcela:

Pi=round(Pinicial); %presion inicial en hectopascales. (valor inicial)
Ti=Tinicial;   %temperatura de la parcela en grados centigrados. (valor inicial)
hri=HRinicial;   %humedad relativa en porcentaje. (valor inicial)

%*********************************************************************************************
%Fin de la lectura de datos.
%*********************************************************************************************
% Parametros de resolucion y extension vertical del metodo:

nP=round(-(Pmin-Pi)/(100*deltaP)); %Calculo del numero de pasos que realizaran el modelo
nP=nP+2; %Le sumo 2 pasos para que despues pueda interpolar en todos los niveles del entorno
% Calculos iniciales

deltaP=deltaP*100;
T(1)=Ti;
hr(1)=hri;
P(1)=Pi;

%Calculo de la tension de vapor de saturacion, la tension de vapor y la relacion de mezcla en el instante inicial.
% La presion de vapor de saturacion se calcula utiizando la expresion (10) del paper de David Bolton (1980)
% The Computation of Equivalente Potential Temperature. Monthly Weather Review. Volume 108. 1046-1053.

Es(1)=611.2*exp(17.67*(T(1)-273.16)/(T(1)+243.5-273.16)); %Es en pascales
rs(1)=(Rd/Rv)*Es(1)/(P(1)-Es(1)); %relacion de mezcla de saturacion(adimencional)
E(1)=Es*hr(1);
r(1)=(Rd/Rv)*E(1)/(P(1)-E(1));   %relacion de mezcla (adimensional)
Pd(1)=P(1)-Es(1); %Presion parcial del aire seco
Kapa=(Rd/Cpd)*(1+Rv/Rd*r(1))/(1+Cpv/Cpd*r(1)); %Kapa para el aire humedo.
Tro(1)=T(1)*(1+Rv*r(1)/Rd)/(1+r(1)); %Temperatura de densidad.

%Ascenso no saturado hasta el nivel de condensacion.
a=1;
j=2;
while (a==1) & (j<nP)
    T(j)=T(j-1)-deltaP*T(j-1)*Kapa(1)/(P(j-1)); %Como r no es funcion de p en este proceso, Kapa permanece constante.
    P(j)=P(j-1)-deltaP;
    Es(j)=611.2*exp(17.67*(T(j)-273.16)/(T(j)+243.5-273.16));
    rs(j)=(Rd/Rv)*Es(j)/(P(j)-Es(j));  %relacion de mezcla de saturacion.
    r(j)=r(1);
    w(j)=0;%El continido de agua liquida es cero en el ascenso no saturado
    Tro(j)=T(j)*(1+Rv*r(j)/Rd)/(1+w(j)+r(j)); %Temperatura de densidad.
    if r(j)<rs(j)   %De ahora en mas los calculos se hacen en base a la relacion de mezcla.
        %Mientras el valor de r no supere al valor de saturacion, continua la iteracion utilizando
        %un proceso reversible no saturado (aire humedo).
        a=1;
else
       a=0;
end
    j=j+1;
end

%Guarda el valor de las variables en el nivel de condensacion por ascenso.

Enca=Es(j-1)/100; %Valor en hPa de la presion de vapor
Tnca=T(j-1)-273.16; %Valor en grados Celsius de la temperatura
Pnca=P(j-1)/100;   %Valor en hPa de la presion


% El proceso humedo va a integrar la ecuacion desde el paso nsat hasta el paso nP.
nsat=j-1;

%A partir de este punto comienza la condensacion. 
%Utilizamos una ecuacion termodinamica que tiene en cuenta las variaciones de Lv con T, pero que no considera la fase hielo.

%Primer paso con Euler
for j=nsat:nsat+1
    %dT/dp se divide en 4 terminos para mayor simplicidad alfa, beta, A y
    %B.
         Es(j)=611.2*exp(17.67*(T(j)-273.16)/(T(j)+243.5-273.16));
         Pd(j)=P(j)-Es(j);
         rs(j)=(Rd/Rv)*Es(j)/(P(j)-Es(j));  %relacion de mezcla de saturacion.
         r(j)=rs(j); %Asumimos que la parcela se encuentra saturada a lo largo de este proceso.
         %Los valores de las variables de humedad de saturacion se suponen iguales a los da la parcela por encima del NCA.
         w(j)=r(1)-rs(j); %El contenido de agua liquida se obtiene por simple resta entre el vapor inicial y el actual. 
         Tro(j)=T(j)*(1+Rv*rs(j)/Rd)/(1+rs(j)); %Temperatura de densidad.   
         alfa=1/(1+Rv*rs(j)/Rd);
         Kapa(j)=(Rd/Cpd)*(1+Rv/Rd*rs(j))/(1+Cpv/Cpd*rs(j)); %Kapa ahora es una funcion variable, porque r comienza a variar.
         Lv(j)=L1-L2*T(j);     %Ecuacion para la dependencia a orden 1 de Lv con T basandose en la ley de Kirchhoff
         Lvcontrol(j)=Lv(j);
         %A y B son dos parametros cuyo cociente nos da la tasa de varaicion de T con P para un proceso reversible. Para obtener
         %las expreciones de A y B hay que descomponer todos los diferenciales de la ecuacion (8) del paper de Lipps y Hemler y 
         %reagrupar los terminos multiplicados por dT y los multiplicados por dP, lo que acompaa a dT se lo llamo B y lo que 
         %acompaa a dP se lo llamo A. 
         A=Cpd*Kapa(j)*T(j)/P(j)+Lv(j)*alfa*rs(j)/Pd(j);
         B=(Cpd+ alfa*P(j)*(Lv(j)^2)*rs(j)/(Pd(j)*Rv*(T(j)^2)) +(Cpd*rs(j)*beta)); 
         %Hasta aca los calculos previos
         %A continuacion el paso de integracion
         T(j+1)=T(j)-deltaP*A/B;
         P(j+1)=P(j)-deltaP;
end


for j=nsat+1:nP
%El resto de los pasos con un metodo centrado (Leap Frog)

         Es(j)=611.2*exp(17.67*(T(j)-273.16)/(T(j)+243.5-273.16));
         Pd(j)=P(j)-Es(j);
         rs(j)=(Rd/Rv)*Es(j)/(P(j)-Es(j));  %relacion de mezcla de saturacion.
         r(j)=rs(j); %Asumimos que la parcela se encuentra saturada a lo largo de este proceso.
 
         %Los valores de las variables de humedad de saturacion se suponen iguales a los da la parcela por encima del NCA.
         w(j)=r(1)-rs(j); %El contenido de agua liquida se obtiene por simple resta entre el vapor inicial y el actual. 
         Tro(j)=T(j)*(1+Rv*rs(j)/Rd)/(1+rs(j)); %Temperatura de densidad.   
         alfa=1/(1+Rv*rs(j)/Rd);
         Kapa(j)=(Rd/Cpd)*(1+Rv/Rd*rs(j))/(1+Cpv/Cpd*rs(j)); %Kapa ahora es una funcion variable, porque r comienza a variar.
          Lv(j)=L1-L2*T(j);     %Ecuacion para la dependencia a orden 1 de Lv con T basandose en la ley de Kirchhoff

         %A y B son dos parametros cuyo cociente nos da la tasa de varaicion de T con P para un proceso reversible. Para obtener
         %las expreciones de A y B hay que descomponer todos los diferenciales de la ecuacion (8) del paper de Lipps y Hemler y 
         %reagrupar los terminos multiplicados por dT y los multiplicados por dP, lo que acompa a a dT se lo llamo B y lo que 
         %acompa a a dP se lo llamo A. 
         A=Cpd*Kapa(j)*T(j)/P(j)+Lv(j)*alfa*rs(j)/Pd(j);
         B=(Cpd+ alfa*P(j)*(Lv(j)^2)*rs(j)/(Pd(j)*Rv*(T(j)^2)) +(Cpd*rs(j)*beta)); 
         %Hasta aca los calculos previos
         %A continuacion el paso de integracion
         T(j+1)=T(j-1)-2*deltaP*A/B;
         P(j+1)=P(j)-deltaP;


end

%En esta parte asignamos el valor de las variables obtenidas a un nombre especifico para poder utilizarlas mas adelante
%en el programa
%*************************
%En esta parte asignamos el valor de las variables obtenidas a un nombre especifico para poder utilizarlas mas adelante
%en el programa
%Guardamos todos estos resultados en la variable de salida.
%for j=1:nP
    i=1:nP;
out(i,1)=T(i)';%Temperatura de la parcela
out(i,2)=P(i)';%Presion de la parcela
out(i,3)=r(i)';%Relacion de mezcla de la parcela

out(i,5)=Es(i)';%Presion de vapor de saturacion de la parcela
out(i,6)=w(i)';%Contenido de agua liquida de la parcela
out(i,7)=Tro(i)';%Temperatura de densidad de la parcela
%end

%Guardamos los valores del NCA tambien
out(1,8)=Enca;
out(1,9)=Tnca;
out(1,10)=Pnca;
