% Metodo de la parcela, utilizando un proceso reversible.
% Utilizando la ecuacion (8) del Paper de Lipps y Hemler (1979)
% Another Look at the Thermodynamic Equation For Deep Convection. Monthly Weather Review Voulume 108 78-84
% Programa realizado por Juan Jose Ruiz (2005)
% Adaptado para la función operativa (2006)
clear all

%*********************************************************************************************
%Vamos a usar proceso Pseudo adiabático:
%*********************************************************************************************

%Metodo a utilizar: Usamos ajuste a la saturacion (0) o la ecuacion termodinamica de Lipps y Hemler con las 
%distintas aproximaciones que permite este programa?


metodo=1;
ice=0;
latenth=2;
proc=0;

deltaP=1; %Valor del paso de presion en hPa.


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
%Lectura de datos, el siguiente modulo controla el ingreso de datos,
%para cargar los datos, el siguiente formato es necesario:
%un archivo ascii sin encabezado que contenga las siguientes columnas:
%Presion (hPa), Temperatura (ºC) y humedad relativa (%)
%el archivo se debe llamar sondeo.txt.
%*********************************************************************************************

S=load('-ascii', 'input.txt');
nlevels=length(S);
Pentorno(1:nlevels)=S(:,1)*100; %Lee la presion del entorno.
Tentorno(1:nlevels)=S(:,2)+273.16; %Lee la temperatura del entorno.
hrentorno(1:nlevels)=S(:,3)/100; %Lee la humedad relativa del entorno.
Pmin=round(Pentorno(nlevels));
%hago esto para que sea mas facil despues rellenar los datos

%Calculamos las variables de humedad en el entorno.
%En lo posible se evita el uso de ciclos for para optimizar la velocidad de ejecucion.
%Esto es, los ciclos se vectorizan para lograr la mayor velocidad de calculo.
%Para eso se utilizan los productos .* y ./ que son las operaciones punto a punto entre matrices y vectores.
    
    Esentorno(1:nlevels)=611.2.*exp(17.67.*(Tentorno(1:nlevels)-273.16)./(Tentorno(1:nlevels)+243.5-273.16)); %Es en pascales
    Eentorno(1:nlevels)=hrentorno(1:nlevels).*Esentorno(1:nlevels); %E en pascales
    rentorno(1:nlevels)=(Rd/Rv)*Eentorno(1:nlevels)./(Pentorno(1:nlevels)-Eentorno(1:nlevels));   %relacion de mezcla (adimensional)
    qentorno(1:nlevels)=rentorno(1:nlevels)./(1+rentorno(1:nlevels)); %Humedad especifica del entorno
    Tdentorno(1:nlevels)=(243.5.*log(Eentorno(1:nlevels)./100)-440.8)./(19.48-log(Eentorno(1:nlevels)./100))+273.16;
    Tventorno(1:nlevels)=Tentorno(1:nlevels).*(1+0.608*qentorno(1:nlevels)); %Temperatura virtual del entorno.
    

%*********************************************************************************************
%Fin de la lectura de datos.
%*********************************************************************************************



%********************************************************************************************************
% GRAFICADO DE LAS CURVAS DEL EMAGRAMA
%********************************************************************************************************     
       figure
%Genero las curvas del emagrama usando la funcion emagrama.m
%Los parametros de entrada son ThetaMin (linea minima de valores de Theta que traza)
%ThetaMax maximo valor de Theta que dibuja
%DeltaTheta intervalo de las titas.
%Los valores recomendados son ThetaMin=190, ThetaMax=420 y DeltaTheta=10 (Todos en K).
%Deltar controla la cantidad de lineas de relacion de mezcla que se trazan.

     emagrama(190,420,20,2);

%********************************************************************************************************
% FIN DEL GRAFICADO DE LAS CURVAS DEL EMAGRAMA
%********************************************************************************************************     


%**********************************************************************************************************
% GRAFICADO DE LOS DATOS DEL ENTORNO (T Y Td). Y proceso para la parcela de superficie y para la mas inestable.
%********************************************************************************************************
       title('Diagrama aerologico (EMAGRAMA)')
       hold on
       axis([180 320 -1000 -150])
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[-1000 -900 -800 -700 -600 -500 -400 -300 -200 -150])
       set(gca,'YTickLabel',{'1000';'900';'800';'700';'600';'500';'400';'300';'200';'150'})
       set(gca,'Xgrid','on')
       set(gca,'Ygrid','on')
       
       hold on   
       semilogy(Tentorno,-(Pentorno/100),'-b','LineWidth',2)%Temperatura del entorno  
       hold on  
       semilogy(Tdentorno,-(Pentorno/100),':b','LineWidth',2)%Temperatura de rocio del entorno 
       xlabel('');
       
       clear P

       print('-depsc', 'out7.eps')

       close 1;
       
%********************************************************************************************************
% FIN DEL GRAFICADO.
%********************************************************************************************************


%********************************************************************************************************
% FIN DEL SCRIPT :-)
%********************************************************************************************************















