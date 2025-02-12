% Esta funcion genera una figura con las curvas del emagrama trazadas
%Las adiabaticas saturadas se trazan utilizando una ecuacion similar a la ecuacion 8 del paper de Lipps y Hemler
%con la diferencia de que se ignoran los contenidos de agua liquida.
%Los parametros de entrada son ThetaMin (linea minima de valores de Theta que traza)
%ThetaMax maximo valor de Theta que dibuja
%DeltaTheta intervalo de las titas.
%Los valores recomendados son ThetaMin=190, ThetaMax=420 y DeltaTheta=10 (Todos en K).
%Deltar controla la cantidad de lineas de relacion de mezcla que se trazan.
%Programa realizado por Juan Jose Ruiz (2005)

function out = emagrama(ThetaMin,ThetaMax,DeltaTheta,Deltar,Pbot)

%*********************************************************************************************
%Parametros iniciales que controlan el funcionamiento del programa
%Este modulo define los parametros iniciales, el valor de las constantes termodinamicas
%*********************************************************************************************
%Chequeamos los valores de los parametros de entrada. (Falta hacer otro tanto con los demas parametros.
if (Deltar<10) & (Deltar>1)
    Deltar=round(Deltar);
else
    Deltar=1
end

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

%Pasos de integracion para el calculo de las curvas del emagrama
deltaP=0.5; %Valor del paso de presion en hPa. para las curvas saturadas
deltaP2=5;  %Valor del paso de presion para las adiabaticas secas.   
Pmin=50;    %Valor de presion hasta donde calcula las curvas del emagrama
Pi=1050;    %Valor de presion de donde parten las curvas del emagrama
% Calculos iniciales
nP2=round(-(Pmin-Pi)/deltaP2); %No de pasos de integracion para las curvas secas y de r constante
nP=round(-(Pmin-Pi)/deltaP);   %N0 de pasos de integracion para las adiabaticas saturadas
deltaP2=deltaP2*100;             %Conversion del paso de integracion a Pascales
deltaP=deltaP*100;

%********************************************************************************************************
% INTERFASE GRAFICA: LOS LOOPS QUE SIGUEN A CONTINUACION GENERAN LAS CURVAS DEL EMAGRAMA.
%********************************************************************************************************
    
    %Calculo las adiabaticas secas para plotearlas en el emagrama.
    Kapa2=Rd/Cpd;
for theta=ThetaMin:DeltaTheta:ThetaMax
    Tseca(1,theta)=theta*((Pi/1000)^Kapa2); %La temperatura inicial esta dada por el valor de theta llevado al nivel base del                                            %emagrama.
    P(1)=Pi*100; %Las adiabaticas siempre empiezan en la base del sondeo
for j=1:nP2
    
    %Usamos capa del aire seco
    Tseca(j+1,theta)=Tseca(j,theta)-deltaP2*Tseca(j,theta)*Kapa2/(P(j)); %Kapa permanece constante.
    P(j+1)=P(j)-deltaP2;
    
end
    save emagrama.mat Tseca
    semilogy(Tseca(:,theta)-273,-(P/100))
       axis([-93 47 -1050 -150])
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[-1050 -1000 -900 -800 -700 -600 -500 -400 -300 -200 -150])
       set(gca,'YTickLabel',{'1050';'1000';'900';'800';'700';'600';'500';'400';'300';'200';'150'})
       set(gca,'Xgrid','on')
       set(gca,'Ygrid','on')

    hold on
end

   %calculo de las lineas de relacion de mezcla de saturacion constante
   
   %Primero calculamos entre 0.1 y 1 cada 0.1
   %Tenemos que calcular dT/dP para un proceso donde se conserve el r de saturacion, para eso
   %escribimos drs en funcion de dT y dP usando Clausius Clayperon e integramos T(P) a rs constante
   %Hay que hacerlo de esta manera porque el grafico del emagrama es T en funcion de P.
   %Antes de integrar la curva necesitamos calcular a que temperatura le corresponde el rs dado en el nivel de 1000 hPa.
   %A partir de ese valor se integra la ecuacion hallada hasta el nivel de 200 hPa.
   
   %Primero calculamos entre 0.1 y 1 cada 0.1
   for rs=1:Deltar:10
       rsat=rs/10000;
      P(1)=Pi*100; 
       es=rsat*P(1)/(rsat+Rd/Rv)/100;
   %Para calcular la temperatura de Rocio para el valor de es hallado, se utiliza la ecuacion (11) del paper de Bolton (1980)
       Tsat(1,rs)=273.16+(243.5*log(es)-440.8)/(19.48-log(es));
       P(1)=Pi*100;
       Lv(1)=L1-L2*Tsat(1,rs);
   for j=1:nP2
       Tsat(j+1,rs)=Tsat(j,rs)-deltaP2*Rv*(Tsat(j,rs)^2)/(P(j)*Lv(j));
       P(j+1)=P(j)-deltaP2;
       Lv(j+1)=L1-L2*Tsat(j+1,rs);
   end
       Tsat1=Tsat;
       save emagrama.mat Tsat1
       semilogy(Tsat(:,rs)-273,-(P/100),'-.r')
       
       hold on
       
   end
   % Calculamos entre 1 y 10 g/kg cada 1 g/kg.   
       for rs=1:Deltar:10
       rsat=rs/1000;
       P(1)=Pi*100; 
       es=rsat*P(1)/(rsat+Rd/Rv)/100;
   %Para calcular la temperatura de Rocio para el valor de es hallado, se utiliza la ecuacion (11) del paper de Bolton (1980)
   %Ver referencia mas arriba.
       Tsat(1,rs)=273.16+(243.5*log(es)-440.8)/(19.48-log(es));
       P(1)=Pi*100;
       Lv(1)=L1-L2*Tsat(1,rs);

   for j=1:nP2
       Tsat(j+1,rs)=Tsat(j,rs)-deltaP2*Rv*(Tsat(j,rs)^2)/(P(j)*Lv(j));
       P(j+1)=P(j)-deltaP2;
       Lv(j+1)=L1-L2*Tsat(j+1,rs);
   
   end 
       Tsat2=Tsat;
       save emagrama.mat Tsat2
       semilogy(Tsat(:,rs)-273,-(P/100),'-.r')
       
       hold on
       
   end 

   % Calculamos entre 10 y 50 g/kg cada 5 g/kg.   
       for rs=10:Deltar*5:50
       rsat=rs/1000;
       P(1)=Pi*100; 
       es=rsat*P(1)/(rsat+Rd/Rv)/100;
       
   %Para calcular la temperatura de Rocio para el valor de es hallado, se utiliza la ecuacion (11) del paper de Bolton (1980)

       Tsat(1,rs)=273.16+(243.5*log(es)-440.8)/(19.48-log(es));
       P(1)=Pi*100;
       Lv(1)=L1-L2*Tsat(1,rs);

   for j=1:nP2
       Tsat(j+1,rs)=Tsat(j,rs)-deltaP2*Rv*(Tsat(j,rs)^2)/(P(j)*Lv(j));
       P(j+1)=P(j)-deltaP2;
       Lv(j+1)=L1-L2*Tsat(j+1,rs);
   
   end 
       Tsat3=Tsat;
       save emagrama.mat Tsat3
       semilogy(Tsat(:,rs)-273,-(P/100),'-.r')
       
       hold on
       save emagrama.mat Tsat 
   end 
   


    %Calculo los procesos saturados humedos.
    %Partiendo de los valores de T en Pmin que corresponden a las titas seleccionadas (de 190 a 420 cada 10 grads)
    %se realiza la integracion hacia el nivel de 1000 hPa suponiendo saturacion pero ningun contenido de agua liquida presente
    %por lo tanto estas curvas representan un proceso pseudo adiabatico, donde la eliminacion del agua liquida fue
    %la unica simplificacion que se realizo con respecto a la ecuacion (8) del paper de Lipps y Hemmler.
    %Dado que el calculo de DT/DP requiere mayor precision para los procesos humedos, se usa el paso de integracion 1 que es mas corto.
for theta=ThetaMin+60:DeltaTheta:ThetaMax
    Tpseudo(nP+1,theta)=Tseca(nP2+1,theta); %Defino de donde arrancan las curvas en Pmin (hago que coincidan con las titas secas)
    P(nP+1)=Pmin*100;%La integracion ahora arranca de Pmin y P va aumentando hasta 1000hPa.
    
    %Primer paso con Euler
for j=nP+1:-1:2
    Kapa2=Rd/Cpd;
    Es(j)=611.2*exp(17.67*(Tpseudo(j,theta)-273.16)/(Tpseudo(j,theta)+243.5-273.16));
    Pd(j)=P(j)-Es(j);
    rs(j)=(Rd/Rv)*Es(j)/(P(j)-Es(j));  %relacion de mezcla de saturacion.
    alfa=1/(1+Rv*rs(j)/Rd);
    %Kapa(j)=(Rd/Cpd)*(1+Rv/Rd*rs(j))/(1+Cpv/Cpd*rs(j)); %Kapa ahora es una funcion variable, porque r comienza a variar.
    Lv(j)=L1-L2*Tpseudo(j,theta);     %Ecuacion para la dependencia a orden 1 de Lv con T basandose en la ley de Kirchhoff        
    A(j)=Cpd*Kapa2*Tpseudo(j,theta)/P(j)+Lv(j)*alfa*rs(j)/Pd(j);
    B(j)=(Cpd+ alfa*P(j)*(Lv(j)^2)*rs(j)/(Pd(j)*Rv*(Tpseudo(j,theta)^2)) +(Cpd*rs(j)*beta));   
    Tpseudo(j-1,theta)=Tpseudo(j,theta)+deltaP*A(j)/B(j); %Kapa permanece constante.
    P(j-1)=P(j)+deltaP;
    
end
    
    
for j=nP:-1:2
    Kapa2=Rd/Cpd;
    Es(j)=611.2*exp(17.67*(Tpseudo(j,theta)-273.16)/(Tpseudo(j,theta)+243.5-273.16));
    Pd(j)=P(j)-Es(j);
    rs(j)=(Rd/Rv)*Es(j)/Pd(j);  %relacion de mezcla de saturacion.
%Los valores de las variables de humedad de saturacion se suponen iguales a los da la parcela por encima del NCA.
    alfa=1/(1+Rv*rs(j)/Rd);
    Lv(j)=L1-L2*Tpseudo(j,theta);     %Ecuacion para la dependencia a orden 1 de Lv con T basandose en la ley de Kirchhoff        
    A=Cpd*Kapa2*Tpseudo(j,theta)/P(j)+Lv(j)*alfa*rs(j)/Pd(j);
    B=(Cpd+ alfa*P(j)*(Lv(j)^2)*rs(j)/(Pd(j)*Rv*(Tpseudo(j,theta)^2)) +(Cpd*rs(j)*beta));   
    Tpseudo(j-1,theta)=Tpseudo(j-1,theta)+2*deltaP*A/B; %Kapa permanece constante.
    P(j-1)=P(j)+deltaP;
    %Las ecuaciones son las mismas que las utilizadas para el calculo del metodo de la parcela, solo que el paso de integracion
    %es al reves para ir desde arriba hacia abajo.
    
end
    semilogy(Tpseudo(:,theta)-273,-(P/100),'--m')
      

    hold on
end
   


%***********************************************************************************************************************
%FIN DE LA INTERFASE GRAFICA DEL EMAGRAMA
%***********************************************************************************************************************
%Esto indica que la funcion termino bien
out=1;












