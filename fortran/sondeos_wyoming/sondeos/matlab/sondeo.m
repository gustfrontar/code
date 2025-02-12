% Metodo de la parcela, utilizando un proceso reversible.
% Utilizando la ecuacion (8) del Paper de Lipps y Hemler (1979)
% Another Look at the Thermodynamic Equation For Deep Convection. Monthly Weather Review Volume 108 78-84
% Programa realizado por Juan Jose Ruiz (2005)
% Adaptado para la funcion operativa (2006)


%Mejoras a realizar en el futuro:

clear all

%*********************************************************************************************
%*********************************************************************************************

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
%el archivo se debe llamar sondeo.txt.
%*********************************************************************************************

S=load('-ascii', 'input.txt');
nlevels=length(S);
Pentorno(1:nlevels)=S(:,1)*100; %Lee la presion del entorno.
Tentorno(1:nlevels)=S(:,2)+273.16; %Lee la temperatura del entorno.
hrentorno(1:nlevels)=S(:,3)/100; %Lee la humedad relativa del entorno.
Pmin=round(Pentorno(nlevels));
DIRE(1:nlevels)=S(:,4)*pi/180;  %Direccion en radianes     
SPEEDE(1:nlevels)=S(:,5)*1.85/3.6;   %Velocidad en M/S

%Calculamos U y V a partir de DIR y SPEED
DIRE(DIRE==-999)=NaN;
SPEEDE(SPEEDE==-999)=NaN;

UE=-SPEEDE.*sin(DIRE);
VE=-SPEEDE.*cos(DIRE);

%hago esto para que sea mas facil despues rellenar los datos
Tparcela2=zeros(nlevels,nlevels);
Pparcela2=zeros(nlevels,nlevels);
rparcela2=zeros(nlevels,nlevels);
qparcela2=zeros(nlevels,nlevels);
Troparcela2=zeros(nlevels,nlevels);
Esparcela2=zeros(nlevels,nlevels);
wparcela2=zeros(nlevels,nlevels);

%Mas adelante en este punto se puede insertar una funcion que realice un
%breve control de calidad de los datos ingeridos, sobre todo si se pretende
%usar este programa como base de un proceso operativo.


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


%*********************************************************************************************
%INICIO DEL CALCULO DEL METODO DE LA PARCELA PARA CALCULAR CAPE Y DCAPE.
%Aca se inicia un ciclo que aplica el metodo de la parcela a los diferentes niveles del sondeo
%Esto se hace segun las especificaciones que se usan para llamar a la funcion parcelita.m
%Este ciclo en particular calcula los ascensos.
%*********************************************************************************************
%*********************************************************************************************

for nivel=1:nlevels
%Esta funcion calcula el proceso pseudo adiabatico de descenso de una parcela.
%Primero calcula la Temperatura isobarica de bulbo humedo (a traves de la conservacion de la entalpia)
%esto es equivalente a suponer que primero consideramos las condiciones del entorno y que a partir de dichas
%condiciones tenemos agua disponible de forma tal que el aire del entorno alcanza la saturacion a presion 
%constante (Se llega a la Tiw). A partir de ese punto la parcela desciende segun un proceso que solo puede
%ser pseudoadiabatico, ya que necesitamos suponer que constantemente se le entrega agua al sistema durante
%el descenso, lo que permite que la parcela se mantenga saturada.   

%Solo calculo este proceso para parcelas de 700 hPa hacia abajo, porque es
%un proceso que intenta representar la evaporacion de la lluvia por debajo
%de la base de nubes y por ende no tiene sentido calcularlo para parcelas
%en niveles medios y superiores que seguramente estan por encima de la base
%de nubes.
%Algunas limitaciones de esta estimacion es que no tiene en cuenta el
%efecto dinamico de la superficie, el efecto de la pileta de aire frio que
%frenaria las descendentes y el efecto de la carga de hidrometeoros que
%podria forzar descendentes desde niveles superiores. Por otro lado, asume
%que la parcela siempre se encuentra saturada, cuando en la realidad las
%descendentes pueden llegar secas a superficie.


result=parcelita2(Tentorno(nivel),Pentorno(nivel),rentorno(nivel),Pentorno(1),deltaP);
nP=length(result(:,1));
nP2=nP;
Tparcela(1:nP,nivel)=result(1:nP,1);
Pparcela(1:nP,nivel)=result(1:nP,2);
rparcela(1:nP,nivel)=result(1:nP,3);
Esparcela(1:nP,nivel)=result(1:nP,4);
Troparcela(1:nP,nivel)=result(1:nP,5);
wparcela(1:nP,nivel)=0;
clear result;



%****************************************************************************************************************    
% Metodo de la parcela, utilizando un proceso reversible. Este paso se realiza utilizando la funcion parcelita.m
% Utilizando la ecuacion (8) del Paper de Lipps y Hemler (1979)
% Another Look at the Thermodynamic Equation For Deep Convection. Monthly Weather Review Voulume 108 78-84
%
%Los parametros de entrada para esta funcion son la presion en el nivel de donde sale la parcela (Pinicial)
%la temperatura del nivel de donde proviene la parcela y la HR (Tinicial y HRinicial).
%La presion hasta la cual se integrara la ecuacion (Pmin), el paso de integracion (deltaP, se recomiendo utilizar
%un valor de 0.5 hPa.) 
%Como el proceso es pseudo-adiabatico, entonces Tro de la parcela es la T virtual. Por tanto
%no tiene sentido plotear las diferencias de T y las de Tro porque las de Tro van a resultar mayores, pero esto 
%no es contradictorio con la teoria de los procesos reversibles porque justamente no estamos considerando ese tipo 
%de proceso en este caso.
result=parcelita(round(Pentorno(nivel)),Tentorno(nivel),hrentorno(nivel),Pmin,deltaP);
%*************************
nP=length(result(:,1));

i=(nP2):(nP2+nP-1);
i2=1:nP;
Tparcela(i,nivel)=result(i2,1);
Pparcela(i,nivel)=result(i2,2);
rparcela(i,nivel)=result(i2,3);

Esparcela(i,nivel)=result(i2,5);
wparcela(i,nivel)=result(i2,6);
Troparcela(i,nivel)=result(i2,7);

Enca(nivel)=result(1,8);
Tnca(nivel)=result(1,9);
Pnca(nivel)=result(1,10);

clear result;

%********************************************************************************************************
%Interpolamos los resultados a los niveles presentes en el sondeo usando la
%funcion INTERP1 de MATLAB.
%********************************************************************************************************

Tparcela2(:,nivel)=interp1(Pparcela(:,nivel),Tparcela(:,nivel),Pentorno);

Pparcela2=Pentorno;

rparcela2(:,nivel)=interp1(Pparcela(:,nivel),rparcela(:,nivel),Pentorno);

Troparcela2(:,nivel)=interp1(Pparcela(:,nivel),Troparcela(:,nivel),Pentorno);

Esparcela2(:,nivel)=interp1(Pparcela(:,nivel),Esparcela(:,nivel),Pentorno);

wparcela2(:,nivel)=interp1(Pparcela(:,nivel),wparcela(:,nivel),Pentorno);
Tdif2(:,nivel)=Tparcela2(:,nivel)-Tentorno';          %Esta diferencia se plotea para obtener el diagrama de estabilidad.
Trodif2(:,nivel)=Troparcela2(:,nivel)-Tventorno';        %Esta diferencia se usa para calcular el CAPE.


%********************************************************************************************************
%Este end corresponde al ciclo que realiza el metodo de la parcela para los distintos niveles del sondeo.
%********************************************************************************************************
end

%********************************************************************************************************
%CALCULO DEL CAPE Y DE LAS VELOCIDADES VERTICALES
%********************************************************************************************************
%La funcion cape.m calcula NA, PA, CAPE, DNA, DPA y DCAPE junto con las velocidades verticales para los 
%distintos niveles del sondeo, a partir de los valores de diferencia de Tro y T antes calculados.
%El parametro final define si para calcular el CAPE usamos las diferencias de T de densidad (lo correcto)
%o las diferencias de Temperatura (lo que usualmente se hace en el EMAGRAMA.

result=cape(Trodif2,Pentorno,1);

%Asignamos los resultados a variables con nombres reconocibles. (los mismos con los que aparecen en la funcion).
%for i=1:nlevels
 %   for j=1:nlevels
        i=1:nlevels;
        vvel(i,i)=result(i,i);
        %  end
        %end
%for i=1:nlevels Loop vectorizado
    i=1:nlevels;
    NA(i)=result(i,nlevels+1);
    PA(i)=result(i,nlevels+2);
    CAPE(i)=result(i,nlevels+3);
    DNA(i)=result(i,nlevels+4);
    DPA(i)=result(i,nlevels+5);
    DCAPE(i)=result(i,nlevels+6);
    %end
clear result;

%Buscamos el nivel donde el CAPE es maximo:
MUCAPE=0; %Valor del CAPE maximo.
MUlevel=1;%Nivel del CAPE maximo.
%for i=1:nlevels
    i=1:nlevels;
    %if (CAPE(i)>MUCAPE)
    MUCAPE=max(PA(i));
    j=find(PA==MUCAPE);
    nj=length(j)
    jj=1:nj
    MUlevel=min(j(jj));
    if(MUCAPE==0);
    MUlevel=1;
    end
    if(Pentorno(j)< 50000);
        MUlevel=1;
    end
    %    end
    %end
MUPlevel=Pentorno(MUlevel);
%Imprime por pantalla estas cantidades.

%**************************************************************************
%Vamos a enmascarar los movimientos descendentes segun el siguiente
%criterio:
%**************************************************************************
%Busco el NCA correspondiente al nivel + alto con CAPE positivo o el nivel
%de 700 hPa.

%Primera posibilidad, no hay CAPE positivo... calculo de superficie a 700.
%La idea es que aunque el sondeo no tenga CAPE en alguna situacion nos
%puede intersar saber que pasaria con las rafagas si se formara una
%tormenta en dicho entorno.

i=find(PA>0 & Pentorno > 70000 );
if(length(i)==0 )
    max_lev=70000; %Fijo el maximo nivel para descendentes en 700 hPa.
else
    max_lev=min(Pentorno(i));
end

for i=1:nlevels
    for j=1:nlevels
     if(i < j)
         if(Pentorno(j) < max_lev)
             Trodif2(i,j)=NaN;
             Tdif2(i,j)=NaN;
             wparcela2(i,j)=NaN;
             vvel(i,j)=NaN;
         end
     end
    end
end




%********************************************************************************************************
% GRAFICADO DE LAS DIFERENCIAS DE TEMPERATURA
%********************************************************************************************************
       %Trodif

       figure
       v=[ -60 -50 -40 -30 -20 -15 -10 -5 -2.5 -1 1 2.5 5 7.5 10 12.5 15 17.5 20];
       vcol=[49 48 47 46 45 44 43 42 41 2 22 23 24 25 26 27 28 29];
       [C,h]=contourf(Pentorno/100,-Pentorno/100,Tdif2,v);
       hold on
       plot(Pentorno/100,-Pentorno/100,'k--','Linewidth',4);
       clabel(C)
       title('Diferencias entre la T de la parcela y la temperatura del entorno (C)')
       plot_jrcol(v,vcol,0);
  
       xmax=Pentorno(1)/100;
       xmin=500;
       ymax=-Pentorno(nlevels)/100;
       ymin=-Pentorno(1)/100;
       axis([xmin xmax ymin ymax])
       title('Diferencias entre la Temperatura virtual de la parcela y la temperatura virtual del entorno (C)')
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[-1000 -900 -800 -700 -600 -500 -400 -300 -200 -100])
       set(gca,'YTickLabel',{'1000';'900';'800';'700';'600';'500';'400';'300';'200';'100'})
       set(gca,'Ygrid','on')
       set(gca,'XTickMode','manual')
       set(gca,'XTick',[100 200 300 400 500 600 700 800 900 1000])
       set(gca,'XTickLabel',{'100';'200';'300';'400';'500';'600';'700';'800';'900';'1000'})
       set(gca,'Xgrid','on')
       xlabel('Nivel de origen (hPa)');
       ylabel('Nivel de llegada (hPa)');
       print('-dpng', 'out2.png')

       
       close 1;

%********************************************************************************************************
% GRAFICADO DE LOS CONTENIDOS DE AGUA LIQUIDA.
%********************************************************************************************************    
       %Wparcela
       figure
       v=[0.0001 1 2.5 5 7.5 10 12.5 15 17.5 20];
       vcol=[31 32 33 34 35 36 37 38 39];
       [C,h]=contourf(Pparcela2/100,-Pparcela2/100,wparcela2*1000,v);
       hold on
       plot(Pparcela2/100,-Pparcela2/100,'k--','Linewidth',4);
       clabel(C)
       title('Condensado total (g/kg)')
       plot_jrcol(v,vcol,0);
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[-1000 -900 -800 -700 -600 -500 -400 -300 -200 -100])
       set(gca,'YTickLabel',{'1000';'900';'800';'700';'600';'500';'400';'300';'200';'100'})
       set(gca,'Ygrid','on')
       set(gca,'XTickMode','manual')
       set(gca,'XTick',[100 200 300 400 500 600 700 800 900 1000])
       set(gca,'XTickLabel',{'100';'200';'300';'400';'500';'600';'700';'800';'900';'1000'})
       set(gca,'Xgrid','on')
       xmax=Pentorno(1)/100;
       xmin=500;
       ymax=-Pentorno(nlevels)/100;
       ymin=-Pentorno(1)/100;
       axis([xmin xmax ymin ymax])
       xlabel('Nivel de origen (hPa)');
       ylabel('Nivel de llegada (hPa)');
       print('-dpng', 'out3.png')
       xmax=Pentorno(1)/100;
       xmin=500;
       %!gs -q -dNOPAUSE -sDEVICE=png256 -r100x100 -sOutputFile=out3.png out3.eps -c quit
       %!rm -f ./out3.eps

       close 1;
          
       
%********************************************************************************************************
% GRAFICADO DE LAS VELOCIDADES VERTICALES
%********************************************************************************************************
       %vvel

       figure
       v=[1 5 10 20 30 40 50 60 70 80 90 100];
       [C,h]=contourf(Pparcela2/100,-Pparcela2/100,vvel,v);
       plot_jrcol(v,[21 22 23 24 25 26 27 28 29 54 55],0);
       hold on
       plot(Pentorno/100,-Pentorno/100,'k--','Linewidth',4);
       clabel(C)
       hold on
       vv=[-1 -5 -10 -20 -30 -40 -50 -60 -70 -80 -90 -100];
       [F,h]=contour(Pentorno/100,-Pentorno/100,vvel,vv,'--b');
       %clabel(F)
       title('Velocidades verticales para las parcelas ascendentes y descendentes (m/s)')
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[-1000 -900 -800 -700 -600 -500 -400 -300 -200 -100])
       set(gca,'YTickLabel',{'1000';'900';'800';'700';'600';'500';'400';'300';'200';'100'})
       set(gca,'Ygrid','on')
       set(gca,'XTickMode','manual')
       set(gca,'XTick',[100 200 300 400 500 600 700 800 900 1000])
       set(gca,'XTickLabel',{'100';'200';'300';'400';'500';'600';'700';'800';'900';'1000'})
       set(gca,'Xgrid','on')
       xmax=Pentorno(1)/100;
       xmin=500;
       ymax=-Pentorno(nlevels)/100;
       ymin=-Pentorno(1)/100;
       axis([xmin xmax ymin ymax])
       xlabel('Nivel de origen (hPa)');
       ylabel('Nivel de llegada (hPa)');
       print('-dpng','-r0','out4.png')
       %!gs -q -dNOPAUSE -sDEVICE=png256 -r100x100 -sOutputFile=out4.png out4.eps -c quit
       %!rm -f ./out4.eps
       
       close 1;
       
%********************************************************************************************************
% GRAFICADO DE LOS CAPES Y DECAPES EN FUNCION DE LA PRESION DEL NIVEL DE ORIGEN DE LA PARCELA
%********************************************************************************************************  
       %CAPE
       figure
       %plot(CAPE,-(Pentorno/100),'-b','LineWidth',2)%CAPE
       hold on
       plot(NA,-(Pentorno/100),'-g','LineWidth',2)%AREA NEGATIVA
       hold on
       plot(PA,-(Pentorno/100),'-r','LineWidth',2)%AREA POSITIVA
       title('CAPE y CINE en funcion de la presion')
       legend('CINE','CAPE')
       axis([-1000 6000 -1000 -500])
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[-1000 -900 -800 -700 -600 -500 ])
       set(gca,'YTickLabel',{'1000';'900';'800';'700';'600';'500'})
       set(gca,'Xgrid','on')
       set(gca,'Ygrid','on')
       xlabel('Energia (J/Kg)');
       ylabel('Nivel de origen (hPa)');
       %legend('CAPE','Area Negativa','Area Positiva')

       print('-dpng','-r0', 'out5.png')
       %!gs -q -dNOPAUSE -sDEVICE=png256  -r100x100 -sOutputFile=out5.png out5.eps -c quit
       %!rm -f ./out5.eps


       close 1;
       
       %DCAPE
       figure
       plot(-DCAPE,-(Pentorno/100),'-b','LineWidth',2)%DCAPE
       hold on
       plot(-DNA,-(Pentorno/100),'-g','LineWidth',2)%AREA NEGATIVA
       hold on
       plot(-DPA,-(Pentorno/100),'-r','LineWidth',2)%AREA POSITIVA
       title('DCAPE(azul), DPA(rojo) y DNA(verde) en funcion de la presion')
       axis([-1000 2000 -1000 -700])
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[-1000 -950 -900 -850 -800 -750 -700])
       set(gca,'YTickLabel',{'1000';'950';'900';'850';'800';'750';'700'})
       set(gca,'Xgrid','on')
       set(gca,'Ygrid','on')
       xlabel('Energia (J/Kg)');
       ylabel('Nivel de origen (hPa)');
       legend('DCAPE','Area Negativa','Area positiva')

       print('-dpng','-r0', 'out6.png')
       %!gs  -dNOPAUSE -sDEVICE=png256 -r100x100 -sOutputFile=out6.png out6.eps -q quit
       %!rm -f ./out6.eps

       %close 1;

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
     Pbot=max(Pentorno(:));
     emagrama(190,420,20,2,Pbot);
%********************************************************************************************************
% FIN DEL GRAFICADO DE LAS CURVAS DEL EMAGRAMA
%********************************************************************************************************     


%**********************************************************************************************************
% GRAFICADO DE LOS DATOS DEL ENTORNO (T Y Td). Y proceso para la parcela de superficie y para la mas inestable.
%********************************************************************************************************
       title('Diagrama aerologico (EMAGRAMA)')
       semilogy(Tparcela2(:,1)-273,-(Pentorno/100),'-.g','LineWidth',2)%metodo de la parcela superficie
       hold on
       semilogy(Tparcela2(:,MUlevel)-273,-(Pentorno/100),'-.r','LineWidth',2)%metodo de la parcela para
       %la parcela mas inestable
       pmax=Pbot/100;
       axis([-93 47 -1050 -150])

      % axis([180 320 -1000 -150])
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[-1000 -900 -800 -700 -600 -500 -400 -300 -200 -150])
       set(gca,'YTickLabel',{'1000';'900';'800';'700';'600';'500';'400';'300';'200';'150'})
       set(gca,'Xgrid','on')
       set(gca,'Ygrid','on')
       
       hold on   
       semilogy(Tentorno-273,-(Pentorno/100),'-b','LineWidth',2)%Temperatura del entorno  
       hold on  
       semilogy(Tdentorno-273,-(Pentorno/100),'-m','LineWidth',2)%Temperatura de rocio del entorno 
       xlabel('Temperatura (C)');
       ylabel('Pres. (hPa)');
       %legend('Parcela de superficie','Parcela mas inestable','Temperatura','Temperatura de rocio')
       
       clear P

       print('-dpng','-r0', 'out7.png')
       %!gs -q -dNOPAUSE  -r100x100 -sDEVICE=png256 -sOutputFile=out7.png out7.eps -c quit
       %!rm -f ./out7.eps

       close 1;
       

%**********************************************************************************************************
% GRAFICADO DE LA HODOGRAFA
%********************************************************************************************************

       
       %Antes de graficar aplicamos un suavizado de promedios moviles de 
       UEs = smooth(Pentorno,UE,7,'rloess');
       VEs = smooth(Pentorno,VE,7,'rloess');
       

       figure


       %Vamos a plotear con diferentes colores de acuerdo al nivel.
       i_pres=find(Pentorno >= 85000 )   %Busco los niveles por debajo de 850.
       plot(UEs(i_pres),VEs(i_pres),'r-','LineWidth',2);
       lastlev=Pentorno(i_pres(end));
       clear i_pres
       hold on
       i_pres=find(Pentorno <= lastlev & Pentorno >= 50000 )
       plot(UEs(i_pres),VEs(i_pres),'g-','LineWidth',2);
       lastlev=Pentorno(i_pres(end));
       clear i_pres
       i_pres=find(Pentorno <= lastlev & Pentorno >= 30000 )
       plot(UEs(i_pres),VEs(i_pres),'b-','LineWidth',2);
       lastlev=Pentorno(i_pres(end));
       clear i_pres
       i_pres=find(Pentorno <= lastlev & Pentorno >= 10000 )
       plot(UEs(i_pres),VEs(i_pres),'k-','LineWidth',2);
       



       %Ploteamos los circulos concentricos.
       alfa=-pi:0.01:pi;
       auxx=cos(alfa);
       auxy=sin(alfa);
       plot(5*auxx,5*auxy,'k--','Linewidth',1);
       text(0,5,'5 m/s','HorizontalAlignment','left')
       hold on
       plot(10*auxx,10*auxy,'k--','Linewidth',1);
       text(0,10,'10 m/s','HorizontalAlignment','left')
       plot(25*auxx,25*auxy,'k--','Linewidth',1);
       text(0,25,'25 m/s','HorizontalAlignment','left')
       plot(50*auxx,50*auxy,'k--','Linewidth',1);
       text(0,50,'50 m/s','HorizontalAlignment','left')
       plot(75*auxx,75*auxy,'k--','Linewidth',1);
       text(0,75,'75 m/s','HorizontalAlignment','left')
       
       
       clear i_pres
       text(UEs(1)+0.5,VEs(1),strcat(num2str(Pentorno(1)/100),' hPa.'),'HorizontalAlignment','left')
       plot(UEs(1),VEs(1),'o')
       i_pres=find(Pentorno==85000);
       if(~isempty(i_pres))
           plot(UEs(i_pres),VEs(i_pres),'o')
           text(UEs(i_pres)+0.5,VEs(i_pres),'850 hPa.','HorizontalAlignment','left')
       end
       clear i_pres
       i_pres=find(Pentorno==50000);
       if(~isempty(i_pres))
           plot(UEs(i_pres),VEs(i_pres),'o')
           text(UEs(i_pres)+0.5,VEs(i_pres),'500 hPa.','HorizontalAlignment','left')
       end
       clear i_pres
       i_pres=find(Pentorno==30000);
       if(~isempty(i_pres))
           plot(UEs(i_pres),VEs(i_pres),'o')
           text(UEs(i_pres)+0.5,VEs(i_pres),'300 hPa.','HorizontalAlignment','left')
       end
       clear i_pres
       i_pres=find(Pentorno==20000);
       if(~isempty(i_pres))
           plot(UEs(i_pres),VEs(i_pres),'o')
           text(UEs(i_pres)+0.5,VEs(i_pres),'200 hPa.','HorizontalAlignment','left')
       end
       
       i_pres=find(Pentorno >= 25000);
       aux=ceil(1.2*max(SPEEDE(i_pres)));
       axis([-aux aux -aux aux]);
       
       %Graficamos las lineas de 0
       plot(-100:100:100,[0 0 0],'k--','LineWidth',1)
       plot([0 0 0],-100:100:100,'k--','LineWidth',1)

       axis('square')
       
      legend('sup-850','850-500','500-300','250...','Location','NorthEastOutside')
       
              title('Hodografa')
              xlabel('Componente U m/s')
              ylabel('Componente V m/s')
       
       print('-dpng','-r0', 'out8.png')
       %!gs -q -dNOPAUSE  -r100x100 -sDEVICE=png256 -sOutputFile=out8.png out8.eps -c quit
       %!rm -f ./out8.eps

       close 1;
       
%********************************************************************************************************
% FIN DEL GRAFICADO.
%********************************************************************************************************


%********************************************************************************************************
% FIN DEL SCRIPT :-)
%********************************************************************************************************















