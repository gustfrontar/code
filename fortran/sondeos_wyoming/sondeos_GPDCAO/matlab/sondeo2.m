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
%Otros parametros son: proceso (proceso=1 ecuacion completa, proceso=0 ecuacion sin el agua liquida)
%latente: Este parametro define como se va a considerar el calor latente de condensacion en las ecuaciones:
%latente=0 Lv constante e igual al valor para 0 grados
%latente=1 Lv constante e igual al valor en la base de la nube
%latente=2 Lv variable y funcion de la temperatura a lo largo del proceso (valor de default)
%El octavo parametro indica si el proceso va a tener el hielo en cuenta o no. Si es 0 no se tiene en cuenta
%si es 1 a partir de una determinada temperatura se considera que el condensado se congela y a partir de ese
%punto se utiliza el Es con respecto al hielo y el L de sublimacion.
result=parcelita(round(Pentorno(nivel)),Tentorno(nivel),hrentorno(nivel),Pmin,deltaP);
%*************************
%Asignamos los resultados a variables con nombres reconocibles a continuacion de los correspondientes al descenso.
nP=length(result(:,1));
%for k=nP2+1:nP2+nP Loop vectorizado.
i=(nP2):(nP2+nP-1);
i2=1:nP;
Tparcela(i,nivel)=result(i2,1);
Pparcela(i,nivel)=result(i2,2);
rparcela(i,nivel)=result(i2,3);

Esparcela(i,nivel)=result(i2,5);
wparcela(i,nivel)=result(i2,6);
Troparcela(i,nivel)=result(i2,7);
%end
Enca(nivel)=result(1,8);
Tnca(nivel)=result(1,9);
Pnca(nivel)=result(1,10);

clear result;

%********************************************************************************************************
%Interpolamos los resultados a los niveles presentes en el sondeo usando la funcion interpol.m
%********************************************************************************************************

result2=interpol(Pparcela(:,nivel),Tparcela(:,nivel),rparcela(:,nivel),Troparcela(:,nivel),Esparcela(:,nivel),wparcela(:,nivel),Pentorno,Tentorno,Tventorno,1,nivel);

%Asignamos los resultados a variables con nombres reconocibles a partir de ahora, el numero 2 al final de las
%variables de la parcela indica que estos son los vectores interpolados a los niveles del sondeo.
nPentorno=length(result2(:,1));

%for k=1:nPentorno Loop vectorizado.
    i=1:nPentorno;
Tparcela2(i,nivel)=result2(i,1);
Pparcela2(i,nivel)=result2(i,2);
rparcela2(i,nivel)=result2(i,3);
Troparcela2(i,nivel)=result2(i,5);
Esparcela2(i,nivel)=result2(i,6);
wparcela2(i,nivel)=result2(i,7);
Tdif2(i,nivel)=result2(i,8);          %Esta diferencia se plotea para obtener el diagrama de estabilidad.
Trodif2(i,nivel)=result2(i,9);        %Esta diferencia se usa para calcular el CAPE.
%end

clear result2;

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

result=cape(Tdif2,Trodif2,Pentorno,1);

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
    MUCAPE=max(CAPE(i));
    j=find(CAPE==MUCAPE);
    nj=length(j)
    jj=1:nj
    MUlevel=min(j(jj));
    if(MUCAPE==0);
    MUlevel=1;
    end
    %    end
    %end
MUPlevel=Pentorno(MUlevel);
%Imprime por pantalla estas cantidades.
MUCAPE;
MUPlevel;

%********************************************************************************************************
% GRAFICADO DE LAS DIFERENCIAS DE TEMPERATURA
%********************************************************************************************************
       %Tdif
       figure
       v=[20 17.5 15 12.5 10 7.5 5 2.5 0 -2.5 -5 -10 -15 -20 -30 -40 -50 -60 -70];
       colormap(jet(19));
       [C,h]=contourf(Pparcela2(:,1)/100,-Pparcela2(:,1)/100,Trodif2,v);
       clabel(C)
       title('Diferencias entre la Tro de la parcela y la temperatura virtual del entorno (C)')
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[-1000 -900 -800 -700 -600 -500 -400 -300 -200 -100])
       set(gca,'YTickLabel',{'1000';'900';'800';'700';'600';'500';'400';'300';'200';'100'})
       set(gca,'Ygrid','on')
       set(gca,'XTickMode','manual')
       set(gca,'XTick',[100 200 300 400 500 600 700 800 900 1000])
       set(gca,'XTickLabel',{'100';'200';'300';'400';'500';'600';'700';'800';'900';'1000'})
       set(gca,'Xgrid','on')
       xlabel('CIMA 2006');
       colorbar;
%       print('-depsc', 'out1.eps')
%       !gs -q -dNOPAUSE -sDEVICE=jpeg -r100x100 -sOutputFile=out1.jpg out1.eps -c quit
%       !rm -f ./out1.eps
%       close 1;
       
       %Trodif
       figure
       v=[20 17.5 15 12.5 10 7.5 5 2.5 0 -2.5 -5 -10 -15 -20 -30 -40 -50 -60 -70];
       colormap(jet(19));
       [C,h]=contourf(Pparcela2(:,1)/100,-Pparcela2(:,1)/100,Tdif2,v);
       clabel(C)
       title('Diferencias entre la Temperatura de la parcela y la temperatura del entorno (C)')
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[-1000 -900 -800 -700 -600 -500 -400 -300 -200 -100])
       set(gca,'YTickLabel',{'1000';'900';'800';'700';'600';'500';'400';'300';'200';'100'})
       set(gca,'Ygrid','on')
       set(gca,'XTickMode','manual')
       set(gca,'XTick',[100 200 300 400 500 600 700 800 900 1000])
       set(gca,'XTickLabel',{'100';'200';'300';'400';'500';'600';'700';'800';'900';'1000'})
       set(gca,'Xgrid','on')
       xlabel('CIMA 2006');
       colorbar;
    %   print('-depsc', 'out2.eps')
    %   !gs -q -dNOPAUSE -sDEVICE=jpeg -r100x100 -sOutputFile=out2.jpg out2.eps -c quit
    %   !rm -f ./out2.eps
       
%       close 1;

%********************************************************************************************************
% GRAFICADO DE LOS CONTENIDOS DE AGUA LIQUIDA.
%********************************************************************************************************    
       %Wparcela
       figure
       v=[20 17.5 15 12.5 10 7.5 5 2.5 1 0.0001];
       colormap(jet(10))
       [C,h]=contourf(Pparcela2(:,1)/100,-Pparcela2(:,1)/100,wparcela2*1000,v);
       clabel(C)
       title('Condensado total (g/kg)')
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[-1000 -900 -800 -700 -600 -500 -400 -300 -200 -100])
       set(gca,'YTickLabel',{'1000';'900';'800';'700';'600';'500';'400';'300';'200';'100'})
       set(gca,'Ygrid','on')
       set(gca,'XTickMode','manual')
       set(gca,'XTick',[100 200 300 400 500 600 700 800 900 1000])
       set(gca,'XTickLabel',{'100';'200';'300';'400';'500';'600';'700';'800';'900';'1000'})
       set(gca,'Xgrid','on')
       xlabel('CIMA 2006');
       colorbar;
     %  print('-depsc', 'out3.eps')
    %   !gs -q -dNOPAUSE -sDEVICE=jpeg -r100x100 -sOutputFile=out3.jpg out3.eps -c quit
    %   !rm -f ./out3.eps

%       close 1;
          
       
%********************************************************************************************************
% GRAFICADO DE LAS VELOCIDADES VERTICALES
%********************************************************************************************************
       %vvel
       figure
       v=[100 90 80 70 60 50 40 30 20 10 5 1];
       colormap(jet(20));
       [C,h]=contourf(Pparcela2(:,1)/100,-Pparcela2(:,1)/100,vvel,v);
       clabel(C)
       title('Velocidades verticales para las parcelas ascendentes y descendentes')
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[-1000 -900 -800 -700 -600 -500 -400 -300 -200 -100])
       set(gca,'YTickLabel',{'1000';'900';'800';'700';'600';'500';'400';'300';'200';'100'})
       set(gca,'Ygrid','on')
       set(gca,'XTickMode','manual')
       set(gca,'XTick',[100 200 300 400 500 600 700 800 900 1000])
       set(gca,'XTickLabel',{'100';'200';'300';'400';'500';'600';'700';'800';'900';'1000'})
       set(gca,'Xgrid','on')
       hold on
       vv=[-1 -5 -10 -20 -30 -40 -50 -60 -70 -80 -90 -100];
       [C,h]=contour(Pparcela2(:,1)/100,-Pparcela2(:,1)/100,vvel,vv);
       clabel(C)
       xlabel('CIMA 2006');
       colorbar;
    %   print('-depsc', 'out4.eps')
    %   !gs -q -dNOPAUSE -sDEVICE=jpeg -r100x100 -sOutputFile=out4.jpg out4.eps -c quit
   %    !rm -f ./out4.eps
       
%       close 1;
       
%********************************************************************************************************
% GRAFICADO DE LOS CAPES Y DECAPES EN FUNCION DE LA PRESION DEL NIVEL DE ORIGEN DE LA PARCELA
%********************************************************************************************************  
       %CAPE
       figure
       plot(CAPE,-(Pparcela2(:,1)/100),'-b','LineWidth',2)%CAPE
       hold on
       plot(NA,-(Pparcela2(:,1)/100),'-g','LineWidth',2)%AREA NEGATIVA
       hold on
       plot(PA,-(Pparcela2(:,1)/100),'-r','LineWidth',2)%AREA POSITIVA
       title('CAPE(azul), AP(rojo) y AN(verde) en funcion de la presion')
       axis([-1000 6000 -1000 -500])
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[-1000 -900 -800 -700 -600 -500 ])
       set(gca,'YTickLabel',{'1000';'900';'800';'700';'600';'500'})
       set(gca,'Xgrid','on')
       set(gca,'Ygrid','on')
       xlabel('CIMA 2006');

%       print('-depsc', 'out5.eps')
%       !gs -q -dNOPAUSE -sDEVICE=jpeg  -r100x100 -sOutputFile=out5.jpg out5.eps -c quit
%       !rm -f ./out5.eps


%       close 1;
       
       %DCAPE
       figure
       plot(DCAPE,-(Pparcela2(:,1)/100),'-b','LineWidth',2)%DCAPE
       hold on
       plot(DNA,-(Pparcela2(:,1)/100),'-g','LineWidth',2)%AREA NEGATIVA
       hold on
       plot(DPA,-(Pparcela2(:,1)/100),'-r','LineWidth',2)%AREA POSITIVA
       title('DCAPE(azul), DPA(rojo) y DNA(verde) en funcion de la presion')
       axis([-6000 1000 -1000 -150])
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[-1000 -900 -800 -700 -600 -500 -400 -300 -200 -150])
       set(gca,'YTickLabel',{'1000';'900';'800';'700';'600';'500';'400';'300';'200';'150'})
       set(gca,'Xgrid','on')
       set(gca,'Ygrid','on')
       xlabel('CIMA 2006');

 %      print('-depsc', 'out6.eps')
 %      !gs  -dNOPAUSE -sDEVICE=jpeg -r100x100 -sOutputFile=out6.jpg out6.eps -q quit
  %     !rm -f ./out6.eps

%       close 1;

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
       semilogy(Tparcela2(:,1),-(Pparcela2(:,1)/100),'-.g','LineWidth',2)%metodo de la parcela superficie
       hold on
       semilogy(Tparcela2(:,MUlevel),-(Pparcela2(:,MUlevel)/100),'-.r','LineWidth',2)%metodo de la parcela para
       %la parcela mas inestable
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
       xlabel('CIMA 2006');
       
       clear P

%       print('-depsc', 'out7.eps')
%       !gs -q -dNOPAUSE -sDEVICE=jpeg -r100x100 -sOutputFile=out7.jpg out7.eps -c quit
%       !rm -f ./out7.eps

%       close 1;
       
%********************************************************************************************************
% FIN DEL GRAFICADO.
%********************************************************************************************************


%********************************************************************************************************
% FIN DEL SCRIPT :-)
%********************************************************************************************************















