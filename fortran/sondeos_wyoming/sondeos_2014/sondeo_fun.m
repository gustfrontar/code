% Metodo de la parcela, utilizando un proceso reversible.
% Utilizando la ecuacion (8) del Paper de Lipps y Hemler (1979)
% Another Look at the Thermodynamic Equation For Deep Convection. Monthly Weather Review Volume 108 78-84
% Programa realizado por Juan Jose Ruiz (2005)
% Adaptado para la funci�n operativa (2006)
% Mejorado (2013)


%Mejoras a realizar en el futuro:
% -Control de calidad de los datos (deteccion de gradientes superadiab�ticos en la atm�sfera libre)
% -Modificaci�n de la capa l�mite (Calculo de la mezcla, t critica y calculo del perfil vertical de CAPE luego de la mezcla)
% -Utilizaci�n del perfil de viento para estimar advecciones en la vertical en capas. Estudiar
%  como se modifica la temperatura en las proximas horas de acuerdo con este efecto y recalcular el perfil de CAPE.
% -Estimar potencial de organizaci�n a partir del perfil vertical de viento (ej. Nro de Richardson)

function []=sondeo_fun(p,t,td,vel,dir,id,fecha,hora)




%p  Perfil de presion en hPa.
%t  Perfil de temperatura en C.
%td Perfil de Td en C.
%dir Direccion del viento en grados.
%vel Velocidad del viento en nudos.

%==========================================================================
%Parametros iniciales que controlan el funcionamiento del programa
%Este modulo define los parametros iniciales, el valor de las constantes termodinamicas
%==========================================================================
%Definicion de constantes necesarias para el calculo:

Cpd=1005.7; %Calor especifico del aire seco a presion constante
Cpv=1860.6; %Calor especifico del vapor de agua a presion constante
Cw=4217.8;  %Calor especifico del agua liquida

Rd=287.04; %Constante del gas para el aire seco.
Rv=461.50; %Constante del gas para el vapor de agua.

g=9.81;    %Aceleracion de la gravedad

zcero=0; %Las alturas son referidas a la altura de la superficie.

Epsilon=Rd/Rv; %Epsilon
%Estos valores estan expresados en J/(Kg*K)

P0=1000; %Presion de referencia en hPa.
L1=3.1447e6; %Constante utilizada en el calculo de Lv(T).
L2=2357.2;  %Constante utilizada en el calculo de Lv(T).

%Calculo de algunos parametros que son funcion de las constantes anteriores para simplificar
%las expresiones.

beta=(Cpv/Cpd-Rv/Rd);
Kapa2=Rd/Cpd;


%==========================================================================  
%Interpolamos las variables que faltan para completar el perfil.
%==========================================================================

logp=log(p);

missing=isnan(t);
t(missing)=interp1(logp(~missing),t(~missing),logp(missing));


missing=isnan(td);
td(missing)=interp1(logp(~missing),td(~missing),logp(missing));

missing=isnan(dir);
dir(missing)=interp1(logp(~missing),dir(~missing),logp(missing));

missing=isnan(vel);
vel(missing)=interp1(logp(~missing),vel(~missing),logp(missing));


%==========================================================================  
%Obtengo la humedad especifica y relativa.
%========================================================================== 

%CAMBIO UNIDADES
p=p*100;                                     %Paso la presion a Pascales.
t=t+273.16;                                  %Temperatura en K. 
td=td+273.16;                                %Temperatura de rocio en K.
dir=dir*pi/180;                              %Direccion en radianes     
vel=vel*1.85/3.6;                            %Velocidad en M/S

%CALCULO OTRAS VARIABLES DE HUMEDAD Y TEMPERATURA
pvapor=611.2.*exp(17.67.*(td-273.16)./(td+243.5-273.16));  %e en pascales, t y td en centigrados.
pvaporsat=611.2.*exp(17.67.*(t-273.16)./(t+243.5-273.16)); %es en pascales, t y td en centigrados.
rmezcla=(Rd/Rv)*pvapor./(p-pvaporsat);       %relacion de mezcla (adimensional).
q=rmezcla./(1+rmezcla);                      %Humedad especifica (adimensional).
tv=(t).*(1+0.608*q);                         %Temperatura virtual, q (adimensional) y t en K.
hr=pvapor./pvaporsat;                        %Humedad relativa (adimensional)
theta=t.*(p/100000).^(-Rd/Cpd);              %Temperatura potencial.

%CALCULO COMPONENTES DEL VIENTO
u=-vel.*sin(dir);                            %Componente U
v=-vel.*cos(dir);                            %Componente V

nlevels=length(p);


%==========================================================================  
%Obtengo la altura correspondiente a cada nivel.
%========================================================================== 
z(1)=zcero;

for ii=2:length(p)

deltaz=-log(p(ii)/p(ii-1))*Rd*(tv(ii)+tv(ii-1))/(2*g);

z(ii)=z(ii-1) + deltaz;

end


%==========================================================================  
%Suavizo el perfil de viento
%==========================================================================
tmpzres=10;
smoothfactor=100;

%Interpolate to a quasi regular logp grid.
windlevels= ~ (isnan(u) | isnan(v) );
zwind=floor(min(tmpzres*z(windlevels)))/tmpzres:tmpzres:max(z(windlevels))+tmpzres;
tmpu=interp1(z(windlevels),u(windlevels),zwind,'spline',NaN);
tmpv=interp1(z(windlevels),v(windlevels),zwind,'spline',NaN);
%Smooth over the quasi regular grid.
us=smooth(zwind,tmpu,smoothfactor,'rloess');
vs=smooth(zwind,tmpv,smoothfactor,'rloess');

thetawind=interp1(z(windlevels),theta(windlevels),zwind,'spline',NaN);
thetawinds=smooth(zwind,thetawind,smoothfactor,'rloess');
%us=NaN(size(u));
%vs=NaN(size(v));
%Interpolate back to the irregular original grid.
%us(windlevels)=interp1(tmpz,tmpu,z(windlevels),'spline',NaN)
%vs(windlevels)=interp1(tmpz,tmpv,z(windlevels),'spline',NaN)


%==========================================================================
%Vamos a usar proceso Pseudo adiabatico:
%==========================================================================

dp=100; %Valor del paso de presion en Pa.

%Obtengo la temperatura de las parcelas ascendentes y descentes para cada
%nivel del perfil.

[Tparcela,Rparcela,Wparcela,Vparcela,PA,NA,DPA,DNA,PNCA,TNCA,PEQ,PLC] =metodo_parcela_fun(t,p,rmezcla,dp);


%Calculo de cortante theta y numero de Richardson.
% =========================================================================
% Cortante vertical del viento en m/s/km
% =========================================================================

for i=1:length(zwind)-1
    auxu            = (us(i+1)-us(i))./(zwind(i+1)-zwind(i));
    auxv            = (vs(i+1)-vs(i))./(zwind(i+1)-zwind(i));
    cortante(i)        = 1000*sqrt(auxu.^2 + auxv.^2);
    altura_cortante(i) = (zwind(i)+zwind(i+1))/2;
end
cortante(i+1)=NaN;

% =========================================================================
% Richardson Number
% =========================================================================

for i=1:length(zwind)-1
    aux0 = 0.5*(thetawinds(i+1)+thetawinds(i));
    aux1 = 1./aux0;                                                                   % 1/titamedio
    aux2 = (thetawinds(i+1)-thetawinds(i))./(zwind(i+1)-zwind(i));                    % dtita/dz
    aux3 = ((cortante(i)/1000).^2);                                                   % (du/dz)^2+(dv/dz)^2
    Ri(i)   = (9.81*aux1.*aux2)./aux3;
end
Ri(i+1)=NaN;

% =========================================================================
% Richardson Number
% =========================================================================
%Busco el NCA correspondiente al nivel + alto con CAPE positivo o el nivel
%de 700 hPa.

%Primera posibilidad, no hay CAPE positivo... calculo de superficie a 700.
%La idea es que aunque el sondeo no tenga CAPE en alguna situacion nos
%puede intersar saber que pasaria con las rafagas si se formara una
%tormenta en dicho entorno.

i=find(PA > 0 & p > 70000 );
if(length(i)==0 )
    max_lev=70000; %Fijo el maximo nivel para descendentes en 700 hPa.
else
    max_lev=min(p(i));
end

for i=1:nlevels
    for j=1:nlevels
     if(i < j)
         if(p(j) < max_lev)
             Vparcela(i,j)=NaN;
             Tparcela(i,j)=NaN;
         end
     end
    end
end

% =========================================================================
% GRAFICADO 
% =========================================================================


fecha_str=num2str(fecha);
if(length(fecha_str) < 8);
    fecha_str=['0' fecha_str];
end


% =========================================================================
% GRAFICADO DE LAS DIFERENCIAS DE TV
% =========================================================================



       figure
       v=[ -5 -2.5 -1 -0.5 0.5 1 1.5 2 3 4 5 6 7 8];
       vcol=[43 42 41 2 21 22 23 24 25 26 27 28 29];
       contourf(p/100,-p/100,Tparcela.*(1+0.608*Rparcela)-repmat(tv',[1 nlevels]),v);
       hold on
       plot(p/100,-p/100,'k--','Linewidth',4);
       colorbar
       title(['Tparcela-Tentorno. Fecha: ' num2str(fecha_str(1:2)) '/' num2str(fecha_str(3:4)) '/' num2str(fecha_str(5:8)) '. Est: ' num2str(id)  ],'FontSize',15)
       plot_jrcol(v,vcol,0);
  
       xmax=p(1)/100;
       if(xmax < 500);xmax=1000;end;
       xmin=500;
       ymax=-p(nlevels)/100;
       ymin=-p(1)/100;
       axis([xmin xmax ymin ymax])
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[-1000 -900 -800 -700 -600 -500 -400 -300 -200 -100])
       set(gca,'YTickLabel',{'1000';'900';'800';'700';'600';'500';'400';'300';'200';'100'})
       set(gca,'Ygrid','on')
       set(gca,'XTickMode','manual')
       set(gca,'XTick',[100 200 300 400 500 600 700 800 900 1000])
       set(gca,'XTickLabel',{'100';'200';'300';'400';'500';'600';'700';'800';'900';'1000'})
       set(gca,'Xgrid','on')
       xlabel('Nivel de origen (hPa)','FontSize',15);
       ylabel('Nivel de llegada (hPa)','FontSize',15);
       set(gca,'FontSize',15)
       colorbar;
       print('-dpng','-r0', ['FIGURE_1_' num2str(id) '_' num2str(hora) '.png'])



% 
% =========================================================================
% GRAFICADO DE LAS DIFERENCIAS DE CONTENIDO DE AGUA LIQUIDA
% =========================================================================

   
       figure
       v=[0.0 1 2.5 5 7.5 10 12.5 15 17.5 20];
       vcol=[31 32 33 34 35 36 37 38 39];
       contourf(p/100,-p/100,Wparcela*1000,v);
       colorbar
       hold on
       plot(p/100,-p/100,'k--','Linewidth',4);
       %if(~datanotfound)
       %clabel(C)
       %end
       title(['Condensado total (g/kg). Fecha: ' num2str(fecha_str(1:2)) '/' num2str(fecha_str(3:4)) '/' num2str(fecha_str(5:8)) '. Est: ' num2str(id)  ],'FontSize',15)
       plot_jrcol(v,vcol,0);
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[-1000 -900 -800 -700 -600 -500 -400 -300 -200 -100])
       set(gca,'YTickLabel',{'1000';'900';'800';'700';'600';'500';'400';'300';'200';'100'})
       set(gca,'Ygrid','on')
       set(gca,'XTickMode','manual')
       set(gca,'XTick',[100 200 300 400 500 600 700 800 900 1000])
       set(gca,'XTickLabel',{'100';'200';'300';'400';'500';'600';'700';'800';'900';'1000'})
       set(gca,'Xgrid','on')
       xmax=p(1)/100;
       if(xmax < 500);xmax=1000;end;
       xmin=500;
       ymax=-p(nlevels)/100;
       ymin=-p(1)/100;
       axis([xmin xmax ymin ymax])
       xlabel('Nivel de origen (hPa)','FontSize',15);
       ylabel('Nivel de llegada (hPa)','FontSize',15);
       set(gca,'FontSize',15)
       print('-dpng','-r0', ['FIGURE_2_' num2str(id) '_' num2str(hora) '.png'])
       xmax=p(1)/100;
       xmin=500;

% =========================================================================
% GRAFICADO DE LAS DIFERENCIAS DE VELOCIDAD VERTICAL
% =========================================================================

       figure
       v=[1 5 10 20 30 40 50 60 70 80 90 100];
       contourf(p/100,-p/100,Vparcela,v);
       plot_jrcol(v,[21 22 23 24 25 26 27 28 29 54 55],0);
       hold on
       plot(p/100,-p/100,'k--','Linewidth',4);
       colorbar
       hold on
       vv=[-1 -5 -10 -20 -30 -40 -50 -60 -70 -80 -90 -100];
       contour(p/100,-p/100,Vparcela,vv,'--b');
       %clabel(F)
       title(['W parcelas. Fecha: ' num2str(fecha_str(1:2)) '/' num2str(fecha_str(3:4)) '/' num2str(fecha_str(5:8)) '. Est: ' num2str(id)  ],'FontSize',15)
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[-1000 -900 -800 -700 -600 -500 -400 -300 -200 -100])
       set(gca,'YTickLabel',{'1000';'900';'800';'700';'600';'500';'400';'300';'200';'100'})
       set(gca,'Ygrid','on')
       set(gca,'XTickMode','manual')
       set(gca,'XTick',[100 200 300 400 500 600 700 800 900 1000])
       set(gca,'XTickLabel',{'100';'200';'300';'400';'500';'600';'700';'800';'900';'1000'})
       set(gca,'Xgrid','on')
       xmax=p(1)/100;
       if(xmax < 500);xmax=1000;end;
       xmin=500;
       ymax=-p(nlevels)/100;
       ymin=-p(1)/100;
       axis([xmin xmax ymin ymax])
       xlabel('Nivel de origen (hPa)','FontSize',15);
       ylabel('Nivel de llegada (hPa)','FontSize',15);
       set(gca,'FontSize',15)
       print('-dpng','-r0', ['FIGURE_3_' num2str(id) '_' num2str(hora) '.png'])

% =========================================================================
% GRAFICADO DE CAPE
% =========================================================================
       
       figure
       hold on
       plot(NA,-(p/100),'-g','LineWidth',2)%AREA NEGATIVA
       hold on
       plot(PA,-(p/100),'-r','LineWidth',2)%AREA POSITIVA
       title(['Area Positiva y Negativa (J/kg). Fecha: ' num2str(fecha_str(1:2)) '/' num2str(fecha_str(3:4)) '/' num2str(fecha_str(5:8)) '. Est: ' num2str(id)  ],'FontSize',15)
       legend('CINE','CAPE')
       axis([-1000 6000 -1000 -500])
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[-1000 -900 -800 -700 -600 -500 ])
       set(gca,'YTickLabel',{'1000';'900';'800';'700';'600';'500'})
       set(gca,'Xgrid','on')
       set(gca,'Ygrid','on')
       xlabel('Energia (J/Kg)','FontSize',15);
       ylabel('Nivel de origen (hPa)','FontSize',15);

       set(gca,'FontSize',15)
       print('-dpng','-r0', ['FIGURE_4_' num2str(id) '_' num2str(hora) '.png'])

 
% =========================================================================
% GRAFICADO DE DCAPE
% =========================================================================      
       
       figure
       hold on
       plot(-DNA,-(p/100),'-g','LineWidth',2)%AREA NEGATIVA
       hold on
       plot(-DPA,-(p/100),'-r','LineWidth',2)%AREA POSITIVA
       title(['Darea positiva y negativa (J/kg). Fecha: ' num2str(fecha_str(1:2)) '/' num2str(fecha_str(3:4)) '/' num2str(fecha_str(5:8)) '. Est: ' num2str(id)  ],'FontSize',15)
       axis([-1000 2000 -1000 -700])
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[-1000 -950 -900 -850 -800 -750 -700])
       set(gca,'YTickLabel',{'1000';'950';'900';'850';'800';'750';'700'})
       set(gca,'Xgrid','on')
       set(gca,'Ygrid','on')
       xlabel('Energia (J/Kg)','FontSize',15);
       ylabel('Nivel de origen (hPa)','FontSize',15);
       legend('Area Negativa','Area positiva')
       set(gca,'FontSize',15)
       print('-dpng','-r0', ['FIGURE_5_' num2str(id) '_' num2str(hora) '.png'])

% =========================================================================
% PLOTEO EL EMAGRAMA
% =========================================================================      
       figure
%Genero las curvas del emagrama usando la funcion emagrama.m
%Los parametros de entrada son ThetaMin (linea minima de valores de Theta que traza)
%ThetaMax maximo valor de Theta que dibuja
%DeltaTheta intervalo de las titas.
%Los valores recomendados son ThetaMin=190, ThetaMax=420 y DeltaTheta=10 (Todos en K).
%Deltar controla la cantidad de lineas de relacion de mezcla que se trazan.
     Pbot=max(p(:));
     emagrama(190,420,20,2,Pbot);
     
     [MUCAPE MUlevel]=max(PA);

       title(['Emagrama. Fecha: ' num2str(fecha_str(1:2)) '/' num2str(fecha_str(3:4)) '/' num2str(fecha_str(5:8)) '. Est: ' num2str(id)  ],'FontSize',15)
       semilogy(Tparcela(:,1)-273,-(p/100),'-.g','LineWidth',2)%metodo de la parcela superficie
       hold on
       semilogy(Tparcela(:,MUlevel)-273,-(p/100),'-.r','LineWidth',2)%metodo de la parcela para
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
       semilogy(t-273,-(p/100),'-b','LineWidth',2)%Temperatura del entorno  
       hold on  
       semilogy(td-273,-(p/100),'-m','LineWidth',2)%Temperatura de rocio del entorno 
       xlabel('Temperatura (C)','FontSize',15);
       ylabel('Pres. (hPa)','FontSize',15);
       %legend('Parcela de superficie','Parcela mas inestable','Temperatura','Temperatura de rocio')
       
       set(gca,'FontSize',15)
       print('-dpng','-r0', ['FIGURE_6_' num2str(id) '_' num2str(hora) '.png'])

      
% =========================================================================
% GRAFICO LA HODOGRAFA
% =========================================================================  

       figure
       hold on
       
       %Vamos a plotear con diferentes colores de acuerdo al nivel.
       i_pres=(zwind <= 2000 );   %Busco los niveles por debajo de 850.
       plot(us(i_pres),vs(i_pres),'r-','LineWidth',2);

       i_pres=(zwind <= 5000 & zwind >= 2000 );
       plot(us(i_pres),vs(i_pres),'g-','LineWidth',2);

       i_pres=(zwind <= 10000 & zwind >= 5000 );
       plot(us(i_pres),vs(i_pres),'b-','LineWidth',2);
       
       i_pres=(zwind <= 15000 & zwind >= 10000 );
       plot(us(i_pres),vs(i_pres),'k-','LineWidth',2);
       
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
       
       
       text(us(1)+0.5,vs(1),strcat(num2str(p(1)/100),' hPa.'),'HorizontalAlignment','left')
       plot(us(1),vs(1),'o')
       i_pres=( zwind==2000);
           plot(us(i_pres),vs(i_pres),'o')
           text(us(i_pres)+0.5,vs(i_pres),'2.000 m.','HorizontalAlignment','left')
       i_pres=( zwind==5000);
           plot(us(i_pres),vs(i_pres),'o')
           text(us(i_pres)+0.5,vs(i_pres),'5.000 m.','HorizontalAlignment','left')
       i_pres=( zwind==10000);
           plot(us(i_pres),vs(i_pres),'o')
           text(us(i_pres)+0.5,vs(i_pres),'10.000 m.','HorizontalAlignment','left')
       i_pres=( zwind==15000);
           plot(us(i_pres),vs(i_pres),'o')
           text(us(i_pres)+0.5,vs(i_pres),'15.000 m.','HorizontalAlignment','left')
     
       
       i_pres=(p >= 25000);
       aux=ceil(1.2*max(vel(i_pres)));
        if( ~isnan(aux) )
         axis([-aux aux -aux aux]);
        end

       %Graficamos las lineas de 0
       plot(-100:100:100,[0 0 0],'k--','LineWidth',1)
       plot([0 0 0],-100:100:100,'k--','LineWidth',1)

       axis('square')
       
      legend('0-2000 m','2000-5000 m','5000-10000 m','10000-15000 m','Location','Best')
      title(['Hodografa. Fecha: ' num2str(fecha_str(1:2)) '/' num2str(fecha_str(3:4)) '/' num2str(fecha_str(5:8)) '. Est: ' num2str(id)  ],'FontSize',15)
              xlabel('Componente U m/s','FontSize',15)
              ylabel('Componente V m/s','FontSize',15)
              set(gca,'FontSize',15)
       print('-dpng','-r0', ['FIGURE_7_' num2str(id) '_' num2str(hora) '.png'])


% =========================================================================
% GRAFICO LA HODOGRAMA HASTA 5000 m
% =========================================================================  
  
       figure
       hold on
       
       %Vamos a plotear con diferentes colores de acuerdo al nivel.
       i_pres=(zwind <= 2000 );   %Busco los niveles por debajo de 850.
       plot(us(i_pres),vs(i_pres),'r-','LineWidth',2);

       i_pres=(zwind <= 5000 & zwind >= 2000 );
       plot(us(i_pres),vs(i_pres),'g-','LineWidth',2);
       
       
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
       
       
       text(us(1)+0.5,vs(1),strcat(num2str(p(1)/100),' hPa.'),'HorizontalAlignment','left')
       plot(us(1),vs(1),'o')
       i_pres=( zwind==2000);
           plot(us(i_pres),vs(i_pres),'o')
           text(us(i_pres)+0.5,vs(i_pres),'2.000 m.','HorizontalAlignment','left')
       i_pres=( zwind==5000);
           plot(us(i_pres),vs(i_pres),'o')
           text(us(i_pres)+0.5,vs(i_pres),'5.000 m.','HorizontalAlignment','left')
       i_pres=( zwind==10000);
           plot(us(i_pres),vs(i_pres),'o')
     
       
         axis([-25 25 -25 25]);


       %Graficamos las lineas de 0
       plot(-100:100:100,[0 0 0],'k--','LineWidth',1)
       plot([0 0 0],-100:100:100,'k--','LineWidth',1)

       axis('square')
       
      legend('0-2000 m','2000-5000 m','Location','Best')
      title(['Hodografa. Fecha: ' num2str(fecha_str(1:2)) '/' num2str(fecha_str(3:4)) '/' num2str(fecha_str(5:8)) '. Est: ' num2str(id)  ],'FontSize',15)
              xlabel('Componente U m/s','FontSize',15)
              ylabel('Componente V m/s','FontSize',15)
              set(gca,'FontSize',15)
       print('-dpng','-r0', ['FIGURE_8_' num2str(id) '_' num2str(hora) '.png'])      


% =========================================================================
% GRAFICO LA CORTANTE VERTICAL (MODULO DEL VECTOR CORTANTE)
% ========================================================================= 

       figure
       hold on
       plot(log(cortante),zwind/1000,'-r','LineWidth',3)
       title(['Cortante (m/s km). Fecha: ' num2str(fecha_str(1:2)) '/' num2str(fecha_str(3:4)) '/' num2str(fecha_str(5:8)) '. Est: ' num2str(id)  ],'FontSize',15)
       axis([log(0.1) log(150) 0 15])
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[0:2.5:15])
       set(gca,'XTick',log([2.5 5 10 20 50 100 150]),'XTickLabel',{2.5;5;10;20;50;100;150})
       set(gca,'Xgrid','on')
       set(gca,'Ygrid','on')
       xlabel('Cortante (m/s km)','FontSize',15);
       ylabel('Altura (km)','FontSize',15);
       set(gca,'FontSize',15)
       print('-dpng','-r0', ['FIGURE_9_' num2str(id) '_' num2str(hora) '.png']) 
       
% =========================================================================
% GRAFICO NUMERO DE RICHARDSON
% =========================================================================      
       
       figure
       hold on
       Ri(Ri <=0)=NaN;
       plot(log(Ri),(zwind/1000),'-b','LineWidth',2)
       title(['Numero de Richardson. Fecha: ' num2str(fecha_str(1:2)) '/' num2str(fecha_str(3:4)) '/' num2str(fecha_str(5:8)) '. Est: ' num2str(id)  ],'FontSize',15)
       axis([log(0.1) log(150) 0 15])
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[0:2.5:15])
       set(gca,'XTick',log([0.001 0.25 1 2 10 20]),'XTickLabel',{0.01;0.25;0.5;1;2;10;20})
       set(gca,'Xgrid','on')
       set(gca,'Ygrid','on')
       xlabel('Numero de Richardson','FontSize',15);
       ylabel('Altura (km)','FontSize',15);
       set(gca,'FontSize',15)
       print('-dpng','-r0', ['FIGURE_10_' num2str(id) '_' num2str(hora) '.png']) 


% =========================================================================
% GRAFICO INTENSIDAD DE VIENTO
% =========================================================================      

       figure
       hold on
       plot(log(sqrt(us.^2+vs.^2)),(zwind/1000),'-r','LineWidth',3)
       title(['Intensidad de viento (m/s). Fecha: ' num2str(fecha_str(1:2)) '/' num2str(fecha_str(3:4)) '/' num2str(fecha_str(5:8)) '. Est: ' num2str(id)  ],'FontSize',15)
       axis([log(0.1) log(100) 0 15])
       set(gca,'YTickMode','manual')
       set(gca,'YTick',[0:2.5:15])
       set(gca,'XTick',log([1 2.5 5 10 25 50 100]),'XTickLabel',{1;2.5;5;10;25;50;100})
       set(gca,'Xgrid','on')
       set(gca,'Ygrid','on')
       xlabel('Magnitud del viento (m/s)','FontSize',15);
       ylabel('Altura (km)','FontSize',15);
       print('-dpng','-r0', ['FIGURE_11_' num2str(id) '_' num2str(hora) '.png']) 
       set(gca,'FontSize',15)

       
% =========================================================================
% FIN DE LA FUNCINO :)
% =========================================================================      














