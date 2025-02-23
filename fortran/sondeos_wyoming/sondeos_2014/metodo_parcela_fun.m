
%Esta funcion calcula el proceso pseudo adiabatico de descenso de una parcela.
%Primero calcula la Temperatura isobarica de bulbo humedo (a traves de la conservacion de la entalpia)
%esto es equivalente a suponer que primero consideramos las condiciones del entorno y que a partir de dichas
%condiciones tenemos agua disponible de forma tal que el aire del entorno alcanza la saturacion a presion 
%constante (Se llega a la Tiw). A partir de ese punto la parcela desciende segun un proceso que solo puede
%ser pseudoadiabatico, ya que necesitamos suponer que constantemente se le entrega agua al sistema durante
%el descenso, lo que permite que la parcela se mantenga saturada.
%Programa realizado por Juan Jose Ruiz (2005)

function [T_out,R_out,W_out,V_out,PA,NA,DPA,DNA,PNCA,TNCA,PEQ,PLC] =metodo_parcela_fun(t,p,rmezcla,dp)

nlevels=length(p);
T_out=NaN(nlevels,nlevels);
W_out=NaN(nlevels,nlevels);
R_out=NaN(nlevels,nlevels);
V_out=NaN(nlevels,nlevels);
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
%Estos valores estan expresados en J/(Kg*K)


L1=3.1447e6; %Constante utilizada en el calculo de Lv(T).
L2=2357.2;  %Constante utilizada en el calculo de Lv(T).

%Calculo de algunos parametros que son funcion de las constantes anteriores para simplificar
%las expresiones.

beta=(Cpv/Cpd-Rv/Rd);

Pmax=max(p);
Pmin=min(p);

for ii=1:length(p)
    
Tini=t(ii);
Pini=p(ii);
rini=rmezcla(ii);

%==========================================================================
%CALCULO DE LA TEMPERATURA ISOBARICA DE BULBO HUMEDO.
%==========================================================================

Lvini=L1-L2*Tini;
Hini=Tini+rini*Lvini/Cpd; %Esta es la cantidad que se conserva durante el proceso.

%Minimizacion para encontrar la Tiw 

tmpt=100:0.5:350;
tmpes=611.2*exp(17.67*(tmpt-273.16)./(tmpt+243.5-273.16)); %Este es el es con respecto al agua.
tmprv=(Rd/Rv)*tmpes./(Pini-tmpes);
Lv=L1-L2*tmpt;
H=tmpt+tmprv.*Lv/Cpd; %este es el valor que deberia ser igual a ini en el estado final

[Hmin iHmin]=min(abs(H-Hini));

Tiw=tmpt(iHmin);

%==========================================================================
%CALCULO EL PROCESO PSEUDO ADIABATICO HACIA ABAJO.
%==========================================================================
clear T P rs

  %Asumo que la parcela se satura y alcanza la Tiw y que se 
  %se encuentra saturada.
  
  % Calculos iniciales
  nP=round((Pmax+dp-Pini)/dp); %No de pasos de integracion a realizar.
  
  
    T(nP)=Tiw;
    P(nP)=Pini;
    Es=611.2*exp(17.67*(Tiw-273.16)/(Tiw+243.5-273.16));
    Pd=Pini-Es;
    rs(nP)=(Rd/Rv)*Es/(Pini-Es);  %relacion de mezcla de saturacion.            
            

if nP>1 %Hacemos la integracion solo si vale la pena, si estamos a 1 niveles por encima de superficie.
    %Primer paso con Euler

  for j=nP:-1:2
   
    alfa=1/(1+Rv*rs(j)/Rd);
    Kapa=(Rd/Cpd)*(1+Rv/Rd*rs(j))/(1+Cpv/Cpd*rs(j)); %Kapa ahora es una funcion variable, porque r comienza a variar.
    Lv=L1-L2*T(j);     %Ecuacion para la dependencia a orden 1 de Lv con T basandose en la ley de Kirchhoff        
    A=Cpd*Kapa*Tini/P(j)+Lv*alfa*rs(j)/Pd;
    B=(Cpd+ alfa*P(j)*(Lv^2)*rs(j)/(Pd*Rv*(T(j)^2)) +(Cpd*rs(j)*beta));   
    T(j-1)=T(j)+dp*A/B; %Kapa permanece constante.
    P(j-1)=P(j)+dp;
    Es=611.2*exp(17.67*(T(j-1)-273.16)/(T(j-1)+243.5-273.16));
    Pd=P(j-1)-Es;
    rs(j-1)=(Rd/Rv)*Es/(Pd);  %relacion de mezcla de saturacion.
    
  end

end


if(nP>1)%Si nP no es mas grande que 1 no genera salidas
%Interpolo los resultados a los niveles que tengo en el sondeo.
levels=(p >= Pini); 
T_out(levels,ii)=interp1(log(P),T,log(p(levels)));
R_out(levels,ii)=interp1(log(P),rs,log(p(levels))); 
 
end


%==========================================================================
%CALCULO EL PROCESO PSEUDO ADIABATICO HACIA ARRIBA
%==========================================================================
clear P T r w

nP=round((Pini-Pmin-dp)/(dp)); %Calculo del numero de pasos

T(1)=t(ii);
P(1)=p(ii);
r(1)=rmezcla(ii);


%Calculo de la tension de vapor de saturacion, la tension de vapor y la relacion de mezcla en el instante inicial.
% La presion de vapor de saturacion se calcula utiizando la expresion (10) del paper de David Bolton (1980)
% The Computation of Equivalente Potential Temperature. Monthly Weather Review. Volume 108. 1046-1053.

Es=611.2*exp(17.67*(T(1)-273.16)/(T(1)+243.5-273.16)); %Es en pascales
rs=(Rd/Rv)*Es/(P(1)-Es); %relacion de mezcla de saturacion(adimencional)
E=(Rv/Rd)*r(1)*P(1)/(1+r(1)*Rv/Rd);
r(1)=(Rd/Rv)*E(1)/(P(1)-E(1));   %relacion de mezcla (adimensional)
Pd=P(1)-Es; %Presion parcial del aire seco
Kapa=(Rd/Cpd)*(1+Rv/Rd*r(1))/(1+Cpv/Cpd*r(1)); %Kapa para el aire humedo.

%ASCENSO NO SATURADO HASTA EL NCA.

j=2;
while 1
    T(j)=T(j-1)-dp*T(j-1)*Kapa/(P(j-1));
    P(j)=P(j-1)-dp;
    Es=611.2*exp(17.67*(T(j)-273.16)/(T(j)+243.5-273.16));
    rs(j)=(Rd/Rv)*Es/(P(j)-Es);  %relacion de mezcla de saturacion.
    r(j)=r(1);
    w(j)=0;%El continido de agua liquida es cero en el ascenso no saturado
    if r(j) > rs(j)
        break
    end
    if j > nP
        break
    end

    j=j+1;
end

%Guarda el valor de las variables en el nivel de condensacion por ascenso.

TNCA(ii)=T(j-1)-273.16; %Valor en grados Celsius de la temperatura
PNCA(ii)=P(j-1)/100;   %Valor en hPa de la presion


% El proceso humedo va a integrar la ecuacion desde el paso nsat hasta el paso nP.
nsat=j-1;

%A partir de este punto comienza la condensacion. 
%Utilizamos una ecuacion termodinamica que tiene en cuenta las variaciones de Lv con T, pero que no considera la fase hielo.

%Primer paso con Euler
for j=nsat:nsat+1
    %dT/dp se divide en 4 terminos para mayor simplicidad alfa, beta, A y
    %B.
         Es=611.2*exp(17.67*(T(j)-273.16)/(T(j)+243.5-273.16));
         Pd=P(j)-Es;
         rs=(Rd/Rv)*Es/Pd;  %relacion de mezcla de saturacion.
         r(j)=rs; %Asumimos que la parcela se encuentra saturada a lo largo de este proceso.
         %Los valores de las variables de humedad de saturacion se suponen iguales a los da la parcela por encima del NCA.
         w(j)=r(1)-rs; %El contenido de agua liquida se obtiene por simple resta entre el vapor inicial y el actual.   
         alfa=1/(1+Rv*rs/Rd);
         Kapa=(Rd/Cpd)*(1+Rv/Rd*rs)/(1+Cpv/Cpd*rs); %Kapa ahora es una funcion variable, porque r comienza a variar.
         Lv=L1-L2*T(j);     %Ecuacion para la dependencia a orden 1 de Lv con T basandose en la ley de Kirchhoff
         %A y B son dos parametros cuyo cociente nos da la tasa de varaicion de T con P para un proceso reversible. Para obtener
         %las expreciones de A y B hay que descomponer todos los diferenciales de la ecuacion (8) del paper de Lipps y Hemler y 
         %reagrupar los terminos multiplicados por dT y los multiplicados por dP, lo que acompa�a a dT se lo llamo B y lo que 
         %acompa�a a dP se lo llamo A. 
         A=Cpd*Kapa*T(j)/P(j)+Lv*alfa*rs/Pd;
         B=(Cpd+ alfa*P(j)*(Lv^2)*rs/(Pd*Rv*(T(j)^2)) +(Cpd*rs*beta)); 
         %Hasta aca los calculos previos
         %A continuacion el paso de integracion
         T(j+1)=T(j)-dp*A/B;
         P(j+1)=P(j)-dp;
end


for j=nsat+1:nP
%El resto de los pasos con un metodo centrado (Leap Frog)

         Es=611.2*exp(17.67*(T(j)-273.16)/(T(j)+243.5-273.16));
         Pd=P(j)-Es;
         rs=(Rd/Rv)*Es/Pd;  %relacion de mezcla de saturacion.
         r(j)=rs; %Asumimos que la parcela se encuentra saturada a lo largo de este proceso.
 
         %Los valores de las variables de humedad de saturacion se suponen iguales a los da la parcela por encima del NCA.
         w(j)=r(1)-rs; %El contenido de agua liquida se obtiene por simple resta entre el vapor inicial y el actual. 
         alfa=1/(1+Rv*rs/Rd);
         Kapa=(Rd/Cpd)*(1+Rv/Rd*rs)/(1+Cpv/Cpd*rs); %Kapa ahora es una funcion variable, porque r comienza a variar.
         Lv=L1-L2*T(j);     

         %A y B son dos parametros cuyo cociente nos da la tasa de varaicion de T con P para un proceso reversible. Para obtener
         %las expreciones de A y B hay que descomponer todos los diferenciales de la ecuacion (8) del paper de Lipps y Hemler y 
         %reagrupar los terminos multiplicados por dT y los multiplicados por dP, lo que acompa�a a dT se lo llamo B y lo que 
         %acompa�a a dP se lo llamo A. 
         A=Cpd*Kapa*T(j)/P(j)+Lv*alfa*rs/Pd;
         B=(Cpd+ alfa*P(j)*(Lv^2)*rs/(Pd*Rv*(T(j)^2)) +(Cpd*rs*beta)); 
         %Hasta aca los calculos previos
         %A continuacion el paso de integracion
         T(j+1)=T(j-1)-2*dp*A/B;
         P(j+1)=P(j)-dp;


end


if(nP>1)
%Interpolo los resultados a los niveles que tengo en el sondeo.
levels=(p <= Pini);  

T_out(levels,ii)=interp1(log(P),T,log(p(levels)));
W_out(levels,ii)=interp1(log(P(1:end-1)),w,log(p(levels)));
R_out(levels,ii)=interp1(log(P(1:end-1)),r,log(p(levels))); 

end


%==========================================================================
%CALCULO EL CAPE / DCAPE / VELOCIDADES VERTICALES.
%==========================================================================
clear Tv_dif

%CALCULAMOS LAS DIFERENCIAS DE TV ENTRE ENTORNO Y PARCELA.
Tv_dif=T_out(:,ii).*(1+0.608*R_out(:,ii))-(t.*(1+0.608*rmezcla))'; %Dif de Tvirtual entre entorno y parcela.

%CALCULAMOS LOS NIVELES DE EQUILIBRIO.

nequib=1; %nequib va a ser la variable que guarde la informacion del maximo nivel de equilibrio. El calculo del area negativa se
%hace hasta este nivel.
    nlibc=1;  %Este sera el primer nivel donde la parcela adquiere empuje positivo. Hay que tener en cuenta que en sondeos 
    %con una capa de mezcla muy desarrollada, la presencia de gradientes superadiabaticos en los primeros metros puede producir
    %que el nivel de libre conveccion aparezca muy por debajo esto tal vez no sea realista en terminos de la formacion de nubes
    %de gran desarrollo vertical sino tal vez de nubes cumulus por encima de la capa mezclada.
    for i=ii+1:nlevels
        if(Tv_dif(i) <= 0) & (Tv_dif(i-1)>0)
            nequib=i;
        end
        if(Tv_dif(i)>0) & (Tv_dif(i-1)<=0) & (nlibc==1)
            nlibc=i;
        end
        if(Tv_dif(end) > 0) %Si llegando al final del sondeo todavia T parcela es mayor que la del entorno
            %entonces fuerzo al nivel de equilibrio a ser el ultimo nivel del sondeo.
            nequib=nlevels;
        end
    end
    PEQ(ii)=p(nequib); %Obtenemos la presion que corresponde al nivel de equilibrio maximo.
    PLC(ii)=p(nlibc);      %Obtenemos la presion que corresponde al nivel de equilibrio minimo.


%==========================================================================
%CALCULO EL CAPE Y VELOCIDAD DE ASCENDENTE
%CALCULO PARA LAS PARCELAS QUE ASCIENDEN. (PA, NA, CAPE Y VVEL DE ASCENSO)
%==========================================================================

    %Las unidades en las que se calcula el CAPE son J/Kg.
    NA(ii)=0; %inicialmente las areas positivas y negativas son 0 (area negativa)
    PA(ii)=0; %area positiva para el nivel j-esimo.
    
    
    if (nequib ~= 1) %Si nequib es 1 entonces no tiene sentido calcular NA o PA. Directamente quedan como 0.
     for i=ii:nlevels-1
      
        Tv_dif_media=(Tv_dif(i)+Tv_dif(i+1))/2; %Diferencia de temperatura media entre 2 niveles sucesivos.
        
        Pmedia=(p(i)+p(i+1))/2;       %Presion media que caracteriza a esa capa.
        Pinc=(p(i)-p(i+1));           %Espesor de la capa entre 2 niveles sucesivos en Pascales.
        
        if (Tv_dif_media > 0) & ((i+1) >=nlibc)   %Si la Dif de Tro media es positiva entonces esa capa la sumamos en el PA
        PA(ii)=PA(ii)+Pinc*Rd*Tv_dif_media/Pmedia;  
        end
        if (Tv_dif_media < 0) & ((i) <nequib)   %Si la Dif de Tro media es negativa y estamos por debajo del nivel de equilibrio
        NA(ii)=NA(ii)+Pinc*Rd*Tv_dif_media/Pmedia;     %Calculamos usando la definicion de AREA NEGATIVA
        end
        
        %A partir de los calculos realizados calculo el perfil de velocidad vertical
        if( (V_out(i,ii)^2+2*Pinc*Rd*Tv_dif_media/Pmedia) > 0) %Como obtengo la velocidad a partir de una variacion de energia
        %tengo problemas cuando la velocidad cambia de signo, porque si la velocidad es negativa y se hace mas negativa,
        %la energia cinetica de la parcela aumenta, aunque en realidad w disminuye. Por eso solo calculo velocidades por encima
        %de 0m/s (que por otra parte son las unicas que tienen sentido porque estamos suponiendo que la parcela asciende 
        %constantemente libre o forzada).
        %
        V_out(i,ii)=(V_out(i,ii)^2+2*Pinc*Rd*Tv_dif_media/Pmedia)^0.5;  %Calculo el incremento de velocidad usando la relacion entre
        %el incremento de cape y el incremento de velocidad ascencional
        else
        V_out(i,ii)=0; %Si como resultado de la integracion obtengo velocidades negativas entonces hago que sean 0 y 
        %arranco nuevamente si es que en algun momento aparece de vuelta area postivia.  
        end
     end
     
    end

%==========================================================================
%CALCULO DCAPE Y VELOCIDADES DESCENDENTES.
%CALCULO PARA LAS PARCELAS QUE DESCIENDEN (DNA, DPA, DCAPE Y VVEL DE DESCENSO)
%==========================================================================


    %Las unidades de w son en M/S

    %DPA es el trabajo que el entorno realiza sobre la parcela para
    %acelerar la descendente.
    %DNA es el trabajo que el entorno realiza sobre la parcela para
    %desacelerar la descendente.
       DNA(ii)=0; %DNA seran los niveles que desaceleran la descendente.
       DPA(ii)=0; %DPA seran los niveles que aceleran la descendente.
       
       
     for i=ii:-1:2

         Tv_dif_media=(Tv_dif(i)+Tv_dif(i-1))/2; %Diferencia de temperatura media entre 2 niveles sucesivos.

         Pmedia=(p(i)+p(i-1))/2;       %Presion media que caracteriza a esa capa.
         Pinc=(-p(i)+p(i-1));          %Espesor de la capa entre 2 niveles sucesivos en Pascales.
         if (Tv_dif_media < 0)   
           DPA(ii)=DPA(ii)+Pinc*Rd*Tv_dif_media/Pmedia;  
         end
         if (Tv_dif_media > 0)   
           DNA(ii)=DNA(ii)+Pinc*Rd*Tv_dif_media/Pmedia;    
         end
         
         %A partir de los calculos realizados calculo el perfil de velocidad vertical. En este caso me interesa calcular 
         %velocidades verticales de descenso unicamente.
         if((-V_out(i,ii)^2+2*Pinc*Rd*Tv_dif_media/Pmedia)<0) %Como obtengo la velocidad a partir de una variacion de energia
         %tengo problemas cuando la velocidad cambia de signo, porque si la velocidad es negativa y se hace mas negativa,
         %la energia cinetica de la parcela aumenta, aunque en realidad w disminuye. Por eso solo calculo velocidades por encima
         %de 0m/s (que por otra parte son las unicas que tienen sentido porque estamos suponiendo que la parcela asciende 
         %constantemente libre o forzada).
         %
         V_out(i-1,ii)=-(abs(-V_out(i,ii)^2+2*Pinc*Rd*Tv_dif_media/Pmedia))^0.5;  %Calculo el incremento de velocidad usando la relacion entre
         %el incremento de cape y el incremento de velocidad ascencional
         else
         V_out(i-1,ii)=0; %Si como resultado de la integracion obtengo velocidades negativas entonces hago que sean 0 y 
         %arranco nuevamente si es que en algun momento aparece de vuelta area postivia.  
         end


    end   
    
    




end %End over levels.




