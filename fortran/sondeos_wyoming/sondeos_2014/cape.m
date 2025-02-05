%Funcion que calcula capes en a partir de los datos de diferencia de temperatura entre la parcela y el entorno
%calculados para un dado sondeo.
%
%Programa realizado por Juan Jose Ruiz (2005)

function out=cape(Trodif2,Pentorno,param)
%param=1;
%***********************************************************************************************************************
%PRIMERO CALCULAMOS EL NIVEL DE LIBRE CONVECCION MAS BAJO Y EL NIVEL DE EQUILIBRIO MAS ALTO.
%***********************************************************************************************************************
Rd=287.04; %Constante del gas para el aire seco.

parametro=1;
if param==0
    parametro=0;
end

nlevels=length(Pentorno);


for j=1:nlevels
    nequib(j)=1; %nequib va a ser la variable que guarde la informacion del maximo nivel de equilibrio. El calculo del area negativa se
%hace hasta este nivel.
    nlibc(j)=1;  %Este sera el primer nivel donde la parcela adquiere empuje positivo. Hay que tener en cuenta que en sondeos 
    %con una capa de mezcla muy desarrollada, la presencia de gradientes superadiabaticos en los primeros metros puede producir
    %que el nivel de libre conveccion aparezca muy por debajo esto tal vez no sea realista en terminos de la formacion de nubes
    %de gran desarrollo vertical sino tal vez de nubes cumulus por encima de la capa mezclada.
    for i=j+1:nlevels
        if(Trodif2(i,j) <= 0) & (Trodif2(i-1,j)>0)
            nequib(j)=i;
        end
        if(Trodif2(i,j)>0) & (Trodif2(i-1,j)<=0) & (nlibc(j)==1)
            nlibc(j)=i;
        end
        if(Trodif2(nlevels,j)>0) %Si llegando al final del sondeo todavia T parcela es mayor que la del entorno
            %entonces fuerzo al nivel de equilibrio a ser el ultimo nivel del sondeo.
            nequib(j)=nlevels;
        end
    end
    Pnequib(j)=Pentorno(nequib(j)); %Obtenemos la presion que corresponde al nivel de equilibrio maximo.
    Pnlibc(j)=Pentorno(nlibc(j));   %Obtenemos la presion que corresponde al nivel de equilibrio minimo.
end

%***********************************************************************************************************************
%FIN DEL CALCULO DEL NIVEL DE LIBRE CONVECCION Y DEL NIVEL DE EQUILIBRIO PARA CADA NIVEL.
%***********************************************************************************************************************

%***********************************************************************************************************************
%ACA SE INICIA EL CALCULO DEL CAPE PROPIAMENTE DICHO Y DE LAS VELOCIDADES DE ASCENSO.
%***********************************************************************************************************************
 vvel=zeros(nlevels,nlevels); %Esta variable va a guardar los datos de velocidad vertical maxima pronosticados por el
 %metodo de la parcela.

%***********************************************************************************************************************
%CALCULO PARA LAS PARCELAS QUE ASCIENDEN. (PA, NA, CAPE Y VVEL DE ASCENSO)
for j=1:nlevels
    %Las unidades en las que se calcula el CAPE son J/Kg.
    NA(j)=0; %inicialmente las areas positivas y negativas son 0 (area negativa)
    PA(j)=0; %area positiva para el nivel j-esimo.
    CAPE(j)=0;
    if (nequib(j) ~= 1) %Si nequib es 1 entonces no tiene sentido calcular NA o PA. Directamente quedan como 0.
     for i=j:nlevels-1
         if parametro == 1
        Trodifmedia=(Trodif2(i,j)+Trodif2(i+1,j))/2; %Diferencia de temperatura media entre 2 niveles sucesivos.
         end
         if parametro == 0
        Trodifmedia=(Tdif2(i,j)+Tdif2(i+1,j))/2; %Diferencia de temperatura media entre 2 niveles sucesivos. 
         end
         Pmedia=(Pentorno(i)+Pentorno(i+1))/2;       %Presion media que caracteriza a esa capa.
        Pinc=(Pentorno(i)-Pentorno(i+1));           %Espesor de la capa entre 2 niveles sucesivos en Pascales.
        if (Trodifmedia > 0) & ((i+1) >=nlibc(j))   %Si la Dif de Tro media es positiva entonces esa capa la sumamos en el PA
        PA(j)=PA(j)+Pinc*Rd*Trodifmedia/Pmedia;  
        end
        if (Trodifmedia < 0) & ((i) <nequib(j))   %Si la Dif de Tro media es negativa y estamos por debajo del nivel de equilibrio
        NA(j)=NA(j)+Pinc*Rd*Trodifmedia/Pmedia;     %Calculamos usando la definicion de AREA NEGATIVA
        end
        %A partir de los calculos realizados calculo el perfil de velocidad vertical
        if((vvel(i,j)^2+2*Pinc*Rd*Trodifmedia/Pmedia)>0) %Como obtengo la velocidad a partir de una variacion de energia
        %tengo problemas cuando la velocidad cambia de signo, porque si la velocidad es negativa y se hace mas negativa,
        %la energia cinetica de la parcela aumenta, aunque en realidad w disminuye. Por eso solo calculo velocidades por encima
        %de 0m/s (que por otra parte son las unicas que tienen sentido porque estamos suponiendo que la parcela asciende 
        %constantemente libre o forzada).
        %
        vvel(i+1,j)=(vvel(i,j)^2+2*Pinc*Rd*Trodifmedia/Pmedia)^0.5;  %Calculo el incremento de velocidad usando la relacion entre
        %el incremento de cape y el incremento de velocidad ascencional
        else
        vvel(i+1)=0; %Si como resultado de la integracion obtengo velocidades negativas entonces hago que sean 0 y 
        %arranco nuevamente si es que en algun momento aparece de vuelta area postivia.  
        end
     end
     CAPE(j)=PA(j)+NA(j);
    end
end

%***********************************************************************************************************************
%CALCULO PARA LAS PARCELAS QUE DESCIENDEN (DNA, DPA, DCAPE Y VVEL DE DESCENSO)

for j=3:nlevels
    %Las unidades de w son en M/S

       DNA(j)=0; %DNA sera el "area negativa" para el DCAPE (parcelas con Trodif por encima de 0).
       DPA(j)=0; %DPA sera el "area positiva" para el DCAPE (parcelas con Trodif por debajo de 0 que aceleren la descendente).
     for i=j:-1:2

         if parametro == 1
        Trodifmedia=(Trodif2(i,j)+Trodif2(i-1,j))/2; %Diferencia de temperatura media entre 2 niveles sucesivos.
         end
         if parametro == 0
        Trodifmedia=(Tdif2(i,j)+Tdif2(i-1,j))/2; %Diferencia de temperatura media entre 2 niveles sucesivos. 
         end
         Pmedia=(Pentorno(i)+Pentorno(i-1))/2;       %Presion media que caracteriza a esa capa.
         Pinc=(-Pentorno(i)+Pentorno(i-1));           %Espesor de la capa entre 2 niveles sucesivos en Pascales.
         if (Trodifmedia < 0)   %Si la Dif de Tro media es negativa entonces esa capa la sumamos en el DPA
         DPA(j)=DPA(j)+Pinc*Rd*Trodifmedia/Pmedia;  
         end
         if (Trodifmedia > 0)    %Si la Dif de Tro media es positiva entonces aporta al DNA.
         DNA(j)=DNA(j)+Pinc*Rd*Trodifmedia/Pmedia;     %Calculamos usando la definicion de AREA NEGATIVA
         end
         
         %A partir de los calculos realizados calculo el perfil de velocidad vertical. En este caso me interesa calcular 
         %velocidades verticales de descenso unicamente.
         if((-vvel(i,j)^2+2*Pinc*Rd*Trodifmedia/Pmedia)<0) %Como obtengo la velocidad a partir de una variacion de energia
         %tengo problemas cuando la velocidad cambia de signo, porque si la velocidad es negativa y se hace mas negativa,
         %la energia cinetica de la parcela aumenta, aunque en realidad w disminuye. Por eso solo calculo velocidades por encima
         %de 0m/s (que por otra parte son las unicas que tienen sentido porque estamos suponiendo que la parcela asciende 
         %constantemente libre o forzada).
         %
         vvel(i-1,j)=-(abs(-vvel(i,j)^2+2*Pinc*Rd*Trodifmedia/Pmedia))^0.5;  %Calculo el incremento de velocidad usando la relacion entre
         %el incremento de cape y el incremento de velocidad ascencional
         else
         vvel(i-1)=0; %Si como resultado de la integracion obtengo velocidades negativas entonces hago que sean 0 y 
         %arranco nuevamente si es que en algun momento aparece de vuelta area postivia.  
         end
     DCAPE(j)=DPA(j)+DNA(j);
    
    end   
    
    

end




for i=1:nlevels
    for j=1:nlevels
        out(i,j)=vvel(i,j);
    end
end
for i=1:nlevels
    out(i,nlevels+1)=NA(i);
    out(i,nlevels+2)=PA(i);
    out(i,nlevels+3)=CAPE(i);
    out(i,nlevels+4)=DNA(i);
    out(i,nlevels+5)=DPA(i);
    out(i,nlevels+6)=DCAPE(i);
end
%***********************************************************************************************************************
%FIN DE LA INTERPOLACION
%***********************************************************************************************************************
