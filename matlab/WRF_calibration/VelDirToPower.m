function [Power TotalPower]=VelDirToPower(Vel,Dir,Parque,MaskTurbine,Option)
%Esta funcion convierte valores de velocidad y direccion de viento en
%valores de potencia producidas por un parque eolico.
%INPUTS:
%Vel -> Es un vector conteniendo valores de velocidad de viento.
%Dir -> Es un vector conteniendo valores de direccion de viento.
%Turbines -> Es una estructura que contiene la relacion entre el viento y
%la potencia producida por todas las turbinas que componen el parque.
%Contiene ademas la informacion del parque (numero de turbinas, etc).
%MaskTurbines -> Es un array logico con dimension igual a la cantidad de
%turbinas presentes en el parque. Se utiliza para determinar si una
%determinada turbina esta prendida o apagada.
%Option -> Es un string que contiene alguna de las siguientes opciones:
%Mean, Mode, Median o Prct y que definen el tipo de ajuste que
%se hara entre los valores de viento y la potencia. 

%OUTPUTS:
%Power -> Es la potencia producida por producida por cada aerogenerador en cada plazo
%de pronostico (cada aerogenerador es una fila y cada plazo una columna del
%array.
%TotalPower es la potencia total producida por el parque en cada plazo de
%pronostico.

%TODO( IMPORTANTE ): El ajuste parametrico no esta funcionando
%correctamente porque no se guarda adecuadamente la info de la funcion en
%los archivos .mat. Hay que investigar esta cuestion o fijar la funcion de
%otra manera. 

NTurbine=length(Parque.Turbine);

NLead=size(Vel,2); %Cantidad de plazos de pronostico que tenemos que transformar a potencia.
NFor=size(Vel,1);  %Cantidad de pronosticos que vamos a traducir a potencia.

Power=NaN(NFor,NLead,NTurbine);

TotalPower=NaN(NFor,NLead);



for i=1:NTurbine
    
    switch Option
        case {'Mean'}
            %Obtengo para este caso, las matrices que me permiten obtener
            %la potencia como funcion de la intensidad de viento y de la
            %direccion.
            
            DirVel=Parque.Turbine(i).DirVelMean;    %El valor central de cada uno de los intervalos en los que dividimos el rango de velocidades.
            DirPow=Parque.Turbine(i).DirPowerMean;  %El valor de la potencia que corresponde a cada uno de los intervalos de velocidad.
            DirFit=Parque.Directions;                %El valor central de los intervalos en los que dividimos el rango de direcciones.
            
        case {'Mode'}
            %Obtengo para este caso, las matrices que me permiten obtener
            %la potencia como funcion de la intensidad de viento y de la
            %direccion.
            
            DirVel=Parque.Turbine(i).DirVelMode;
            DirPow=Parque.Turbine(i).DirPowerMode;
            DirFit=Parque.Directions;
            
        case {'Median'}
            %Obtengo para este caso, las matrices que me permiten obtener
            %la potencia como funcion de la intensidad de viento y de la
            %direccion.
            
            DirVel=Parque.Turbine(i).DirVelMedian;
            DirPow=Parque.Turbine(i).DirPowerMedian;
            DirFit=Parque.Directions;
            
        case {'Parametric'}
            %En este caso, como el ajuste es parametrico lo que obtengo son
            %los parametros de la funcion sigmoidea que ajustamos a la
            %curva viento-potencia.
            
            DirParametric=Parque.Turbine(i).DirParametric;
            DirFit=Parque.Directions;
            Fun=Parque.Turbine(i).DirFun;
            
    end  
    
    
    switch Option
        case{'Mean','Mode','Median'}
   
          %Tengo una curva de calibracion para cada una de las N
          %direcciones en las que dividimos el rango de direcciones. Segun
          %la direccion de cada plazo de pronostico tengo que seleccionar
          %la curva correspondiente.
         for ifor=1:NFor 
          for il=1:NLead
          
                DirDist=abs(Dir(ifor,il)-DirFit);
                [TmpMin MinLoc]=min(DirDist);  %Busco que indice corresponde al bin de direcciones al cual pertenece el pronostico actual para este plazo.
                
                TmpVel=DirVel{MinLoc};
                TmpPow=DirPow{MinLoc};
            
                %Uso interpolacion por Splines (podria ser tambien lineal)
                %para determinar el valor de potencia que corresponde al
                %valor de viento pronosticado en este plazo.
                Power(ifor,il,i)=interp1(TmpVel,TmpPow,Vel(ifor,il),'spline');
                       
          end
         end
            
            
        case{'Parametric'}
          %Tengo un conjunto de parametros para cada una de las N
          %direcciones en las que dividimos el rango de direcciones. Segun
          %la direccion de cada plazo de pronostico tengo que seleccionar
          %los parametros correspondients.  
         for ifor=1:NFor   
          for il=1:NLead
          
                DirDist=abs(Dir(ifor,il)-DirFit);
                [TmpMin MinLoc]=min(DirDist);  %Busco que indice corresponde al bin de direcciones al cual pertenece el pronostico actual para este plazo.
                
                TmpParametric=DirParametric{MinLoc};
                %TmpFun=Fun{MinLoc};
                
                TmpFun=@(x,vel)(x(1)*x(2)*exp(x(3)*vel))./(x(1)+x(2)*(exp(x(3)*vel)-1));
            
                %Uso un evaluador de la funcion que toma como input los
                %parametros del ajuste y la velocidad del viento.
                Power(ifor,il,i)=TmpFun(TmpParametric,Vel(ifor,il));
                       
          end     
         end
          
    end
    
end

for ifor=1:NFor
  for il=1:NLead
    TotalPower(ifor,il)=sum(squeeze(Power(ifor,il,:)).*MaskTurbine);
  end
end
    

return