%**************************************************************************
%    ESTA FUNCION BUSCA QUE MODELOS PUEDO REMOVER DEL ESNEMBLE PARA 
%    CONSEGUIR N DATOS PREVIOS PARA REALIZAR LA CALIBRACION
%**************************************************************************
function [i_a ensemble]=test_arch_fun(ensemble,ndatos,ensmin)

%**************************************************************************
%           ensemble= ensemble contiene los pronosticos de precipitacion una
%           columna por cada uno de los modelos
%           ndatos = numero minimo de datos previos que vamos a usar
%           ensmin= numero minimo de miembros que tendra nuestro ensemble.
%           i_a= columnas que van a quedar
% La idea es considerar todos los modelos menos el modelo i y ver cuantas
% filas tienen al menos un dato nan. Para esas filas veamos cuantos datos
% no-nan tiene el modelo i. Es decir cuantos datos extra podria aportar el
% modelo i. Si el modelo i tiene datos nan donde otros modelos tienen datos
% es probable que aporte pocos datos. Los modelos que tienen nan donde los
% demas tienen datos aportan menor cantidad de datos. Estos modelos se van
% descartando y nuevamente chequeamos la cantidad de datos disponibles para
% verificar si podemos seguir adelante con el ensemble.
%**************************************************************************

    [filas columnas]=size(ensemble);

    %primero saco los miembros que no tienen el núermo de datos suficientes
    faltantes=sum(isnan(ensemble));
    resto=filas-faltantes-ndatos;

    i_a=find(resto>0); %Me quedo solo con las que tienen mas que ndatos no nan.
    
if(length(i_a)>= ensmin)
        temp=ensemble(:,i_a);
        clear ensemble
        ensemble=temp;
        clear temp 
   
        loop=0;
   while (loop==0)     

        
      if(length(find(any(isnan(ensemble),2)==0))> ndatos); %Tengo la cantidad de datos suficientes?
            loop=1; %Si es si entonces termina el loop aca.
            clear temp
            temp=ensemble(find(any(isnan(ensemble),2)==0),:);
            [temp1 temp2]=size(temp);
            clear ensemble
            ensemble=temp(temp1-ndatos:temp1,:); %Me quedo solo con los últimos ndatos elementos
            %de esta manera garantizo que todos los rank histograms se
            %construyan con la misma cantidad de datos.

      else
            clear i_a temp aporte
            loop=0; %Sino comienzo a sacar modelos buscando los que menos aportan.
         
            [filas columnas]=size(ensemble);
        for i_model=1:columnas
            temp=ensemble;
            temp(:,i_model)=1;
            i_nan=find(any(isnan(temp),2)==1);
            %Busco cuantos datos no nan me aporta cuanto alguno de los
            %otros es NaN.
            aporte(i_model)=length(find(isnan(ensemble(i_nan,i_model))==0));
            clear i_nan temp 
        end

        i_a=find(aporte > min(aporte)); %Voy a sacar el modelo que menos datos aporte.

        temp=ensemble(:,i_a);

        if(length(i_a)>=ensmin)
            
        clear ensemble
        ensemble=temp;
        clear temp
        
        else
             
            i_a=0;
            ensemble=[];
            return
            
        end
        
    end
   end
        
        
else 
        i_a=0;
        ensemble=[];
        return
end
    
    