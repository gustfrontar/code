function [J nablaJ]=j_and_gradj(x,x0b,y,H,invR,invB0,dt,bst,ntimes)
%function [J]=j_and_gradj(x,x0b,y,H,invR,invB0,dt,bst,ntimes)

%Esta funcion calcula para un dado x la funcion de costo de 4DVAR y su
%gradiente.


%X es la condicion inicial del estado al inicio de la ventana.
%y son las observaciones.
%H es el operador de las observaciones.
%invR es la matriz de covarianza de los errores de las observaciones.
%invB0 es la inversa de la matriz de covarianza de los errores del background en el tiempo
%0.
%dt es el paso de tiempo.
%ntimes es la cantidad de tiempos en la ventana de asimilacion.



J=(x'-x0b')'*invB0*(x'-x0b')+(y(1,:)'-H*(x'))'*invR*(y(1,:)'-H*(x'));

nablaJb=2*invB0*(x'-x0b');

nablaJo=H'*invR*(y(1,:)'-H*(x'));



TLM=eye(3);
for ii=1:ntimes
    
   %corro el modelo para obtener la solucion no lineal y poder calcular el
   %J.
   
   %Obtengo el tangente lineal y el adjunto.
   for it=1:bst
       [tlm,adm] = stepit_tl_ad(x,dt);
       x=stepit(x,dt);
       TLM=tlm*TLM;
   end
   d=invR*(y(ii+1,:)'-H*(x'));

   
   J=J+(y(ii+1,:)'-H*(x'))'*d;
   
   %Calculamos el adjunto para el paso ii.
  
   nablaJo=nablaJo+TLM'*H'*d;
    
end

nablaJ=nablaJb-2*nablaJo;